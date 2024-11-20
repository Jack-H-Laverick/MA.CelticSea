
# Calculate absolute landings by gear and guild across EU flags

#### Set up ####

rm(list=ls())                                                                 # Wipe the brain

packages <- c("tidyverse", "exactextractr", "raster", "sf", "furrr")          # List handy packages
lapply(c(packages), library, character.only = TRUE)                           # Load packages

source("./R scripts/@_Region file.R")                                         # Get region mask

plan(multisession)                                                            # Set up parallel processing

domain <- st_transform(readRDS("./Objects/Domains.rds"), crs = 4326) %>%      # reproject to match EU data
  st_union() %>%                                                              # Create whole domain shape 
  st_as_sf() %>% 
  mutate(Keep = T)

landings_polygon <- read_sf("../Shared data/FDI_spatial_data_EU28/EU28/landings_pol/")

#### Detect which c-squares are in the model domain ####

EU_domain <- st_intersects(domain, landings_polygon, sparse = F) %>%       # Which EU polygons are near the model domain?
  t() %>%                                                                     # Transpose to indicate row not columns
  landings_polygon[.,] %>%                                                         # Subset the Eu data spatially
  rownames_to_column(var = "landings_polygon") 

EU_landings <- str_glue("../Shared data/FDI_spatial_data_EU28/EU28/spatial_landings_tableau_pts_{2014:2019}_EU28.csv") %>%   # Programmatically build file names
  future_map(~{ read.csv(.x) %>%                                                # Import each EU landings file
                dplyr::select(year, gear_type, cscode, species, totwghtlandg,	totvallandg) %>% # Drop some columns, totwghtlandg is "total weight landed", totvallandg is "total value landed"
                filter(cscode %in% EU_domain$cscode)},                          # Limit to stat squares near our model domain
            .progress = T) %>%                         
  bind_rows() %>% 
  rename(Gear_code = gear_type)

ggplot() +
  geom_sf(data = EU_domain) +
  geom_sf(data = domain)

#### Save out gear list for agregation ####

EU_gears <- group_by(EU_landings, Gear_code, year) %>% 
  summarise(totwghtlandg = sum(totwghtlandg)) %>% 
  summarise(totwghtlandg = mean(totwghtlandg)) %>% 
  arrange(totwghtlandg) %>% 
  left_join(read.csv("../Shared data/FDI_spatial_data_EU28/STECF gear codes.csv")) %>% 
  mutate(Gear_name = factor(Gear_name, Gear_name))
  
ggplot(EU_gears) +
  geom_col(aes(x = totwghtlandg, y = Gear_name), fill = "cadetblue", colour = "white") +
  theme_minimal()+
  labs(y = NULL, x = "Total landed weight (tonnes)",
       subtitle = "Fishing gears active in the model domain according to STECF")

ggsave("./Figures/fish/STECF reported gears.png", width = 21, height = 10, dpi = 700, units = "cm", bg = "white")

#### Scale EU landings by the proportion of fishing effort according to GFW in the model domain ####

tictoc::tic()
weights <- dplyr::select(EU_Arctic, EU_polygon, Gear_type) %>%                # Limit to information needed to calculate the proportion of fishing effort in the model domain
  split(f = as.factor(as.numeric(.$EU_polygon))) %>%                          # Isolate each shape for fast parallel processing
  future_map( ~{                                                              # In parallel
    mutate(.x, total = if_else(Gear_type == "Mobile",                         # If this is a mobile gear
                               exact_extract(GFW_mobile, .x, fun = "sum"),    # Get all mobile fishing effort from GFW, else static effort
                               exact_extract(GFW_static, .x, fun = "sum"))) %>% # This is the total effort to scale features to within a polygon
    st_intersection(domain) %>%                                               # Crop the polygons to the model domain
    mutate(feature = if_else(Gear_type == "Mobile",                           # Now count fishing effort again
                             as.numeric(exact_extract(GFW_mobile, ., fun = "sum")),       
                             as.numeric(exact_extract(GFW_static, ., fun = "sum")))) %>%  
    st_drop_geometry()}, .progress = T) %>%                                   # Drop geometry for a non-spatial join
  data.table::rbindlist() %>%                                                 # Bind into a dataframe
  mutate(GFW_Scale = feature/total) %>%                                       # Get the proportion of effort per polygon in the domain
  replace_na(list(GFW_Scale = 1)) %>%                                         # If there was no GFW activity in the polygon replace NA with 1 to not use this for scaling
  dplyr::select(GFW_Scale, EU_polygon)
tictoc::toc()

#### Convert EU landings to a matrix by guild and gear ####

corrected_landings <- st_drop_geometry(EU_Arctic) %>%
  left_join(weights) %>% 
  mutate(corrected_weight = ttwghtl * GFW_Scale) %>%                          # Scale features effort per gear by the proportion of GFW activity by gear type in the model domain 
  group_by(Aggregated_gear, Guild, year) %>% 
  summarise(corrected_weight = sum(corrected_weight, na.rm = TRUE)) %>%       # Total within years by guild and gear
  ungroup() %>% 
  right_join(landings_target) %>%                                             # Reintroduce unrepresented combinations of gear, guild, and year
  filter(Aggregated_gear != "Dropped") %>%                                    # Ditch the unneeded gear class
  replace_na(replace = list(corrected_weight = 0)) %>%                        # Nas are actually landings of 0
  group_by(Aggregated_gear, Guild) %>% 
  summarise(corrected_weight = mean(corrected_weight, na.rm = TRUE)) %>%      # Average across years 
  ungroup() %>% 
  pivot_wider(names_from = Aggregated_gear, values_from = corrected_weight) %>% # Spread dataframe to look like a matrix
  column_to_rownames('Guild') %>%                                             # Remove character column
  as.matrix() %>%                                                             # Convert to matrix
  .[order(row.names(.)), order(colnames(.))]                                  # Alphabetise rows and columns

saveRDS(corrected_landings, "./Objects/EU landings by gear and guild.rds")

heatmap(corrected_landings)
