
# Files contain daily mean of freshwater discharge and discharge of biogeochemical tracers
# These aren't necessarily paired, so we scale the total daily nutrient load by daily runoff, instead of pixel by pixel

#### Set up ####

rm(list=ls())                                                                   # Wipe the brain

packages <- c("tidyverse", "sf", "furrr", "terra", "ncdf4", "data.table", "tictoc") # List packages
lapply(packages, library, character.only = TRUE)                                # Load packages
source("./R scripts/@_Region file.R")                                           # Define project region 

#plan(sequential)                                                                # Choose the method to paralelise by with furrr
plan(multisession, workers = 3)                                                # Choose the method to paralelise by with furrr
                                                                                # The job is memory limited, so only use 2 workers (still halves the time)

all_files <- c(list.files("I:/Science/MS-Marine/MA/CNRM_hist/rivers/", recursive = TRUE, full.names = TRUE, pattern = ".nc"),
               list.files("I:/Science/MS-Marine/MA/GFDL_hist/rivers/", recursive = TRUE, full.names = TRUE, pattern = ".nc"),
               list.files("I:/Science/MS-Marine/MA/CNRM_ssp126/rivers/", recursive = TRUE, full.names = TRUE, pattern = ".nc"),
               list.files("I:/Science/MS-Marine/MA/CNRM_ssp370/rivers/", recursive = TRUE, full.names = TRUE, pattern = ".nc"),
               list.files("I:/Science/MS-Marine/MA/GFDL_ssp126/rivers/", recursive = TRUE, full.names = TRUE, pattern = ".nc"),
               list.files("I:/Science/MS-Marine/MA/GFDL_ssp370/rivers/", recursive = TRUE, full.names = TRUE, pattern = ".nc")) %>%
  as.data.frame() %>%                                                           # Turn the vector into a dataframe/tibble
  rename(value = 1) %>% 
  separate(value, into = c(NA, "year"), 
           remove = FALSE, sep = "_y") %>%                                      # Extract the year from the file name
  rename(file = "value") %>% 
  mutate(year = str_sub(year, end = -4),                                        # Drop file extension to get number
         forcing = case_when(str_detect(file, "CNRM") ~ "CNRM",
                             str_detect(file, "GFDL") ~ "GFDL",
                             T ~ NA),
         ssp = case_when(str_detect(file, "hist") ~ "hist",
                         str_detect(file, "ssp126") ~ "ssp126",
                         str_detect(file, "ssp370") ~ "ssp370",
                         T ~ NA))

domains <- readRDS("./Objects/Domains.rds")                                     # Import domain polygon

raw <- nc_open("I:/Science/MS-Marine/MA/Hindcast/rivers/domain_cfg_zps.closea.compressed.nc")

lat <- ncvar_get(raw, "nav_lat") %>% 
  reshape2::melt(varnames = c("x", "y"), value.name =  "Latitude")

lon <- ncvar_get(raw, "nav_lon") %>% 
  reshape2::melt(varnames = c("x", "y"), value.name =  "Longitude")

nc_close(raw)

grid <- left_join(lat, lon) 

#### Check domain overlaps point estimates of river runoff ####

buffer <- st_buffer(domains, 50000) %>% 
  st_union() %>% 
  st_sf(Domain = TRUE) %>% 
  st_transform(crs = 4326)

example <- rast(all_files[1,1], subds = "rorunoff", lyrs = 1) %>%               # Get an example set of points to plot
  as.data.frame(xy = TRUE, na.rm = FALSE) %>%                                   # Drop empty cells
  left_join(grid) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  st_join(buffer) %>% 
  drop_na(Domain)
  
ggplot() +
#   geom_sf(data = river_expansion, fill = "orange", colour = "orange") +       # Uncomment me once you've built your extra polygons.
   geom_sf(data = buffer, fill = "red", colour = "red") +
   geom_sf(data = example, aes(colour = rorunoff_1), size = 0.2) +
  theme_minimal() +
   labs(caption = "Check domain overlaps point estimates of river runoff") 

ggsave_map("./Figures/saltless/check.04.1.png", plot = last_plot())

#### Create larger domain polygon ####

domains <- readRDS("./Objects/Domains.rds") %>%                                 # Import original domain polygon
  st_union() %>%                                                                # Join inshore and offshore zone
#  st_union(river_expansion) %>%                                                # Expand the polygon to catch distributed river run off 
#  nngeo::st_remove_holes() %>%                                                 # Remove holes
  st_sf() %>%                                                                   # reinstate class
  mutate(Keep = T) %>%    
  st_make_valid() %>% 
  st_buffer(50000)                                                              # A buffer was easier for the Norwegian basin than a polygon extension

#### Work out which pixels are in the model domain ####

coords <- example %>% 
  st_transform(sf::st_crs(domains)) %>%                                         # Transform points to the same crs as domain polygon
  st_join(domains) %>%                                                          # Check which points are in the domain
  drop_na(Keep) %>%                                                             # Drop points outside the domain
  select(x, y) %>% 
  st_drop_geometry()

#### Extract data ####

tic()
rivers <- future_pmap(all_files, function(file, year, forcing, ssp){

# index <- 3
# file <- all_files$file[index] ; year <- all_files$year[index]
# forcing <- all_files$forcing[index] ;ssp <- all_files$ssp[index]

# Processing the data like this avoids costly calls to pivot_longer
  
nutrients <- rast(file, subds = c("ronh4", "rono3")) %>%                        # Extract nutrient data
  as.data.frame(na.rm = T, xy = T) %>%                                          # Convert to XY dataframe of non-empty pixels
  right_join(coords) %>%                                                        # Limit to pixels of interest 
  select(-x, -y) %>%                                                            # Don't need spatial information anymore
  as.matrix() %>%                                                               # Fast daily summary            
  colSums(na.rm = TRUE)                                                         # Total Kg of water in the model domain

gc()

nutrients <- (nutrients/14.0067) * 1e3                                          # Now go from grams of Nitrogen to mols, then millimols
nutrients <- nutrients*86400                                                    # Scaled to daily from per seconds 

runoff <- rast(file, subds = c("rorunoff","dA")) %>%                            # Extract river runoff
    as.data.frame(na.rm = T, xy = T) %>%                                        # Convert to XY dataframe of non-empty pixels
    right_join(coords) %>% 
    mutate(across(starts_with("rorunoff"), ~ .x * dA)) %>%                      # scale runoff to cell area
    select(-dA, -x, -y) %>%                                                     # Don't need spatial information anymore
    as.matrix() %>%                                                             # Fast daily summary
    colSums(na.rm = TRUE)                                                       # Total G per second of Nitrogen in the model domain

gc()

days <- length(runoff)                                                         # There are leap years in some files, so don't just assume 365

data <- data.frame(Year = year,                                                 # Combine variables
                   Day = 1:days,             
                   Forcing = forcing,
                   SSP = ssp,
                   Runoff = (runoff*86400)/1e3) %>%                             # scaled to daily from per seconds, and Kg to m^3
  mutate(NH4 = nutrients[1:days]/Runoff,                                        # and then per unit water to get a concentration
         NO3 = nutrients[(days+1):(days*2)]/Runoff)                             # So grams of Nitrogen per m^3
  
  }, .progress =TRUE) %>%                        
  data.table::rbindlist() %>%                                                   # Bind each year,
  mutate(Date = as.Date(as.numeric(Day)-1, origin = str_glue("{Year}-01-01")))  # Add a date column
toc() # 45 minutes

saveRDS(rivers, "./Objects/NE River input.rds")

#### Plot ####

ggplot(data = rivers) +
  geom_area(aes(x = Date, y = Runoff), fill = "blue") +
  facet_grid(rows = vars(Forcing), cols = vars(SSP)) +
  theme_minimal() +
  labs(y = expression("Freshwater input (M"^{3}*".D"^{-1}*")"), 
       subtitle = "NEMO-ERSEM input of freshwater discharge from rivers into model domain",
       caption = "Did we generate a time series?") +
  NULL

ggsave("./Figures/saltless/check.04.4.png", dpi = 500, width = 18, height = 10 , units = "cm", bg = "white")

ggplot(data = rivers) +
  geom_area(aes(x = Date, y = NH4), fill = "darkgreen") +
  facet_grid(rows = vars(Forcing), cols = vars(SSP)) +
  theme_minimal() +
  labs(y = expression("Ammonium concentration (mmolN.M"^{-3}*")"), 
       subtitle = "NEMO-ERSEM nutrient concentration of freshwater discharge into model domain",
       caption = "Did we generate a time series?") +
  NULL

ggsave("./Figures/saltless/check.04.5.png", dpi = 500, width = 18, height = 10 , units = "cm", bg = "white")

ggplot(data = rivers) +
  geom_area(aes(x = Date, y = NO3), fill = "darkgreen") +
  facet_grid(rows = vars(Forcing), cols = vars(SSP)) +
  theme_minimal() +
  labs(y = expression("Nitrate concentration (mmolN.M"^{-3}*")"), 
       subtitle = "NEMO-ERSEM nutrient concentration of freshwater discharge into model domain",
       caption = "Did we generate a time series?") +
  NULL

ggsave("./Figures/saltless/check.04.6.png", dpi = 500, width = 18, height = 10 , units = "cm", bg = "white")


#mean(filter(rivers, Year == "2000")$NO3)

