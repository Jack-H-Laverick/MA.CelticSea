
# Interpolate the exchange at the vertical boundary from NEMO-MEDUSA model output
# saveRDS("./Objects/vertical boundary/.")  # Marker so network script can see where the data is being saved too, it's buried in a function

#### Setup ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("tidyverse", "nemoRsem", "furrr", "ncdf4", "tictoc")                                 # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages
source("./R scripts/@_Region file.R")                                       # Define project region 

plan(multisession)

domain <- readRDS("./Objects/Domains.rds") %>%                              # Get the horizontal area to extract over 
  select(Shore) %>% 
  filter(Shore =="Offshore")

example <- list.files("I:/Science/MS-Marine/MA/CNRM_ssp370",            # File to pull dimensions from 
                      recursive = T, full.names = TRUE, pattern = "difvho")[5]

#### Create summary scheme to interpolate a depth layer over a given area #####

Bathymetry <- readRDS("./Objects/NE_grid.rds") %>%                          # Import NEMO-MEDUSA bathymetry
  st_drop_geometry() %>%                                                    # Drop sf geometry column 
  select(-c("x", "y"), latitude = Latitude, longitude = Longitude)          # Clean column so the bathymetry is joined by lat/lon

scheme <- scheme_interp_slice(get_spatial(example, depthdim = "depth"), SDepth, domain) # Get a scheme for linear interpolation between 2 depth layers

start <- scheme_to_start()                                                  # Get netcdf vectors which define the minimum
count <- scheme_to_count()                                                  # amount of data to import

scheme <- scheme_reframe(scheme) %>%                                        # Adjust scheme indices so they match the array subset
  left_join(Bathymetry) %>%                                                 # Attach bathymetry to summary scheme
  filter(depth < Bathymetry & SDepth < Bathymetry) %>%                      # Drop points where the target depth or next deeper layer are below the sea floor
  group_by(y, x) %>%                                                        # Redefine the group column as removing points can disrupt this
  mutate(group = cur_group_id()) %>%                                        # Create a new grouping column for the summary scheme
  ungroup()

summary <- filter(scheme, layer == 1) %>%                                   # Create the metadata to attach to summaries
  arrange(group) %>%                                                        # Summaries will be returned in group order, so make sure these match
  mutate(depth = SDepth) %>%                                                # Lets return the depth we interpolated to
  select(x, y, longitude, latitude, depth)                                  # As well as horizontal information

#### Extract ####

W_files <- rbind(categorise_files("I:/Science/MS-Marine/MA/CNRM_ssp370", recursive = TRUE),      # For projection runs
                 categorise_files("I:/Science/MS-Marine/MA/CNRM_ssp126", recursive = TRUE),      # From multiple SSPs
                 categorise_files("I:/Science/MS-Marine/MA/GFDL_ssp370", recursive = TRUE),     
                 categorise_files("I:/Science/MS-Marine/MA/GFDL_ssp126", recursive = TRUE),     
                 categorise_files("I:/Science/MS-Marine/MA/CNRM_hist", recursive = TRUE),        # and forcings from historical runs too
                 categorise_files("I:/Science/MS-Marine/MA/GFDL_hist", recursive = TRUE)) %>%   # Build metadata for each file
  drop_na() %>% 
  select(-Name) %>% 
  filter(Type %in% c( "difvho", "wo")) %>% 
  filter(Year > 2009) %>% 
  split(., f = list(paste(.$Month, .$Year, .$Forcing, .$SSP)))                  # Specify the timestep to average files over.
  
tic()
W_files %>%
  #.[1:12] %>%
  future_map(NEMO_ERSEM, analysis = "slabR",                         # Interpolate grid_W files in paralell
           out_dir = "./Objects/vertical boundary", scheme = scheme,
           start = start, count = count, summary = summary,
           collapse_days = FALSE, .progress = T)
toc() # 2.7 hours

#### check ####

# NE.01.2015 <- readRDS("./Objects/vertical boundary/NE.01.2015.rds") %>% 
#   mutate(day = rep(1:6, each = nrow(summary)))
# 
# ggplot(NE.01.2015) +
#   geom_raster(aes(x = x, y = y, fill = Vertical_diffusivity)) +
#   #geom_raster(aes(x = x, y = y, fill = Vertical_velocity)) +
#   facet_wrap(vars(day)) +
#   NULL
