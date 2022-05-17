
# Create an object defining the geographic extent of the model domain

#### Set up ####

rm(list=ls())                                                   

Packages <- c("tidyverse", "sf", "stars", "rnaturalearth")                  # List handy packages
lapply(Packages, library, character.only = TRUE)                            # Load packages
source("./R scripts/@_Region file.R")                                       # Define project region 

world <- ne_countries(scale = "medium", returnclass = "sf") %>%             # Get a world map
  st_transform(crs = crs)                                                   # Assign polar projection

Domain <- readRDS("./Objects/Celtic Sea Habitats.rds") %>% 
  mutate(Shore = ifelse(str_detect(Habitat, "S"), "Inshore", "Offshore")) 

#### Convert habitats object to domain ####

Offshore <- Domain %>% 
  filter(Shore == "Offshore") %>% 
  st_union() %>% 
  .[2] %>% 
  nngeo::st_remove_holes() %>% 
  st_sf(Shore = "Offshore") %>% 
  rename(geometry = ".")

Inshore <- Domain %>% 
  filter(Shore == "Inshore") %>% 
  group_by(Shore) %>% 
  summarise()

Domain <- bind_rows(Inshore, Offshore) %>% 
  st_make_valid() %>% 
  mutate(area = as.numeric(st_area(.)),
         Elevation = exactextractr::exact_extract(raster::raster("../Shared data/GEBCO_2020.nc"), ., "mean")) %>% # nb, Inshore mean depth is deeper than 60 m because of deep areas close to shore.
  st_transform(crs)

  saveRDS(Domain, "./Objects/Domains.rds")
