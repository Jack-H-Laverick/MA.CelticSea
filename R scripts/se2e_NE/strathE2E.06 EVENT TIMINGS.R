
## Overwrite the entries in the example Celtic Sea event timing parameters file

#### Setup ####

rm(list=ls())                                                               # Wipe the brain
library(tidyverse)
library(sf)
source("./R scripts/@_Region file.R")

Events <- read.csv(stringr::str_glue("./StrathE2E/{implementation}/2010-2019/Param/event_timing_CELTIC_SEA_2003-2013.csv")) # Read in example Physical drivers

#### Last minute data manipulation ####

area <- readRDS("./Objects/Domains.rds") %>%                            # Calculate surface area
  st_transform(crs= 4326) %>% 
  st_area() %>% 
  as.numeric(.)/1e6                                                     # Convert m^2 to km^2 

#### Update event timings file ####

# Events[1,"Value"] <- 0 # Pelagic_fish_spawning_start_day
# Events[2,"Value"] <- 0 # Pelagic_fish_spawning_duration_(days)
# Events[3,"Value"] <- 0 # Pelagic_fish_recruitment_start_day
# Events[4,"Value"] <- 0 # Pelagic_fish_recruitment_duration_(days)

# Events[5,"Value"] <- 0 # Demersal_fish_spawning_start_day
# Events[6,"Value"] <- 0 # Demersal_fish_spawning_duration_(days)
# Events[7,"Value"] <- 0 # Demersal_fish_recruitment_start_day
# Events[8,"Value"] <- 0 # Demersal_fish_recruitment_duration_(days)

# Events[9,"Value"] <- 0 # Filt/dep_benthos_spawning_start_day
# Events[10,"Value"] <- 0 # Filt/dep_benthos_spawning_duration_(days)
# Events[11,"Value"] <- 0 # Filt/dep_benthos_recruitment_start_day
# Events[12,"Value"] <- 0 # Filt/dep_benthos_recruitment_duration_(days)

# Events[13,"Value"] <- 0 # Carn/scav_benthos_spawning_start_day
# Events[14,"Value"] <- 0 # Carn/scav_benthos_spawning_duration_(days)
# Events[15,"Value"] <- 0 # Carn/scav_benthos_recruitment_start_day
# Events[16,"Value"] <- 0 # Carn/scav_benthos_recruitment_duration_(days)

# Events[17,"Value"] <- 0 # Migratory_fish_switch_(0=off_1=on)
# Events[18,"Value"] <- 0 # Migratory_fish_ocean_biomass_(Tonnes_wet_weight)
# Events[19,"Value"] <- 0 # Migratory_fish_carbon_to_wet_weight_(g/g)
 Events[20,"Value"] <- sum(area) # Model_domain_sea_surface_area_(km2)
# Events[21,"Value"] <- 0 # Propn_of_ocean_population_entering_model_domain_each_year
# Events[22,"Value"] <- 0 # Imigration_start_day
# Events[23,"Value"] <- 0 # Imigration_end_day_(must_be_later_than_start_day_even_if_migration_disabled)
# Events[24,"Value"] <- 0 # Propn_of_peak_popn_in_model_domain_which_remains_and_does_not_emigrate
# Events[25,"Value"] <- 0 # Emigration_start_day
# Events[26,"Value"] <- 0 # Emigration_end_day_(must_be_later_than_start_day_even_if_migration_disabled)

write.csv(Events,
          file = stringr::str_glue("./StrathE2E/{implementation}/2010-2019/Param/event_timing_{toupper(implementation)}_2010-2019.csv"), 
          row.names = F)
