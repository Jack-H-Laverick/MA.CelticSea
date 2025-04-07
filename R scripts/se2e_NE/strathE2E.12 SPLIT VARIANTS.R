
## Create model variants for new time periods

rm(list=ls())                                                                  # Wipe the brain
library(MiMeMo.tools)
library(StrathE2E2)
source("./R scripts/@_Region file.R")

#### Copy first variant ####

decades <- data.frame(Start = seq(2010, 2060, by = 10),                         # Which time periods are we buiding driving data for?
                      Stop = seq(2019, 2069, by = 10)) %>% 
  rowid_to_column()

runs <- expand.grid(Force = c("GFDL", "CNRM"), S = c("ssp370", "ssp126"),   # Get a combination of forcings and SSPs
                    rowid = decades$"rowid") %>%                                # For each decade we are extracting
  left_join(decades) %>%                                                        # Add the beginning and end for each time period 
  select(-rowid) %>% 
  data.frame()

pmap(runs, function(Force, S, Start, Stop){
  
R.utils::copyDirectory(stringr::str_glue("./StrathE2E/{implementation}/2010-2019/"), # Copy 
                       stringr::str_glue("./StrathE2E/{implementation}/{as.character(Start)}-{as.character(Stop)}-{Force}-{S}/")) # Into
})

#### Rinse decades to change the set up file to read one time period, delete others ####

pmap(runs, safely(function(Force, S, Start, Stop){
  
        # Change the set up file to point to new driving data
        
        Setup_file <- read.csv(stringr::str_glue("./StrathE2E/{implementation}/{as.character(Start)}-{as.character(Stop)}-{Force}-{S}/MODEL_SETUP.csv"))
        
        Setup_file[2,1] <- stringr::str_glue("physics_{toupper(implementation)}_{as.character(Start)}-{as.character(Stop)}-{Force}-{S}.csv")
        Setup_file[3,1] <- stringr::str_glue("chemistry_{toupper(implementation)}_{as.character(Start)}-{as.character(Stop)}-{Force}-{S}.csv")

        write.csv(Setup_file,
                  file = stringr::str_glue("./StrathE2E/{implementation}/{as.character(Start)}-{as.character(Stop)}-{Force}-{S}/MODEL_SETUP.csv"),
                  row.names = F)
        
        # Delete other files for this variant
        
        unwanted_files <- list.files(str_glue("./StrathE2E/{implementation}/{as.character(Start)}-{as.character(Stop)}-{Force}-{S}/Driving/"),
                                     full.names = TRUE) %>% 
          .[-grep(str_glue("{as.character(Start)}-{as.character(Stop)}-{Force}-{S}.csv"), .)]                                         # Drop files which start this year
        
        unlink(unwanted_files)                                                  # Delete the rest
        
      }))
