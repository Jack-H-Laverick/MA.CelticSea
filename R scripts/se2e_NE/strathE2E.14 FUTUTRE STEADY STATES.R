
## Using the initial conditions at steady state for the previous decade gets us the 2050s, but we still error in the 60s and onwards 

library(MiMeMo.tools)
library(StrathE2E2)
source("./R scripts/@_Region file.R")

decades <- data.frame(Start = seq(2020, 2060, by = 10),                         # Which time periods are we buiding driving data for?
                      Stop = seq(2029, 2069, by = 10)) %>% 
  rowid_to_column()

runs <- expand.grid(Force = c("GFDL", "CNRM"), S = c("ssp370", "ssp126"),       # Get a combination of forcings and SSPs
                    rowid = decades$"rowid") %>%                                # For each decade we are extracting
  left_join(decades) %>%                                                        # Add the beginning and end for each time period 
  select(-rowid) %>% 
  data.frame() %>% 
  mutate(Years = 50)

pmap(runs, safely(function(Force, S, Start, Stop, Years){
  
        file.copy(stringr::str_glue("./StrathE2E/{implementation}/{as.character(Start-10)}-{as.character(Stop-10)}-{Force}-{S}/Param/initial_values_{toupper(implementation)}_{as.character(Start-10)}-{as.character(Stop-10)}-{Force}-{S}.csv"),
                  stringr::str_glue("./StrathE2E/{implementation}/{as.character(Start)}-{as.character(Stop)}-{Force}-{S}/Param")) 
        
        ## Update set up file
        
        Setup_file <- read.csv(stringr::str_glue("./StrathE2E/{implementation}/{as.character(Start)}-{as.character(Stop)}-{Force}-{S}/MODEL_SETUP.csv"))
        
        Setup_file[4,1] <- stringr::str_glue("initial_values_{toupper(implementation)}_{as.character(Start-10)}-{as.character(Stop-10)}-{Force}-{S}.csv")
        
        write.csv(Setup_file,
                  file = stringr::str_glue("./StrathE2E/{implementation}/{as.character(Start)}-{as.character(Stop)}-{Force}-{S}/MODEL_SETUP.csv"),
                  row.names = F)
        
        #### Run to steady state ####        
        
        model <- e2e_read(implementation, str_glue("{as.character(Start)}-{as.character(Stop)}-{Force}-{S}"),
                           models.path = "StrathE2E/", results.path = "StrathE2E/Results/")
        
        results <- e2e_run(model,nyears = Years)
        
        png(filename=str_glue("./StrathE2E/IC_{Start}_{Stop}_{Force}_{S}.png"), width = 30, height = 20, units = "cm", res = 500)
        e2e_plot_ts(model, results)                                             # Have we reached a steady state?
        dev.off()
        
        #### Update starting conditions ####
        
        e2e_extract_start(model, results, csv.output = TRUE)                    # Update starting conditions to the end of a simulation
        
        file.rename(stringr::str_glue("./StrathE2E/{implementation}/{as.character(Start)}-{as.character(Stop)}-{Force}-{S}/Param/initial_values-base.csv"),
                    stringr::str_glue("./StrathE2E/{implementation}/{as.character(Start)}-{as.character(Stop)}-{Force}-{S}/Param/initial_values_{toupper(implementation)}_{as.character(Start)}-{as.character(Stop)}-{Force}-{S}.csv"))
        
        unlink(stringr::str_glue("./StrathE2E/{implementation}/{as.character(Start)}-{as.character(Stop)}-{Force}-{S}/Param/initial_values_{toupper(implementation)}{as.character(Start-10)}-{as.character(Stop-10)}-{Force}-{S}.csv"))
        unlink(stringr::str_glue("./StrathE2E/{implementation}/{as.character(Start)}-{as.character(Stop)}-{Force}-{S}/Param/initial_values-test.csv"))
        
        ## Update set up file
        
        Setup_file <- read.csv(stringr::str_glue("./StrathE2E/{implementation}/{as.character(Start)}-{as.character(Stop)}-{Force}-{S}/MODEL_SETUP.csv"))
        
        Setup_file[4,1] <- stringr::str_glue("initial_values_{toupper(implementation)}_{as.character(Start)}-{as.character(Stop)}-{Force}-{S}.csv")
        
        write.csv(Setup_file,
                  file = stringr::str_glue("./StrathE2E/{implementation}/{as.character(Start)}-{as.character(Stop)}-{Force}-{S}/MODEL_SETUP.csv"),
                  row.names = F)
        
      }, quiet = FALSE))

#### Check things updated ####

# model <- e2e_read(implementation,str_glue("2040-2049-CNRM-ssp126"), models.path = "StrathE2E/", results.path = "StrathE2E/Results/")
# 
# results <- e2e_run(model,nyears = 10)                                # Run the model
# 
# e2e_plot_ts(model, results)                                          # Should start from a steady state

