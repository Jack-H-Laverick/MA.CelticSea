
## Sometimes the water exchanges fail to balance, this script checks for this and performs a correction

rm(list=ls())                                                                     # Wipe the brain
library(MiMeMo.tools)
source("./R scripts/@_Region file.R")

Files <- list.files(str_glue("./StrathE2E/{implementation}/2010-2019"), recursive = TRUE, full.names = TRUE, pattern = "physics")
Drivers <- map(Files, read.csv)

balance_check <- function(physics_driver, W = 1 ) physics_driver$SO_SI_flow*W + physics_driver$SI_OceanIN < physics_driver$SI_OceanOUT

map(Drivers, balance_check, W = 0.134)    # TRUE == a problem!

Adjust_drivers <- function(physics_driver, W = 1 ) {

#physics_driver <- Drivers[[1]] ; W = 0.134
#physics_driver <- Fixed[[1]] ; W = 0.134
  
  ratio <- physics_driver$SI_OceanIN/(physics_driver$SI_OceanIN + physics_driver$SI_OceanOUT)   # Inflows as a ratio of ocean in and out flows

  check <- physics_driver$SO_SI_flow*W + physics_driver$SI_OceanIN - physics_driver$SI_OceanOUT # If everything balances this should be 0
  
  adjustments <- data.frame(Balance = check,                                    # How far off are we
                           OUT = check * (1-ratio)) %>%                         # Using the ratio of ocean ins and out, calculate a share of the imbalance to correct using outflows
    mutate(IN = (Balance - OUT)*-1,                                             # Adjust the inflows by what is left of the imbalance after correcting outflows
           check = (OUT-IN) - Balance) %>%                                      # Check the adjustments sum to the initial imbalance, should be 0
    mutate(RECHECK = physics_driver$SO_SI_flow*W + 
                     (physics_driver$SI_OceanIN + IN) - 
                     (physics_driver$SI_OceanOUT + OUT)) %>%                    # StrathE2E can correct a positive imbalance but not a negative one
      mutate(IN = ifelse(RECHECK > 0, 0,                                        # If there is insufficient inflow still (due to precision error)
                         IN+1e-14))                                             # add a tiny bit of inflow.

  physics_driver$SI_OceanIN <- physics_driver$SI_OceanIN + adjustments$IN 
  physics_driver$SI_OceanOUT <- physics_driver$SI_OceanOUT + adjustments$OUT 

return(physics_driver)
  
}

Fixed <- map(Drivers, Adjust_drivers, W = 0.134)

map(Fixed, balance_check, W = 0.6)  # TRUE == problem! # Increasing W can remove some of the TRUEs. You edit this in the physical parameters file.

#### Commit by overwriting the driving files ####

map2(Fixed, Files, write.csv, row.names = F)

## Manually changed well mixedness parameter to 0.6 to get things working.

