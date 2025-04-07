
## Visualise summarised nemo-medusa model output

#### Set up ####

rm(list=ls(all.names = TRUE))                                               # Wipe the brain

packages <- c("tidyverse", "nemoRsem", "furrr", "tictoc")                       # List packages
lapply(packages, library, character.only = TRUE)                                # Load packages
plan(multisession)                                                          # Instructions for parallel processing

TS <- readRDS("./Objects/TS.rds") #%>% 
#  filter(Year < 2051)

    # Read in time series
vars_ts <- c("NO3_avg", "NH4_avg", 
             "Diatoms_avg", "Other_phytoplankton_avg", 
             "Detritus_avg", "Temperature_avg")                                 # Variables to plot   

SP <- readRDS("./Objects/SPATIAL.rds") %>%                                      # Read in spatial data
  map(drop_na, Shore)

vars_sp <- str_remove(vars_ts, "_avg") %>%                                  # Tweak the variable names for spatial plots
  c("Speed")
  
#### Plotting ####
    
walk(vars_ts, ts_plot)                                                      # Save a time series figure for each variable.

future_map2(rep(SP, each = length(vars_sp)),                                # For each decade
            rep(vars_sp, times = length(SP)), point_plot,                   # And each variable
            .progress = TRUE)                                               # Plot spatial maps in parallel

#ggplot(SP[["2010.S"]]) +
#  geom_raster(aes(x=x, y=y, fill = Shore, alpha = Bathymetry != 0))
