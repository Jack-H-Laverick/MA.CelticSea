
## Overwrite example boundary data

#### Setup ####

rm(list=ls())                                                               # Wipe the brain

library(MiMeMo.tools)
source("./R scripts/@_Region file.R")

Boundary_template <- read.csv(stringr::str_glue("./StrathE2E/{implementation}/2010-2019/Driving/chemistry_CELTIC_SEA_2003-2013.csv"))  # Read in example boundary drivers


##!! data fix

# My_boundary_data<- readRDS("./Objects/Boundary measurements.rds") %>%                        # Import data
#   mutate(Compartment = factor(Compartment, levels = c("Inshore S", "Offshore S", "Offshore D"),
#                               labels = c("Inshore S" = "SI", "Offshore S" = "SO", "Offshore D" = "D"))) %>%
#   select(-Shore, -slab_layer) %>%
#   pivot_wider(names_from = c(Compartment, Variable), names_sep = "_", values_from = Measured) %>%   # Spread columns to match template
#   mutate(SO_phyt = SO_Diatoms + SO_Other_phytoplankton,
#          SI_phyt = SI_Diatoms + SI_Other_phytoplankton,
#          D_phyt = D_Diatoms + D_Other_phytoplankton) %>% 
#   saveRDS("./Objects/Boundary measurements.rds")

## Iterate over different time periods ##

decades <- data.frame(Start = seq(2010, 2060, by = 10),                         # Which time periods are we buiding driving data for?
                      Stop = seq(2019, 2069, by = 10)) %>% 
  rowid_to_column()

runs <- expand.grid(Force = c("GFDL", "CNRM"), S = c("ssp370", "ssp126"),   # Get a combination of forcings and SSPs
                    rowid = decades$"rowid") %>%                                # For each decade we are extracting
  left_join(decades) %>%                                                        # Add the beginning and end for each time period 
  select(-rowid) %>% 
  data.frame()
  
pmap(runs, safely(function(Force, S, Start, Stop, Boundary_template){

#Start <- 2020 ; Stop <- 2029 ; Force <- "GFDL" ; S <- "ssp370"       

#### Last minute data manipulation ####

My_boundary_data<- readRDS("./Objects/Boundary measurements.rds") %>% # Import data
  filter(between(Year, Start, Stop), Forcing == Force, SSP !=S) %>%   # Limit to outputs from a specific run and time
  group_by(Month) %>%                                                 # Average across years
  summarise(across(D_NO3:D_phyt, ~ mean(.x, na.rm = T))) %>% 
  ungroup() %>% 
  arrange(Month)                                                                           # Order months ascending

# ## To get Mike a monthly transient
# My_boundary_data <- readRDS("./Objects/Boundary measurements.rds") %>% # Import data
#   group_by(Year, Month, Forcing, SSP) %>%                                                 # Average across days
#   summarise(across(D_NO3:D_phyt, ~ mean(.x, na.rm = T))) %>%
#   ungroup() %>%
#   arrange(Month) %>%                                                                        # Order months ascending
#   saveRDS(stringr::str_glue("./StrathE2E/{implementation}/2010-2019/Driving/chemistry_transient.rds"))


# My_overhang <- readRDS("./Objects/overhang exchanges.rds") %>% 
#   filter(between(Year, Start, Stop), Forcing == Force, SSP %in% c("hist", S), Direction == "Upwelling") %>%                            # Limit to reference period 
#   group_by(Month) %>%                                                                      # Average across years
#   summarise(NO3 = mean(NO3, na.rm = T),
#             NH4 = mean(NH4, na.rm = T),
#             Detritus = mean(Detritus, na.rm = T)) %>%  
#   ungroup() %>% 
#   arrange(Month)                                                                           # Order months ascending

My_river_N <- readRDS("./Objects/NE River input.rds") %>%   
  filter(between(Year, Start, Stop), Forcing == Force, SSP !=S) %>%                          # Limit to outputs from a specific run and time
  mutate(Month = lubridate::month(Date)) %>% 
  group_by(Month) %>%                                                                        # Average across years
  summarise(NO3 = mean(NO3, na.rm = T),
            NH4 = mean(NH4, na.rm = T)) %>%  
  ungroup() %>% 
  arrange(Month)                                                                             # Order months ascending

My_atmosphere <- readRDS("./Objects/Atmospheric N deposition.rds") %>% 
  filter(between(Year, Start, Stop), SSP !=S) %>%                                            # Limit to outputs from a specific run and time
  group_by(Month, Oxidation_state, Shore,  Year) %>%
  summarise(Measured = sum(Measured, na.rm = T)) %>%                                         # Sum across deposition states 
  summarise(Measured = mean(Measured, na.rm = T)) %>%                                        # Average over years
  ungroup() %>% 
  pivot_wider(names_from = c(Shore, Oxidation_state), values_from = Measured) %>%            # Spread to match template
  arrange(Month)                                                                             # Order months ascending

#### Create new file ####

Boundary_new <- rename(Boundary_template, SO_ammonia = SO_ammona, D_nitrate = D_intrate) %>% # Fix Mike's typos
                mutate(SO_nitrate = My_boundary_data$SO_NO3,
                       SO_ammonia = My_boundary_data$SO_NH4,
                       SO_phyt = My_boundary_data$SO_phyt,
                       SO_detritus = My_boundary_data$SO_Detritus,
                       D_nitrate = My_boundary_data$D_NO3, 
                       D_ammonia = My_boundary_data$D_NH4, 
                       D_phyt = My_boundary_data$D_phyt,
                       D_detritus = My_boundary_data$D_Detritus,
                       SI_nitrate = My_boundary_data$SI_NO3,
                       SI_ammonia = My_boundary_data$SI_NH4,
                       SI_phyt = My_boundary_data$SI_phyt, 
                       SI_detritus = My_boundary_data$SI_Detritus,
                       ## Rivers
                       RIV_nitrate = My_river_N$NO3,     
                       RIV_ammonia = My_river_N$NH4,          
                       RIV_detritus = 0,
                       ## Atmosphere, daily deposition as monthly averages
                       SO_ATM_nitrate_flux = My_atmosphere$Offshore_O,
                       SO_ATM_ammonia_flux = My_atmosphere$Offshore_R,
                       SI_ATM_nitrate_flux = My_atmosphere$Inshore_O,
                       SI_ATM_ammonia_flux = My_atmosphere$Inshore_R, 
                       SI_other_nitrate_flux = 0,   # Can be used for scenarios
                       SI_other_ammonia_flux = 0,
                       SO_other_nitrate_flux = 0,   # Can be used for scenarios
                       SO_other_ammonia_flux = 0,
                       # ## Overhang
                       #  DO_nitrate = My_overhang$NO3,
                       #  DO_ammonia	= My_overhang$NH4,
                       #  DO_detritus = My_overhang$Detritus
                       ) 
 
# For the filters above it is easier to do != to an ssp, so you keep the historical run. This means we need to flip the label when saving the file
 ssp_label <- case_when(S =="ssp370" ~ "ssp126",
                        S =="ssp126" ~ "ssp370")
 
write.csv(Boundary_new, file = stringr::str_glue("./StrathE2E/{implementation}/2010-2019/Driving/chemistry_{toupper(implementation)}_{as.character(Start)}-{as.character(Stop)}-{Force}-{ssp_label}.csv"), row.names = F)

     }), Boundary_template)
