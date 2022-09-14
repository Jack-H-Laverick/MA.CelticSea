
## Overwrite example boundary data

#### Setup ####

rm(list=ls())                                                               # Wipe the brain

library(MiMeMo.tools)

Boundary_template <- read.csv("./StrathE2E/Celtic_Sea_NM/2003-2013/Driving/chemistry_CELTIC_SEA_2003-2013.csv")  # Read in example boundary drivers

#### Last minute data manipulation ####

My_boundary_data<- readRDS("./Objects/Boundary measurements.rds") %>%                        # Import data
  filter(between(Year, 2003, 2013)) %>%                                                      # Limit to reference period
  group_by(Month, Compartment, Variable) %>%                                                 # Average across years
  summarise(Measured = mean(Measured, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(Month) %>%                                                                         # Order months ascending
  mutate(Compartment = factor(Compartment, levels = c("Inshore S", "Offshore S", "Offshore D"),
                              labels = c("Inshore S" = "SI", "Offshore S" = "SO", "Offshore D" = "D"))) %>%
  pivot_wider(names_from = c(Compartment, Variable), names_sep = "_", values_from = Measured) # Spread columns to match template

My_DIN_fix <- readRDS("./Objects/Ammonia to DIN.rds")

My_atmosphere <- readRDS("./Objects/Atmospheric N deposition.rds") %>% 
  filter(between(Year, 2003, 2013)) %>%                                                      # Limit to reference period
  group_by(Month, Oxidation_state, Shore,  Year) %>%
  summarise(Measured = sum(Measured, na.rm = T)) %>%                                         # Sum across deposition states 
  summarise(Measured = mean(Measured, na.rm = T)) %>%                                        # Average over years
  ungroup() %>% 
  pivot_wider(names_from = c(Shore, Oxidation_state), values_from = Measured) %>%            # Spread to match template
  arrange(Month)                                                                             # Order months ascending

#### Create new file ####

Boundary_new <- mutate(Boundary_template, 
                       SO_nitrate = My_boundary_data$SO_DIN * (1-filter(My_DIN_fix, Depth_layer == "Shallow")$Proportion), # Multiply DIN by the proportion of total DIN as nitrate
                       SO_ammonia = My_boundary_data$SO_DIN * filter(My_DIN_fix, Depth_layer == "Shallow")$Proportion, # Multiply DIN by the proportion of total DIN as ammonium
                       SO_phyt = My_boundary_data$SO_Phytoplankton,
                       SO_detritus = My_boundary_data$SO_Detritus,
                       D_nitrate = My_boundary_data$D_DIN * (1-filter(My_DIN_fix, Depth_layer == "Deep")$Proportion), # Multiply DIN by the proportion of total DIN as nitrate
                       D_ammonia = My_boundary_data$D_DIN * filter(My_DIN_fix, Depth_layer == "Deep")$Proportion, # Multiply DIN by the proportion of total DIN as ammonium
                       D_phyt = My_boundary_data$D_Phytoplankton,
                       D_detritus = My_boundary_data$D_Detritus,
                       SI_nitrate = My_boundary_data$SI_DIN * (1-filter(My_DIN_fix, Depth_layer == "Shallow")$Proportion), # Multiply DIN by the proportion of total DIN as nitrate
                       SI_ammonia = My_boundary_data$SI_DIN * filter(My_DIN_fix, Depth_layer == "Shallow")$Proportion, # Multiply DIN by the proportion of total DIN as ammonium
                       SI_phyt = My_boundary_data$SI_Phytoplankton, 
                       SI_detritus = My_boundary_data$SI_Detritus,
                       ## Rivers
                       # RIV_nitrate = My_river_N$Nitrate,     
                       # RIV_ammonia = My_river_N$Ammonia,          
                       # RIV_detritus = 0,
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
                       # DO_nitrate = My_overhang$DIN *
                       #   (Boundary_template$D_intrate/(Boundary_template$D_intrate + Boundary_template$D_ammonia)),    # Multiply DIN by the proportion of total DIN as nitrate
                       # DO_ammonia	= My_overhang$DIN *
                       #   (Boundary_template$D_ammonia/(Boundary_template$D_intrate + Boundary_template$D_ammonia)),
                       # DO_detritus = My_overhang$Detritus
) %>% 
  select(-c(SO_ammona, D_intrate))                                          # Fix Mike's typos

write.csv(Boundary_new, file = "./StrathE2E/Celtic_Sea_NM/2003-2013/Driving/chemistry_CELTIC_SEA_2003-2013.csv", row.names = F)
