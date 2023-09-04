
## Overwrite example driving data (boundaries and physics)

#### Setup ####

rm(list=ls())                                                               # Wipe the brain

library(MiMeMo.tools)

Physics_template <- read.csv("./StrathE2E/Celtic_Sea_NM/2010-2019/Driving/physics_CELTIC_SEA_2003-2013.csv") # Read in example Physical drivers

Depths <- read.csv("./StrathE2E/Celtic_Sea_NM/2010-2019/Param/physical_parameters_CELTIC_SEA.csv", nrows = 3)# Import Mike's original depths for scaling.

#### Last minute data manipulation ####

My_scale <- readRDS("./Objects/Domains.rds") %>%                            # Calculate the volume of the three zones
  sf::st_drop_geometry() %>% 
  mutate(S = c(T, T),
         D = c(F, T)) %>% 
  gather(key = "slab_layer", value = "Exists", S, D) %>% 
  filter(Exists == T) %>%
  mutate(Elevation = c(Depths[3,1], Depths[1,1], Depths[2,1])) %>%          # Pulls Mike's original depths instead of GEBCO 
  mutate(Volume = area * abs(Elevation)) %>% 
  dplyr::select(Shore, slab_layer, Volume)

My_light <- readRDS("./Objects/Air temp and light.rds") %>% 
  filter(between(Year, 2010, 2019), grepl("Light", Type)) %>%               # Limit to reference period and variable
  group_by(Month) %>%                                                       # Average across months
  summarise(Measured = mean(Measured, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(Month)                                                            # Order to match template

My_H_Flows <- readRDS("./Objects/H-Flows.rds") %>% 
  filter(between(Year, 2010, 2019)) %>%                                     # Limit to reference period
  group_by(across(-c(Year, Flow))) %>%                                      # Group over everything except year and variable of interest
  summarise(Flow = mean(Flow, na.rm = T)) %>%                               # Average flows by month over years
  ungroup() %>% 
  left_join(My_scale) %>%                                                   # Attach compartment volumes
  mutate(Flow = Flow/Volume) %>%                                            # Scale flows by compartment volume
  mutate(Flow = abs(Flow * 86400)) %>%                                      # Multiply for total daily from per second, and correct sign for "out" flows
  arrange(Month)                                                            # Order by month to match template

My_V_Diff <- readRDS("./Objects/vertical diffusivity.rds") %>%
  filter(between(Year, 2010, 2019)) %>%                                     # Limit to reference period
  group_by(Month) %>% 
  summarise(V_diff = mean(Vertical_diffusivity, na.rm = T)) %>% 
  ungroup() %>% 
  bind_rows(data.frame(Month = c(12,1,2), V_diff = NA)) %>%                 # Replace months with no data (introduces -Infs) 
  arrange(Month) %>%                                                        # Order by month to allow interpolation
  rbind(., .) %>%                                                           # Need to repeat the data as the points to interpolate are at the end of the cycle
  mutate(V_diff = zoo::na.spline(V_diff, na.rm = FALSE)) %>%                # Interpolate by cubic spline
  slice(12:23) %>%                                                          # Retrieve the completed cycle
  arrange(Month)                                                            # Order by month to match template
  
My_volumes <- readRDS("./Objects/TS.rds") %>% 
  filter(between(Year, 2010, 2019)) %>%                                     # Limit to reference period
  group_by(Compartment, Month) %>%                                          # By compartment and month
  summarise(across(Salinity_avg:Ice_conc_avg, mean, na.rm = T)) %>%         # Average across years for multiple columns
  ungroup() %>% 
  arrange(Month)                                                            # Order by month to match template

My_Rivers <- readRDS("./Objects/River volume input.rds") %>% 
  filter(between(Year, 2010, 2019)) %>%                                     # Limit to reference period
  group_by(Month) %>% 
  summarise(Runoff = mean(Runoff, na.rm = T)) %>%                           # Average by month across years
  ungroup() %>% 
  arrange(as.numeric(Month))                                                # Order by month to match template

#### Create new file ####

Physics_new <- mutate(Physics_template, SLight = My_light$Measured,
                      ## Flows, should be proportions of volume per day
                      SO_OceanIN = filter(My_H_Flows, slab_layer == "S", Shore == "Offshore", Neighbour == "Ocean", Direction == "In")$Flow,
                      D_OceanIN = filter(My_H_Flows, slab_layer == "D", Shore == "Offshore", Neighbour == "Ocean", Direction == "In")$Flow,
                      SI_OceanIN = filter(My_H_Flows, slab_layer == "S", Shore == "Inshore", Neighbour == "Ocean", Direction == "In")$Flow,
                      SI_OceanOUT = filter(My_H_Flows, slab_layer == "S", Shore == "Inshore", Neighbour == "Ocean", Direction == "Out")$Flow,
                      SO_SI_flow = filter(My_H_Flows, slab_layer == "S", Shore == "Offshore", Neighbour == "Inshore", Direction == "Out")$Flow,
                      # ## log e transformed suspended particulate matter concentration in zones
                      # SO_LogeSPM = log(filter(My_SPM, Shore == "Offshore")$SPM),  
                      # SI_LogeSPM = log(filter(My_SPM, Shore == "Inshore")$SPM),
                      ## Temperatures in volumes for each zone
                      SO_temp = filter(My_volumes, Compartment == "Offshore S")$Temperature_avg,
                      D_temp = filter(My_volumes, Compartment == "Offshore D")$Temperature_avg,
                      SI_temp = filter(My_volumes, Compartment == "Inshore S")$Temperature_avg ,
                      ## River inflow,
                      Rivervol_SI = My_Rivers$Runoff / filter(My_scale, Shore == "Inshore")$Volume, # Scale as proportion of inshore volume
                      ## Vertical diffusivity
                      log10Kvert = log10(My_V_Diff$V_diff),
                      ## Daily proportion disturbed by natural bed shear stress
                      # habS1_pdist = filter(My_Stress, Shore == "Inshore", Habitat == "Silt")$Disturbance, 
                      # habS2_pdist = filter(My_Stress, Shore == "Inshore", Habitat == "Sand")$Disturbance,
                      # habS3_pdist = filter(My_Stress, Shore == "Inshore", Habitat == "Gravel")$Disturbance,
                      # habD1_pdist = filter(My_Stress, Shore == "Offshore", Habitat == "Silt")$Disturbance,
                      # habD2_pdist = filter(My_Stress, Shore == "Offshore", Habitat == "Sand")$Disturbance,
                      # habD3_pdist = filter(My_Stress, Shore == "Offshore", Habitat == "Gravel")$Disturbance, 
                      # ## Monthly mean significant wave height inshore                     
                      # Inshore_waveheight = My_Waves$Waves,
                      # ## Overhang variables
                      # D_SO_upwelling = My_V_Flows$Upwelling,
                      # SO_D_downwelling = My_V_Flows$Downwelling, 
                      # DO_log10Kvert = log10(My_overhang_diffusivity$V_diff),
                      # DO_mixLscale = 0.9,
                      # DO_D_upwelling = filter(My_overhang_velocity, Direction == "Upwelling")$Flow,
                      # D_DO_downwelling = filter(My_overhang_velocity, Direction == "Downwelling")$Flow
) 
                     
write.csv(Physics_new, file = "./StrathE2E/Celtic_Sea_NM/2010-2019/Driving/physics_CELTIC_SEA_2010-2019.csv", row.names = F)
