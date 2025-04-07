
library(MiMeMo.tools)
source("./R scripts/@_Region file.R")

#### Copy in Mikes initial conditions - get by starting with Ascension island making a couple of manual tweaks ####

# file.copy("./Data/Norwegian_Basin_Mike/Norwegian_Basin/2010-2019/Param/initial_values-NORWEGIAN_BASIN_2010-2019.csv",
#           str_glue("./StrathE2E/Norwegian_Basin/2010-2019-{ssp}/Param/initial_values-NORWEGIAN_BASIN_2010-2019.csv"))
# 
# Setup_file <- read.csv(stringr::str_glue("./StrathE2E/{implementation}/2010-2019-{ssp}/MODEL_SETUP.csv"))
# 
# Setup_file[4,1] <- "initial_values-NORWEGIAN_BASIN_2010-2019.csv"
# 
# write.csv(Setup_file,
#           file = stringr::str_glue("./StrathE2E/{implementation}/2010-2019-{ssp}/MODEL_SETUP.csv"),
#           row.names = F)

## Mike 2

file.copy("./Data/Mike2/initial_values-NORWEGIAN_BASIN_2010-2019.csv",
          str_glue("./StrathE2E/Norwegian_Basin/2010-2019-{ssp}/Param/initial_values-NORWEGIAN_BASIN_2010-2019.csv"))

Setup_file <- read.csv(stringr::str_glue("./StrathE2E/{implementation}/2010-2019-{ssp}/MODEL_SETUP.csv"))

Setup_file[4,1] <- "initial_values-NORWEGIAN_BASIN_2010-2019.csv"

write.csv(Setup_file,
          file = stringr::str_glue("./StrathE2E/{implementation}/2010-2019-{ssp}/MODEL_SETUP.csv"),
          row.names = F)

#### Fix model by making sure offshore overhang isn't the only offshore habitat ####

Physical_parameters <- read.csv(stringr::str_glue("./StrathE2E/{implementation}/2010-2019-{ssp}/Param/physical_parameters_{toupper(implementation)}.csv"))

Physical_parameters[9,"Value"] <- 0.001  # Area_proportion_of_offshore_rock_habitat_d0_(sum_of_all_8_habitat_areas_must=1)
Physical_parameters[13,"Value"] <- (Physical_parameters[13,"Value"] - 0.001) # Area_proportion_of_deep_ocean_boundary_(false_seabed)_(sum_of_all_8_seabed_habitat_areas_plus_deep_ocean_boundary_must=1)

#### Change the proportion of the layer depth available to photosynthesis ####

Physical_parameters[66,"Value"] <- 0.5  # offshore
Physical_parameters[67,"Value"] <- (Physical_parameters[13,"Value"] - 0.115) # inshore
Physical_parameters[68,"Value"] <- (Physical_parameters[13,"Value"] - 0.115) # inshore macrophytes

write.csv(Physical_parameters,
          file = stringr::str_glue("./StrathE2E/{implementation}/2010-2019-{ssp}/Param/physical_parameters_{toupper(implementation)}.csv"), 
          row.names = F)

#### Update half-saturation coefficients based on box thickness ####

file.copy("./Data/Mike2/fitted_uptake_mort_rates_NORWEGIAN_BASIN_2010-2019.csv",
          str_glue("./StrathE2E/Norwegian_Basin/2010-2019-{ssp}/Param/fitted_uptake_mort_rates_NORWEGIAN_BASIN_2010-2019.csv"))

Setup_file <- read.csv(stringr::str_glue("./StrathE2E/{implementation}/2010-2019-{ssp}/MODEL_SETUP.csv"))

Setup_file[9,1] <- "fitted_uptake_mort_rates_NORWEGIAN_BASIN_2010-2019.csv"

write.csv(Setup_file,
          file = stringr::str_glue("./StrathE2E/{implementation}/2010-2019-{ssp}/MODEL_SETUP.csv"),
          row.names = F)

## Turn this off in the model fitting process 

read.csv("./StrathE2E/Norwegian_Basin/2010-2019-ssp370/Param/control/optimize_ecology.csv") %>% 
  mutate(Value = case_when(str_detect(Description...these.can.all.be.edited.DURING.a.run.and.have.immediate.effect, 
                                      "half_saturation_coefficients") ~ 0,
                            T ~ Value)) %>% 
  rename(`Description - these can all be edited DURING a run and have immediate effect` = Description...these.can.all.be.edited.DURING.a.run.and.have.immediate.effect) %>% 
  write.csv("./StrathE2E/Norwegian_Basin/2010-2019-ssp370/Param/control/optimize_ecology.csv", row.names = FALSE)

#### Put nominal fishing in offshore rock to avoid bug in the package ####

file.copy("./Data/Mike2/fishing_distribution_NORWEGIAN_BASIN_2010-2019.csv",
          str_glue("./StrathE2E/Norwegian_Basin/2010-2019-{ssp}/Param/fishing_distribution_NORWEGIAN_BASIN_2010-2019.csv"), overwrite = TRUE)

#### Turn off fishing by setting activity to 0 for now ####

file.copy("./Data/Norwegian_Basin_Mike/Norwegian_Basin/2010-2019/Param/fishing_activity_NORWEGIAN_BASIN_2010-2019.csv",
          str_glue("./StrathE2E/Norwegian_Basin/2010-2019-{ssp}/Param/fishing_activity_NORWEGIAN_BASIN_2010-2019.csv"), overwrite = TRUE)




