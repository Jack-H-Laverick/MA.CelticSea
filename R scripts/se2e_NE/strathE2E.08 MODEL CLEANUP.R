
library(MiMeMo.tools)
source("./R scripts/@_Region file.R")

# Remove the files which have been replaced by ones for the new region
unlink(stringr::str_glue("./StrathE2E/{implementation}/2010-2019/Driving/chemistry_CELTIC_SEA_2003-2013.csv"))
unlink(stringr::str_glue("./StrathE2E/{implementation}/2010-2019/Param/physical_parameters_CELTIC_SEA.csv"))
unlink(stringr::str_glue("./StrathE2E/{implementation}/2010-2019/Driving/physics_CELTIC_SEA_2003-2013.csv"))     # Delete old file
unlink(stringr::str_glue("./StrathE2E/{implementation}/2010-2019/Param/event_timing_CELTIC_SEA_2003-2013.csv"))

# unlink(stringr::str_glue("./StrathE2E/{implementation}/2010-2019-{ssp}/Param/fishing_fleet_CELTIC_SEA_2003-2013.csv"))
# unlink(stringr::str_glue("./StrathE2E/{implementation}/2010-2019-{ssp}/Param/fishing_activity_CELTIC_SEA_2003-2013.csv"))
# unlink(stringr::str_glue("./StrathE2E/{implementation}/2010-2019-{ssp}/Param/fishing_power_CELTIC_SEA.csv"))
# unlink(stringr::str_glue("./StrathE2E/{implementation}/2010-2019-{ssp}/Param/fishing_discards_CELTIC_SEA.csv"))
# unlink(stringr::str_glue("./StrathE2E/{implementation}/2010-2019-{ssp}/Param/fishing_processing_CELTIC_SEA.csv"))
# unlink(stringr::str_glue("./StrathE2E/{implementation}/2010-2019-{ssp}/Param/fishing_distribution_CELTIC_SEA.csv"))
# unlink(stringr::str_glue("./StrathE2E/{implementation}/2010-2019-{ssp}/Param/fishing_gear_multiplier.csv"))
# 
# unlink(stringr::str_glue("./StrathE2E/{implementation}/2010-2019-{ssp}/Target/annual_observed_CELTIC_SEA_2003-2013.csv"))
# unlink(stringr::str_glue("./StrathE2E/{implementation}/2010-2019-{ssp}/Target/monthly_observed_CELTIC_SEA_2003-2013.csv"))

# Update file which tells StrathE2E where to find driving files

Setup_file <- read.csv(stringr::str_glue("./StrathE2E/{implementation}/2010-2019/MODEL_SETUP.csv")) # Read in example Physical drivers

Setup_file[1,1] <- stringr::str_glue("physical_parameters_{toupper(implementation)}.csv")
Setup_file[2,1] <- stringr::str_glue("physics_{toupper(implementation)}_2010-2019-CNRM-ssp126.csv")
Setup_file[3,1] <- stringr::str_glue("chemistry_{toupper(implementation)}_2010-2019-CNRM-ssp126.csv")
Setup_file[5,1] <- stringr::str_glue("event_timing_{toupper(implementation)}_2010-2019.csv")

# Setup_file[11,1] <- stringr::str_glue("fishing_fleet_{toupper(implementation)}_2010-2019.csv")
# Setup_file[12,1] <- stringr::str_glue("fishing_activity_{toupper(implementation)}_2010-2019.csv")
# Setup_file[13,1] <- stringr::str_glue("fishing_power_{toupper(implementation)}_2010-2019.csv")
# Setup_file[14,1] <- stringr::str_glue("fishing_discards_{toupper(implementation)}_2010-2019.csv")
# Setup_file[15,1] <- stringr::str_glue("fishing_processing_{toupper(implementation)}_2010-2019.csv")
# Setup_file[16,1] <- stringr::str_glue("fishing_distribution_{toupper(implementation)}_2010-2019.csv")
# Setup_file[17,1] <- stringr::str_glue("fishing_gear_multiplier_{toupper(implementation)}_2010-2019.csv")
# 
# Setup_file[23,1] <- stringr::str_glue("annual_observed_{toupper(implementation)}_2010-2019.csv")
# Setup_file[24,1] <- stringr::str_glue("monthly_observed_{toupper(implementation)}_2010-2019.csv")

write.csv(Setup_file,
          file = stringr::str_glue("./StrathE2E/{implementation}/2010-2019/MODEL_SETUP.csv"),
          row.names = F)
