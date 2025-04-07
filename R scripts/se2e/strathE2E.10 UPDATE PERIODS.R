
## Create model variants for new time periods

rm(list=ls())                                                                  # Wipe the brain
library(MiMeMo.tools)
library(StrathE2E2)
source("./R scripts/@_Region file.R")

#### Rinse decades for climate change updates ####

map2(seq(2020, 2090, by = 10),
     seq(2029, 2099, by = 10), ~{
       
       R.utils::copyDirectory(str_glue("./StrathE2E/Models/{implementation}/2010-2019"),          
                              str_glue("./StrathE2E/Models/{implementation}/{as.character(.x)}-{as.character(.y)}"))          
       
     })

walk2(seq(2020, 2090, by = 10),
      seq(2029, 2099, by = 10), ~{
        
        update_boundary_period_MA(.x, .y, str_glue("./StrathE2E/Models/{implementation}/{as.character(.x)}-{as.character(.y)}"), island = FALSE)
        update_physics_period_MA(.x, .y, str_glue("./StrathE2E/Models/{implementation}/{as.character(.x)}-{as.character(.y)}"), ice = FALSE, island = FALSE)
        
        # Change the set up file to point to new driving data
        
        Setup_file <- read.csv(stringr::str_glue("./StrathE2E/Models/{implementation}/{as.character(.x)}-{as.character(.y)}/MODEL_SETUP.csv"))
        
        Setup_file[2,1] <- stringr::str_glue("physics_{toupper(implementation)}_{as.character(.x)}-{as.character(.y)}.csv")
        Setup_file[3,1] <- stringr::str_glue("chemistry_{toupper(implementation)}_{as.character(.x)}-{as.character(.y)}.csv")
        
        write.csv(Setup_file,
                  file = stringr::str_glue("./StrathE2E/Models/{implementation}/{as.character(.x)}-{as.character(.y)}/MODEL_SETUP.csv"),
                  row.names = F)
      })

