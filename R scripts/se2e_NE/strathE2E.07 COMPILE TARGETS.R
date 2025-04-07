
## Add target data 

library(tidyverse)
source("./R scripts/@_Region file.R")

#### Annual target data ####

annual_template <- read.csv(stringr::str_glue("./StrathE2E/{implementation}/2010-2019/Target/annual_observed_CELTIC_SEA_2003-2013.csv"))  # Read in example boundary drivers

targets <- read.csv(stringr::str_glue("./Objects/fitting/PP_target_{implementation}.csv"))

annual_new <- mutate(annual_template, Annual_measure = case_when(Description == targets$Description ~ targets$Annual_measure,
                                                                 T ~ Annual_measure),
                                      SD_of_measure = case_when(Description == targets$Description ~ targets$SD_of_measure,
                                                                 T ~ SD_of_measure), # Add SD
                     Use1_0 = if_else(Description == targets$Description, 1, Use1_0)) # switch on 

write.csv(annual_new, file = stringr::str_glue("./StrathE2E/{implementation}/2010-2019/Target/annual_observed_{toupper(implementation)}_2010-2019.csv"), row.names = F)

#### Monthly target data ####

targets <- read.csv(stringr::str_glue("./Objects/fitting/CHLa_target_{implementation}.csv"))

monthly_template <- read.csv(stringr::str_glue("../Celtic Sea/Data/Celtic_Sea_ERSEM_4/2003-2013/Target/monthly_observed_CELTIC_SEA_2003-2013.csv")) %>%
filter(!Variable %in% targets$Variable)

monthly_new <- bind_rows(monthly_template, targets) 

write.csv(monthly_new, file = stringr::str_glue("./StrathE2E/{implementation}/2010-2019/Target/monthly_observed_{toupper(implementation)}_2010-2019.csv"), row.names = F)

