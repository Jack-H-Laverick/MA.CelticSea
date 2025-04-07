# 
# ## Make any manual adjustments to the model necessary to stop it keeling over
# 
# #### Setup ####
# 
# rm(list=ls())                                                               # Wipe the brain
# 
# library(MiMeMo.tools)
# source("./R scripts/@_Region file.R")
# 
# #### Perform manual corrections ####
# 
# walk2(seq(2020, 2090, by = 10),
#       seq(2029, 2099, by = 10), ~{
#         
# #### Fix boundary file ####
# 
# read.csv(stringr::str_glue("./StrathE2E/Models/{implementation}/{as.character(.x)}-{as.character(.y)}/Driving/physics_{toupper(implementation)}_{as.character(.x)}-{as.character(.y)}.csv")) %>% 
#   mutate(SO_SI_flow = 0.01) %>%                   # NEMO-MEDUSA struggled to find a meaningful value because of spatial resolution.
#   write.csv(row.names = F,
#             file = stringr::str_glue("./StrathE2E/Models/{implementation}/{as.character(.x)}-{as.character(.y)}/Driving/physics_{toupper(implementation)}_{as.character(.x)}-{as.character(.y)}.csv"))
# 
#       }, quiet = FALSE)
