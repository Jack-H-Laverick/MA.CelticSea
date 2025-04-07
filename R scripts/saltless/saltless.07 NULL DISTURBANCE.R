
# Create User define habitat disturbance object

#### Set up ####

rm(list=ls())

library(tidyverse)

user_pdist <- expand.grid(Month = month.name, 
                          Habitat = c("Silt", "Sand", "Gravel"), 
                          Shore = c("Inshore", "Offshore")) %>% 
  mutate(Disturbance = case_when(Shore =="Inshore" ~ 3.266419e-4,
                                 Shore =="Offshore" ~ 	9.619828e-8))

saveRDS(user_pdist, "./Objects/Habitat disturbance.rds")

