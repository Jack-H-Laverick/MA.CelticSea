
# readRDS("./Objects/NE_Days/.")    # Marker so network script can see where the data is coming from

#### Set up ####

rm(list=ls())                                                               # Wipe the brain
Packages <- c("tidyverse", "nemoRsem", "data.table", "furrr")                         # List packages
lapply(Packages, library, character.only = TRUE)                                # Load packages
source("./R scripts/@_Region file.R")                                       # Define project region 
plan(multisession)                                                          # Choose the method to parallelise by with furrr

Transects <- readRDS("./Objects/Boundary_transects.rds")                    # Import transects to sample at

#### Summarise along transects ####

Summary <- list.files("./Objects/NE_Days/", full.names = T) %>%              # Get the names of all data files
   future_map(NE_boundary_summary, Transects, 
              vars = c("NO3", "NH4", "Detritus", "Diatoms", "Other_phytoplankton"), .progress = T) # Sample NE output along domain boundary

#### Save water exchanges between compartments ####

Flows <- map(Summary, `[[`, 1) %>%                                          # Subset the summary results
  data.table::rbindlist() %>% 
  saveRDS("./Objects/H-Flows.rds")                                          

#### Save boundary conditions ####

Boundary <- map(Summary, `[[`, 2) %>%                                       # Subset the summary results
  data.table::rbindlist()
saveRDS(Boundary, "./Objects/Boundary measurements.rds")                         

# ggplot(Boundary) + geom_line(aes(x= Date, y = Measured, colour = Forcing, linetype = SSP), alpha = 0.5) +
#   facet_grid(rows = vars(Variable), cols = vars(Compartment), scales = "free_y") +
#   theme_minimal() +
#   labs(y = "Measured at ocean boundary", caption = "Average NEMO-MEDUSA outputs along our model perimeter") +
#   theme(legend.position = "top")
# 
# ggsave("./Figures/flows/Boundary variables.png", last_plot(), dpi = 500, width = 18, height = 10, units = "cm", bg = "white")

ggplot(Boundary) + geom_line(aes(x= Date, y = Measured, colour = Forcing, linetype = SSP), alpha = 0.5) +
  facet_grid(rows = vars(Variable), cols = vars(Compartment), scales = "free_y") +
  theme_minimal() +
  labs(y = "Measured at ocean boundary", caption = "Average NEMO-ERSEM outputs along our model perimeter") +
  theme(legend.position = "top")

ggsave("./Figures/flows/Boundary variables.png", last_plot(), dpi = 500, width = 18, height = 10, units = "cm", bg = "white")

#### Last minute reformatting for compiler ####

My_boundary_data<- readRDS("./Objects/Boundary measurements.rds") %>%                        # Import data
  mutate(Compartment = factor(Compartment, levels = c("Inshore S", "Offshore S", "Offshore D"),
                              labels = c("Inshore S" = "SI", "Offshore S" = "SO", "Offshore D" = "D"))) %>%
  select(-Shore, -slab_layer) %>%
  pivot_wider(names_from = c(Compartment, Variable), names_sep = "_", values_from = Measured) %>%   # Spread columns to match template
  mutate(SO_phyt = SO_Diatoms + SO_Other_phytoplankton,
         SI_phyt = SI_Diatoms + SI_Other_phytoplankton,
         D_phyt = D_Diatoms + D_Other_phytoplankton) %>%
  saveRDS("./Objects/Boundary measurements.rds")
