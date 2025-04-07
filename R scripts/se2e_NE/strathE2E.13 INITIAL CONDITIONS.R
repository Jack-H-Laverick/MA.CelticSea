
#### Run the 4 variants to steady state for the baseline time period

library(StrathE2E2)
library(MiMeMo.tools)
source("./R scripts/@_Region file.R")

runs <- expand.grid(Force = c("GFDL", "CNRM"), S = c("ssp370", "ssp126"), Years = 200)

pmap(runs, safely(function(Force, S, Years){
  
model <- e2e_read(implementation, str_glue("2010-2019-{Force}-{S}"), models.path = "StrathE2E/", results.path = "StrathE2E/Results/")

results <- e2e_run(model,nyears = Years)                                # Run the model

png(filename=str_glue("./StrathE2E/IC_2010_2019_{Force}_{S}.png"), width = 30, height = 20, units = "cm", res = 500)
e2e_plot_ts(model, results)                                          # Have we reached a steady state?
dev.off()

#### Update starting conditions ####

e2e_extract_start(model, results, csv.output = TRUE)                # Update starting conditions to the end of a simulation

file.rename(stringr::str_glue("./StrathE2E/{implementation}/2010-2019-{Force}-{S}/Param/initial_values-base.csv"),
            stringr::str_glue("./StrathE2E/{implementation}/2010-2019-{Force}-{S}/Param/initial_values_{toupper(implementation)}_2010-2019-{Force}-{S}.csv"))
 
#unlink(stringr::str_glue("./StrathE2E/{implementation}/2010-2019-{Force}-{S}/Param/initial_values_{toupper(implementation)}_2010-2019.csv"))

## Update set up file

Setup_file <- read.csv(stringr::str_glue("./StrathE2E/{implementation}/2010-2019-{Force}-{S}/MODEL_SETUP.csv"))

Setup_file[4,1] <- stringr::str_glue("initial_values_{toupper(implementation)}_2010-2019-{Force}-{S}.csv")

write.csv(Setup_file,
          file = stringr::str_glue("./StrathE2E/{implementation}/2010-2019-{Force}-{S}/MODEL_SETUP.csv"),
          row.names = F) 

}))

#### Check things updated ####

# model <- e2e_read(implementation, str_glue("2010-2019-CNRM-ssp126"), models.path = "StrathE2E/", results.path = "StrathE2E/Results/")
# 
# results <- e2e_run(model,nyears = 5)                                 # Run the model
# 
# e2e_plot_ts(model, results)                                          # Should start from a steady state
