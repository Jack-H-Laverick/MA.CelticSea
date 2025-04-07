
library(StrathE2E2)
library(MiMeMo.tools)
source("./R scripts/@_Region file.R")

model <- e2e_read(implementation,str_glue("2010-2019-{ssp}"), models.path = "StrathE2E/", 
                  results.path = "StrathE2E/Results/", model.ident = "test_fitting")

results <- e2e_run(model,nyears = 5)                                # Run the model

e2e_plot_ts(model, results)                                          # Have we reached a steady state?

e2e_compare_obs(selection="ANNUAL", model, results=results)
e2e_compare_obs(selection="MONTHLY", model, results=results)

tic()
fitting_data <- e2e_optimize_eco(model, nyears=50, n_iter=500, 
                                 start_temperature=1, csv.output=TRUE)
toc() # 6.5 hours

# need to change the model set up file to use the new values.