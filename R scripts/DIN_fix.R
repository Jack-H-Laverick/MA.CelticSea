
library(MiMeMo.tools)

domain <- readRDS("./Objects/Domains.rds")

all_data <- read.csv("./Data/ICESDataPortalDownload/7d63f4fa-3a74-4e1e-9f7d-dda797d3227c.txt", sep = "\t") %>% 
  select(Cruise, Station, Day, Month, Year, Latitude..degrees_north., Longitude..degrees_east., Depth..m.,
         Nitrate.Nitrogen..NO3.N...umol.l., Ammonium.Nitrogen..NH4.N...umol.l.) %>% 
  drop_na(Nitrate.Nitrogen..NO3.N...umol.l., Ammonium.Nitrogen..NH4.N...umol.l.) %>% 
  st_as_sf(coords = c("Longitude..degrees_east.","Latitude..degrees_north."), crs = 4326) %>% # Convert to sf for spatial filter later
  st_transform(crs = 3035) %>%  
  group_by(Cruise, Station, Day, Month, Year) %>%                           # Per cast
  arrange(Depth..m., .by_group = TRUE)                                      # Order depths ascending

#### Calculate proportion ####

shallow_proportion <- all_data %>% 
  mutate(weights = calculate_depth_share(Depth..m., min_depth = 0, max_depth = 40), # Calculate share of the depth column per sample
         Depth_layer = "Shallow")                                           # Label depth layer 

deep_proportion <- all_data %>% 
  mutate(weights = calculate_depth_share(Depth..m., min_depth = 40, max_depth = 400), # Calculate share of the depth column per sample
         Depth_layer = "Deep")                                              # Label depth layer 

final <- bind_rows(shallow_proportion, deep_proportion) %>%                 # Combine estimates
  filter(weights > 0) %>%                                                   # Drop samples outside the depth window
  group_by(Cruise, Station, Day, Month, Year, Depth_layer) %>% 
  summarise(Ammonium = weighted.mean(Ammonium.Nitrogen..NH4.N...umol.l., weights), # Weighted averages
            DIN =  weighted.mean(Nitrate.Nitrogen..NO3.N...umol.l. + Ammonium.Nitrogen..NH4.N...umol.l., weights),
            Samples = n()) %>%                                              # Number of samples contributing to each estimate
  ungroup() %>% 
  mutate(Proportion = Ammonium/DIN) %>%                                     # Get a proportion of ammonium to total DIN
  st_join(domain) %>%                                                       # Check which are in the model domain
  st_drop_geometry() %>%                                                    # Simplify the output
  drop_na() %>% 
  group_by(Depth_layer, Month) %>%                                          # Decided not to group by shore because there were few inshore samples   
  summarise(Proportion = weighted.mean(Proportion, Samples),                # Calculate average, weighting by the number of samples
            Casts = n()) %>%                                                # Number of CTD casts contributing to each estimate
  complete(Depth_layer, Month = 1:12) %>%                                   # Add missing months 
  group_by(Depth_layer) %>% 
  rbind(., .) %>%                                                           # Duplicate the data because we can't have missing months at the end of a time series (and it's a cycle)
  arrange(Depth_layer) %>%                                                  # Order data for interpolation
  slice(-24) %>%                                                            # Remove missing month at end of time series
  mutate(Proportion = zoo::na.spline(Proportion)) %>%                       # Interpolate
  slice(1:12) %>%                                                           # Remove duplicates 
  ungroup() 

saveRDS(final, "./Objects/Ammonia to DIN.rds")


ggplot(final) +
  geom_line(aes(x = Month, y = Casts, colour = Depth_layer)) +
  theme_minimal()

ggplot(final) +
  geom_line(aes(x = Month, y = Proportion, colour = Depth_layer)) +
  theme_minimal()


