
## Extract chlorophyll and primary production data from remote sensing sources

#### set up ####

library(raster)
library(ncdf4)
library(exactextractr)
library(sf)
library(tidyverse)
library(furrr)
source("./R scripts/@_Region file.R")

plan("multisession")

domains <- readRDS("./Objects/Domains.rds") %>% 
  st_transform(crs = 4326) %>% 
  st_union() %>% 
  st_as_sf()

#### Extract primary production ####

series <- future_map(list.files("../Shared data/CMEMS/PP_data/", full.names = TRUE, pattern = ".nc"), ~{
  
  data <- raster(.x)
  data[is.na(data)] <- 0

PP <- mutate(domains, PP = exact_extract(data, domains, "median"),
    Date = as.Date(str_sub(str_extract(.x, "[0-9]+"), start = 1, 8), format = "%Y%m%d")) %>% 
    st_drop_geometry()

}, .progress = TRUE) %>% 
  data.table::rbindlist()

ggplot(series) +
  geom_line(aes(x=Date, y = PP))

Mike <- mutate(series, 
               Year = lubridate::year((Date)), 
               Days = lubridate::days_in_month(Date),
               PP = PP * Days) %>%                                              # scale to the total per month
  mutate(Annual_measure = PP *(16/106) / 14.01) %>%                             # Convert Carbon to Nitrogen using Redfield, and mg to mmol
  group_by(Year) %>% 
  summarise(Annual_measure = sum(Annual_measure, na.rm = TRUE)) %>%             # Total to the year
  summarise(SD_of_measure = sd(Annual_measure),
            Annual_measure = mean(Annual_measure)) %>%                             # Decadal summary
  mutate(Description = "Annual_total_primary_production",
         Region = implementation,
         Time_period = "2010-2019",
         Source = "Copernicus remote sensing data from OCEANCOLOUR_GLO_BGC_L4_MY_009_104") 

write.csv(Mike, str_glue("./Objects/fitting/PP_target_{implementation}.csv"), row.names = FALSE)
  
#### Ectract Chl-a ####

series <- future_map(list.files("../Shared data/CMEMS/CHL_data/", full.names = TRUE, pattern = ".nc"), ~{

  data <- raster(.x)
  data[is.na(data)] <- 0
  
  PP <- mutate(domains, CHLa = exact_extract(data, domains, "median"),
      Date = as.Date(str_sub(str_extract(.x, "[0-9]+"), start = 1, 8), format = "%Y%m%d")) %>%
      st_drop_geometry()

}, .progress = TRUE) %>%
  data.table::rbindlist()

ggplot(series) +
  geom_line(aes(x=Date, y = CHLa))

Mike <- mutate(series, 
               Month = lubridate::month(Date),
               Year = lubridate::year(Date)) %>%
  group_by(Month, Year) %>%
  summarise(median = median(CHLa, na.rm = TRUE)) %>% 
  replace_na(list(median = 0)) %>%
  summarise(lower_centile = quantile(median, 0.2),
            upper_centile = quantile(median, 0.8),
            median = median(median, na.rm = TRUE)) %>% 
  mutate(Month = month.abb[Month],
         Comments = str_glue("Copernicus remote sensing data from OCEANCOLOUR_GLO_BGC_L4_MY_009_104 for {implementation} 2010-2019"),
         low_cent_value = 0.2,
         upp_cent_value = 0.8,
         Variable = "surface_chlorophyll",
         Units = "mgm3")

  write.csv(Mike, str_glue("./Objects/fitting/CHLa_target_{implementation}.csv"), row.names = FALSE)
