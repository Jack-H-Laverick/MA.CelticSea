
#### Setup                                            ####

library(tidyverse)
library(stringr)
source("./R scripts/@_Region file.R")

discard_rate <- readRDS("./Data/Norwegian_sea_Tanguy2/Norwegian_sea/2010-2019/Object/EU discard rates.rds") %>%   # Import data
.[,-1]

landings_raw <- readRDS("./Data/Norwegian_sea_Tanguy2/Norwegian_sea/2010-2019/Object/International landings.rds")  # Units tonnes/m2/year

effort <- t(readRDS("./Data/Norwegian_sea_Tanguy2/Norwegian_sea/2010-2019/Object/International effort by gear.rds"))        # Units sec/m2/day

distribution <- t(readRDS("./Data/Norwegian_sea_Tanguy2/Norwegian_sea/2010-2019/Object/International effort proportion by gear and habitat.rds"))

lookup <- read.csv("./Data/lookup_gear.csv") %>% select(-X) %>% arrange(neworder)  # Import and order tables according to StrathE2E
hablookup <- read.csv("./Data/lookup_habitat.csv") %>% select(-X) %>% arrange(hneworder)
glookup <- read.csv("./Data/lookup_guild.csv") %>% select(-X) %>% arrange(gneworder)

domain_size <- readRDS("./Objects/Domains.rds") %>%                         # We need landings as tonnes per m^2
  sf::st_union() %>% 
  sf::st_area() %>% 
  as.numeric()

#### quickly copy over Tanguy's fish processing and fleet files ####

processing <- file.copy("./Data/Norwegian_sea_Tanguy2/Norwegian_sea/2010-2019/Param/fishing_processing_NORWEGIAN_BASIN_2010-2019.csv",
                        str_glue("./StrathE2E/{implementation}/2010-2019-{ssp}/Param/fishing_processing_NORWEGIAN_BASIN_2010-2019.csv"))

fleet <-file.copy("./Data/Norwegian_sea_Tanguy2/Norwegian_sea/2010-2019/Param/fishing_fleet_NORWEGIAN_BASIN_2010-2019.csv",
                  str_glue("./StrathE2E/{implementation}/2010-2019-{ssp}/Param/fishing_fleet_NORWEGIAN_BASIN_2010-2019.csv"))

mults <- read.csv("./Data/Norwegian_sea_Tanguy2/Norwegian_sea/2010-2019/Param/fishing_gear_multiplier.csv", sep = ";") %>% 
  select(-Gear_code) %>% 
  full_join(rename(lookup, Gear_name = newgears, Gear_code = gearcodes)) %>% 
  drop_na(rawgears) %>% 
  select(Gear_name, Gear_code, starts_with("Multi")) %>% 
  mutate(Multiplier_to_be_applied_to_activity = 1) %>% 
  write.csv(str_glue("./StrathE2E/{implementation}/2010-2019-{ssp}/Param/fishing_gear_multiplier_NORWEGIAN_BASIN_2010-2019.csv"),row.names=FALSE)
  
#### Insert missing factor levels ####

blank_fleetxguild <- matrix(0, 12, 12)

colnames(blank_fleetxguild) <- c(colnames(effort), paste0("blank", 1:4))
rownames(blank_fleetxguild) <- colnames(landings_raw)

blank_fleetxguild[1, 1:8] <- effort[1,]
effort <- blank_fleetxguild[1, ] 

blank_fleetxguild[] <- 0
blank_fleetxguild <- t(blank_fleetxguild)
blank_fleetxguild[1:8, 1:12] <- discard_rate

discard_rate <- blank_fleetxguild

blank_fleetxguild[] <- 0
blank_fleetxguild[1:8, 1:12] <- landings_raw

landings_raw <- blank_fleetxguild


missing_habs <- distribution[,1:4]
missing_habs[] <-0
colnames(missing_habs) <- str_replace(colnames(missing_habs), "In", "Off")  

distribution <- cbind(distribution, missing_habs)

missing_gears <- distribution[1:4,]
missing_gears[] <-0
rownames(missing_gears) <- paste0("blank", 1:4)  

distribution <- rbind(distribution, missing_gears)

#### Calculate catch and discards                     ####

landings <- landings_raw * 1e6 / 360                                        # Convert landings to g/m2/day

catch <- landings / (1-discard_rate)                                        # Inflate landings with discards to total catch.

catch[!is.finite(catch)] <- landings[!is.finite(catch)]                     # 0s and infinities mean no discard, so are overwritten with landings

#catch["Gillnets", "Cetacean"] <- catch["Gillnets", "Cetacean"] + (15.8 * 1e6 / 360 / domain_size) # Add extra discards following Mike's stories (see Notes)
#catch["Gillnets", "Birds"] <- catch["Gillnets", "Birds"] +(2.015 * 1e6 / 360/ domain_size)        # Converting the units as for landings
#catch["Gillnets", "Pinnipeds"] <- catch["Gillnets", "Pinnipeds"] + (6.2 * 1e6 / 360 / domain_size) 
#catch["Shrimp trawl", "Demersal (quota limited)"] <- catch["Shrimp trawl", "Demersal (quota limited)"] + (1822.57 * 1e6 / 360 / domain_size)
#catch["Shrimp trawl", "Planktivore"] <- catch["Shrimp trawl", "Planktivore"] + (3867.89 * 1e6 / 360 / domain_size)
#catch["Longlines_and_Jigging", "Birds"] <- catch["Longlines_and_Jigging", "Birds"]+ (6.819 * 1e6 / 360 / domain_size)

discard_weight <- catch - landings

all.equal(landings + discard_weight, catch)                                 # Quick check things balance

#### Rearrange the distribution data                  ####

new_distribution <- distribution[lookup$oldorder, hablookup$holdorder]
colnames(new_distribution) <- hablookup$newhabs

gear_hab <- data.frame(Gear_name = lookup$newgears,
                       Gear_code = lookup$gearcodes,
                       new_distribution)

write.csv(gear_hab, str_glue("./StrathE2E/{implementation}/2010-2019-{ssp}/Param/fishing_distribution_{toupper(implementation)}_2010-2019.csv"),
          row.names=FALSE)

#### Rearrange the landings, catch data, and discards ####

rearranged <- map(list(landings, catch, discard_weight), ~{

 new <- as.data.frame(.x) %>%                                  # Units here are gWW/m2/day
    mutate(Demersal = `Demersal (non quota)` +                 # Combine demersal guilds
                      `Demersal (quota limited)`) %>% 
    .[lookup$oldorder, glookup$goldorder] %>%                  # Reorder rows and columns
    .[, !names(.) %in% c("Demersal (non quota)",               # Drop unwanted columns
                         "Demersal (quota limited)", 
                         "Zooplankton omnivorous")]
 row.names(new) <- NULL                                        # Drop rownames (defaults back to row number)                               
 return(new) 
})

landings_new <- rearranged[[1]] ; catch_new <- rearranged[[2]] ; discards_new <- rearranged[[3]]

all.equal((landings_new + discards_new), catch_new)            # Check everything still balances

landings_new$Demersal[7]
discards_new$Demersal[7]
catch_new$Demersal[7]

#### Recalculate the discard_rate data                ####

discard_rate_new <- discards_new / catch_new           # Units here are dimensionless (proportion of catch weight discarded)

discard_rate_new[is.na(discard_rate_new)] <- 1         # Where catch is zero, set discard rate to 1

discard_rate_new[12, "Macrophyte"] <- 0                # Set the discard rate of kelp by Kelp harvesters to 0

#Add the Gearname and Gearcode columns
discard_rate_final <- data.frame(Gear_name=lookup$newgears, Gear_code=lookup$gearcodes, discard_rate_new) %>% 
  setNames(c("Gear_name","Gear_code","Discardrate_PF","Discardrate_DF","Discardrate_MF",
             "Discardrate_FDB","Discardrate_CSB","Discardrate_CZ","Discardrate_BD",
             "Discardrate_SL","Discardrate_CT","Discardrate_KP"))

write.csv(discard_rate_final, str_glue("./StrathE2E/{implementation}/2010-2019-{ssp}/Param/fishing_discards_{toupper(implementation)}_2010-2019.csv"),
          row.names=FALSE)

#### Rearrange the effort (activity rate) data        ####

activity <- data.frame("Gear_name" = lookup$newgears,
                       "Gear_code" = lookup$gearcodes,
                       "Activity_(s/m2/d)" = effort[lookup$oldorder],
                       "Plough_rate_(m2/s)"= lookup$abrasionrate)
row.names(activity) <- NULL

write.csv(activity,str_glue("./StrathE2E/{implementation}/2010-2019-{ssp}/Param/fishing_activity_{toupper(implementation)}_2010-2019.csv"), row.names=FALSE)

#### create the fishing power table                   ####

#Table of nitrogen per unit wet weight - from Table 18 of SE2E North Sea implementation
mMNpergWW <- c(PF = 2.038, DF = 1.340, MF = 2.314, FDB = 0.503,
               CSB = 1.006, CZ = 1.258, BD = 2.518, SL = 2.518, 
               CT = 2.518, KP = 2.070) 

power <- data.frame(activity[,c("Gear_name", "Gear_code")],    # Combine gear names and code
                    catch_new / activity[, "Activity_.s.m2.d."]) %>% # With fishing power
  setNames(c("Gear_name","Gear_code","Power_PF","Power_DF",    # Replace column names
             "Power_MF", "Power_FDB","Power_CSB","Power_CZ",
             "Power_BD","Power_SL", "Power_CT","Power_KP"))
power[is.na(power)] <- 0                                       # Overwrite Nas with 0

power <- mutate(power, Power_PF = Power_PF * mMNpergWW["PF"],  # Convert to nitrogen units
                Power_DF = Power_DF * mMNpergWW["DF"],    
                Power_MF =Power_MF * mMNpergWW["MF"], 
                Power_FDB = Power_FDB * mMNpergWW["FDB"],
                Power_CSB = Power_CSB * mMNpergWW["CSB"],
                Power_CZ = Power_CZ * mMNpergWW["CZ"],
                Power_BD = Power_BD * mMNpergWW["BD"],
                Power_SL = Power_SL * mMNpergWW["SL"], 
                Power_CT = Power_CT * mMNpergWW["CT"],
                Power_KP = Power_KP * mMNpergWW["KP"])     

write.csv(power, str_glue("./StrathE2E/{implementation}/2010-2019-{ssp}/Param/fishing_power_{toupper(implementation)}_2010-2019.csv"),
          row.names=FALSE)

#### Target data                                      ####

discard_weight_target <- (discards_new / 1e6 * 360) %>%      # Return to total tonnes per year
  setNames(c("Discardweight_PF","Discardweight_DF","Discardweight_MF",
             "Discardweight_FDB","Discardweight_CSB","Discardweight_CZ","Discardweight_BD",
             "Discardweight_SL","Discardweight_CT","Discardweight_KP"))

discard_weight_target <- data.frame(Gear_name = lookup$newgears, 
                                    Gear_code = lookup$gearcodes,
                                    discard_weight_target) 

write.csv(discard_weight_target, str_glue("./StrathE2E/{implementation}/2010-2019-{ssp}/Target/TARGET_raw_discards_t_m2_y_{toupper(implementation)}_2010-2019.csv"),
          row.names = FALSE)

#Now calculate the total ANNUAL landings, catch and discards of each guild gWW/m2/y
#and convert to Nitrogen units (mMN/year)
#landings_N <-(colSums(landings_new)) * 360 * mMNpergWW
#catch_N <-(colSums(catch_new)) * 360 * mMNpergWW
#discards_N <-(colSums(discards_new)) * 360 * mMNpergWW

#discard_rate_tot <- discards_N/catch_N
#discard_rate_tot[is.na(discard_rate_tot)] <- 0

#saveRDS(discard_rate_tot, "TARGET_discard_rate_mMN_m2_y_BARENTS_SEA_2011-2019.rds")

#### Reality check                                    ####

BSarea <-domain_size                                             # Barents Sea total area in m2
landings_tonnes <- (colSums(landings_new)) * 360 * BSarea / 1e6 # Does this match the data from ICES/FAO, Norway and STECF ???

test <- readRDS("./Data/Norwegian_sea_Tanguy2/Norwegian_sea/2010-2019/Object/International landings.rds") %>%     # Re-import international landings in tonnes
  colSums() %>%                                                 # Total over gears
  as.data.frame() %>% 
  rename("start_weight" = '.') %>%                              
  mutate(start_weight = start_weight * BSarea) %>%              # Scale to Barents Sea
  rownames_to_column("Guild") %>%                               
  full_join(landings_tonnes %>%                                 # Match to the new landings which have been converted to tonnes
              as.data.frame() %>%                               # Process as above to allow a join
              rename("check_weight" = '.') %>% 
              rownames_to_column("Guild")) %>% 
  drop_na()                                                     # Drop demersal mismatch for easy testing next. 

all.equal(test$start_weight, test$check_weight)                 # Match!

