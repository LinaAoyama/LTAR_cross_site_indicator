##Load packages
library(tidyverse)
library(lubridate)

##Set working directory
datpath <- getwd()

##Load data
#CPER
data_CPER_weather <- read_csv(paste(datpath, "/Data/CPER/CARM_weather_2013_2023.csv", sep = ""))
data_CPER_beef <- read_csv(paste(datpath, "/Data/CPER/CARM_BeefProduction_kg-ha.csv", sep = ""))
data_CPER_forage <- read_csv(paste(datpath, "/Data/CPER/CARM_ForagePdn_kg-ha.csv", sep = ""))
data_CPER_grsp <- read_csv(paste(datpath, "/Data/CPER/CARM_GRSP_DetectionsPerPoint.csv", sep = ""))

#ABS
data_ABS_ANPP <-read_csv(paste(datpath, "/Data/ABS/ABS_ANPP.csv", sep = ""))
data_ABS_birds <- read_csv(paste(datpath, "/Data/ABS/ABS_birds.csv", sep = ""))
data_ABS_forage_quality <- read_csv(paste(datpath, "/Data/ABS/ABS_forage_quality.csv", sep = ""))
data_ABS_GHG <- read_csv(paste(datpath, "/Data/ABS/ABS_GHG.csv", sep = ""))
data_ABS_plant_diversity <- read_csv(paste(datpath, "/Data/ABS/ABS_plant_diversity.csv", sep = ""))

##Organize data
#This is a function for calculating standard error
se<-function(x){
  sd(x, na.rm = TRUE)/sqrt(length(x))
} 

#CPER
#average of forage
avg_ecosites_CPER_forage <- data_CPER_forage %>%
  group_by(YearSampled, Treatment, Ecosite) %>% #summary by ecosites
  summarise(mean_C4 = mean(C4PG), se_C4 = se(C4PG),
            mean_C3 = mean(C3PG), se_C3 = se(C3PG))
colnames(avg_ecosites_CPER_forage)[1] <- "Year"
avg_CPER_forage <- data_CPER_forage %>%
  group_by(YearSampled, Treatment) %>%
  summarise(mean_C4 = mean(C4PG), se_C4 = se(C4PG),
            mean_C3 = mean(C3PG), se_C3 = se(C3PG))
colnames(avg_CPER_forage)[1] <- "Year"
#average of grassland sparrows
avg_ecosites_CPER_grsp <- data_CPER_grsp %>%
  group_by(Year, Treatment, Ecosite) %>% #summary by ecosites
  summarise(mean_grsp = mean(GRSP_point), se_grsp = se(GRSP_point))
avg_CPER_grsp <- data_CPER_grsp %>%
  group_by(Year, Treatment) %>%
  summarise(mean_grsp = mean(GRSP_point), se_grsp = se(GRSP_point))
#transform beef production data to long format
CPER_beef_long <- data_CPER_beef %>%
  select(Year, Beef_Production_kg_ha_CARM, Beef_Production_kg_ha_TRM) %>%
  pivot_longer(cols = Beef_Production_kg_ha_CARM:Beef_Production_kg_ha_TRM, 
               names_to = "Treatment", values_to = "Beef_Production") 
CPER_beef_long$Treatment <- gsub("Beef_Production_kg_ha_", "", as.character(CPER_beef_long$Treatment) )
CPER_beef_long$Treatment <- gsub("CARM", "AGM", as.character(CPER_beef_long$Treatment))
CPER_beef_long$Treatment <- gsub("TRM", "TGM", as.character(CPER_beef_long$Treatment))
#combine data
CPER_combined <- CPER_beef_long %>%
  left_join(avg_CPER_forage, by = c("Year", "Treatment")) %>%
  left_join(avg_CPER_grsp, by = c("Year", "Treatment"))
CPER_combined_simple <- CPER_combined %>%
  select(Year, Treatment, Beef_Production, mean_C4, mean_C3,  mean_grsp) 
colnames(CPER_combined_simple) <- c("Year", "Treatment", "Beef_Production", "C4", "C3", "Grassland_Sparrow")
CPER_combined_ecosites <- CPER_beef_long %>%
  left_join(avg_ecosites_CPER_forage , by = c("Year", "Treatment")) %>%
  left_join(avg_ecosites_CPER_grsp, by = c("Year", "Treatment", "Ecosite"))

#ABS
#average of ANPP
avg_ecosites_ABS_ANPP <- data_ABS_ANPP %>%
  group_by(Year, Treatment, Pasture_Type) %>% #summary by pasture type: semi-natural and improved
  summarise(mean_ANPP = mean(ANPP_kg_ha), se_ANPP = se(ANPP_kg_ha))%>%
  na.omit()
avg_ABS_ANPP <- data_ABS_ANPP %>%
  group_by(Year, Treatment) %>%
  summarise(mean_ANPP = mean(ANPP_kg_ha), se_ANPP = se(ANPP_kg_ha)) %>%
  na.omit()
#average bird species richness #only sampled in 2021
calculate_ABS_bird <- data_ABS_birds %>%
  group_by(Year, Treatment, Pasture_Type, Unit_ID) %>% 
  summarise(bird_sp_richness = length(unique(Bird_Spp))) 
avg_ecosites_ABS_bird <- calculate_ABS_bird %>%
  group_by(Year, Treatment, Pasture_Type) %>%
  summarise(mean_bird_sp = mean(bird_sp_richness), se_bird_sp = se(bird_sp_richness))
avg_ABS_bird <- calculate_ABS_bird %>%
  group_by(Year, Treatment) %>%
  summarise(mean_bird_sp = mean(bird_sp_richness), se_bird_sp = se(bird_sp_richness))
#average plant species richness 
calculate_ABS_plantsp <- data_ABS_plant_diversity %>%
  group_by(Year, Treatment, Pasture_Type, Unit_ID) %>% 
  summarise(plant_sp_richness = length(unique(Species_Code))) 
avg_ecosites_ABS_plantsp <- calculate_ABS_plantsp %>%
  group_by(Year, Treatment, Pasture_Type) %>%
  summarise(mean_plant_sp = mean(plant_sp_richness), se_plant_sp = se(plant_sp_richness))
avg_ABS_plantsp <- calculate_ABS_plantsp %>%
  group_by(Year, Treatment) %>%
  summarise(mean_plant_sp = mean(plant_sp_richness), se_plant_sp = se(plant_sp_richness))
#average GHG
data_ABS_GHG$Date <- mdy(data_ABS_GHG$Date)
avg_ecosites_ABS_GHG <- data_ABS_GHG %>%
  na.omit()%>%
  mutate(Year = year(Date)) %>%
  group_by(Year, Treatment, Pasture_Type) %>%
  summarise(mean_N2O = mean(`N2O_conc_mg_m-2_min-1`), se_N2O = se(`N2O_conc_mg_m-2_min-1`), 
            mean_CO2 = mean(`CO2_conc_mg_m-2_min-1`), se_CO2 = se(`CO2_conc_mg_m-2_min-1`))
avg_ABS_GHG <- data_ABS_GHG %>%
  na.omit()%>%
  mutate(Year = year(Date)) %>%
  group_by(Year, Treatment) %>%
  summarise(mean_N2O = mean(`N2O_conc_mg_m-2_min-1`), se_N2O = se(`N2O_conc_mg_m-2_min-1`), 
            mean_CO2 = mean(`CO2_conc_mg_m-2_min-1`), se_CO2 = se(`CO2_conc_mg_m-2_min-1`))
#average crude protein 
data_ABS_forage_quality$Date <- mdy(data_ABS_forage_quality$Date)
avg_ecosites_ABS_forage_quality <- data_ABS_forage_quality %>%
  na.omit()%>%
  mutate(Year = year(Date)) %>%
  mutate(Treatment = case_when(Treatment == "FB" ~ "BAU", 
                               Treatment == "PBG" ~ "ASP")) %>% 
  group_by(Year, Treatment, Pasture_Type) %>%
  summarise(mean_crude_protein = mean(CP_DM_per), se_crude_protein = se(CP_DM_per))
avg_ABS_forage_quality <- data_ABS_forage_quality %>%
  na.omit()%>%
  mutate(Year = year(Date)) %>%
  mutate(Treatment = case_when(Treatment == "FB" ~ "BAU", 
                               Treatment == "PBG" ~ "ASP")) %>% ##Double check with Shefali!
  group_by(Year, Treatment) %>%
  summarise(mean_crude_protein = mean(CP_DM_per), se_crude_protein = se(CP_DM_per))
#combine ABS data
ABS_combined_ecosites <- avg_ecosites_ABS_ANPP %>%
  left_join(avg_ecosites_ABS_forage_quality, by = c("Year", "Treatment", "Pasture_Type")) %>%
  left_join(avg_ecosites_ABS_plantsp, by = c("Year", "Treatment", "Pasture_Type")) %>%
  full_join(avg_ecosites_ABS_bird, by = c("Year", "Treatment", "Pasture_Type")) %>%
  full_join(avg_ecosites_ABS_GHG, by = c("Year", "Treatment", "Pasture_Type")) 
ABS_combined <- avg_ABS_ANPP %>%
  left_join(avg_ABS_forage_quality, by = c("Year", "Treatment")) %>%
  left_join(avg_ABS_plantsp, by = c("Year", "Treatment")) %>%
  full_join(avg_ABS_bird, by = c("Year", "Treatment")) %>%
  full_join(avg_ABS_GHG, by = c("Year", "Treatment")) 
  
