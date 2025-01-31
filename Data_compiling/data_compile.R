##Compile data

#load packages
library(tidyverse)

#set working directory
datpath <- getwd()

#CPER
data_CPER_weather <- read_csv(paste(datpath, "/Data/CPER/CARM_weather_2013_2023.csv", sep = ""))
data_CPER_beef <- read_csv(paste(datpath, "/CPER/CARM_BeefProduction_kg-ha.csv", sep = ""))
data_CPER_forage <- read_csv(paste(datpath, "/CPER/CARM_ForagePdn_kg-ha.csv", sep = ""))
data_CPER_grsp <- read_csv(paste(datpath, "/CPER/CARM_GRSP_DetectionsPerPoint.csv", sep = ""))

#ABS
data_ABS_ANPP <-read_csv(paste(datpath, "/ABS/ABS_ANPP.csv", sep = ""))
data_ABS_birds <- read_csv(paste(datpath, "/ABS/ABS_birds.csv", sep = ""))
data_ABS_forage_quality <- read_csv(paste(datpath, "/ABS/ABS_forage_quality.csv", sep = ""))
data_ABS_GHG <- read_csv(paste(datpath, "/ABS/ABS_GHG.csv", sep = ""))
data_ABS_plant_diversity <- read_csv(paste(datpath, "/ABS/ABS_plant_diversity.csv", sep = ""))

#This is a function for calculating standard error
se<-function(x){
  sd(x, na.rm = TRUE)/sqrt(length(x))
} 

##Organize data

#CPER
#average of forage
avg_ecosites_CPER_forage <- data_CPER_forage %>%
  group_by(YearSampled, Treatment, Ecosite) %>%
  summarise(mean_C4 = mean(C4PG), se_C4 = se(C4PG),
            mean_C3 = mean(C3PG), se_C3 = se(C3PG))
avg_CPER_forage <- data_CPER_forage %>%
  group_by(YearSampled, Treatment) %>%
  summarise(mean_C4 = mean(C4PG), se_C4 = se(C4PG),
            mean_C3 = mean(C3PG), se_C3 = se(C3PG))
colnames(avg_CPER_forage)[1] <- "Year"
#average of grassland sparrows
avg_ecosites_CPER_grsp <- data_CPER_grsp %>%
  group_by(Year, Treatment, Ecosite) %>%
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
