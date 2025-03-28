#spider diagrams
library(fmsb)
library(tidyverse)

#CPER 
#calculate max and min
CPER_maxmin <- data.frame(
  Treatment = c("Max", "Min"),
  Beef_Production = c(quantile(CPER_beef_long$Beef_Production, 0.85, na.rm = TRUE), quantile(CPER_beef_long$Beef_Production, 0.15, na.rm = TRUE)),
  C4 = c(quantile(data_CPER_forage$C4PG, 0.85, na.rm = TRUE), quantile(data_CPER_forage$C4PG, 0.15, na.rm = TRUE)),
  C3 = c(quantile(data_CPER_forage$C3PG, 0.85, na.rm = TRUE), quantile(data_CPER_forage$C3PG, 0.15, na.rm = TRUE)),
  Grassland_Sparrow = c(quantile(data_CPER_grsp$GRSP_point, 0.85, na.rm = TRUE), quantile(data_CPER_grsp$GRSP_point, 0.15, na.rm = TRUE))
)

#calculate averages across years by treatment
#pre- and post-treatment differentiate by variable
#beef production's pre-treatment data is 2013 and treatment effect is 2014-2023
#veg and bird responses' pre-treatment data is 2013 and 2014 and treatment effect is 2015-2023
CPER_beef_ave <- CPER_beef_long %>%
  mutate(Pre_Post = case_when(Year == 2013 ~ "Pre", 
                              Year > 2013 ~ "Post")) %>%
  group_by(Treatment, Pre_Post) %>%
  summarize(Beef_Production = mean(Beef_Production))
CPER_veg_bird_ave <- CPER_combined_simple %>%
  select(Year, Treatment, C4, C3, Grassland_Sparrow) %>%
  mutate(Pre_Post = case_when(Year < 2015 ~ "Pre", 
                              Year > 2014 ~ "Post")) %>%
  group_by(Treatment, Pre_Post) %>%
  summarize(C4 = mean(C4),
            C3 = mean(C3),
            Grassland_Sparrow = mean(Grassland_Sparrow))
CPER_average <- CPER_beef_ave %>%
  left_join(CPER_veg_bird_ave, by = c("Treatment", "Pre_Post"))
CPER_pre_summary <- CPER_average %>%
  group_by(Pre_Post)%>%
  summarise(Beef_Production = mean(Beef_Production),C4 = mean(C4),C3 = mean(C3),
            Grassland_Sparrow = mean(Grassland_Sparrow)) %>%
  filter(Pre_Post == "Pre") %>%
  mutate(Treatment = "Pre")

#combine data and max/min table
CPER_all <- rbind(CPER_maxmin,CPER_pre_summary[,-1], CPER_average[-c(2,4), -2])
  
#create spider diagrams
#radarchart(CPER_all[,2:5],
#           cglty = 1, cglcol = "gray",
#           pcol = c(4,3, 2), plwd = 2 , plty = 1)

radarchart(CPER_all[-3,2:5], #remove pre-treatment
           cglty = 1, cglcol = "gray",
           pcol = c("#AF69EE", "#FFA500"), 
           plwd = 2, plty = 1, title = "CPER",
           vlab = c("Beef Production", "C4 Grass", "C3 Grass", "Grassland Sparrow")) #Aspirational practice reduced beef production but increased C3 grass. No change in grassland sparrow counts or forage.

legend("topright",
       legend = c("Aspirational", "Conventional"),
       col = c("#AF69EE", "#FFA500"),
       fill = c("#AF69EE", "#FFA500"),
       bty = "n",
       cex = 1.2)

#CPER - by ecosite
#calculate averages
CPER_average_ecosites <- CPER_combined_ecosites %>%
  group_by(Treatment, Ecosite) %>%
  summarise(Beef_Production = mean(Beef_Production),
            C4 = mean(mean_C4),
            C3 = mean(mean_C3),
            Grassland_Sparrow = mean(mean_grsp))
#Loamy
CPER_loamy_average <- CPER_average_ecosites %>%
  filter(Ecosite == "Loamy")
CPER_loamy <- rbind(CPER_maxmin, CPER_loamy_average[,-2 ])
radarchart(CPER_loamy[,2:5], #remove pre-treatment
           cglty = 1, cglcol = "gray",
           pcol = c("#AF69EE", "#FFA500"), 
           plwd = 2, plty = 1, title = "CPER Loamy",
           vlab = c("Beef Production", "C4 Grass", "C3 Grass", "Grassland Sparrow")) #Aspirational practice reduced beef production but increased C3 grass. No change in grassland sparrow counts or forage.
legend("topright",
       legend = c("Aspirational", "Conventional"),
       col = c("#AF69EE", "#FFA500"),
       fill = c("#AF69EE", "#FFA500"),
       bty = "n",
       cex = 1.2)

#Salt Flats
CPER_saltflats_average <- CPER_average_ecosites %>%
  filter(Ecosite == "Salt Flats")
CPER_saltflats <- rbind(CPER_maxmin, CPER_saltflats_average[,-2 ])
radarchart(CPER_saltflats[,2:5], #remove pre-treatment
           cglty = 1, cglcol = "gray",
           pcol = c("#AF69EE", "#FFA500"), 
           plwd = 2, plty = 1, title = "CPER Salt Flats",
           vlab = c("Beef Production", "C4 Grass", "C3 Grass", "Grassland Sparrow")) #Aspirational practice reduced beef production but increased C3 grass. No change in grassland sparrow counts or forage.
legend("topright",
       legend = c("Aspirational", "Conventional"),
       col = c("#AF69EE", "#FFA500"),
       fill = c("#AF69EE", "#FFA500"),
       bty = "n",
       cex = 1.2)

#Sandy
CPER_sandy_average <- CPER_average_ecosites %>%
  filter(Ecosite == "Sandy")
CPER_sandy <- rbind(CPER_maxmin, CPER_sandy_average[,-2 ])
radarchart(CPER_sandy[,2:5], #remove pre-treatment
           cglty = 1, cglcol = "gray",
           pcol = c("#AF69EE", "#FFA500"), 
           plwd = 2, plty = 1, title = "CPER Sandy",
           vlab = c("Beef Production", "C4 Grass", "C3 Grass", "Grassland Sparrow")) #Aspirational practice reduced beef production but increased C3 grass. No change in grassland sparrow counts or forage.
legend("topright",
       legend = c("Aspirational", "Conventional"),
       col = c("#AF69EE", "#FFA500"),
       fill = c("#AF69EE", "#FFA500"),
       bty = "n",
       cex = 1.2)
#ABS - all
#calculate max and min
ABS_maxmin <- data.frame(
  Treatment = c("4th-Q", "1st-Q"),
  ANPP = c(quantile(data_ABS_ANPP$ANPP_kg_ha, 0.85, na.rm = TRUE), quantile(data_ABS_ANPP$ANPP_kg_ha, 0.15, na.rm = TRUE)),
  Crude_Protein = c(quantile(data_ABS_forage_quality$CP_DM_per, 0.85, na.rm = TRUE), quantile(data_ABS_forage_quality$CP_DM_per, 0.15, na.rm = TRUE)),
  Plant_Richness = c(quantile(calculate_ABS_plantsp$plant_sp_richness, 0.85, na.rm = TRUE), quantile(calculate_ABS_plantsp$plant_sp_richness, 0.25, na.rm = TRUE)),
  Bird_Richness = c(quantile(calculate_ABS_bird$bird_sp_richness, 0.85, na.rm = TRUE), quantile(calculate_ABS_bird$bird_sp_richness, 0.15,na.rm = TRUE)),
  N2O = c(quantile(data_ABS_GHG$'N2O_conc_mg_m-2_min-1', 0.85, na.rm = TRUE), quantile(data_ABS_GHG$'N2O_conc_mg_m-2_min-1', 0.15, na.rm = TRUE)),
  CO2 = c(quantile(data_ABS_GHG$'CO2_conc_mg_m-2_min-1', 0.85, na.rm = TRUE), quantile(data_ABS_GHG$'CO2_conc_mg_m-2_min-1', 0.15, na.rm = TRUE))
)

#calculate averages across years by treatment
ABS_average <- ABS_combined %>%
  group_by(Treatment) %>%
  summarise(ANPP = mean(mean_ANPP, na.rm = TRUE), 
            Crude_Protein = mean(mean_crude_protein, na.rm = TRUE),
            Plant_Richness = mean(mean_plant_sp, na.rm = TRUE),
            Bird_Richness = mean(mean_bird_sp, na.rm = TRUE),
            N2O = mean(mean_N2O, na.rm = TRUE),
            CO2 = mean(mean_CO2, na.rm = TRUE))

#combine data and max/min table
ABS_all <- rbind(ABS_maxmin, ABS_average)

#create spider diagrams
radarchart(ABS_all[,2:7],
           cglty = 1, cglcol = "gray",
           pcol = c("#AF69EE", "#FFA500"), 
           plwd = 2 , plty = 1, title = "ABS",
           vlab = c("ANPP", "Crude Protein", "Plant Richness", "Bird Richness", "N2O", "CO2"))
legend("topright",
       legend = c("Aspirational", "Conventional"),
       col = c("#AF69EE", "#FFA500"),
       fill = c("#AF69EE", "#FFA500"),
       bty = "n",
       cex = 1.2) 

#ABS - by ecosite
#Semi-Natural Grassland (SN)

#calculate average 
ABS_SN_average <- ABS_combined_ecosites %>%
  filter(Pasture_Type == "SN") %>%
  group_by(Treatment) %>%
  summarise(ANPP = mean(mean_ANPP, na.rm = TRUE), 
            Crude_Protein = mean(mean_crude_protein, na.rm = TRUE),
            Plant_Richness = mean(mean_plant_sp, na.rm = TRUE),
            Bird_Richness = mean(mean_bird_sp, na.rm = TRUE),
            N2O = mean(mean_N2O, na.rm = TRUE),
            CO2 = mean(mean_CO2, na.rm = TRUE))
#combine data and max/min table
ABS_SN <- rbind(ABS_maxmin, ABS_SN_average)

#create spider diagrams
radarchart(ABS_SN[,2:7],
           cglty = 1, cglcol = "gray",
           pcol = c("#AF69EE", "#FFA500"), 
           plwd = 2 , plty = 1,
           vlab = c("ANPP", "Crude Protein", "Plant Richness", "Bird Richness", "N2O", "CO2"))
legend("topright",
       legend = c("Aspirational", "Conventional"),
       col = c("#AF69EE", "#FFA500"),
       fill = c("#AF69EE", "#FFA500"),
       bty = "n",
       cex = 1.2)

#Improved Pasture (IMP)
#calculate average 
ABS_IMP_average <- ABS_combined_ecosites %>%
  filter(Pasture_Type == "IMP") %>%
  group_by(Treatment) %>%
  summarise(ANPP = mean(mean_ANPP, na.rm = TRUE), 
            Crude_Protein = mean(mean_crude_protein, na.rm = TRUE),
            Plant_Richness = mean(mean_plant_sp, na.rm = TRUE),
            Bird_Richness = mean(mean_bird_sp, na.rm = TRUE),
            N2O = mean(mean_N2O, na.rm = TRUE),
            CO2 = mean(mean_CO2, na.rm = TRUE))
#combine data and max/min table
ABS_IMP <- rbind(ABS_maxmin, ABS_IMP_average)

#create spider diagrams
radarchart(ABS_IMP[,2:7],
           cglty = 1, cglcol = "gray",
           pcol = c("#AF69EE", "#FFA500"), 
           plwd = 2 , plty = 1,
           vlab = c("ANPP", "Crude Protein", "Plant Richness", "Bird Richness", "N2O", "CO2"))
legend("topright",
       legend = c("Aspirational", "Conventional"),
       col = c("#AF69EE", "#FFA500"),
       fill = c("#AF69EE", "#FFA500"),
       bty = "n",
       cex = 1.2)
