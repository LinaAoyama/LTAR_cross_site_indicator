#spider diagrams
library(fmsb)
library(tidyverse)

#CPER 
#calculate max and min
CPER_maxmin <- data.frame(
  Treatment = c("Max", "Min"),
  Beef_Production = c(28.3, 9.9),
  C4 = c(1191.5467, 166.1279),
  C3 = c(608.68719, 86.58672),
  Grassland_Sparrow = c(3.3260870, 0.3958333)
)

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
radarchart(CPER_all[,2:5],
           cglty = 1, cglcol = "gray",
           pcol = c(4,3, 2), plwd = 2 , plty = 1)

radarchart(CPER_all[-3,2:5], #remove pre-treatment
           cglty = 1, cglcol = "gray",
           pcol = c("#AF69EE", "#FFA500"), 
           plwd = 2, plty = 1,
           vlab = c("Beef Production", "C4 Grass", "C3 Grass", "Grassland Sparrow")) #Aspirational practice reduced beef production but increased C3 grass. No change in grassland sparrow counts or forage.
legend("topright",
       legend = c("Aspirational", "Conventional"),
       col = c("#AF69EE", "#FFA500"),
       fill = c("#AF69EE", "#FFA500"),
       bty = "n",
       cex = 0.8)

#ABS
#calculate max and min
ABS_maxmin <- data.frame(
  Treatment = c("Max", "Min"),
  ANPP = c(max(data_ABS_ANPP$ANPP_kg_ha), min(data_ABS_ANPP$ANPP_kg_ha)),
  Crude_Protein = c(max(data_ABS_forage_quality$CP_DM_per), min(data_ABS_forage_quality$CP_DM_per)),
  Plant_Richness = c(max(calculate_ABS_plantsp$plant_sp_richness), min(calculate_ABS_plantsp$plant_sp_richness)),
  Bird_Richness = c(max(calculate_ABS_bird$bird_sp_richness), min(calculate_ABS_bird$bird_sp_richness)),
  N2O = c(max(data_ABS_GHG$`N2O_conc_mg_m-2_min-1`), min(data_ABS_GHG$`N2O_conc_mg_m-2_min-1`)),
  CO2 = c(max(data_ABS_GHG$`CO2_conc_mg_m-2_min-1`), min(data_ABS_GHG$`CO2_conc_mg_m-2_min-1`))
)
