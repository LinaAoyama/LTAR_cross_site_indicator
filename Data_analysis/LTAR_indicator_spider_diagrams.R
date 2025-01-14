#spider diagrams
library(fmsb)

#CPER 
#calculate max and min
CPER_maxmin <- data.frame(
  Year = c("Max", "Min"),
  Treatment = c("Max", "Min"),
  Beef_Production = c(28.3, 9.9),
  C4 = c(1191.5467, 166.1279),
  C3 = c(608.68719, 86.58672),
  Forage = c(2717.3197, 369.9638),
  Grassland_Sparrow = c(3.3260870, 0.3958333)
)

#combine data and max/min table
CPER_all <- rbind(CPER_maxmin, CPER_combined_simple)

radarchart(CPER_all[1:4,3:7],
           cglty = 1, cglcol = "gray",
           pcol = c(4,3), plwd = 2)
