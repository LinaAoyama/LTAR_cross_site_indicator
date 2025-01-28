#Timeseries of means
library(ggplot2)
library(ggpubr)
library(lubridate)

#CPER 

#make year as date 
#year(CPER_combined$Year) #this isn't working properly

#timeseries of means
#beef production
CPER_beef_timeseries <- ggplot(CPER_beef_long, aes(y = Beef_Production, x = Year, col = Treatment))+
  geom_point()+
  geom_line()+
  theme(text = element_text(size=16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15))+
  scale_color_manual(name = "Treatment", values = c("#AF69EE", "#FFA500"))
#forage
CPER_forage_timeseries <- ggplot(avg_CPER_forage, )


