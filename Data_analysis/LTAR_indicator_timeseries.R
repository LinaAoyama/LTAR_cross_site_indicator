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
        axis.title = element_text(size = 15),
        axis.title.x = element_blank())+
  scale_color_manual(name = "Treatment", 
                     values = c("#AF69EE", "#FFA500"),
                     labels = c("Aspirational", "Conventional"))+
  scale_x_continuous(breaks = 2013:2023)+
  ylab("Beef Production \n (kg/ha)")

#forage
CPER_C4_timeseries <- ggplot(avg_CPER_forage, aes(y = mean_C4, x = Year, col = Treatment))+ 
  geom_point()+
  geom_line()+
  #geom_point(aes(y = mean_C3, x = Year, col = Treatment))+
  #geom_line(aes(y = mean_C3, x = Year, col = Treatment), linetype = "dashed")+
  theme(text = element_text(size=16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15),
        axis.title.x = element_blank())+
  scale_color_manual(name = "Treatment", values = c("#AF69EE", "#FFA500"),
                     labels = c("Aspirational", "Conventional"))+
  scale_x_continuous(breaks = 2013:2023)+
  ylab("C4 Perennial Grass \n (kg/ha)")

CPER_C4_timeseries_ecosites <- ggplot(CPER_combined_ecosites, aes(y = mean_C4, x = Year, col = Treatment))+ 
  geom_point()+
  geom_line()+
  facet_wrap(~Ecosite, ncol = 1)+
  #geom_point(aes(y = mean_C3, x = Year, col = Treatment))+
  #geom_line(aes(y = mean_C3, x = Year, col = Treatment), linetype = "dashed")+
  theme(text = element_text(size=16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15),
        axis.title.x = element_blank())+
  scale_color_manual(name = "Treatment", values = c("#AF69EE", "#FFA500"),
                     labels = c("Aspirational", "Conventional"))+
  scale_x_continuous(breaks = 2013:2023)+
  ylab("C4 Perennial Grass \n (kg/ha)")

CPER_C3_timeseries <- ggplot(avg_CPER_forage, aes(y = mean_C3, x = Year, col = Treatment))+ 
  geom_point()+
  geom_line()+
  #geom_point(aes(y = mean_C3, x = Year, col = Treatment))+
  #geom_line(aes(y = mean_C3, x = Year, col = Treatment), linetype = "dashed")+
  theme(text = element_text(size=16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15),
        axis.title.x = element_blank())+
  scale_color_manual(name = "Treatment", values = c("#AF69EE", "#FFA500"),
                     labels = c("Aspirational", "Conventional"))+
  scale_x_continuous(breaks = 2013:2023)+
  ylab("C3 Perennial Grass \n (kg/ha)")

CPER_C3_timeseries_ecosites <- ggplot(CPER_combined_ecosites, aes(y = mean_C3, x = Year, col = Treatment))+ 
  geom_point()+
  geom_line()+
  facet_wrap(~Ecosite, ncol = 1)+
  #geom_point(aes(y = mean_C3, x = Year, col = Treatment))+
  #geom_line(aes(y = mean_C3, x = Year, col = Treatment), linetype = "dashed")+
  theme(text = element_text(size=16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15),
        axis.title.x = element_blank())+
  scale_color_manual(name = "Treatment", values = c("#AF69EE", "#FFA500"),
                     labels = c("Aspirational", "Conventional"))+
  scale_x_continuous(breaks = 2013:2023)+
  ylab("C3 Perennial Grass \n (kg/ha)")


#birds
CPER_bird_timeseries <-ggplot(avg_CPER_grsp%>%filter(Year != "2024"), aes(y = mean_grsp, x = Year, col = Treatment))+
  geom_point()+
  geom_line()+
  theme(text = element_text(size=16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15))+
  scale_color_manual(name = "Treatment", values = c("#AF69EE", "#FFA500"))+
  scale_x_continuous(breaks = 2013:2023)+
  ylab(bquote(atop("Grasshopper Sparrows",(count~"/"~"0.07"~km2))))

CPER_bird_timeseries_ecosites <-ggplot(CPER_combined_ecosites%>%filter(Year != "2024"), aes(y = mean_grsp, x = Year, col = Treatment))+
  geom_point()+
  geom_line()+
  facet_wrap(~Ecosite, ncol = 1)+
  theme(text = element_text(size=16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15))+
  scale_color_manual(name = "Treatment", values = c("#AF69EE", "#FFA500"),
                     labels = c("Aspirational", "Conventional"))+
  scale_x_continuous(breaks = 2013:2023)+
  ylab(bquote(atop("Grasshopper Sparrows",(count~"/"~"0.07"~km2))))

#combined timeseries
ggarrange(CPER_beef_timeseries, CPER_C4_timeseries, CPER_C3_timeseries,
          CPER_bird_timeseries,
          nrow = 4, ncol = 1, 
          common.legend = TRUE,
          align = "hv")

#timeseries of diff from long-term mean of TGM
CPER_diff <- CPER_combined_simple %>%
  mutate(diff_Beef_Production = Beef_Production-22.66,
         diff_C4 = C4-490.6265,
         diff_C3 = C3-367.34855,
         diff_grsp = Grassland_Sparrow-1.4468599) 
  #mutate(diff_Beef_Production = ifelse(Treatment == "AGM", 
  #                                     Beef_Production-20.52, Beef_Production-22.66),
  #       diff_C4 = ifelse(Treatment == "AGM",
  #                        C4-428.5136, C4-490.6265),
  #       diff_C3 = ifelse(Treatment == "AGM",
  #                        C3-424.29629, C3-367.34855),
  #       diff_grsp = ifelse(Treatment == "AGM",
  #                          Grassland_Sparrow-1.4884259, Grassland_Sparrow-1.4468599))
#beef production
CPER_diff_beef_timeseries <- ggplot(CPER_diff, aes(y = diff_Beef_Production, x = Year, fill = Treatment))+
  geom_bar(stat = "identity", position=position_dodge())+
  geom_hline(yintercept = 0)+
  theme(text = element_text(size=16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15),
        axis.title.x = element_blank())+
  scale_fill_manual(name = "Treatment", 
                    values = c("#AF69EE", "#FFA500"),
                    labels = c("Aspirational", "Conventional"))+
  scale_x_continuous(breaks = 2013:2023)+
  ylab("Beef Production \n (kg/ha)")

#forage
CPER_diff_C4_timeseries <- ggplot(CPER_diff, aes(y = diff_C4, x = Year, fill = Treatment))+ #C4 is solid
  geom_bar(stat = "identity", position=position_dodge())+
  geom_hline(yintercept = 0)+
  theme(text = element_text(size=16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15),
        axis.title.x = element_blank())+
  scale_fill_manual(name = "Treatment", values = c("#AF69EE", "#FFA500"))+
  scale_x_continuous(breaks = 2013:2023)+
  ylab("C4 Perennial Grass \n (kg/ha)")
CPER_diff_C3_timeseries <- ggplot(CPER_diff, aes(y = diff_C3, x = Year, fill = Treatment))+ #C4 is solid
  geom_bar(stat = "identity", position=position_dodge())+
  geom_hline(yintercept = 0)+
  theme(text = element_text(size=16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15),
        axis.title.x = element_blank())+
  scale_fill_manual(name = "Treatment", values = c("#AF69EE", "#FFA500"))+
  scale_x_continuous(breaks = 2013:2023)+
  ylab("C3 Perennial Grass \n (kg/ha)")

#birds
CPER_diff_grsp_timeseries <-ggplot(CPER_diff, aes(y = diff_grsp, x = Year, fill = Treatment))+
  geom_bar(stat = "identity", position=position_dodge())+
  geom_hline(yintercept = 0)+
  theme(text = element_text(size=16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.title = element_text(size = 15))+
  scale_fill_manual(name = "Treatment", values = c("#AF69EE", "#FFA500"))+
  scale_x_continuous(breaks = 2013:2023)+
  ylab("Grasshopper Sparrows \n (count/0.07km2)")

#combined
ggarrange(CPER_diff_beef_timeseries, CPER_diff_C4_timeseries, 
          CPER_diff_C3_timeseries, CPER_diff_grsp_timeseries, 
          nrow = 4, ncol = 1, common.legend = TRUE, align = "hv")
