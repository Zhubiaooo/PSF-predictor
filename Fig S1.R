library(openxlsx)
library(dplyr)
library(ggplot2)
library(emmeans)

pot_sm_data = read.xlsx("Coexistence_data.xlsx", sheet = "Pot_conditioning_phase_SM", rowNames = F, colNames = T)
#pot_bio_data = subset(pot_bio_data, density != "0")
head(pot_sm_data)

pot_sm_data$Times=as.Date(pot_sm_data$time,origin="1897-12-30")
pot_sm_data$Times = as.factor(pot_sm_data$Times)
pot_sm_data$drought = ifelse(pot_sm_data$drought == "D", "Drought", "Ambient")

mod = lmer(SM ~ drought * Times + (1|block/code), data = pot_sm_data)
summary(mod)
anova(mod)

emm1 = emmeans(mod, specs = pairwise ~ Times * drought, type = 'response', adjust = 'none')
emm1_multi = multcomp::cld(emm1,alpha=0.05,Letters=letters,adjust="none",decreasing = T)
emm1_multi$.group <- trimws(emm1_multi$.group)

#emm1 = emmeans(mod, specs = pairwise ~ drought | focal, type = 'response', adjust = 'none')
#emm1_multi = multcomp::cld(emm1,alpha=0.05,Letters=letters,adjust="none",decreasing = T)
bio_data_sum = bio_data_sum %>% left_join(emm1_multi)
bio_data_sum$abbrev_focal = factor(bio_data_sum$abbrev_focal, levels = c("Ac","Bb","Cal","Car","Pb","Sp"))


pot_sm_data_mean = pot_sm_data %>% group_by(Times, time, drought) %>%
  summarise(mean_SM = mean(SM),sd_SM = sd(SM, na.rm = TRUE),       
            se_SM = sd_SM / sqrt(n()), .groups = 'drop') %>% left_join(emm1_multi)

#time_ser = c(45475, 45500, 45525, 45550, 45575)
#as.Date(time_ser,origin="1897-12-30")

unique(pot_sm_data_mean$time)
unique(pot_sm_data_mean$Times)

ggplot(data = pot_sm_data_mean, aes(x = time, y = mean_SM, fill = drought)) +
  geom_errorbar(aes(ymin = mean_SM - se_SM, ymax = mean_SM + se_SM, group = drought), 
                width = 0, color = "black", show.legend = F) +
  geom_point(data = pot_sm_data, aes(x = time, y = SM, color = drought),size=1.5, 
             pch = 21, position = position_jitterdodge(2), alpha = 0.5, show.legend = F) +
  geom_text(data = pot_sm_data_mean, mapping = aes(x = time + 2.5,y = mean_SM+0.1,label = .group, 
                                                   group = drought), color = "black", size = 3) +
  geom_point(size=2.5,alpha=1, pch = 21, aes(fill = drought), color = "black") +
  scale_fill_manual(values = c("#70A7C3","#A67C2A")) + 
  scale_color_manual(values = c("#70A7C3","#A67C2A")) + 
  #geom_line(aes(x = abbrev_focal, y = mean_PSF, group = drought), color = "black", size = 0.5, inherit.aes = FALSE) + 
  theme_bw() + mytheme +
  scale_y_continuous(labels = scales::label_comma(accuracy =0.1)) + 
  labs(x = "Times",y = "Soil volumetric water content (%)",fill = NULL) + 
  annotate("text", x = 45505, y = 7.5, parse = TRUE, size = 4, 
           label = expression("Watering treatment:" ~ italic(p) < 0.001)) + 
  theme(legend.position = c(0.82, 0.88),
        legend.key = element_blank(),
        axis.text.x = element_text(angle = 25, hjust = 1, vjust = 1),
        legend.background = element_rect(fill = NA)) + 
  scale_x_continuous(breaks=c(45481,45504,45518,45533,45550,45563,45590), 
                     labels=c("2022-07-09", "2022-08-01","2022-08-15","2022-08-30","2022-09-16","2022-09-29","2022-10-26")) -> Fig_S1; Fig_S1





