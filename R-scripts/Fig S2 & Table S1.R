## Effects of conditioning species, drought and their interactions on plant aboveground biomass
library(openxlsx)
library(dplyr)
library(emmeans)
library(lme4)
library(lmerTest)

Soil_group = read.xlsx("Coexistence_data.xlsx", sheet = "Pot_conditioning_phase", rowNames = T, colNames = T)
Soil_group$Sample_ID = rownames(Field_group)
Soil_group$drought = as.factor(Soil_group$drought)
Soil_group$focal = as.factor(Soil_group$focal)
Soil_group$block = as.factor(Soil_group$block)
colnames(Soil_group)
Soil_group$bio_sq = sqrt(Soil_group$biomass)
Soil_group = Soil_group %>% 
  mutate(abbrev_focal = recode(focal,`1` = "Bb",`2` = "Ac",`3` = "Car",
                               `4` = "Cal",`5` = "Sp",`6` = "Pb"))

Soil_group$drought2 = ifelse(Soil_group$drought == "G", "Ambient", "Drought")
Soil_group$drought2 = factor(Soil_group$drought2, levels = c("Ambient", "Drought"))

# Table S1
bio_mod = lmer(bio_sq ~ drought2 * abbrev_focal + (1|block), data = Soil_group)
anova(bio_mod, type = 3)

# Fig S2
bio_data_sum = Soil_group %>% group_by(drought, focal) %>% 
  summarise(mean_bio = mean(bio_sq),sd_bio = sd(bio_sq, na.rm = TRUE),       
            se_bio = sd_bio / sqrt(n()), .groups = 'drop') %>%
  mutate(abbrev_focal = recode(focal,`1` = "Bb",`2` = "Ac",`3` = "Car",
                               `4` = "Cal",`5` = "Sp",`6` = "Pb"))


bio_data_sum$drought2 = ifelse(bio_data_sum$drought == "G", "Ambient", "Drought")
bio_data_sum$drought2 = factor(bio_data_sum$drought2, levels = c("Ambient", "Drought"))


###
emm1 = emmeans(bio_mod, specs = pairwise ~ abbrev_focal * drought2, type = 'response', adjust = 'tukey')
emm1_multi = multcomp::cld(emm1,alpha=0.05,Letters=letters,adjust="none", decreasing = T)
emm1_multi$.group <- trimws(emm1_multi$.group)

bio_data_sum = bio_data_sum %>% left_join(emm1_multi)
bio_data_sum$abbrev_focal = factor(bio_data_sum$abbrev_focal, levels = c("Ac","Bb","Cal","Car","Pb","Sp"))

pd = position_dodge(.8)
library(ggplot2)
ggplot(bio_data_sum, aes(x = abbrev_focal, y = mean_bio)) + 
  geom_errorbar(aes(ymin = mean_bio - se_bio, ymax = mean_bio + se_bio, group = drought2), 
                position=pd, width = 0, color = "black", show.legend = F) +
  geom_text(data = bio_data_sum, mapping = aes(x = abbrev_focal,y = mean_bio+0.5,label = .group, group = drought2), 
            color = "black", size = 3,position = position_dodge(.8)) +
  geom_point(data = Soil_group, aes(x = abbrev_focal, y = bio_sq, color = drought2),
             position = position_jitterdodge(.2), alpha = 0.5, show.legend = F) + 
  geom_point(size=3,position=pd,alpha=1, pch = 21, aes(fill = drought2), color = "black") +
  scale_fill_manual(values = c("#70A7C3","#A67C2A")) + 
  scale_color_manual(values = c("#70A7C3","#A67C2A")) + 
  theme_bw() + mytheme +
  annotate("text", x = 1.4, y = 1.5, parse = TRUE, size = 3.5, 
           label = expression("W × C:" ~ italic(p) < 0.001)) + 
  scale_y_continuous(labels = scales::label_comma(accuracy =0.1)) + 
  labs(x = "Soil conditioning species",y = "Aboverground biomass (g, sqrt)",fill = NULL) + 
  theme(legend.position = c(0.85, 0.13),
        legend.key = element_blank(),
        legend.background = element_rect(fill = NA)) -> Fig_S2; Fig_S2
