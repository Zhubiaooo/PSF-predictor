library(openxlsx)
library(dplyr)

pot_bio_data = read.xlsx("Coexistence_data.xlsx", sheet = "Pot_feedback_phase", rowNames = F, colNames = T)
#pot_bio_data = subset(pot_bio_data, density != "0")
dim(pot_bio_data)
#write.csv(filtered_data_all,"filtered_data_all-11-24-3.csv")
################################################################################
## 
pot_bio_data_mix = subset(pot_bio_data, group == "1" & above_bio != 0)
pot_bio_data_mix = pot_bio_data_mix[,c(2:6,15)]; colnames(pot_bio_data_mix)[6] = "mix_bio"
pot_bio_data_mono = subset(pot_bio_data, group == "0" & above_bio != 0) 
pot_bio_data_mono = pot_bio_data_mono[,c(2:6,15)]; colnames(pot_bio_data_mono)[6] = "mono_bio"
colnames(pot_bio_data_mono)
##
RII_data_all = pot_bio_data_mono %>% left_join(pot_bio_data_mix)
#RII_data_all = merge(pot_bio_data_mix, pot_bio_data_mono, by = c("density", "drought", "condi_sp", "block", "focal_sp"))
## 
shapiro.test(sqrt(RII_data_all$mono_bio))
shapiro.test(sqrt(RII_data_all$mix_bio))
RII_data_all$mono_bio = sqrt(RII_data_all$mono_bio)
RII_data_all$mix_bio = sqrt(RII_data_all$mix_bio)
RII_data_all$CI = (RII_data_all$mix_bio * 2)/(RII_data_all$mono_bio + RII_data_all$mix_bio)

RII_data_all$pair = paste0(RII_data_all$focal_sp,RII_data_all$condi_sp)
unique(RII_data_all$pair)

################################################################################
lme_data = subset(RII_data_all, density == "L")

lme_data_A = lme_data[,1:6]
colnames(lme_data_A)[6] = "bio_val"; lme_data_A$type = "mix"
lme_data_B = lme_data[,c(1:5,7)]
colnames(lme_data_B)[6] = "bio_val"; lme_data_B$type = "mono"

lme_data_test = rbind(lme_data_A, lme_data_B)
lme_data_test$bio_val_sq = sqrt(lme_data_test$bio_val)

lme_data_test$drought = as.factor(lme_data$drought)
lme_data_test$density = as.factor(lme_data$density)
lme_data_test$condi_sp = as.factor(lme_data$condi_sp)
lme_data_test$focal_sp = as.factor(lme_data$focal_sp)
lme_data_test$type = as.factor(lme_data_test$type)
lme_data_test$species_pair = paste0(lme_data_test$focal_sp,"_",lme_data_test$condi_sp)

lme_data_test <- lme_data_test %>%
  mutate(pair = recode(species_pair,"1_5" = "15","5_1" = "15","2_1" = "12","1_2" = "12", "3_2" = "23", "2_3" = "23", "4_2" = "24", "2_4" = "24", "5_2" = "25", "2_5" = "25",
                       "2_6" = "26", "6_2" = "26","3_1" = "13", "1_3" = "13","3_4" = "34", "4_3" = "34","3_5" = "35", "5_3" = "35","3_6" = "36", "6_3" = "36", "4_1" = "14","1_4" = "14",  
                       "4_5" = "45", "5_4" = "45","6_1" = "16", "1_6" = "16","6_4" = "46", "4_6" = "46","6_5" = "56","5_6" = "56")) %>% as.data.frame()
unique(lme_data_test$pair)

lme_data_test = lme_data_test %>% 
  mutate(abbrev_condi = recode(condi_sp,`1` = "Bb",`2` = "Ac",`3` = "Car",
                               `4` = "Cal",`5` = "Sp",`6` = "Pb"))
lme_data_test$drought2 = ifelse(lme_data_test$drought == "G", "Ambient", "Drought")

### Adding covariates
Soil_group = read.xlsx("Coexistence_data.xlsx", sheet = "Pot_conditioning_phase", rowNames = T, colNames = T)
Soil_group$Sample_ID = rownames(Field_group)
colnames(Soil_group)
Soil_group$condi_bio = sqrt(Soil_group$biomass)
Soil_group = Soil_group %>% 
  mutate(abbrev_condi = recode(focal,`1` = "Bb",`2` = "Ac",`3` = "Car",
                               `4` = "Cal",`5` = "Sp",`6` = "Pb"))
Soil_group$drought2 = ifelse(Soil_group$drought == "G", "Ambient", "Drought")
lme_data_test2 = lme_data_test %>% left_join(Soil_group[,c("abbrev_condi","drought2","block","condi_bio")])
lme_data_test2$block = as.factor(lme_data_test2$block)

mod_lme = lmer(bio_val_sq ~ condi_bio + drought * condi_sp * focal_sp * type + (1|block), data = lme_data_test2)
anova(mod_lme, type = 3)
shapiro.test(residuals(mod_lme))
ranova(mod_lme)
summary(mod_lme)

emm1 = emmeans(mod_lme, specs = pairwise ~ focal_sp, type = 'response', adjust = 'tukey')
emm1_multi = multcomp::cld(emm1,alpha=0.05,Letters=letters,adjust="none",decreasing = T)
emm1_multi$.group <- trimws(emm1_multi$.group)

pd = position_dodge(.4)
colnames(lme_data_test)
Bio_mono_sum = subset(lme_data_test, bio_val_sq != "NA") %>% group_by(focal_sp) %>% 
  summarise(mean_bio = mean(bio_val_sq),sd_bio = sd(bio_val_sq, na.rm = TRUE),       
            se_bio = sd_bio / sqrt(n()), .groups = 'drop') %>%
  left_join(emm1_multi)

Bio_mono_sum <- Bio_mono_sum %>%
  mutate(abbrev_focal = recode(focal_sp,`1` = "Bb",`2` = "Ac",`3` = "Car",
                               `4` = "Cal",`5` = "Sp",`6` = "Pb"))

Bio_mono_sum$abbrev_focal = factor(Bio_mono_sum$abbrev_focal, levels = c("Ac","Bb","Cal","Car","Pb","Sp"))

ggplot(Bio_mono_sum, aes(x = abbrev_focal, y = mean_bio)) + 
  geom_errorbar(aes(ymin = mean_bio - se_bio, ymax = mean_bio + se_bio), 
                position=pd, width = 0, color = "black", show.legend = F) +
  geom_text(data = Bio_mono_sum, mapping = aes(x = abbrev_focal,y = mean_bio + se_bio + 0.01,label = .group), 
            color = "black", size = 3) +
  geom_point(size=3,position=pd,alpha=1, pch = 21, aes(fill = drought2), color = "black", fill = "grey") +
  #geom_line(aes(x = abbrev_focal, y = mean_bio, group = drought2), color = "black", size = 0.5, inherit.aes = FALSE) + 
  theme_bw() + mytheme +
  scale_y_continuous(labels = scales::label_comma(accuracy =0.1)) + 
  labs(x = "Responding species",y = "Aboveground biomass (g, sqrt)",fill = NULL, tag = "a") + 
  theme(legend.position = c(0.80, 0.85),
        legend.key = element_blank(),
        legend.background = element_rect(fill = NA)) -> Fig_S4a; Fig_S4a

################################################################################
mod_lme = lmer(bio_val_sq ~ condi_bio + drought * condi_sp * focal_sp * type + (1|block), data = lme_data_test2)
#View(subset(lme_data_test, abbrev_focal == "Bb" & drought == "D"))
anova(mod_lme, type = 3)
shapiro.test(residuals(mod_lme))

emm1 = emmeans(mod_lme, specs = pairwise ~ drought * condi_sp, type = 'response', adjust = 'tukey')
emm1_multi = multcomp::cld(emm1,alpha=0.05,Letters=letters,adjust="none",decreasing = T)
emm1_multi$.group <- trimws(emm1_multi$.group)

pd = position_dodge(.4)
colnames(lme_data_test)
Bio_mono_sum = subset(lme_data_test, bio_val_sq != "NA") %>% group_by(drought, condi_sp) %>% 
  summarise(mean_bio = mean(bio_val_sq),sd_bio = sd(bio_val_sq, na.rm = TRUE),       
            se_bio = sd_bio / sqrt(n()), .groups = 'drop') %>%
  left_join(emm1_multi)

Bio_mono_sum <- Bio_mono_sum %>%
  mutate(abbrev_condi = recode(condi_sp,`1` = "Bb",`2` = "Ac",`3` = "Car",
                               `4` = "Cal",`5` = "Sp",`6` = "Pb"))

Bio_mono_sum$abbrev_condi = factor(Bio_mono_sum$abbrev_condi, levels = c("Ac","Bb","Cal","Car","Pb","Sp"))
Bio_mono_sum$drought2 = ifelse(Bio_mono_sum$drought == "G", "Ambient", "Drought") 

ggplot(Bio_mono_sum, aes(x = abbrev_condi, y = mean_bio, fill = drought2)) + 
  geom_errorbar(aes(ymin = mean_bio - se_bio, ymax = mean_bio + se_bio, group = drought2), 
                position=pd, width = 0, color = "black", show.legend = F) +
  geom_text(data = Bio_mono_sum, mapping = aes(x = abbrev_condi,y = mean_bio + se_bio + 0.01,label = .group, group = drought2), 
            color = "black", size = 3,position = position_dodge(.4)) +
  geom_point(size=3,position=pd,alpha=1, pch = 21, aes(fill = drought2), color = "black") +
  #geom_line(aes(x = abbrev_condi, y = mean_bio, group = drought2), color = "black", size = 0.5, inherit.aes = FALSE) + 
  theme_bw() + mytheme +
  scale_fill_manual(values = c("#70A7C3","#A67C2A")) + 
  scale_color_manual(values = c("#70A7C3","#A67C2A")) + 
  #scale_y_continuous(labels = scales::label_comma(accuracy =0.1)) + 
  labs(x = "Conditioning species",y = "Aboveground biomass (g, sqrt)",fill = NULL, tag = "b") + 
  theme(legend.position = c(0.15, 0.85),
        legend.key = element_blank(),
        legend.background = element_rect(fill = NA)) -> Fig_S4b; Fig_S4b

library(patchwork)
Fig_S4a/Fig_S4b

