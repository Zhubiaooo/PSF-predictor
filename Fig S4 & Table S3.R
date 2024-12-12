library(openxlsx)
library(dplyr)

pot_bio_data = read.xlsx("Coexistence_data.xlsx", sheet = "Pot_feedback_phase", rowNames = F, colNames = T)
#pot_bio_data = subset(pot_bio_data, density != "0")
dim(pot_bio_data)
#write.csv(filtered_data_all,"filtered_data_all-11-24-3.csv")
################################################################################
## 选择同种邻居或者异种邻居
pot_bio_data_mix = subset(pot_bio_data, group == "1" & above_bio != 0)
pot_bio_data_mix = pot_bio_data_mix[,c(2:6,15)]; colnames(pot_bio_data_mix)[6] = "mix_bio"
pot_bio_data_mono = subset(pot_bio_data, group == "0" & above_bio != 0) 
pot_bio_data_mono = pot_bio_data_mono[,c(2:6,15)]; colnames(pot_bio_data_mono)[6] = "mono_bio"
colnames(pot_bio_data_mono)
##
RII_data_all = pot_bio_data_mono %>% left_join(pot_bio_data_mix)
#RII_data_all = merge(pot_bio_data_mix, pot_bio_data_mono, by = c("density", "drought", "condi_sp", "block", "focal_sp"))
## 转换
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

mod_lme = lmer(bio_val_sq ~ drought * condi_sp * focal_sp * type + (1|block), data = lme_data_test)
#View(subset(lme_data_test, abbrev_focal == "Bb" & drought == "D"))
anova(mod_lme, type = 3)
shapiro.test(residuals(mod_lme))

emm1 = emmeans(mod_lme, specs = pairwise ~ drought | condi_sp, type = 'response', adjust = 'tukey')
emm1_multi = multcomp::cld(emm1,alpha=0.05,Letters=letters,adjust="none",decreasing = T)
emm1_multi$.group <- trimws(emm1_multi$.group)

### Fig S4
RII_data_all <- RII_data_all %>%
  mutate(abbrev_focal = recode(focal_sp,`1` = "Bb",`2` = "Ac",`3` = "Car",
                               `4` = "Cal",`5` = "Sp",`6` = "Pb"))
RII_data_all <- RII_data_all %>%
  mutate(abbrev_condi = recode(condi_sp,`1` = "Bb",`2` = "Ac",`3` = "Car",
                               `4` = "Cal",`5` = "Sp",`6` = "Pb"))

RII_data_all$drought2 = ifelse(RII_data_all$drought == "G", "Ambient", 
                               ifelse(RII_data_all$drought == "D", "Drought", "Sterilized"))
## Aboveground biomass in Monoculture 
################################################################################
pd = position_dodge(.4)

Bio_mono_sum = RII_data_all %>% group_by(drought2, abbrev_condi, abbrev_focal) %>% 
  summarise(mean_bio = mean(mono_bio),sd_bio = sd(mono_bio, na.rm = TRUE),       
            se_bio = sd_bio / sqrt(n()), .groups = 'drop') %>%
  mutate(abbrev_condi = ifelse(is.na(abbrev_condi), "Sterilized", abbrev_condi))


Bio_mono_sum$mean_bio2 = round(Bio_mono_sum$mean_bio, 2)
Bio_mono_sum$drought2 = factor(Bio_mono_sum$drought2, levels = c("Sterilized", "Ambient", "Drought"))

library(scales)
library(ggforce)
ggplot(subset(Bio_mono_sum, drought2 != "Sterilized"), aes(abbrev_condi, abbrev_focal)) +
  geom_tile(aes(fill = mean_bio), color = "white") +  #展示所有相关系数，颜色代表相关系数, color = 'gray'
  scale_fill_gradient2(low = ("#A67C2A"),mid = "white",high = muted("#001260"), midpoint = 1, limits=c(0.45,1.51)) + # muted
  geom_text(aes(label = mean_bio2), size = 2.8, color = 'black') + 
  labs(x="Soil conditioning species",y="Response species",fill = NULL, tag = "a") + 
  theme_classic() + mytheme + 
  geom_hline(aes(yintercept = 1.5), linetype = 1) +
  geom_hline(aes(yintercept = 2.5), linetype = 1) +
  geom_hline(aes(yintercept = 3.5), linetype = 1) +
  geom_hline(aes(yintercept = 4.5), linetype = 1) +
  geom_hline(aes(yintercept = 5.5), linetype = 1) +
  theme(legend.position = "right") + #"bottom"
  theme(axis.text.x = element_text(),
        axis.text.y = element_text(),
        axis.ticks = element_blank(),
        axis.line = element_blank()) +
  scale_x_discrete(expand = c(0,0)) + #scale_y_discrete(expand = c(0,0)) + 
  guides(fill = guide_colorbar(barwidth = 0.8, barheight = 10, 
                               title = "Plant mass in monoculture", title.position = "right",
                               title.theme = element_text(angle = 90, vjust = 0))) + 
  facet_row(~drought2, scales = "free_x", space = "free", strip.position = "bottom")  -> Fig_S4a; Fig_S4a

## Aboveground biomass in Mixture 
################################################################################
pd = position_dodge(.4)
Bio_mix_sum = subset(RII_data_all, mix_bio != "NA") %>% group_by(drought2, abbrev_condi, abbrev_focal) %>% 
  summarise(mean_bio = mean(mix_bio),sd_bio = sd(mix_bio, na.rm = TRUE),       
            se_bio = sd_bio / sqrt(n()), .groups = 'drop') %>%
  mutate(abbrev_condi = ifelse(is.na(abbrev_condi), "Sterilized", abbrev_condi))


Bio_mix_sum$mean_bio2 = round(Bio_mix_sum$mean_bio, 2)
Bio_mix_sum$drought2 = factor(Bio_mix_sum$drought2, levels = c("Sterilized", "Ambient", "Drought"))

ggplot(subset(Bio_mix_sum, drought2 != "Sterilized"), aes(abbrev_condi, abbrev_focal)) +
  geom_tile(aes(fill = mean_bio), color = "white") +
  scale_fill_gradient2(low = ("#A67C2A"),mid = "white",high = muted("#001260"), midpoint = 1, limits=c(0.45,1.51)) + # muted
  geom_text(aes(label = mean_bio2), size = 2.8, color = 'black') + 
  labs(x="Soil conditioning species",y="Response species",fill = NULL, tag = "b") + 
  theme_classic() + mytheme + 
  theme(legend.position = "right") + #"bottom"
  geom_hline(aes(yintercept = 1.5), linetype = 1) +
  geom_hline(aes(yintercept = 2.5), linetype = 1) +
  geom_hline(aes(yintercept = 3.5), linetype = 1) +
  geom_hline(aes(yintercept = 4.5), linetype = 1) +
  geom_hline(aes(yintercept = 5.5), linetype = 1) +
  theme(axis.text.x = element_text(),
        axis.text.y = element_text(),
        axis.ticks = element_blank(),
        axis.line = element_blank()) +
  scale_x_discrete(expand = c(0,0)) + #scale_y_discrete(expand = c(0,0)) + 
  guides(fill = guide_colorbar(barwidth = 0.8, barheight = 10, 
                               title = "Plant mass in mixture", title.position = "right",
                               title.theme = element_text(angle = 90))) + 
  facet_row(~drought2, scales = "free_x", space = "free", strip.position = "bottom")  -> Fig_S4b; Fig_S4b # 

library(patchwork)
Fig_S4a/Fig_S4b

