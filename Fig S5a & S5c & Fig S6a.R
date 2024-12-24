library(openxlsx)
library(dplyr)
library(lme4)
library(lmerTest)

pot_bio_data = read.xlsx("Coexistence_data.xlsx", sheet = "Pot_feedback_phase", rowNames = F, colNames = T)
#pot_bio_data = subset(pot_bio_data, density != "0")
################################################################################
## 
pot_bio_data_mix = subset(pot_bio_data, group == "1" & above_bio != 0)
pot_bio_data_mix = pot_bio_data_mix[,c(2:6,15)]; colnames(pot_bio_data_mix)[6] = "mix_bio"
pot_bio_data_mono = subset(pot_bio_data, group == "0" & above_bio != 0) 
pot_bio_data_mono = pot_bio_data_mono[,c(2:6,15)]; colnames(pot_bio_data_mono)[6] = "mono_bio"
colnames(pot_bio_data_mono)
##
#RII_data_all = pot_bio_data_mono %>% left_join(pot_bio_data_mix)
RII_data_all = merge(pot_bio_data_mix, pot_bio_data_mono, by = c("density", "drought", "condi_sp", "block", "focal_sp"))
## 
shapiro.test(sqrt(RII_data_all$mono_bio))
shapiro.test(sqrt(RII_data_all$mix_bio))
RII_data_all$mono_bio = sqrt(RII_data_all$mono_bio)
RII_data_all$mix_bio = sqrt(RII_data_all$mix_bio)

## 
RII_data_all$RCI = (RII_data_all$mix_bio - RII_data_all$mono_bio)/RII_data_all$mono_bio

#### 
pot_bio_data = RII_data_all

pot_bio_data$Code_focal = paste0(pot_bio_data$density,"_", pot_bio_data$drought, "_", pot_bio_data$focal_sp, "_", pot_bio_data$condi_sp)

### 
α_AB = c(paste0("L_D_", 1:6, "_", 1:6), paste0("L_G_", 1:6, "_", 1:6))

### 
γ_AB = c(paste0("0_0_", 1:6, "_", 0))

### 
β_AB = setdiff(unique(pot_bio_data$Code_focal), c(α_AB, γ_AB))
#View(as.data.frame(β_AB))

### 
pot_bio_data_β = subset(pot_bio_data, Code_focal %in% β_AB)
unique(pot_bio_data_β$Code_focal)
# View(pot_bio_data_β)
pot_bio_data_β$pair = substr(pot_bio_data_β$Code_focal, 5, 7) 
pot_bio_data_β$combi[pot_bio_data_β$pair == "1_2" | pot_bio_data_β$pair == "2_1"] <- "1_2_2_1" 
pot_bio_data_β$combi[pot_bio_data_β$pair == "3_2" | pot_bio_data_β$pair == "2_3"] <- "3_2_2_3" 
pot_bio_data_β$combi[pot_bio_data_β$pair == "4_2" | pot_bio_data_β$pair == "2_4"] <- "4_2_2_4" 
pot_bio_data_β$combi[pot_bio_data_β$pair == "5_2" | pot_bio_data_β$pair == "2_5"] <- "5_2_2_5" 
pot_bio_data_β$combi[pot_bio_data_β$pair == "6_2" | pot_bio_data_β$pair == "2_6"] <- "6_2_2_6" 
pot_bio_data_β$combi[pot_bio_data_β$pair == "1_3" | pot_bio_data_β$pair == "3_1"] <- "1_3_3_1" 
pot_bio_data_β$combi[pot_bio_data_β$pair == "4_3" | pot_bio_data_β$pair == "3_4"] <- "4_3_3_4" 
pot_bio_data_β$combi[pot_bio_data_β$pair == "5_3" | pot_bio_data_β$pair == "3_5"] <- "5_3_3_5" 
pot_bio_data_β$combi[pot_bio_data_β$pair == "6_3" | pot_bio_data_β$pair == "3_6"] <- "6_3_3_6" 
pot_bio_data_β$combi[pot_bio_data_β$pair == "1_6" | pot_bio_data_β$pair == "6_1"] <- "1_6_6_1" 
pot_bio_data_β$combi[pot_bio_data_β$pair == "4_6" | pot_bio_data_β$pair == "6_4"] <- "4_6_6_4" 
pot_bio_data_β$combi[pot_bio_data_β$pair == "5_6" | pot_bio_data_β$pair == "6_5"] <- "5_6_6_5" 
pot_bio_data_β$combi[pot_bio_data_β$pair == "1_4" | pot_bio_data_β$pair == "4_1"] <- "1_4_4_1" 
pot_bio_data_β$combi[pot_bio_data_β$pair == "5_4" | pot_bio_data_β$pair == "4_5"] <- "5_4_4_5" 
pot_bio_data_β$combi[pot_bio_data_β$pair == "5_1" | pot_bio_data_β$pair == "1_5"] <- "5_1_1_5" 

### 
pot_bio_data_α = subset(pot_bio_data, Code_focal %in% α_AB)
dim(pot_bio_data_α)
# View(pot_bio_data_α)
### 
pot_bio_data_γ = subset(pot_bio_data, Code_focal %in% γ_AB)
dim(pot_bio_data_γ)
# View(pot_bio_data_γ)

## 
pot_bio_data_α <- pot_bio_data_α %>%
  mutate(abbrev_focal = recode(focal_sp,`1` = "Bb",`2` = "Ac",`3` = "Car",
                               `4` = "Cal",`5` = "Sp",`6` = "Pb"))
pot_bio_data_α <- pot_bio_data_α %>%
  mutate(abbrev_condi = recode(condi_sp,`1` = "Bb",`2` = "Ac",`3` = "Car",
                               `4` = "Cal",`5` = "Sp",`6` = "Pb"))

pot_bio_data_α$drought2 = ifelse(pot_bio_data_α$drought == "G", "Ambient", 
                           ifelse(pot_bio_data_α$drought == "D", "Drought", "Sterilized"))

pot_bio_data_α$drought2 = as.factor(pot_bio_data_α$drought2)
pot_bio_data_α$focal_sp = as.factor(pot_bio_data_α$focal_sp)
pot_bio_data_α$condi_sp = as.factor(pot_bio_data_α$condi_sp)
# View(pot_bio_data_α)

### Adding covariates
Soil_group = read.xlsx("Coexistence_data.xlsx", sheet = "Pot_conditioning_phase", rowNames = T, colNames = T)
Soil_group$Sample_ID = rownames(Field_group)
colnames(Soil_group)
Soil_group$condi_bio = sqrt(Soil_group$biomass)
Soil_group = Soil_group %>% 
  mutate(abbrev_condi = recode(focal,`1` = "Bb",`2` = "Ac",`3` = "Car",
                               `4` = "Cal",`5` = "Sp",`6` = "Pb"))
Soil_group$drought2 = ifelse(Soil_group$drought == "G", "Ambient", "Drought")

pot_bio_data_α2 = pot_bio_data_α %>% left_join(Soil_group[,c("abbrev_condi","drought2","block","condi_bio")])

## intrinsic growth ability
lme1 = lmer(mono_bio ~  condi_bio + drought2 * abbrev_focal + (1|block), pot_bio_data_α2)
anova(lme1, type = "3")
summary(lme1)
ranova(lme1)


## 
pot_bio_data_β <- pot_bio_data_β %>%
  mutate(abbrev_focal = recode(focal_sp,`1` = "Bb",`2` = "Ac",`3` = "Car",
                               `4` = "Cal",`5` = "Sp",`6` = "Pb"))
pot_bio_data_β <- pot_bio_data_β %>%
  mutate(abbrev_condi = recode(condi_sp,`1` = "Bb",`2` = "Ac",`3` = "Car",
                               `4` = "Cal",`5` = "Sp",`6` = "Pb"))

pot_bio_data_β$drought2 = ifelse(pot_bio_data_β$drought == "G", "Ambient", 
                                 ifelse(pot_bio_data_β$drought == "D", "Drought", "Sterilized"))

pot_bio_data_β$drought2 = as.factor(pot_bio_data_β$drought2)
pot_bio_data_β$focal_sp = as.factor(pot_bio_data_β$focal_sp)
pot_bio_data_β$condi_sp = as.factor(pot_bio_data_β$condi_sp)
# View(pot_bio_data_β)
colnames(pot_bio_data_β)
unique(pot_bio_data_β$combi)

### Adding covariates
Soil_group = read.xlsx("Coexistence_data.xlsx", sheet = "Pot_conditioning_phase", rowNames = T, colNames = T)
Soil_group$Sample_ID = rownames(Field_group)
colnames(Soil_group)
Soil_group$condi_bio = sqrt(Soil_group$biomass)
Soil_group = Soil_group %>% 
  mutate(abbrev_condi = recode(focal,`1` = "Bb",`2` = "Ac",`3` = "Car",
                               `4` = "Cal",`5` = "Sp",`6` = "Pb"))
Soil_group$drought2 = ifelse(Soil_group$drought == "G", "Ambient", "Drought")

pot_bio_data_β2 = pot_bio_data_β %>% left_join(Soil_group[,c("abbrev_condi","drought2","block","condi_bio")])

## competition ability
lme1 = lmer(RCI ~ condi_bio + drought2 * abbrev_focal + (1|block) + (1|combi), pot_bio_data_β2)
anova(lme1, type = "3")
summary(lme1)
ranova(lme1)

## Fig S5a
pot_bio_growth = pot_bio_data_α %>% group_by(abbrev_focal, drought2) %>% 
  summarise(bio_mono = mean(mono_bio),sd_mono = sd(mono_bio, na.rm = TRUE),       
            se_mono = sd_mono / sqrt(n()), .groups = 'drop') 

pd = position_dodge(.5)
library(ggplot2)
pot_bio_growth$abbrev_focal = factor(pot_bio_growth$abbrev_focal, levels = c("Ac","Bb","Cal","Car","Pb","Sp"))

ggplot(pot_bio_growth, aes(x = abbrev_focal, y = bio_mono)) + 
  geom_errorbar(aes(ymin = bio_mono - se_mono, ymax = bio_mono + se_mono, group = drought2), 
                position=pd, width = 0, color = "black", show.legend = F) +
  geom_point(size=3,position=pd,alpha=1, pch = 21, aes(fill = drought2), color = "black") +
  scale_fill_manual(values = c("#70A7C3","#A67C2A")) + 
  scale_color_manual(values = c("#70A7C3","#A67C2A")) + 
  #geom_line(aes(x = abbrev_focal, y = bio_mono, group = drought2), color = "black", size = 0.5, inherit.aes = FALSE) + 
  theme_bw() + mytheme +
  annotate("text", x = 4.5, y = 0.6, parse = TRUE, size = 3.5, label = expression("W" ~ ":" ~ italic(p) == 0.815)) + 
  annotate("text", x = 4.5, y = 0.5, parse = TRUE, size = 3.5, label = expression("R" ~ ":" ~ italic(p) == 0.273)) + 
  annotate("text", x = 4.5, y = 0.4, parse = TRUE, size = 3.5, label = expression("W × R" ~ ":" ~ italic(p) == 0.326)) +
  scale_y_continuous(labels = scales::label_comma(accuracy =0.01)) + 
  #geom_hline(aes(yintercept = 0), linetype = "dashed") +  
  labs(x = "Response species",y = "Intrinsic growth rate",fill = NULL, tag = "a") + 
  theme(legend.position = c(0.12, 0.88),
        legend.key = element_blank(),
        legend.background = element_rect(fill = NA)) -> Fig_S5a; Fig_S5a

## Fig S5C
field_mono_mean = subset(pot_bio_data_β, RCI != "NA") %>% group_by(drought2, abbrev_focal) %>%
  summarise(mean_bio = mean(RCI),sd_bio = sd(RCI, na.rm = TRUE),       
            se_bio = sd_bio / sqrt(n()), .groups = 'drop') 

ggplot(field_mono_mean, aes(x = abbrev_focal, y = mean_bio)) + 
  geom_errorbar(aes(ymin = mean_bio - se_bio, ymax = mean_bio + se_bio, group = drought2), 
                position=pd, width = 0, color = "black", show.legend = F) +
  geom_point(size=3,position=pd,alpha=1, pch = 21, aes(fill = drought2), color = "black") +
  scale_fill_manual(values = c("#70A7C3","#A67C2A")) + 
  scale_color_manual(values = c("#70A7C3","#A67C2A")) + 
  theme_bw() + mytheme +
  #annotate("text", x = 3.8, y = 0.30, parse = TRUE, size = 3.5, color = "#70A7C3", label = "na") +
  annotate("text", x = 5.5, y = 0.6, parse = TRUE, size = 3.5, label = expression("W" ~ ":" ~ italic(p) == 0.628)) + 
  annotate("text", x = 5.5, y = 0.5, parse = TRUE, size = 3.5, label = expression("R" ~ ":" ~ italic(p) == 0.233)) + 
  annotate("text", x = 5.5, y = 0.40, parse = TRUE, size = 3.5, label = expression("W × R" ~ ":" ~ italic(p) == 0.614)) +
  scale_y_continuous(labels = scales::label_comma(accuracy =0.01)) + 
  labs(x = "Responding species",y = "Competitive ability",fill = NULL, tag = "c") + 
  theme(legend.position = "none",
        legend.key = element_blank(),
        legend.background = element_rect(fill = NA)) -> Fig_S5C; Fig_S5C

################################################################################
################################## Fig S6a #####################################
################################################################################
colnames(pot_bio_growth)[3:5] = c("mono_mean","sd_mono","se_mono")

all_predict_data = subset(pot_bio_data_β, RCI != "NA") %>% group_by(drought2, abbrev_condi, abbrev_focal) %>%
  summarise(mean_CI = mean(RCI),sd_CI = sd(RCI, na.rm = TRUE),se_CI = sd_CI / sqrt(n()), .groups = 'drop') %>% 
  left_join(pot_bio_growth)

all_predict_data$Code = paste0(all_predict_data$abbrev_condi,"-",all_predict_data$abbrev_focal)
unique(all_predict_data$Code)
all_predict_data$combi[all_predict_data$Code == "Bb-Ac" | all_predict_data$Code == "Ac-Bb"] <- "Ac-Bb" 
all_predict_data$combi[all_predict_data$Code == "Bb-Car" | all_predict_data$Code == "Car-Bb"] <- "Car-Bb" 
all_predict_data$combi[all_predict_data$Code == "Bb-Cal" | all_predict_data$Code == "Cal-Bb"] <- "Cal-Bb" 
all_predict_data$combi[all_predict_data$Code == "Bb-Sp" | all_predict_data$Code == "Sp-Bb"] <- "Sp-Bb" 
all_predict_data$combi[all_predict_data$Code == "Bb-Pb" | all_predict_data$Code == "Pb-Bb"] <- "Bb-Pb" 
all_predict_data$combi[all_predict_data$Code == "Ac-Car" | all_predict_data$Code == "Car-Ac"] <- "Car-Ac" 
all_predict_data$combi[all_predict_data$Code == "Ac-Cal" | all_predict_data$Code == "Cal-Ac"] <- "Cal-Ac" 
all_predict_data$combi[all_predict_data$Code == "Ac-Sp" | all_predict_data$Code == "Sp-Ac"] <- "Sp-Ac" 
all_predict_data$combi[all_predict_data$Code == "Ac-Pb" | all_predict_data$Code == "Pb-Ac"] <- "Pb-Ac" 
all_predict_data$combi[all_predict_data$Code == "Car-Cal" | all_predict_data$Code == "Cal-Car"] <- "Cal-Car" 
all_predict_data$combi[all_predict_data$Code == "Car-Sp" | all_predict_data$Code == "Sp-Car"] <- "Sp-Car" 
all_predict_data$combi[all_predict_data$Code == "Car-Pb" | all_predict_data$Code == "Pb-Car"] <- "Pb-Car" 
all_predict_data$combi[all_predict_data$Code == "Cal-Sp" | all_predict_data$Code == "Sp-Cal"] <- "Sp-Cal" 
all_predict_data$combi[all_predict_data$Code == "Cal-Pb" | all_predict_data$Code == "Pb-Cal"] <- "Pb-Cal" 
all_predict_data$combi[all_predict_data$Code == "Sp-Pb" | all_predict_data$Code == "Pb-Sp"] <- "Pb-Sp" 


AAA = subset(all_predict_data, drought2 == "Ambient" & mean_CI != "NA")
BBB = subset(all_predict_data, drought2 == "Drought" & mean_CI != "NA")

bestFitM2(data= AAA, x= "mono_mean", y = "mean_CI")
FitM(data = AAA, x= "mono_mean", y = "mean_CI",model = "line2P" )

bestFitM2(data= BBB, x= "mono_mean", y = "mean_CI")
FitM(data = BBB, x= "mono_mean", y = "mean_CI",model = "line3P" )

ggplot(data = all_predict_data, aes(x = mono_mean, y = mean_CI, color = drought2)) + 
  geom_smooth(data = subset(all_predict_data, drought2 == "Ambient"), linetype = 2,
              method = "lm", se = TRUE, aes(fill = drought2)) +
  geom_smooth(data = subset(all_predict_data, drought2 == "Drought"), linetype = 2,
              method = "lm", se = TRUE, aes(fill = drought2), formula = y ~ poly(x, 2, raw=TRUE)) +
  scale_color_manual(values = c("#70A7C3","#A67C2A")) + 
  scale_fill_manual(values = c("#B7D3E1","#D2BD94")) + 
  ggnewscale::new_scale_fill() + 
  geom_point(size = 3, aes(fill = drought2), pch = 21, color = "black") + 
  scale_fill_manual(values = c("#70A7C3","#A67C2A")) + 
  theme_bw() + mytheme + theme(legend.position = c(0.15, 0.85)) + 
  geom_line(aes(group=interaction(combi, drought2), color = drought2), alpha=0.2, size = 0.6) + 
  scale_x_continuous(labels = scales::label_comma(accuracy =0.01)) +
  scale_y_continuous(labels = scales::label_comma(accuracy =0.01)) + 
  annotate("text", x = 0.8, y = 1.30, parse = TRUE, size = 3.5, color = "#70A7C3",
           label = expression(italic(R)^2 == 0.014 ~ "," ~ italic(p) == 0.533)) +
  annotate("text", x = 0.8, y = 1.10, parse = TRUE, size = 3.5, color = "#A67C2A",
           label = expression(italic(R)^2 == 0.130 ~ "," ~ italic(p) == 0.152)) +
  labs(x = "Intrinsic growth ability",
       y = "Competition ability", tag = "a") -> Fig_S6a; Fig_S6a

