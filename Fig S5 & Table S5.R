################################################################################
# 读入田间数据
field_data = read.xlsx("Coexistence_data.xlsx", sheet = "Field_exp", rowNames = F, colNames = T)
head(field_data)

## 混种与单种数据
field_data_2023 = subset(field_data, year == "2023")
field_data_2023$ind_bio_sq = sqrt(field_data_2023$ind_bio)
field_data_2023$drought2 = ifelse(field_data_2023$drought == "G", "Ambient", "Drought")

field_data_2023 <- field_data_2023 %>%
  mutate(abbrev_focal = recode(species,`1` = "Bb",`2` = "Ac",`3` = "Car",
                               `4` = "Cal",`5` = "Sp",`6` = "Pb"))

## Monoculture
field_data_mono = subset(field_data_2023, type == "Mono")

###
aaaaaa = subset(field_data_mono, ind_bio_sq != "NA")
mod = lmer(ind_bio_sq ~ drought2 * abbrev_focal + (1|block), data = aaaaaa)
anova(mod, type = 3)

emm1 = emmeans(mod, specs = pairwise ~ abbrev_focal, type = 'response', adjust = 'tukey')
emm1_multi = multcomp::cld(emm1,alpha=0.05,Letters=letters,adjust="none",decreasing = T)
emm1_multi$.group <- trimws(emm1_multi$.group)

field_mono_mean = subset(field_data_mono, ind_bio_sq != "NA") %>% group_by(drought2, abbrev_focal) %>%
  summarise(mono_mean = mean(ind_bio_sq),sd_mono = sd(ind_bio_sq, na.rm = TRUE),       
            se_mono = sd_mono / sqrt(n()), .groups = 'drop') %>%
  left_join(emm1_multi)

### plot
ggplot(field_mono_mean, aes(x = abbrev_focal, y = mono_mean)) + 
  geom_errorbar(aes(ymin = mono_mean - se_mono, ymax = mono_mean + se_mono, group = drought2), 
                position=pd, width = 0, color = "black", show.legend = F) +
  geom_text(data = subset(field_mono_mean, drought2 == "Drought"), mapping = aes(x = abbrev_focal,y = mono_mean+0.06,label = .group, group = drought2), 
            color = "black", size = 3,position = position_dodge(.5)) +
  geom_point(size=3,position=pd,alpha=1, pch = 21, aes(fill = drought2), color = "black") +
  scale_fill_manual(values = c("#70A7C3","#A67C2A")) + 
  scale_color_manual(values = c("#70A7C3","#A67C2A")) + 
  theme_bw() + mytheme +
  annotate("text", x = 3.8, y = 0.61, parse = TRUE, size = 3.5, color = "#70A7C3", label = "na") +
  annotate("text", x = 5.0, y = 0.50, parse = TRUE, size = 3.5, label = expression("W" ~ ":" ~ italic(p) == 0.008)) + 
  annotate("text", x = 5.0, y = 0.40, parse = TRUE, size = 3.5, label = expression("G" ~ ":" ~ italic(p) < 0.001)) + 
  annotate("text", x = 5.0, y = 0.30, parse = TRUE, size = 3.5, label = expression("W × G" ~ ":" ~ italic(p) == 0.597)) +
  scale_y_continuous(labels = scales::label_comma(accuracy =0.01)) + 
  labs(x = "Growing species",y = "Intrinsic growth rate",fill = NULL) + 
  theme(legend.position = "none",
        legend.key = element_blank(),
        legend.background = element_rect(fill = NA)) -> Fig_S4aa; Fig_S4aa


field_mono_mean2 = subset(field_data_mono, ind_bio_sq != "NA")  %>% group_by(drought2) %>% 
  summarise(mono_mean = mean(ind_bio_sq),sd_mono = sd(ind_bio_sq, na.rm = TRUE),       
            se_mono = sd_mono / sqrt(n()), .groups = 'drop')

field_mono_mean2$.group = c("b","a")

ggplot(field_mono_mean2, aes(x = drought2, y = mono_mean)) + 
  geom_errorbar(aes(ymin = mono_mean - se_mono, ymax = mono_mean + se_mono, group = drought2), 
                position=pd, width = 0, color = "black", show.legend = F) +
  geom_point(size=3,position=pd,alpha=1, pch = 21, aes(fill = drought2), color = "black") +
  geom_text(data = field_mono_mean2, mapping = aes(x = drought2,y = mono_mean + se_mono + 0.03,label = .group, group = drought2), 
            color = "black", size = 3,position = position_dodge(.5)) +
  scale_fill_manual(values = c("#70A7C3","#A67C2A")) + 
  scale_color_manual(values = c("#70A7C3","#A67C2A")) + 
  theme_bw() + mytheme +
  scale_y_continuous(labels = scales::label_comma(accuracy =0.01)) + 
  labs(x = NULL,y = NULL, fill = NULL) + 
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 25, hjust = 1, vjust = 1),
        legend.key = element_blank(),
        legend.background = element_rect(fill = NA)) -> Fig_S4ab; Fig_S4ab

g <- ggplotGrob(Fig_S4ab)
Fig_S4aa + annotation_custom(g,xmin=0.5,xmax=2.2,ymin=0.7,ymax=1.4) -> Fig_S4a; Fig_S4a

################################################################################
# 读入田间数据
field_data = read.xlsx("Coexistence_data.xlsx", sheet = "Field_exp", rowNames = F, colNames = T)
#field_data$ind_bio = field_data$biomass/field_data$survive
#field_data$ind_bio = ifelse(field_data$ind_bio == "NaN", 0, field_data$ind_bio)
field_data$ind_bio = sqrt(field_data$ind_bio)
head(field_data)

## 混种与单种数据
field_data_mix = subset(field_data, type == "Mix" & year == "2023")
field_data_mono = subset(field_data, type == "Mono" & year == "2023")

pair = unique(field_data_mix$pair); length(pair)
block = unique(field_data_mix$block)
drought = unique(field_data_mix$drought)
final_RII_data = NULL

for (i in pair) {
  for (ii in block) {
    for (iii in drought) {
      mix_select_data = subset(field_data_mix, pair == i & block == ii & drought == iii)
      #
      #sp_A = mix_select_data$species[1]; sp_B = mix_select_data$species[2]
      sp_A = min(mix_select_data$species)  # 较小的值
      sp_B = max(mix_select_data$species)  # 较大的值
      
      mix_sp_A = mix_select_data[mix_select_data$species %in% sp_A,]
      mix_sp_B = mix_select_data[mix_select_data$species %in% sp_B,]
      #
      mono_sp_A = subset(field_data_mono, pair == sp_A & block == ii & drought == iii)
      mono_sp_B = subset(field_data_mono, pair == sp_B & block == ii & drought == iii)
      ##
      RII_sp_A = (mix_sp_A$ind_bio * 2)/(mix_sp_A$ind_bio + mono_sp_A$ind_bio)
      RII_sp_B = (mix_sp_B$ind_bio * 2)/(mix_sp_B$ind_bio + mono_sp_B$ind_bio)
      RII_data = data.frame(pair = i, block = ii, drought = iii, `Species A` = sp_A, `Species B` = sp_B,
                            RII_sp_A = RII_sp_A, RII_sp_B = RII_sp_B) %>% as.data.frame()
      final_RII_data = rbind(final_RII_data, RII_data)
    }
  }
}
final_RII_data

## 数据转换
head(final_RII_data)
pair = unique(final_RII_data$pair); length(pair)
block = unique(final_RII_data$block)
drought = unique(final_RII_data$drought)
CI_field_data = NULL

for (i in pair) {
  for (ii in block) {
    for (iii in drought) {
      mix_select_data = subset(final_RII_data, pair == i & block == ii & drought == iii)
      Spcies_A_data = mix_select_data[,c(1:4,6)]; colnames(Spcies_A_data)[c(4,5)] = c("species", "CI")
      Spcies_B_data = mix_select_data[,c(1:3,5,7)]; colnames(Spcies_B_data)[c(4,5)] = c("species", "CI")
      data_trans = rbind(Spcies_A_data, Spcies_B_data)
      CI_field_data = rbind(CI_field_data, data_trans)
    }
  }
}

CI_field_data

## 
field_data_2023 = CI_field_data
field_data_2023$drought2 = ifelse(field_data_2023$drought == "G", "Ambient", "Drought")

field_data_2023 <- field_data_2023 %>%
  mutate(abbrev_focal = recode(species,`1` = "Bb",`2` = "Ac",`3` = "Car",
                               `4` = "Cal",`5` = "Sp",`6` = "Pb"))

## drought X Growing species
bio_mod = lmer(CI ~ drought2 * abbrev_focal + (1|block), data = field_data_2023)
anova(bio_mod)

emm1 = emmeans(bio_mod, specs = pairwise ~ drought2 * abbrev_focal, type = 'response', adjust = 'tukey')
emm1_multi = multcomp::cld(emm1,alpha=0.05,Letters=letters,adjust="none",decreasing = T)
emm1_multi$.group <- trimws(emm1_multi$.group)

field_mono_mean = subset(field_data_2023, CI != "NA") %>% group_by(drought2, abbrev_focal) %>%
  summarise(mean_bio = mean(CI),sd_bio = sd(CI, na.rm = TRUE),       
            se_bio = sd_bio / sqrt(n()), .groups = 'drop') %>%
  left_join(emm1_multi)

### plot
ggplot(field_mono_mean, aes(x = abbrev_focal, y = mean_bio)) + 
  geom_errorbar(aes(ymin = mean_bio - se_bio, ymax = mean_bio + se_bio, group = drought2), 
                position=pd, width = 0, color = "black", show.legend = F) +
  geom_text(data = field_mono_mean, mapping = aes(x = abbrev_focal,y = mean_bio + se_bio + 0.03,label = .group, group = drought2), 
            color = "black", size = 3,position = position_dodge(.5)) +
  geom_point(size=3,position=pd,alpha=1, pch = 21, aes(fill = drought2), color = "black") +
  scale_fill_manual(values = c("#70A7C3","#A67C2A")) + 
  scale_color_manual(values = c("#70A7C3","#A67C2A")) + 
  theme_bw() + mytheme +
  annotate("text", x = 3.8, y = 0.30, parse = TRUE, size = 3.5, color = "#70A7C3", label = "na") +
  annotate("text", x = 1.5, y = 0.30, parse = TRUE, size = 3.5, label = expression("W" ~ ":" ~ italic(p) == 0.704)) + 
  annotate("text", x = 1.5, y = 0.20, parse = TRUE, size = 3.5, label = expression("G" ~ ":" ~ italic(p) < 0.001)) + 
  annotate("text", x = 1.5, y = 0.10, parse = TRUE, size = 3.5, label = expression("W × G" ~ ":" ~ italic(p) == 0.015)) +
  scale_y_continuous(labels = scales::label_comma(accuracy =0.01), limits = c(0.00, 1.22)) + 
  labs(x = "Growing species",y = "Competitive ability",fill = NULL) + 
  theme(legend.position = "none",
        legend.key = element_blank(),
        legend.background = element_rect(fill = NA)) -> Fig_S4ba; Fig_S4ba


field_mono_mean = subset(field_data_2023, CI != "NA") %>% group_by(drought2) %>%
  summarise(mean_bio = mean(CI),sd_bio = sd(CI, na.rm = TRUE),       
            se_bio = sd_bio / sqrt(n()), .groups = 'drop') 
field_mono_mean$.group = c("a","a")

### plot
ggplot(field_mono_mean, aes(x = drought2, y = mean_bio)) + 
  geom_errorbar(aes(ymin = mean_bio - se_bio, ymax = mean_bio + se_bio, group = drought2), 
                position=pd, width = 0, color = "black", show.legend = F) +
  geom_text(data = field_mono_mean, mapping = aes(x = drought2,y = mean_bio + se_bio +0.01,label = .group, group = drought2), 
            color = "black", size = 3, position = position_dodge(.5)) +
  geom_point(size=3,position=pd,alpha=1, pch = 21, aes(fill = drought2), color = "black") +
  scale_fill_manual(values = c("#70A7C3","#A67C2A")) + 
  scale_color_manual(values = c("#70A7C3","#A67C2A")) + 
  theme_bw() + mytheme +
  scale_y_continuous(labels = scales::label_comma(accuracy =0.01)) + 
  labs(x = NULL,y = NULL,fill = NULL) + 
  theme(legend.position = "none",
        legend.key = element_blank(),
        axis.text.x = element_text(angle = 25, hjust = 1, vjust = 1),
        legend.background = element_rect(fill = NA)) -> Fig_S4bb; Fig_S4bb

g <- ggplotGrob(Fig_S4bb)
Fig_S4ba + annotation_custom(g,xmin=4.5,xmax=6.2,ymin=0.7-0.8,ymax=1.4-0.8) -> Fig_S4b; Fig_S4b

################################################################################
## plant performance in mixture
# 读入田间数据
field_data = read.xlsx("Coexistence_data.xlsx", sheet = "Field_exp", rowNames = F, colNames = T)
#field_data$ind_bio = field_data$biomass/field_data$survive
#field_data$ind_bio = ifelse(field_data$ind_bio == "NaN", 0, field_data$ind_bio)
head(field_data)

## 混种与单种数据
field_data_2023 = subset(field_data, year == "2023")
field_data_2023$ind_bio_sq = sqrt(field_data_2023$ind_bio)
field_data_2023$drought2 = ifelse(field_data_2023$drought == "G", "Ambient", "Drought")

field_data_2023 <- field_data_2023 %>%
  mutate(abbrev_focal = recode(species,`1` = "Bb",`2` = "Ac",`3` = "Car",
                               `4` = "Cal",`5` = "Sp",`6` = "Pb"))

## mixture
field_mix_all = subset(field_data_2023, type == "Mix")
bio_mod = lmer(ind_bio_sq ~ drought2 * abbrev_focal + (1|block), data = field_mix_all)
anova(bio_mod)

################################################################################
## Growing species
emm1 = emmeans(bio_mod, specs = pairwise ~ drought2 * abbrev_focal, type = 'response', adjust = 'tukey')
emm1_multi = multcomp::cld(emm1,alpha=0.05,Letters=letters,adjust="none",decreasing = T)
emm1_multi$.group <- trimws(emm1_multi$.group)

## abbrev_focal
bio_field_mean = subset(field_mix_all, ind_bio_sq != "NA") %>% group_by(drought2, abbrev_focal) %>% 
  summarise(mean_bio = mean(ind_bio_sq),sd_bio = sd(ind_bio_sq, na.rm = TRUE),       
            se_bio = sd_bio / sqrt(n()), .groups = 'drop')%>%
  left_join(emm1_multi) 

pd = position_dodge(.5)
ggplot(bio_field_mean, aes(x = abbrev_focal, y = mean_bio, color = drought2)) + 
  geom_errorbar(aes(ymin = mean_bio - se_bio, ymax = mean_bio + se_bio, group = drought2), 
                position=pd, width = 0, color = "black", show.legend = F) +
  geom_text(data = subset(bio_field_mean, drought2 == "Ambient"), mapping = aes(x = abbrev_focal,y = mean_bio + se_bio + 0.03,label = .group), 
            color = "black", size = 3) +
  #geom_point(data = field_mix_all, aes(x = abbrev_focal, y = ind_bio_sq, color = abbrev_focal),
  #           position = position_jitterdodge(.2), alpha = 0.5, show.legend = F) + 
  geom_point(size=3,position=pd,alpha=1, pch = 21, aes(fill = drought2), color = "black") +
  scale_fill_manual(values = c("#70A7C3","#A67C2A")) + 
  scale_color_manual(values = c("#70A7C3","#A67C2A")) + 
  theme_bw() + mytheme +
  annotate("text", x = 5.2, y = 0.30, parse = TRUE, size = 3.5, label = expression("W" ~ ":" ~ italic(p) == 0.022)) + 
  annotate("text", x = 5.2, y = 0.20, parse = TRUE, size = 3.5, label = expression("G" ~ ":" ~ italic(p) < 0.001)) + 
  annotate("text", x = 5.2, y = 0.10, parse = TRUE, size = 3.5, label = expression("W × G" ~ ":" ~ italic(p) == 0.096)) +
  scale_y_continuous(labels = scales::label_comma(accuracy =0.01)) + 
  labs(x = "Growing species",y = "Cpmpetitive outcomes",fill = NULL) + 
  theme(legend.position = "none",
        legend.key = element_blank(),
        legend.background = element_rect(fill = NA)) -> Fig_S4ca; Fig_S4ca

## drought
emm1 = emmeans(bio_mod, specs = pairwise ~ drought2, type = 'response', adjust = 'none')
emm1_multi = multcomp::cld(emm1,alpha=0.05,Letters=letters,adjust="none",decreasing = T)
emm1_multi$.group <- trimws(emm1_multi$.group)

field_mix_all = subset(field_data_2023, type == "Mix")
bio_field_mean = subset(field_mix_all, ind_bio_sq != "NA") %>% group_by(drought2) %>% 
  summarise(mean_bio = mean(ind_bio_sq),sd_bio = sd(ind_bio_sq, na.rm = TRUE),       
            se_bio = sd_bio / sqrt(n()), .groups = 'drop') %>%
  left_join(emm1_multi[,c("drought2",".group")])

pd = position_dodge(.5)
library(ggplot2)

ggplot(bio_field_mean, aes(x = drought2, y = mean_bio)) + 
  geom_errorbar(aes(ymin = mean_bio - se_bio, ymax = mean_bio + se_bio), 
                position=pd, width = 0, color = "black", show.legend = F) +
  geom_text(data = bio_field_mean, mapping = aes(x = drought2,y = mean_bio+0.05,label = .group), 
            color = "black", size = 3) +
  geom_point(size=3.5,position=pd,alpha=1, pch = 21, aes(fill = drought2), color = "black") +
  scale_fill_manual(values = c("#70A7C3","#A67C2A")) + 
  scale_color_manual(values = c("#70A7C3","#A67C2A")) + 
  theme_bw() + mytheme +
  scale_y_continuous(labels = scales::label_comma(accuracy =0.01)) + 
  labs(x = NULL,y = NULL, fill = NULL) + 
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 25, hjust = 1, vjust = 1),
        legend.key = element_blank(),
        legend.background = element_rect(fill = NA)) -> Fig_S4cb; Fig_S4cb

g <- ggplotGrob(Fig_S4cb)
Fig_S4ca + annotation_custom(g,xmin=0.5,xmax=2.2,ymin=0.7,ymax=1.4) -> Fig_S4c; Fig_S4c


library(patchwork) # 4.79 x 9.27
Fig_S4a/Fig_S4b/Fig_S4c
