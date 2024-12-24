################################################################################
###################################  Fig S5b  ##################################
################################################################################
field_data = read.xlsx("Coexistence_data.xlsx", sheet = "Field_exp", rowNames = F, colNames = T)
head(field_data)

## 
field_data_2023 = subset(field_data, year == "2023")
field_data_2023$ind_bio_sq = sqrt(field_data_2023$ind_bio)
field_data_2023$drought2 = ifelse(field_data_2023$drought == "G", "Ambient", "Drought")

field_data_2023 <- field_data_2023 %>%
  mutate(abbrev_focal = recode(species,`1` = "Bb",`2` = "Ac",`3` = "Car",
                               `4` = "Cal",`5` = "Sp",`6` = "Pb"))

## Monoculture
field_data_mono = subset(field_data_2023, type == "Mono")

###
field_data_mono2 = subset(field_data_mono, ind_bio_sq != "NA")

## Table S3
mod = lmer(ind_bio_sq ~ drought2 * abbrev_focal + (1|block), data = field_data_mono2)
anova(mod, type = 3)
summary(mod)
ranova(mod)

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
  annotate("text", x = 1.2, y = 1.30, parse = TRUE, size = 3.5, label = expression("W" ~ ":" ~ italic(p) == 0.008)) + 
  annotate("text", x = 1.2, y = 1.20, parse = TRUE, size = 3.5, label = expression("G" ~ ":" ~ italic(p) < 0.001)) + 
  annotate("text", x = 1.2, y = 1.10, parse = TRUE, size = 3.5, label = expression("W × G" ~ ":" ~ italic(p) == 0.597)) +
  scale_y_continuous(labels = scales::label_comma(accuracy =0.01)) + 
  labs(x = "Growing species",y = "Intrinsic growth rate",fill = NULL, tag = "b") + 
  theme(legend.position = "none",
        legend.key = element_blank(),
        legend.background = element_rect(fill = NA)) -> Fig_S5b; Fig_S5b


################################################################################
###################################  Fig S5d  ##################################
################################################################################
field_data = read.xlsx("Coexistence_data.xlsx", sheet = "Field_exp", rowNames = F, colNames = T)
#field_data$ind_bio = field_data$biomass/field_data$survive
#field_data$ind_bio = ifelse(field_data$ind_bio == "NaN", 0, field_data$ind_bio)
field_data$ind_bio = sqrt(field_data$ind_bio)
head(field_data)

## 
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
      sp_A = min(mix_select_data$species)  # 
      sp_B = max(mix_select_data$species)  # 
      
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

## 
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
unique(field_data_2023$pair)

## Table S3
bio_mod = lmer(CI ~ drought2 * abbrev_focal + (1|block) + (1|pair), data = field_data_2023)
anova(bio_mod, type = "3")
summary(bio_mod)
ranova(bio_mod)

emm1 = emmeans(bio_mod, specs = pairwise ~ drought2 * abbrev_focal, type = 'response', adjust = 'tukey')
emm1_multi = multcomp::cld(emm1,alpha=0.05,Letters=letters,adjust="none",decreasing = T)
emm1_multi$.group <- trimws(emm1_multi$.group)

field_CI_mean = subset(field_data_2023, CI != "NA") %>% group_by(drought2, abbrev_focal) %>%
  summarise(mean_CI = mean(CI),sd_CI = sd(CI, na.rm = TRUE),       
            se_CI = sd_CI / sqrt(n()), .groups = 'drop') %>%
  left_join(emm1_multi)

### plot
ggplot(field_CI_mean, aes(x = abbrev_focal, y = mean_CI)) + 
  geom_errorbar(aes(ymin = mean_CI - se_CI, ymax = mean_CI + se_CI, group = drought2), 
                position=pd, width = 0, color = "black", show.legend = F) +
  geom_text(data = field_CI_mean, mapping = aes(x = abbrev_focal,y = mean_CI + se_CI + 0.03,label = .group, group = drought2), 
            color = "black", size = 3,position = position_dodge(.5)) +
  geom_point(size=3,position=pd,alpha=1, pch = 21, aes(fill = drought2), color = "black") +
  scale_fill_manual(values = c("#70A7C3","#A67C2A")) + 
  scale_color_manual(values = c("#70A7C3","#A67C2A")) + 
  theme_bw() + mytheme +
  annotate("text", x = 3.8, y = 0.30, parse = TRUE, size = 3.5, color = "#70A7C3", label = "na") +
  annotate("text", x = 5.2, y = 0.30, parse = TRUE, size = 3.5, label = expression("W" ~ ":" ~ italic(p) == 0.702)) + 
  annotate("text", x = 5.2, y = 0.20, parse = TRUE, size = 3.5, label = expression("G" ~ ":" ~ italic(p) < 0.001)) + 
  annotate("text", x = 5.2, y = 0.10, parse = TRUE, size = 3.5, label = expression("W × G" ~ ":" ~ italic(p) == 0.015)) +
  scale_y_continuous(labels = scales::label_comma(accuracy =0.01), limits = c(0.00, 1.22)) + 
  labs(x = "Growing species",y = "Competitive ability",fill = NULL, tag = "d") + 
  theme(legend.position = "none",
        legend.key = element_blank(),
        legend.background = element_rect(fill = NA)) -> Fig_S5d; Fig_S5d
