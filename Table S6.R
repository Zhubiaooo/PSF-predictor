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

## 混种与单种数据
field_data_2023 = CI_field_data
field_data_2023$drought2 = ifelse(field_data_2023$drought == "G", "Ambient", "Drought")

field_data_2023 <- field_data_2023 %>%
  mutate(abbrev_focal = recode(species,`1` = "Bb",`2` = "Ac",`3` = "Car",
                               `4` = "Cal",`5` = "Sp",`6` = "Pb"))

## 
bio_mod = lmer(CI ~ drought2 * abbrev_focal + (1|block), data = field_data_2023)
anova(bio_mod)

## drought X Growing species
emm1 = emmeans(bio_mod, specs = pairwise ~ drought2 | abbrev_focal, type = 'response', adjust = 'none')
emm1_multi = multcomp::cld(emm1,alpha=0.05,Letters=letters,adjust="none",decreasing = T)
emm1_multi$.group <- trimws(emm1_multi$.group)

## abbrev_focal
bio_field_mean = subset(field_data_2023, CI != "NA") %>% group_by(drought2, abbrev_focal) %>% 
  summarise(mean_bio = mean(CI),sd_bio = sd(CI, na.rm = TRUE),       
            se_bio = sd_bio / sqrt(n()), .groups = 'drop') 

pd = position_dodge(.5)
ggplot(bio_field_mean, aes(x = abbrev_focal, y = mean_bio, color = drought2)) + 
  geom_errorbar(aes(ymin = mean_bio - se_bio, ymax = mean_bio + se_bio, group = drought2), 
                position=pd, width = 0, color = "black", show.legend = F) +
  #geom_text(data = bio_field_mean, mapping = aes(x = abbrev_focal,y = mean_bio+0.12,label = .group), 
  #          color = "black", size = 3) +
  #geom_point(data = field_mix_all, aes(x = abbrev_focal, y = ind_bio_sq, color = abbrev_focal),
  #           position = position_jitterdodge(.2), alpha = 0.5, show.legend = F) + 
  geom_point(size=3,position=pd,alpha=1, pch = 21, aes(fill = drought2), color = "black") +
  scale_fill_manual(values = c("#70A7C3","#A67C2A")) + 
  scale_color_manual(values = c("#70A7C3","#A67C2A")) + 
  theme_bw() + mytheme +
  #geom_hline(aes(yintercept = 1), linetype = "dashed") +
  scale_y_continuous(labels = scales::label_comma(accuracy =0.1)) + 
  labs(x = "Growing species",y = "Competition ability", fill = NULL, tag = "a") + 
  theme(legend.position = c(0.22,0.85),
        legend.key = element_blank(),
        legend.background = element_rect(fill = NA)) +
  coord_flip() -> p_field_CI_a; p_field_CI_a



####
bio_mod = lmer(CI ~ drought2 * abbrev_focal + (1|block), data = field_data_2023)
anova(bio_mod)

emm1 = emmeans(bio_mod, specs = pairwise ~ drought2 | abbrev_focal, type = 'response', adjust = 'tukey')
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
  geom_text(data = field_mono_mean, mapping = aes(x = abbrev_focal,y = mean_bio+0.06,label = .group, group = drought2), 
            color = "black", size = 3,position = position_dodge(.5)) +
  geom_point(size=3,position=pd,alpha=1, pch = 21, aes(fill = drought2), color = "black") +
  scale_fill_manual(values = c("#70A7C3","#A67C2A")) + 
  scale_color_manual(values = c("#70A7C3","#A67C2A")) + 
  #geom_line(aes(x = abbrev_focal, y = mean_PSF, group = drought2), color = "black", size = 0.5, inherit.aes = FALSE) + 
  theme_bw() + mytheme +
  scale_y_continuous(labels = scales::label_comma(accuracy =0.1), limits = c(0.00,1.22)) + 
  labs(x = "Response species",y = "Competition ability\nestimated in field experiment",fill = NULL) + 
  theme(legend.position = c(0.12, 0.88),
        legend.key = element_blank(),
        legend.background = element_rect(fill = NA)) -> pS4CC; pS4CC


field_mono_mean = subset(field_data_2023, CI != "NA") %>% group_by(drought2) %>%
  summarise(mean_bio = mean(CI),sd_bio = sd(CI, na.rm = TRUE),       
            se_bio = sd_bio / sqrt(n()), .groups = 'drop') 

### plot
ggplot(field_mono_mean, aes(x = drought2, y = mean_bio)) + 
  geom_errorbar(aes(ymin = mean_bio - se_bio, ymax = mean_bio + se_bio, group = drought2), 
                position=pd, width = 0, color = "black", show.legend = F) +
  #geom_text(data = field_mono_mean, mapping = aes(x = drought2,y = mean_bio+0.06,label = .group, group = drought2), 
  #          color = "black", size = 3,position = position_dodge(.5)) +
  geom_point(size=3,position=pd,alpha=1, pch = 21, aes(fill = drought2), color = "black") +
  scale_fill_manual(values = c("#70A7C3","#A67C2A")) + 
  scale_color_manual(values = c("#70A7C3","#A67C2A")) + 
  #geom_line(aes(x = drought2, y = mean_PSF, group = drought2), color = "black", size = 0.5, inherit.aes = FALSE) + 
  theme_bw() + mytheme +
  scale_y_continuous(labels = scales::label_comma(accuracy =0.01)) + 
  labs(x = NULL,y = NULL,fill = NULL) + 
  theme(legend.position = "none",
        legend.key = element_blank(),
        axis.text.x = element_text(angle = 25, hjust = 1, vjust = 1),
        legend.background = element_rect(fill = NA)) 


### 成对比较物种对之间差异
field_mix_all = field_data_2023

pair = unique(field_mix_all$pair)
drought = unique(field_mix_all$drought)

final_result_t = NULL

for (i in drought) {
  for (ii in pair) {
    select_data = subset(field_mix_all, drought == i & pair == ii)
    Species_A = min(select_data$species); Species_B = max(select_data$species)
    Species_A_data = select_data[select_data$species %in% Species_A, ]
    Species_B_data = select_data[select_data$species %in% Species_B, ]
    ##
    Species_A_mean = subset(Species_A_data, CI != "NA") %>% group_by(drought, pair) %>% 
      summarise(Sp_A = Species_A, mean_bio_A = mean(CI),sd_bio_A = sd(CI, na.rm = TRUE),       
                se_bio_A = sd_bio_A / sqrt(n()), .groups = 'drop') 
    Species_B_mean = subset(Species_B_data, CI != "NA") %>% group_by(drought, pair) %>% 
      summarise(Sp_B = Species_B, mean_bio_B = mean(CI),sd_bio_B = sd(CI, na.rm = TRUE),       
                se_bio_B = sd_bio_B / sqrt(n()), .groups = 'drop') 
    ##
    result_t_test = tryCatch({
      t_result = t.test(Species_A_data$CI, Species_B_data$CI)
      data.frame(Effect = "CI", drought = i, pair = ii, t_value_A = t_result$statistic, p_value_A = t_result$p.value)
    }, error = function(e) {
      data.frame(Effect = "CI", drought = i, pair = ii, t_value_A = NA, p_value_A = NA)
    })
    result_t_test = result_t_test %>% left_join(Species_A_mean) %>% left_join(Species_B_mean)
    final_result_t = rbind(final_result_t, result_t_test)
  }
}


final_result_t$p_value = round(final_result_t$p_value, 3)
final_result_t$mean_bio_A = round(final_result_t$mean_bio_A, 2)
final_result_t$sd_bio_A = round(final_result_t$sd_bio_A, 2)
final_result_t$se_bio_A = round(final_result_t$se_bio_A, 2)

final_result_t$mean_bio_B = round(final_result_t$mean_bio_B, 2)
final_result_t$sd_bio_B = round(final_result_t$sd_bio_B, 2)
final_result_t$se_bio_B = round(final_result_t$se_bio_B, 2)


final_result_t <- final_result_t %>%
  mutate(abbrev_Sp_A = recode(Sp_A,`1` = "Bb",`2` = "Ac",`3` = "Car",
                              `4` = "Cal",`5` = "Sp",`6` = "Pb")) %>%
  mutate(abbrev_Sp_B = recode(Sp_B,`1` = "Bb",`2` = "Ac",`3` = "Car",
                              `4` = "Cal",`5` = "Sp",`6` = "Pb")) %>%
  mutate(Pair_code = paste0(abbrev_Sp_A, "-", abbrev_Sp_B))

#write.csv(final_result_t,"final_result_t_CI.csv")
#write.csv(final_t_mono_test,"final_t_mono_test.csv")



head(field_mix_all)
colnames(field_mix_all)[5] = "ind_bio_sq" 

field_mix_mean = field_mix_all %>% group_by(drought, pair, species) %>%
  tidyr::drop_na(ind_bio_sq) %>%
  summarise(mean_bio = mean(ind_bio_sq),sd_bio = sd(ind_bio_sq, na.rm = TRUE),       
            se_bio = sd_bio / sqrt(n()), .groups = 'drop') %>% as.data.frame()

null_data = data.frame(drought = rep("G", 5),
                       pair = c("13","23","34","35","36"),
                       species = rep("3", 5),
                       mean_bio = rep("NA", 5),
                       sd_bio = rep("NA", 5),
                       se_bio = rep("NA", 5))

field_mix_mean = rbind(field_mix_mean, null_data)
# 转换数据
pair = unique(field_mix_all$pair)
drought = unique(field_mix_all$drought)
field_mix_all_tran = NULL
for (i in drought) {
  for (ii in pair) {
    select_data = subset(field_mix_mean, drought == i & pair == ii)
    colnames(select_data)[3] = "focal_sp"
    select_data$compite_sp = rev(select_data$focal_sp)
    select_data$compite_sp = rev(select_data$focal_sp)
    field_mix_all_tran = rbind(field_mix_all_tran, select_data)
  }
}

field_mix_all_tran <- field_mix_all_tran %>%
  mutate(abbrev_focal = recode(focal_sp,`1` = "Bb",`2` = "Ac",`3` = "Car",
                               `4` = "Cal",`5` = "Sp",`6` = "Pb"))
field_mix_all_tran <- field_mix_all_tran %>%
  mutate(abbrev_condi = recode(compite_sp,`1` = "Bb",`2` = "Ac",`3` = "Car",
                               `4` = "Cal",`5` = "Sp",`6` = "Pb"))

field_mix_all_tran$mean_bio = as.numeric(field_mix_all_tran$mean_bio)
field_mix_all_tran$mean_bio2 = round(field_mix_all_tran$mean_bio, 2)
field_mix_all_tran$drought2 = ifelse(field_mix_all_tran$drought == "G", "Ambient", "Drought")


ggplot(field_mix_all_tran, aes(abbrev_condi, abbrev_focal)) +
  geom_tile(aes(fill = mean_bio), color = "white") +
  #scale_fill_gradient2(low = muted("#F6B338"),mid = "white",high = muted("#155198"), midpoint = 0.75, limits=c(0,1.45)) + # muted
  scale_fill_gradient2(low = ("#A67C2A"),mid = "white",high = muted("#001260"), midpoint = 0.75, limits=c(0,1.45)) + # muted
  geom_text(aes(label = mean_bio2), size = 2.8, color = 'black') + 
  labs(y = "Growing species",x = "Competing species",fill = NULL, tag = "b") + 
  theme_classic() + mytheme + 
  geom_hline(aes(yintercept = 1.5), linetype = 1) +
  geom_hline(aes(yintercept = 2.5), linetype = 1) +
  geom_hline(aes(yintercept = 3.5), linetype = 1) +
  geom_hline(aes(yintercept = 4.5), linetype = 1) +
  geom_hline(aes(yintercept = 5.5), linetype = 1) +
  theme(legend.position = "right") + #"bottom"
  theme(#axis.text.y = element_text(face = "italic"),
    #axis.text.x = element_text(angle = 35, vjust = 1, hjust = 1),
    axis.text.x = element_text(),
    axis.text.y = element_text(),
    axis.ticks = element_blank(),
    axis.line = element_blank()) +
  scale_x_discrete(expand = c(0,0)) + #scale_y_discrete(expand = c(0,0)) + 
  guides(fill = guide_colorbar(barwidth = 0.8, barheight = 10)) + 
  #geom_abline(intercept = 0, slope = 1, color = "black", size = 1, linetype = "dashed") + 
  facet_row(~drought2, scales = "free_x", space = "free") -> p_field_CI_b; p_field_CI_b 


summary(field_mix_all_tran$mean_bio)
bb = (p_field_CI_a|p_field_CI_b) + plot_layout(widths = c(0.3, 0.7)); bb



#aa/bb

field_mix_CI = field_mix_all_tran
colnames(field_mix_CI)[c(4:6)] = c("mean_CI", "sd_CI", "se_CI")


