################################################################################
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

bio_mod = lmer(ind_bio_sq ~ drought2 * abbrev_focal * type + (1|block), data = field_data_2023)
anova(bio_mod)

## 移除单种
bio_mod = lmer(ind_bio_sq ~ drought2 * abbrev_focal + (1|block), data = subset(field_data_2023, type == "Mix"))
anova(bio_mod)

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
  #geom_point(data = field_mix_all, aes(x = drought2, y = mean_bio, color = drought2),
  #           position = position_jitterdodge(.2), alpha = 0.5, show.legend = F) + 
  geom_point(size=3.5,position=pd,alpha=1, pch = 21, aes(fill = drought2), color = "black") +
  scale_fill_manual(values = c("#70A7C3","#A67C2A")) + 
  scale_color_manual(values = c("#70A7C3","#A67C2A")) + 
  #geom_line(aes(x = drought2, y = mean_bio, group = drought2), color = "black", size = 0.5, inherit.aes = FALSE) + 
  theme_bw() + mytheme +
  scale_y_continuous(labels = scales::label_comma(accuracy =0.01)) + 
  labs(x = NULL,y = NULL, fill = NULL) + 
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 25, hjust = 1, vjust = 1),
        legend.key = element_blank(),
        legend.background = element_rect(fill = NA)) -> p_fied_a; p_fied_a

####
emm1 = emmeans(bio_mod, specs = pairwise ~ abbrev_focal, type = 'response', adjust = 'tukey')
emm1_multi = multcomp::cld(emm1,alpha=0.05,Letters=letters,adjust="none",decreasing = T)
emm1_multi$.group <- trimws(emm1_multi$.group)

field_mono_mean = subset(field_mix_all, ind_bio_sq != "NA") %>% group_by(drought2, abbrev_focal) %>%
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
  #geom_line(aes(x = abbrev_focal, y = mean_PSF, group = drought2), color = "black", size = 0.5, inherit.aes = FALSE) + 
  theme_bw() + mytheme +
  scale_y_continuous(labels = scales::label_comma(accuracy =0.1)) + 
  labs(x = "Growing species",y = "Plant performance in mixture\nestimated in field experiment",fill = NULL) + 
  theme(legend.position = c(0.12, 0.88),
        legend.key = element_blank(),
        legend.background = element_rect(fill = NA)) -> pS4BB; pS4BB



################################################################################
## Growing species
emm1 = emmeans(bio_mod, specs = pairwise ~ drought2 | abbrev_focal, type = 'response', adjust = 'none')
emm1_multi = multcomp::cld(emm1,alpha=0.05,Letters=letters,adjust="none",decreasing = T)
emm1_multi$.group <- trimws(emm1_multi$.group)


emm1 = emmeans(bio_mod, specs = pairwise ~ abbrev_focal, type = 'response', adjust = 'none')
emm1_multi = multcomp::cld(emm1,alpha=0.05,Letters=letters,adjust="none",decreasing = T)
emm1_multi$.group <- trimws(emm1_multi$.group)

## abbrev_focal
field_mix_all = subset(field_data_2023, type == "Mix")
bio_field_mean = subset(field_mix_all, ind_bio_sq != "NA") %>% group_by(abbrev_focal) %>% 
  summarise(mean_bio = mean(ind_bio_sq),sd_bio = sd(ind_bio_sq, na.rm = TRUE),       
            se_bio = sd_bio / sqrt(n()), .groups = 'drop') %>%
  left_join(emm1_multi[,c("abbrev_focal",".group")])

pd = position_dodge(.5)
ggplot(bio_field_mean, aes(x = abbrev_focal, y = mean_bio)) + 
  geom_errorbar(aes(ymin = mean_bio - se_bio, ymax = mean_bio + se_bio), 
                position=pd, width = 0, color = "black", show.legend = F) +
  geom_text(data = bio_field_mean, mapping = aes(x = abbrev_focal,y = mean_bio+0.12,label = .group), 
            color = "black", size = 3) +
  #geom_point(data = field_mix_all, aes(x = abbrev_focal, y = ind_bio_sq, color = abbrev_focal),
  #           position = position_jitterdodge(.2), alpha = 0.5, show.legend = F) + 
  geom_point(size=3.5,position=pd,alpha=1, pch = 21, aes(fill = abbrev_focal), color = "black", fill = "grey") +
  #scale_color_manual(values = c("#EB7771","#1EBAB2","#A3866E","#615A8C","#EB913F","#2873A7")) +
  #scale_fill_manual(values = c("#EB7771","#1EBAB2","#A3866E","#615A8C","#EB913F","#2873A7")) +
  #geom_line(aes(x = abbrev_focal, y = mean_bio, group = abbrev_focal), color = "black", size = 0.5, inherit.aes = FALSE) + 
  theme_bw() + mytheme +
  scale_y_continuous(labels = scales::label_comma(accuracy =0.1)) + 
  labs(x = "Growing species",y = "Individual biomass in field (g, sqrt)", fill = NULL, tag = "b") + 
  theme(legend.position = "none",
        legend.key = element_blank(),
        legend.background = element_rect(fill = NA)) -> p_fied_b;p_fied_b


################################################################################
emm1 = emmeans(bio_mod, specs = pairwise ~ abbrev_focal, type = 'response', adjust = 'none')
emm1_multi = multcomp::cld(emm1,alpha=0.05,Letters=letters,adjust="none",decreasing = T)
emm1_multi$.group <- trimws(emm1_multi$.group)

## abbrev_focal
field_mix_all = subset(field_data_2023, type == "Mix")
bio_field_mean = subset(field_mix_all, ind_bio_sq != "NA") %>% group_by(drought2, abbrev_focal) %>% 
  summarise(mean_bio = mean(ind_bio_sq),sd_bio = sd(ind_bio_sq, na.rm = TRUE),       
            se_bio = sd_bio / sqrt(n()), .groups = 'drop')%>%
  left_join(emm1_multi[,c("abbrev_focal",".group")]) 

pd = position_dodge(.5)
ggplot(bio_field_mean, aes(x = abbrev_focal, y = mean_bio, color = drought2)) + 
  geom_errorbar(aes(ymin = mean_bio - se_bio, ymax = mean_bio + se_bio, group = drought2), 
                position=pd, width = 0, color = "black", show.legend = F) +
  geom_text(data = subset(bio_field_mean, drought2 == "Drought"), mapping = aes(x = abbrev_focal,y = mean_bio+0.12,label = .group), 
            color = "black", size = 3) +
  #geom_point(data = field_mix_all, aes(x = abbrev_focal, y = ind_bio_sq, color = abbrev_focal),
  #           position = position_jitterdodge(.2), alpha = 0.5, show.legend = F) + 
  geom_point(size=3,position=pd,alpha=1, pch = 21, aes(fill = drought2), color = "black") +
  scale_fill_manual(values = c("#70A7C3","#A67C2A")) + 
  scale_color_manual(values = c("#70A7C3","#A67C2A")) + 
  theme_bw() + mytheme +
  #geom_hline(aes(yintercept = 1), linetype = "dashed") +
  scale_y_continuous(labels = scales::label_comma(accuracy =0.1)) + 
  labs(x = "Growing species",y = "Plant performance\n(i.e.,individual biomass)", fill = NULL) + 
  theme(legend.position = c(0.85,0.22),
        legend.key = element_blank(),
        legend.background = element_rect(fill = NA)) +
  coord_flip() -> p_field_bio_a; p_field_bio_a

library(patchwork)
(p_fied_a|p_fied_b) + plot_layout(widths = c(0.3, 0.7))


### 成对比较物种对之间差异
field_mix_all = subset(field_data_2023, type == "Mix")
field_mono_all = subset(field_data_2023, type == "Mono")

pair = unique(field_mix_all$pair)
drought = unique(field_mix_all$drought)

final_result_t = NULL
final_t_mono_test = NULL

for (i in drought) {
  for (ii in pair) {
    select_data = subset(field_mix_all, drought == i & pair == ii)
    Species_A = min(select_data$species); Species_B = max(select_data$species)
    Species_A_data = select_data[select_data$species %in% Species_A, ]
    Species_B_data = select_data[select_data$species %in% Species_B, ]
    ##
    Species_A_mean = subset(Species_A_data, ind_bio_sq != "NA") %>% group_by(drought, pair) %>% 
      summarise(Sp_A = Species_A, mean_bio_A = mean(ind_bio_sq),sd_bio_A = sd(ind_bio_sq, na.rm = TRUE),       
                se_bio_A = sd_bio_A / sqrt(n()), .groups = 'drop') 
    Species_B_mean = subset(Species_B_data, ind_bio_sq != "NA") %>% group_by(drought, pair) %>% 
      summarise(Sp_B = Species_B, mean_bio_B = mean(ind_bio_sq),sd_bio_B = sd(ind_bio_sq, na.rm = TRUE),       
                se_bio_B = sd_bio_B / sqrt(n()), .groups = 'drop') 
    ##
    result_t_test = t.test(Species_A_data$ind_bio_sq, Species_B_data$ind_bio_sq)
    result_t_test = data.frame(Effect = "ind_bio", drought = i, pair = ii,
                               t_value = result_t_test$statistic,p_value = result_t_test$p.value)
    
    result_t_test = result_t_test %>% left_join(Species_A_mean) %>% left_join(Species_B_mean)
    final_result_t = rbind(final_result_t, result_t_test)
    
    ### 与单一种植比较差异
    Species_A_mono = subset(field_mono_all, drought == i & species == Species_A )
    Species_B_mono = subset(field_mono_all, drought == i & species == Species_B )
    ##
    ##
    result_A_test = tryCatch({
      t_result = t.test(Species_A_data$ind_bio_sq, Species_A_mono$ind_bio_sq)
      data.frame(Sp_A = Species_A, Effect = "ind_bio", drought = i, pair = ii, t_value_A = t_result$statistic, p_value_A = t_result$p.value)
    }, error = function(e) {
      data.frame(Sp_A = Species_A, Effect = "ind_bio", drought = i, pair = ii, t_value_A = NA, p_value_A = NA)
    })
    
    result_B_test = tryCatch({
      t_result = t.test(Species_B_data$ind_bio_sq, Species_B_mono$ind_bio_sq)
      data.frame(Sp_B = Species_B, Effect = "ind_bio", drought = i, pair = ii, t_value_B = t_result$statistic, p_value_B = t_result$p.value)
    }, error = function(e) {
      data.frame(Sp_B = Species_B, Effect = "ind_bio", drought = i, pair = ii, t_value_B = NA, p_value_B = NA)
    })
    
    t_mono_test = result_A_test %>% left_join(result_B_test)
    final_t_mono_test = rbind(final_t_mono_test, t_mono_test)
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


final_t_mono_test$p_value_A = round(final_t_mono_test$p_value_A, 3)
final_t_mono_test$p_value_B = round(final_t_mono_test$p_value_B, 3)

final_t_mono_test <- final_t_mono_test %>%
  mutate(abbrev_Sp_A = recode(Sp_A,`1` = "Bb",`2` = "Ac",`3` = "Car",
                              `4` = "Cal",`5` = "Sp",`6` = "Pb")) %>%
  mutate(abbrev_Sp_B = recode(Sp_B,`1` = "Bb",`2` = "Ac",`3` = "Car",
                              `4` = "Cal",`5` = "Sp",`6` = "Pb")) %>%
  mutate(Pair_code = paste0(abbrev_Sp_A, "-", abbrev_Sp_B))


#write.csv(final_result_t,"final_result_t.csv")
#write.csv(final_t_mono_test,"final_t_mono_test.csv")
head(field_mix_all)
field_mix_mean = subset(field_mix_all, ind_bio_sq != "NA") %>% group_by(drought, pair, species) %>%
  summarise(mean_bio = mean(ind_bio_sq),sd_bio = sd(ind_bio_sq, na.rm = TRUE),       
            se_bio = sd_bio / sqrt(n()), .groups = 'drop') 

# 转换数据
pair = unique(field_mix_all$pair)
drought = unique(field_mix_all$drought)
field_mix_all_tran = NULL
for (i in drought) {
  for (ii in pair) {
    select_data = subset(field_mix_mean, drought == i & pair == ii)
    colnames(select_data)[3] = "focal_sp"
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


field_mix_all_tran$mean_bio2 = round(field_mix_all_tran$mean_bio, 2)
field_mix_all_tran$drought2 = ifelse(field_mix_all_tran$drought == "G", "Ambient", "Drought")

ggplot(field_mix_all_tran, aes(abbrev_condi, abbrev_focal)) +
  geom_tile(aes(fill = mean_bio), color = "white") +
  scale_fill_gradient2(low = ("#A67C2A"),mid = "white",high = muted("#001260"), midpoint = 0.75, limits=c(0,1.45)) + # muted
  geom_text(aes(label = mean_bio2), size = 2.8, color = 'black') + 
  labs(y="Growing species",x="Competing species",fill = NULL, tag = "b") + 
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
  facet_row(~drought2, scales = "free_x", space = "free") -> p_field_bio_b; p_field_bio_b

summary(field_mix_all_tran$mean_bio)
aa = (p_field_bio_a|p_field_bio_b) + plot_layout(widths = c(0.3, 0.7)); aa


field_mix_bio = field_mix_all_tran
