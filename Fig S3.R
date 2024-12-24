library(openxlsx)
field_svw_data = read.xlsx("Field_svm_data.xlsx", sheet = "2022", colNames = T, rowNames = F)
head(field_svw_data)
field_svw_data$Times=as.Date(field_svw_data$Times,origin="1899-12-30")


library(tidyr)
field_svw_long <- gather(field_svw_data, key = "group", value = "svm", -Times) %>%
  mutate(group2 = recode(group ,"D1.site1" = "Drought", "D2.site1" = "Drought","D3.site2" = "Drought","D4.site2" = "Drought",
                         "G1.site1" = "Ambient", "G2.site1" = "Ambient", "G3.site2" = "Ambient", "G4.site2" = "Ambient"))
head(field_svw_long)

unique(field_svw_long$group)

field_svw_mean = field_svw_long %>% group_by(Times, group2) %>%
  summarise(mean_SVW = mean(svm),sd_SVW = sd(svm, na.rm = TRUE),       
            se_SVW = sd_SVW / sqrt(n()), .groups = 'drop')

field_svw_long %>% group_by(group2) %>%
  summarise(mean_SVW = mean(svm),sd_SVW = sd(svm, na.rm = TRUE),       
            se_SVW = sd_SVW / sqrt(n()), .groups = 'drop')

mod = lmer(svm ~ factor(group2) + (1|group), data = field_svw_long)
anova(mod)


library(ggplot2)
colnames(field_svw_mean)
ggplot(data = field_svw_mean, aes(x = Times, y = mean_SVW, color = group2)) +
  geom_line(size = 0.8, alpha = 1) + 
  geom_ribbon(aes(x = Times, ymin = mean_SVW - se_SVW, ymax = mean_SVW + se_SVW, 
                  fill = group2), alpha = I(0.4), show.legend = F, color = "transparent") + 
  scale_fill_manual(values = c("#70A7C3","#A67C2A")) + 
  scale_color_manual(values = c("#70A7C3","#A67C2A")) + 
  theme_bw() + mytheme +
  scale_y_continuous(labels = scales::label_comma(accuracy =0.1)) + 
  labs(x = NULL,y = "Soil volumetric water content (%)",fill = NULL) + 
  theme(legend.position = c(0.15, 0.90),
        legend.key = element_blank(),
        axis.text.x = element_text(angle = 25, hjust = 1, vjust = 1),
        legend.background = element_rect(fill = NA)) -> Fig_S3; Fig_S3
