library(BestFitM)
library(ggtrendline)
library(openxlsx)
library(dplyr)
library(ggplot2)

################################################################################
#################################### Fig 4 #####################################
################################################################################
# loading database of common garden experiment 
field_data = read.xlsx("Coexistence_data.xlsx", sheet = "Field_exp", rowNames = F, colNames = T)
#field_data$ind_bio = field_data$biomass/field_data$survive
#field_data$ind_bio = ifelse(field_data$ind_bio == "NaN", 0, field_data$ind_bio)
head(field_data)
field_data_2023 = subset(field_data, year == "2023")
field_data_2023$ind_bio_sq = sqrt(field_data_2023$ind_bio)
field_data_2023$drought2 = ifelse(field_data_2023$drought == "G", "Ambient", "Drought")

field_data_2023 <- field_data_2023 %>%
  mutate(abbrev_focal = recode(species,`1` = "Bb",`2` = "Ac",`3` = "Car",
                               `4` = "Cal",`5` = "Sp",`6` = "Pb"))

## Monoculture
field_data_mono = subset(field_data_2023, type == "Mono")
field_mono_mean = subset(field_data_mono, ind_bio_sq != "NA") %>% group_by(drought2, abbrev_focal) %>%
  summarise(mono_mean = mean(ind_bio_sq),sd_mono = sd(ind_bio_sq, na.rm = TRUE),       
            se_mono = sd_mono / sqrt(n()), .groups = 'drop')

################################################################################
all_predict_data = read.xlsx("all_predict_data.xlsx", rowNames = F, colNames = T)
all_predict_data$mono_mean = NULL
all_predict_data$mono_mean_bef = NULL
all_predict_data = all_predict_data %>% left_join(field_mono_mean)

unique(all_predict_data$mono_mean)
predict_data_mean = all_predict_data %>% group_by(drought2, abbrev_focal) %>%
  summarise(PSF_G = mean(mean_PSF_G), PSF_CI = mean(mean_PSF_CI)) %>%
  left_join(field_mono_mean)

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

################################################################################
### PLOT
AAA = subset(predict_data_mean, drought2 == "Ambient" & mono_mean != "NA")
BBB = subset(predict_data_mean, drought2 == "Drought")

bestFitM2(data= AAA, x= "PSF_G", y = "mono_mean")
FitM(data = AAA, x= "PSF_G", y = "mono_mean",model = "line2P" )

bestFitM2(data= BBB, x= "PSF_G", y = "mono_mean")
FitM(data = BBB, x= "PSF_G", y = "mono_mean",model = "line3P" )

ggplot(data = predict_data_mean, aes(x = PSF_G, y = mono_mean, color = drought2)) + 
  geom_smooth(data = subset(predict_data_mean, drought2 == "Ambient"), linetype = 2,
              method = "lm", se = TRUE, aes(fill = drought2), alpha = 0.5) +
  geom_smooth(data = subset(predict_data_mean, drought2 == "Drought"), linetype = 2,
              method = "lm", se = TRUE, aes(fill = drought2), formula = y ~ poly(x, 2, raw=TRUE), alpha = 0.5) +
  scale_color_manual(values = c("#70A7C3","#A67C2A")) + 
  scale_fill_manual(values = c("#B7D3E1","#D2BD94")) + 
  ggnewscale::new_scale_fill() + 
  geom_point(size = 3, aes(fill = drought2), pch = 21, color = "black") + 
  scale_fill_manual(values = c("#70A7C3","#A67C2A")) + 
  theme_bw() + mytheme + theme(legend.position = c(0.18,0.85)) + 
  scale_x_continuous(labels = scales::label_comma(accuracy =0.01)) +
  scale_y_continuous(labels = scales::label_comma(accuracy =0.01)) + 
  annotate("text", x = -0.28, y = 0.10, parse = TRUE, size = 3.5, color = "#70A7C3",
           label = expression(italic(R)^2 == 0.144 ~ "," ~ italic(p) == 0.539)) +
  annotate("text", x = -0.28, y = -0.05, parse = TRUE, size = 3.5, color = "#A67C2A",
           label = expression(italic(R)^2 == 0.858 ~ "," ~ italic(p) == 0.054)) +
  labs(#x = expression(PSF[growth] ~ "(Ln " ~ frac(Mass[home-mono], Mass[away-mono]) ~ ")"),
       x = "",
       y = "Intrinsic growth ability in\ncommon garden experiment", tag = "a") -> Fig_4a; Fig_4a


###
AAA = subset(predict_data_mean, drought2 == "Ambient" & mono_mean != "NA")
BBB = subset(predict_data_mean, drought2 == "Drought")
colnames(AAA)
bestFitM2(data= AAA, x= "PSF_CI", y = "mono_mean")
FitM(data = AAA, x= "PSF_CI", y = "mono_mean", model = "line2P" )

bestFitM2(data= BBB, x= "PSF_CI", y = "mono_mean")
FitM(data = BBB, x= "PSF_CI", y = "mono_mean",model = "line2P" )

ggplot(data = subset(predict_data_mean, mono_mean != "NA"), aes(x = PSF_CI, y = mono_mean, color = drought2)) + 
  geom_smooth(data = subset(predict_data_mean, drought2 == "Ambient"), linetype = 2,
              method = "lm", se = TRUE, aes(fill = drought2), alpha = 0.5) +
  geom_smooth(data = subset(predict_data_mean, drought2 == "Drought"), linetype = 2,
              method = "lm", se = TRUE, aes(fill = drought2), alpha = 0.5) +
  scale_color_manual(values = c("#70A7C3","#A67C2A")) + 
  scale_fill_manual(values = c("#B7D3E1","#D2BD94")) + 
  ggnewscale::new_scale_fill() + 
  geom_point(size = 3, aes(fill = drought2), pch = 21, color = "black") + 
  scale_fill_manual(values = c("#70A7C3","#A67C2A")) + 
  theme_bw() + mytheme + 
  scale_x_continuous(labels = scales::label_comma(accuracy =0.01)) +
  scale_y_continuous(labels = scales::label_comma(accuracy =0.01)) + 
  annotate("text", x = -0.05, y = 0.10, parse = TRUE, size = 3.5, color = "#70A7C3",
           label = expression(italic(R)^2 == 0.005 ~ "," ~ italic(p) == 0.909)) +
  annotate("text", x = -0.05, y = -0.05, parse = TRUE, size = 3.5, color = "#A67C2A",
           label = expression(italic(R)^2 == 0.125 ~ "," ~ italic(p) == 0.492)) +
  labs(#x = expression(PSF[competitiveness] ~ "(Ln " ~ frac(CI[home], CI[away]) ~ ")"),
       x = "",
       y = "Intrinsic growth ability in\ncommon garden experiment", tag = "b") -> Fig_4b; Fig_4b


################################################################################
### PLOT
AAA = subset(all_predict_data, drought2 == "Ambient" & mean_CI != "NA")
BBB = subset(all_predict_data, drought2 == "Drought" & mean_CI != "NA")

bestFitM2(data= AAA, x= "mean_PSF_G", y = "mean_CI")
FitM(data = AAA, x= "mean_PSF_G", y = "mean_CI",model = "line2P" )

bestFitM2(data= BBB, x= "mean_PSF_G", y = "mean_CI")
FitM(data = BBB, x= "mean_PSF_G", y = "mean_CI",model = "line3P" )

ggplot(data = all_predict_data, aes(x = mean_PSF_G, y = mean_CI, color = drought2)) + 
  geom_smooth(data = subset(all_predict_data, drought2 == "Ambient"),
              method = "lm", se = TRUE, aes(fill = drought2), alpha = 0.5) +
  geom_smooth(data = subset(all_predict_data, drought2 == "Drought"),
              method = "lm", se = TRUE, aes(fill = drought2), formula = y ~ poly(x, 2, raw=TRUE), alpha = 0.5) +
  scale_color_manual(values = c("#70A7C3","#A67C2A")) + 
  scale_fill_manual(values = c("#B7D3E1","#D2BD94")) + 
  ggnewscale::new_scale_fill() + 
  geom_point(size = 3, aes(fill = drought2), pch = 21, color = "black") + 
  scale_fill_manual(values = c("#70A7C3","#A67C2A")) + 
  geom_line(aes(group=interaction(combi, drought2), color = drought2), alpha=0.2, size = 0.6) + 
  theme_bw() + mytheme + theme(legend.position = "none") + 
  scale_x_continuous(labels = scales::label_comma(accuracy =0.01)) +
  scale_y_continuous(labels = scales::label_comma(accuracy =0.01)) + 
  annotate("text", x = -0.28, y = 0.10, parse = TRUE, size = 3.5, color = "#70A7C3",
           label = expression(italic(R)^2 == 0.252 ~ "," ~ italic(p) == 0.011)) +
  annotate("text", x = -0.28, y = -0.00, parse = TRUE, size = 3.5, color = "#A67C2A",
           label = expression(italic(R)^2 == 0.493 ~ "," ~ italic(p) < 0.001)) +
  labs(x = expression(PSF[growth] ~ "(Ln " ~ frac(Mass[home-mono], Mass[away-mono]) ~ ")"),
       #x = "",
       y = "Competition ability in\ncommon garden experiment", tag = "c") -> Fig_4c; Fig_4c

################################################################################
AAA = subset(all_predict_data, drought2 == "Ambient" & mean_CI != "NA")
BBB = subset(all_predict_data, drought2 == "Drought" & mean_CI != "NA")
colnames(AAA)
bestFitM2(data= AAA, x= "mean_PSF_CI", y = "mean_CI")
FitM(data = AAA, x= "mean_PSF_CI", y = "mean_CI",model = "line2P" )

bestFitM2(data= BBB, x= "mean_PSF_CI", y = "mean_CI")
FitM(data = BBB, x= "mean_PSF_CI", y = "mean_CI",model = "line2P" )

colnames(all_predict_data)
ggplot(data = all_predict_data, aes(x = mean_PSF_CI, y = mean_CI, color = drought2)) + 
  geom_smooth(data = subset(all_predict_data, drought2 == "Ambient"),
              method = "lm", se = TRUE, aes(fill = drought2)) +
  geom_smooth(data = subset(all_predict_data, drought2 == "Drought"), linetype = 2,
              method = "lm", se = TRUE, aes(fill = drought2)) +
  scale_color_manual(values = c("#70A7C3","#A67C2A")) + 
  scale_fill_manual(values = c("#B7D3E1","#D2BD94")) + 
  ggnewscale::new_scale_fill() + 
  geom_point(size = 3, aes(fill = drought2), pch = 21, color = "black") + 
  scale_fill_manual(values = c("#70A7C3","#A67C2A")) + 
  theme_bw() + mytheme + 
  geom_line(aes(group=interaction(combi, drought2), color = drought2), alpha=0.2, size = 0.6) + 
  scale_x_continuous(labels = scales::label_comma(accuracy =0.01)) +
  scale_y_continuous(labels = scales::label_comma(accuracy =0.01)) + 
  annotate("text", x = -0.3, y = 0.15, parse = TRUE, size = 3.5, color = "#70A7C3",
           label = expression(italic(R)^2 == 0.164 ~ "," ~ italic(p) == 0.045)) +
  annotate("text", x = -0.3, y = 0.05, parse = TRUE, size = 3.5, color = "#A67C2A",
           label = expression(italic(R)^2 == 0.048 ~ "," ~ italic(p) == 0.244)) +
  labs(x = expression(PSF[comp] ~ "(Ln " ~ frac(CI[home], CI[away]) ~ ")"),
       #x = "",
       y = "Competition ability in\ncommon garden experiment", tag = "d") -> Fig_4d; Fig_4d


library(patchwork)
(Fig_4a|Fig_4b)/(Fig_4c|Fig_4d) -> Fig_4; Fig_4


################################################################################
################################### Fig s6 #####################################
################################################################################
AAA = subset(all_predict_data, drought2 == "Ambient" & mean_CI != "NA")
BBB = subset(all_predict_data, drought2 == "Drought" & mean_CI != "NA")

bestFitM2(data= AAA, x= "mono_mean", y = "mean_CI")
FitM(data = AAA, x= "mono_mean", y = "mean_CI",model = "line3P" )

bestFitM2(data= BBB, x= "mono_mean", y = "mean_CI")
FitM(data = BBB, x= "mono_mean", y = "mean_CI",model = "line3P" )

ggplot(data = all_predict_data, aes(x = mono_mean, y = mean_CI, color = drought2)) + 
  geom_smooth(data = subset(all_predict_data, drought2 == "Ambient"), linetype = 1,
              method = "lm", se = TRUE, aes(fill = drought2), formula = y ~ poly(x, 2, raw=TRUE)) +
  geom_smooth(data = subset(all_predict_data, drought2 == "Drought"), linetype = 1,
              method = "lm", se = TRUE, aes(fill = drought2), formula = y ~ poly(x, 2, raw=TRUE)) +
  scale_color_manual(values = c("#70A7C3","#A67C2A")) + 
  scale_fill_manual(values = c("#B7D3E1","#D2BD94")) + 
  ggnewscale::new_scale_fill() + 
  geom_point(size = 3, aes(fill = drought2), pch = 21, color = "black") + 
  scale_fill_manual(values = c("#70A7C3","#A67C2A")) + 
  theme_bw() + mytheme + theme(legend.position = c(0.15, 0.15)) + 
  geom_line(aes(group=interaction(combi, drought2), color = drought2), alpha=0.2, size = 0.6) + 
  scale_x_continuous(labels = scales::label_comma(accuracy =0.01)) +
  scale_y_continuous(labels = scales::label_comma(accuracy =0.01)) + 
  annotate("text", x = 0.92, y = 0.30, parse = TRUE, size = 3.5, color = "#70A7C3",
           label = expression(italic(R)^2 == 0.305 ~ "," ~ italic(p) == 0.018)) +
  annotate("text", x = 0.92, y = 0.20, parse = TRUE, size = 3.5, color = "#A67C2A",
           label = expression(italic(R)^2 == 0.312 ~ "," ~ italic(p) == 0.006)) +
  labs(x = "Intrinsic growth ability",
       y = "Competition ability", tag = "b") -> Fig_S6b; Fig_S6b

##
#(Fig_S6a|Fig_S6b) -> Fig_S6; Fig_S6

