library(BestFitM)
library(ggtrendline)
library(openxlsx)

all_predict_data = read.xlsx("all_predict_data.xlsx", rowNames = F, colNames = T)

predict_data_mean = all_predict_data %>% group_by(drought2, abbrev_focal) %>%
  summarise(PSF_G = mean(mean_PSF_G), PSF_CI = mean(mean_PSF_CI)) %>%
  left_join(field_mono_mean)

AAA = subset(predict_data_mean, drought2 == "Ambient" & mono_mean != "NA")
BBB = subset(predict_data_mean, drought2 == "Drought")

bestFitM2(data= AAA, x= "PSF_G", y = "mono_mean")
FitM(data = AAA, x= "PSF_G", y = "mono_mean",model = "line2P" )

bestFitM2(data= BBB, x= "PSF_G", y = "mono_mean")
FitM(data = BBB, x= "PSF_G", y = "mono_mean",model = "line3P" )

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
       y = "Intrinsic growth ability\nestimated in field experiment", tag = "a") -> Fig_4a; Fig_4a


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
       y = "Intrinsic growth ability\nestimated in field experiment", tag = "b") -> Fig_4b; Fig_4b


################################################################################
# Drought  # Ambient
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
  labs(#x = expression(PSF[growth] ~ "(Ln " ~ frac(Mass[home-mono], Mass[away-mono]) ~ ")"),
       x = "",
       y = "Competition ability\nestimated in field experiment", tag = "c") -> Fig_4c; Fig_4c


### PLOT
AAA = subset(all_predict_data, drought2 == "Ambient")
BBB = subset(all_predict_data, drought2 == "Drought")

bestFitM2(data= AAA, x= "mean_PSF_G", y = "mean_bio")
FitM(data = AAA, x= "mean_PSF_G", y = "mean_bio",model = "line3P" )

bestFitM2(data= BBB, x= "mean_PSF_G", y = "mean_bio")
FitM(data = BBB, x= "mean_PSF_G", y = "mean_bio",model = "line3P" )


ggplot(data = all_predict_data, aes(x = mean_PSF_G, y = mean_bio, color = drought2)) + 
  geom_smooth(data = subset(all_predict_data, drought2 == "Ambient"),method = "lm", 
              se = TRUE, aes(fill = drought2), formula = y ~ poly(x, 2, raw=TRUE), alpha = 0.5) +
  geom_smooth(data = subset(all_predict_data, drought2 == "Drought"), method = "lm", 
              se = TRUE, aes(fill = drought2), formula = y ~ poly(x, 2, raw=TRUE), alpha = 0.5) +
  scale_color_manual(values = c("#70A7C3","#A67C2A")) + 
  scale_fill_manual(values = c("#B7D3E1","#D2BD94")) + 
  ggnewscale::new_scale_fill() + 
  geom_point(size = 3, aes(fill = drought2), pch = 21, color = "black") + 
  scale_fill_manual(values = c("#70A7C3","#A67C2A")) + 
  theme_bw() + mytheme + 
  geom_line(aes(group=interaction(combi, drought2), color = drought2), alpha=0.2, size = 0.6) + 
  scale_x_continuous(labels = scales::label_comma(accuracy =0.01)) +
  scale_y_continuous(labels = scales::label_comma(accuracy =0.01)) + 
  annotate("text", x = -0.32, y = 0.00, parse = TRUE, size = 3.5, color = "#70A7C3",
           label = expression(italic(R)^2 == 0.144 ~ "," ~ italic(p) == 0.539)) +
  annotate("text", x = -0.32, y = -0.15, parse = TRUE, size = 3.5, color = "#A67C2A",
           label = expression(italic(R)^2 == 0.858 ~ "," ~ italic(p) == 0.054)) +
  labs(x = expression(PSF[growth] ~ "(Ln " ~ frac(Mass[home-mono], Mass[away-mono]) ~ ")" ~ "\n" ~ 
                            "estimated in PSF experiment"),
       y = "Plant performance in mixture\nestimated in field experiment", tag = "e") -> Fig_4e; Fig_4e

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
  labs(#x = expression(PSF[competitiveness] ~ "(Ln " ~ frac(CI[home], CI[away]) ~ ")"),
       x = "",
       y = "Competition ability\nestimated in field experiment", tag = "d") -> Fig_4d; Fig_4d


AAA = subset(all_predict_data, drought2 == "Ambient" & mean_CI != "NA")
BBB = subset(all_predict_data, drought2 == "Drought" & mean_CI != "NA")
colnames(AAA)
bestFitM2(data= AAA, x= "mean_PSF_CI", y = "mean_bio")
FitM(data = AAA, x= "mean_PSF_CI", y = "mean_bio",model = "line2P" )

bestFitM2(data= BBB, x= "mean_PSF_CI", y = "mean_bio")
FitM(data = BBB, x= "mean_PSF_CI", y = "mean_bio",model = "line2P" )

ggplot(data = all_predict_data, aes(x = mean_PSF_CI, y = mean_bio, color = drought2)) + 
  geom_smooth(data = subset(all_predict_data, drought2 == "Ambient"),
              method = "lm", se = TRUE, aes(fill = drought2), alpha = 0.5) +
  geom_smooth(data = subset(all_predict_data, drought2 == "Drought"), linetype = 2,
              method = "lm", se = TRUE, aes(fill = drought2), alpha = 0.5) +
  scale_color_manual(values = c("#70A7C3","#A67C2A")) + 
  scale_fill_manual(values = c("#B7D3E1","#D2BD94")) + 
  ggnewscale::new_scale_fill() + 
  geom_point(size = 3, aes(fill = drought2), pch = 21, color = "black") + 
  scale_fill_manual(values = c("#70A7C3","#A67C2A")) + 
  theme_bw() + mytheme + 
  geom_line(aes(group=interaction(combi, drought2), color = drought2), alpha=0.2, size = 0.6) + 
  scale_x_continuous(labels = scales::label_comma(accuracy =0.01)) +
  scale_y_continuous(labels = scales::label_comma(accuracy =0.01)) + 
  annotate("text", x = -0.35, y = 1.40, parse = TRUE, size = 3.5, color = "#70A7C3",
           label = expression(italic(R)^2 == 0.184 ~ "," ~ italic(p) == 0.033)) +
  annotate("text", x = -0.35, y = 1.28, parse = TRUE, size = 3.5, color = "#A67C2A",
           label = expression(italic(R)^2 < 0.001 ~ "," ~ italic(p) == 0.873)) +
  labs(x = expression(PSF[competitiveness] ~ "(Ln " ~ frac(CI[home], CI[away]) ~ ")" ~ "\n" ~ 
                    "estimated in PSF experiment"),
       y = "Plant performance in mixture\nestimated in field experiment", tag = "f") -> Fig_4f; Fig_4f



library(patchwork)

(Fig_4a|Fig_4b)/(Fig_4c|Fig_4d)/(Fig_4e|Fig_4f) -> Fig_4


################################################################################
################################### Fig s7 #####################################
################################################################################

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
       y = "Competition ability", tag = "a") -> Fig_S5a; Fig_S5a


################################################################################
bestFitM2(data= AAA, x= "mono_mean", y = "mean_bio")
FitM(data = AAA, x= "mono_mean", y = "mean_bio",model = "line3P" )

bestFitM2(data= BBB, x= "mono_mean", y = "mean_bio")
FitM(data = BBB, x= "mono_mean", y = "mean_bio",model = "line3P" )

ggplot(data = all_predict_data, aes(x = mono_mean, y = mean_bio, color = drought2)) + 
  geom_smooth(data = subset(all_predict_data, drought2 == "Ambient"), linetype = 1,
              method = "lm", se = TRUE, aes(fill = drought2), formula = y ~ poly(x, 2, raw=TRUE)) +
  geom_smooth(data = subset(all_predict_data, drought2 == "Drought"), linetype = 1,
              method = "lm", se = TRUE, aes(fill = drought2), formula = y ~ poly(x, 2, raw=TRUE)) +
  scale_color_manual(values = c("#70A7C3","#A67C2A")) + 
  scale_fill_manual(values = c("#B7D3E1","#D2BD94")) + 
  ggnewscale::new_scale_fill() + 
  geom_point(size = 3, aes(fill = drought2), pch = 21, color = "black") + 
  scale_fill_manual(values = c("#70A7C3","#A67C2A")) + 
  theme_bw() + mytheme + 
  geom_line(aes(group=interaction(combi, drought2), color = drought2), alpha=0.2, size = 0.6) + 
  scale_x_continuous(labels = scales::label_comma(accuracy =0.01)) +
  scale_y_continuous(labels = scales::label_comma(accuracy =0.01)) + 
  annotate("text", x = 1, y = 0.30, parse = TRUE, size = 3.5, color = "#70A7C3",
           label = expression(italic(R)^2 == 0.305 ~ "," ~ italic(p) == 0.018)) +
  annotate("text", x = 1, y = 0.20, parse = TRUE, size = 3.5, color = "#A67C2A",
           label = expression(italic(R)^2 == 0.312 ~ "," ~ italic(p) == 0.006)) +
  labs(x = "Competition ability",
       y = "Plant performance in mixture", tag = "b") -> Fig_S5b; Fig_S5b


### 田间生长潜力与竞争力之间关系
###
AAA = subset(all_predict_data, drought2 == "Ambient" & mean_CI != "NA")
BBB = subset(all_predict_data, drought2 == "Drought" & mean_CI != "NA")
colnames(AAA)
bestFitM2(data= AAA, x= "mean_CI", y = "mean_bio")
FitM(data = AAA, x= "mean_CI", y = "mean_bio",model = "line2P" )

bestFitM2(data= BBB, x= "mean_CI", y = "mean_bio")
FitM(data = BBB, x= "mean_CI", y = "mean_bio",model = "line2P" )

colnames(all_predict_data)
ggplot(data = all_predict_data, aes(x = mean_CI, y = mean_bio, color = drought2)) + 
  geom_smooth(data = subset(all_predict_data, drought2 == "Ambient"), linetype = 1,
              method = "lm", se = TRUE, aes(fill = drought2)) +
  geom_smooth(data = subset(all_predict_data, drought2 == "Drought"), linetype = 1,
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
  annotate("text", x = 1, y = 0.30, parse = TRUE, size = 3.5, color = "#70A7C3",
           label = expression(italic(R)^2 == 0.498 ~ "," ~ italic(p) < 0.001)) +
  annotate("text", x = 1, y = 0.20, parse = TRUE, size = 3.5, color = "#A67C2A",
           label = expression(italic(R)^2 == 0.458 ~ "," ~ italic(p) < 0.001)) +
  labs(x = "Competition ability",
       y = "Plant performance in mixture", tag = "c") -> Fig_S5c; Fig_S5c


(Fig_S5a/Fig_S5b/Fig_S5c) -> Fig_S5; Fig_S5




