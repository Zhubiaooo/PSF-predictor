library(openxlsx)
library(dplyr)
library(lme4)
library(lmerTest)

pot_bio_data = read.xlsx("Coexistence_data.xlsx", sheet = "Pot_feedback_phase", rowNames = F, colNames = T)
#pot_bio_data = subset(pot_bio_data, density != "0")

#write.csv(filtered_data_all,"filtered_data_all-11-24-3.csv")
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

##
pot_bio_data = RII_data_all

pot_bio_data$Code_focal = paste0(pot_bio_data$density,"_", pot_bio_data$drought, "_", pot_bio_data$focal_sp, "_", pot_bio_data$condi_sp)

### group: Home
α_AB = c(paste0("L_D_", 1:6, "_", 1:6), paste0("L_G_", 1:6, "_", 1:6))

### group: Gamma
γ_AB = c(paste0("0_0_", 1:6, "_", 0))

### group: Away
β_AB = setdiff(unique(pot_bio_data$Code_focal), c(α_AB, γ_AB))
#View(as.data.frame(β_AB))

### 
pot_bio_data_β = subset(pot_bio_data, Code_focal %in% β_AB)
unique(pot_bio_data_β$Code_focal)
#View(pot_bio_data_β)
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

### Home
pot_bio_data_α = subset(pot_bio_data, Code_focal %in% α_AB)
dim(pot_bio_data_α)
# View(pot_bio_data_α)
### Gamma
pot_bio_data_γ = subset(pot_bio_data, Code_focal %in% γ_AB)
dim(pot_bio_data_γ)
# View(pot_bio_data_γ)
################################################################################
drought = unique(pot_bio_data_β$drought) # "G" "D"
combi = unique(pot_bio_data_β$combi)
#ii = "G"; iii = "4_6_6_4"

final_RII_data = NULL
for (ii in drought) {
  for (iii in combi) {
    select_data_β = subset(pot_bio_data_β, drought == ii & combi == iii)
    ### lables of species A and species B
    sp_A = substr(unique(select_data_β$combi), 5, 5) 
    sp_B = substr(unique(select_data_β$combi), 7, 7) 
    
    ### Home database
    α_A = subset(pot_bio_data_α, density == "L" & drought == ii & condi_sp == sp_A & focal_sp == sp_A) %>% tidyr::drop_na(mono_bio)
    α_B = subset(pot_bio_data_α, density == "L" & drought == ii & condi_sp == sp_B & focal_sp == sp_B) %>% tidyr::drop_na(mono_bio)
    colnames(α_A)[colnames(α_A) == "mono_bio"] <- "AinA mean"; colnames(α_B)[colnames(α_B) == "mono_bio"] <- "BinB mean"
    
    ### Away database
    β_A = subset(select_data_β, condi_sp == sp_B) %>% tidyr::drop_na(mono_bio)
    β_B = subset(select_data_β, condi_sp == sp_A) %>% tidyr::drop_na(mono_bio)
    colnames(β_A)[colnames(β_A) == "mono_bio"] <- "AinB mean"; colnames(β_B)[colnames(β_B) == "mono_bio"] <- "BinA mean"
    
    ### Gamma database
    γ_A = subset(pot_bio_data_γ, density == "0" & drought == "0" & focal_sp == sp_A) %>% tidyr::drop_na(mono_bio)
    γ_B = subset(pot_bio_data_γ, density == "0" & drought == "0" & focal_sp == sp_B) %>% tidyr::drop_na(mono_bio)
    #colnames(γ_A)[15] = "AinR mean"; colnames(γ_B)[15] = "BinR mean"
    colnames(γ_A)[colnames(γ_A) == "mono_bio"] <- "AinR mean"; colnames(γ_B)[colnames(γ_B) == "mono_bio"] <- "BinR mean"
    
    #### Merge Datasets
    #cal_total_data = β_A[,c("density","drought","β_A_AGB")] %>% dplyr::left_join(β_B[,c("density","drought","β_B_AGB")], by = c("density","drought"))
    cal_total_data = β_A[,c("density","drought","block","AinB mean")] %>% 
      left_join(β_B[,c("density","drought","block","BinA mean")], by = c("density","drought","block")) %>% 
      left_join(α_A[,c("density","drought","block","AinA mean")], by = c("density","drought","block")) %>%
      left_join(α_B[,c("density","drought","block","BinB mean")], by = c("density","drought","block")) %>% 
      left_join(γ_A[,c("block","AinR mean")], by = c("block")) %>%
      left_join(γ_B[,c("block","BinR mean")], by = c("block"))
    cal_total_data$`Species A` = sp_A; cal_total_data$`Species B` = sp_B; cal_total_data$species_pair = paste0(sp_A,"_",sp_B)
    final_RII_data = rbind(final_RII_data, cal_total_data)
  }
}

dim(final_RII_data)

final_RII_data$PSF_sp_A = log(final_RII_data$`AinA mean`/final_RII_data$`AinB mean`)
final_RII_data$PSF_sp_B = log(final_RII_data$`BinB mean`/final_RII_data$`BinA mean`)

#final_RII_data$PSF_sp_A = (final_RII_data$`AinA mean` - final_RII_data$`AinB mean`)/final_RII_data$`AinB mean`
#final_RII_data$PSF_sp_B = (final_RII_data$`BinB mean` - final_RII_data$`BinA mean`)/final_RII_data$`BinA mean`

### Relationship between PSF and plant intrinsic growth rate
PSF_data = final_RII_data[,c("drought","block","Species A","PSF_sp_A","Species B","PSF_sp_B","species_pair")]

PSF_data <- PSF_data %>%
  mutate(pair = recode(species_pair,"1_5" = "15","2_1" = "12", "2_3" = "23", "2_4" = "24", "2_5" = "25",
                       "2_6" = "26", "3_1" = "13", "3_4" = "34", "3_5" = "35", "3_6" = "36", "4_1" = "14", 
                       "4_5" = "45", "6_1" = "16", "6_4" = "46", "6_5" = "56")) %>% as.data.frame()

PSF_data_sp_A = subset(PSF_data[,c("drought", "block", "pair", "PSF_sp_A","Species A","Species B")], PSF_sp_A != "NA" & PSF_sp_A != "Inf" & PSF_sp_A != "-Inf" & PSF_sp_A != "NaN")
colnames(PSF_data_sp_A)[c(4:6)] = c("PSF_val", "focal_sp","condi_sp")

PSF_data_sp_B = subset(PSF_data[,c("drought", "block", "pair", "PSF_sp_B","Species B","Species A")], PSF_sp_B != "NA" & PSF_sp_B != "Inf" & PSF_sp_B != "-Inf" & PSF_sp_B != "NaN")
colnames(PSF_data_sp_B)[c(4:6)] = c("PSF_val", "focal_sp","condi_sp")

PSF_data = rbind(PSF_data_sp_A, PSF_data_sp_B) %>% as.data.frame() 

PSF_data$drought = as.factor(PSF_data$drought)
PSF_data$focal_sp = as.factor(PSF_data$focal_sp)
PSF_data$condi_sp = as.factor(PSF_data$condi_sp)

PSF_data <- PSF_data %>%
  mutate(abbrev_focal = recode(focal_sp,`1` = "Bb",`2` = "Ac",`3` = "Car",
                               `4` = "Cal",`5` = "Sp",`6` = "Pb"))
PSF_data <- PSF_data %>%
  mutate(abbrev_condi = recode(condi_sp,`1` = "Bb",`2` = "Ac",`3` = "Car",
                               `4` = "Cal",`5` = "Sp",`6` = "Pb"))

PSF_data$drought2 = ifelse(PSF_data$drought == "G", "Ambient", 
                           ifelse(PSF_data$drought == "D", "Drought", "Sterilized"))
str(PSF_data)

# PSF_data_mono = PSF_data

### Adding covariates
Soil_group = read.xlsx("Coexistence_data.xlsx", sheet = "Pot_conditioning_phase", rowNames = T, colNames = T)
Soil_group$Sample_ID = rownames(Field_group)
colnames(Soil_group)
Soil_group$condi_bio = sqrt(Soil_group$biomass)
Soil_group = Soil_group %>% 
  mutate(abbrev_condi = recode(focal,`1` = "Bb",`2` = "Ac",`3` = "Car",
                               `4` = "Cal",`5` = "Sp",`6` = "Pb"))
Soil_group$drought2 = ifelse(Soil_group$drought == "G", "Ambient", "Drought")

### LMMs 
################################################################################
PSF_data2 = PSF_data %>% left_join(Soil_group[,c("abbrev_condi","drought2","block","condi_bio")])

mod_lme = lmer(PSF_val ~ condi_bio + drought2 * abbrev_focal + (1|block) + (1|pair), data = PSF_data2)
anova(mod_lme, type = 3)
shapiro.test(residuals(mod_lme))
summary(mod_lme)
ranova(mod_lme)

##
PSF_data_mean = subset(PSF_data, PSF_val != "NA") %>% group_by(drought2, abbrev_focal) %>% 
  summarise(mean_PSF = mean(PSF_val),sd_PSF = sd(PSF_val, na.rm = TRUE),       
            se_PSF = sd_PSF / sqrt(n()), .groups = 'drop') %>%
  left_join(emm1_multi)


pd = position_dodge(.5)
library(ggplot2)
PSF_data_mean$abbrev_focal = factor(PSF_data_mean$abbrev_focal, levels = c("Ac","Bb","Cal","Car","Pb","Sp"))

ggplot(PSF_data_mean, aes(x = abbrev_focal, y = mean_PSF)) + 
  geom_errorbar(aes(ymin = mean_PSF - se_PSF, ymax = mean_PSF + se_PSF, group = drought2), 
                position=pd, width = 0, color = "black", show.legend = F) +
  geom_text(data = PSF_data_mean, mapping = aes(x = abbrev_focal,y = mean_PSF+se_PSF+0.05,label = .group, group = drought2), 
            color = "black", size = 3,position = position_dodge(.5)) +
  geom_point(size=3,position=pd,alpha=1, pch = 21, aes(fill = drought2), color = "black") +
  scale_fill_manual(values = c("#70A7C3","#A67C2A")) + 
  scale_color_manual(values = c("#70A7C3","#A67C2A")) + 
  #geom_line(aes(x = abbrev_focal, y = mean_PSF, group = drought2), color = "black", size = 0.5, inherit.aes = FALSE) + 
  theme_bw() + mytheme +
  scale_y_continuous(labels = scales::label_comma(accuracy =0.1), limits = c(-0.65, 0.8)) + 
  labs(x = "Response species",
       y = expression(PSF[growth] ~ "(Ln " ~ frac(Mass[home-mono], Mass[away-mono]) ~ ")"), 
       fill = NULL, tag = "a") + # italic
  theme(legend.position = c(0.12, 0.88),
        legend.key = element_blank(),
        legend.background = element_rect(fill = NA)) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed") -> Fig_2a; Fig_2a

PSF_data_growth = PSF_data ## Storing Data

################################################################################
## Competitive abilities
library(openxlsx)
library(dplyr)

pot_bio_data_row = read.xlsx("Coexistence_data.xlsx", sheet = "Pot_feedback_phase", rowNames = F, colNames = T)

## 
pot_bio_data_mix = subset(pot_bio_data_row, group == "1" & above_bio != 0)
pot_bio_data_mix = pot_bio_data_mix[,c(2:6,15)]; colnames(pot_bio_data_mix)[6] = "mix_bio"
pot_bio_data_mono = subset(pot_bio_data_row, group == "0" & above_bio != 0) 
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
RII_data_all$RCI = (RII_data_all$mix_bio * 2)/(RII_data_all$mono_bio + RII_data_all$mix_bio)
#### Calculation of plant-soil feedback based on competition coefficient
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
#View(pot_bio_data_β)
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
################################################################################
drought = unique(pot_bio_data_β$drought) # "G" "D"
combi = unique(pot_bio_data_β$combi)
#ii = "G"; iii = "4_6_6_4"

final_RII_data = NULL
for (ii in drought) {
  for (iii in combi) {
    select_data_β = subset(pot_bio_data_β, drought == ii & combi == iii)
    ### 
    sp_A = substr(unique(select_data_β$combi), 5, 5) 
    sp_B = substr(unique(select_data_β$combi), 7, 7) 
    
    ### 
    α_A = subset(pot_bio_data_α, density == "L" & drought == ii & condi_sp == sp_A & focal_sp == sp_A) %>% tidyr::drop_na(RCI)
    α_B = subset(pot_bio_data_α, density == "L" & drought == ii & condi_sp == sp_B & focal_sp == sp_B) %>% tidyr::drop_na(RCI)
    colnames(α_A)[colnames(α_A) == "RCI"] <- "AinA mean"; colnames(α_B)[colnames(α_B) == "RCI"] <- "BinB mean"
    
    ### 
    β_A = subset(select_data_β, condi_sp == sp_B) %>% tidyr::drop_na(RCI)
    β_B = subset(select_data_β, condi_sp == sp_A) %>% tidyr::drop_na(RCI)
    colnames(β_A)[colnames(β_A) == "RCI"] <- "AinB mean"; colnames(β_B)[colnames(β_B) == "RCI"] <- "BinA mean"
    
    ### 
    γ_A = subset(pot_bio_data_γ, density == "0" & drought == "0" & focal_sp == sp_A) %>% tidyr::drop_na(RCI)
    γ_B = subset(pot_bio_data_γ, density == "0" & drought == "0" & focal_sp == sp_B) %>% tidyr::drop_na(RCI)
    #colnames(γ_A)[15] = "AinR mean"; colnames(γ_B)[15] = "BinR mean"
    colnames(γ_A)[colnames(γ_A) == "RCI"] <- "AinR mean"; colnames(γ_B)[colnames(γ_B) == "RCI"] <- "BinR mean"
    
    #### 
    #cal_total_data = β_A[,c("density","drought","β_A_AGB")] %>% dplyr::left_join(β_B[,c("density","drought","β_B_AGB")], by = c("density","drought"))
    cal_total_data = β_A[,c("density","drought","block","AinB mean")] %>% 
      left_join(β_B[,c("density","drought","block","BinA mean")], by = c("density","drought","block")) %>% 
      left_join(α_A[,c("density","drought","block","AinA mean")], by = c("density","drought","block")) %>%
      left_join(α_B[,c("density","drought","block","BinB mean")], by = c("density","drought","block")) %>% 
      left_join(γ_A[,c("block","AinR mean")], by = c("block")) %>%
      left_join(γ_B[,c("block","BinR mean")], by = c("block"))
    cal_total_data$`Species A` = sp_A; cal_total_data$`Species B` = sp_B; cal_total_data$species_pair = paste0(sp_A,"_",sp_B)
    final_RII_data = rbind(final_RII_data, cal_total_data)
  }
}

dim(final_RII_data)

final_RII_data$PSF_sp_A = log(final_RII_data$`AinA mean`/final_RII_data$`AinB mean`)
final_RII_data$PSF_sp_B = log(final_RII_data$`BinB mean`/final_RII_data$`BinA mean`)

### The relationship between PSF and competitiveness
PSF_data = final_RII_data[,c("drought","block","Species A","PSF_sp_A","Species B","PSF_sp_B","species_pair")]

PSF_data <- PSF_data %>%
  mutate(pair = recode(species_pair,"1_5" = "15","2_1" = "12", "2_3" = "23", "2_4" = "24", "2_5" = "25",
                       "2_6" = "26", "3_1" = "13", "3_4" = "34", "3_5" = "35", "3_6" = "36", "4_1" = "14", 
                       "4_5" = "45", "6_1" = "16", "6_4" = "46", "6_5" = "56")) %>% as.data.frame()



PSF_data_sp_A = subset(PSF_data[,c("drought", "block", "pair", "PSF_sp_A","Species A","Species B")], PSF_sp_A != "NA" & PSF_sp_A != "Inf" & PSF_sp_A != "-Inf" & PSF_sp_A != "NaN")
colnames(PSF_data_sp_A)[c(4:6)] = c("PSF_val", "focal_sp","condi_sp")

PSF_data_sp_B = subset(PSF_data[,c("drought", "block", "pair", "PSF_sp_B","Species B","Species A")], PSF_sp_B != "NA" & PSF_sp_B != "Inf" & PSF_sp_B != "-Inf" & PSF_sp_B != "NaN")
colnames(PSF_data_sp_B)[c(4:6)] = c("PSF_val", "focal_sp","condi_sp")

PSF_data = rbind(PSF_data_sp_A, PSF_data_sp_B) %>% as.data.frame() 

PSF_data$drought = as.factor(PSF_data$drought)
PSF_data$focal_sp = as.factor(PSF_data$focal_sp)
PSF_data$condi_sp = as.factor(PSF_data$condi_sp)

PSF_data <- PSF_data %>%
  mutate(abbrev_focal = recode(focal_sp,`1` = "Bb",`2` = "Ac",`3` = "Car",
                               `4` = "Cal",`5` = "Sp",`6` = "Pb"))
PSF_data <- PSF_data %>%
  mutate(abbrev_condi = recode(condi_sp,`1` = "Bb",`2` = "Ac",`3` = "Car",
                               `4` = "Cal",`5` = "Sp",`6` = "Pb"))

PSF_data$drought2 = ifelse(PSF_data$drought == "G", "Ambient", 
                           ifelse(PSF_data$drought == "D", "Drought", "Sterilized"))
str(PSF_data)

### 
Soil_group = read.xlsx("Coexistence_data.xlsx", sheet = "Pot_conditioning_phase", rowNames = T, colNames = T)
Soil_group$Sample_ID = rownames(Field_group)
colnames(Soil_group)
Soil_group$condi_bio = sqrt(Soil_group$biomass)
Soil_group = Soil_group %>% 
  mutate(abbrev_condi = recode(focal,`1` = "Bb",`2` = "Ac",`3` = "Car",
                               `4` = "Cal",`5` = "Sp",`6` = "Pb"))
Soil_group$drought2 = ifelse(Soil_group$drought == "G", "Ambient", "Drought")
PSF_data2 = PSF_data %>% left_join(Soil_group[,c("abbrev_condi","drought2","block","condi_bio")])

# Table S4
mod_lme = lmer(PSF_val ~ condi_bio + drought2 * abbrev_focal + (1|block) + (1|pair), data = PSF_data2)
#View(subset(PSF_data, abbrev_focal == "Bb" & drought == "D"))
anova(mod_lme, type = 3)
shapiro.test(residuals(mod_lme))
ranova(mod_lme)
summary(mod_lme)

emm1 = emmeans(mod_lme, specs = pairwise ~ drought2 * abbrev_focal, type = 'response', adjust = 'tukey')
emm1_multi = multcomp::cld(emm1,alpha=0.05,Letters=letters,adjust="none",decreasing = T)
emm1_multi$.group <- trimws(emm1_multi$.group)


##
PSF_data_mean = subset(PSF_data, PSF_val != "NA") %>% group_by(drought2, abbrev_focal) %>% 
  summarise(mean_PSF = mean(PSF_val),sd_PSF = sd(PSF_val, na.rm = TRUE),       
            se_PSF = sd_PSF / sqrt(n()), .groups = 'drop') %>%
  left_join(emm1_multi)

pd = position_dodge(.5)
library(ggplot2)
PSF_data_mean$abbrev_focal = factor(PSF_data_mean$abbrev_focal, levels = c("Ac","Bb","Cal","Car","Pb","Sp"))
#######
ggplot(PSF_data_mean, aes(x = abbrev_focal, y = mean_PSF)) + 
  geom_errorbar(aes(ymin = mean_PSF - se_PSF, ymax = mean_PSF + se_PSF, group = drought2), 
                position=pd, width = 0, color = "black", show.legend = F) +
  geom_text(data = PSF_data_mean, mapping = aes(x = abbrev_focal,y = mean_PSF + se_PSF + 0.03,label = .group, group = drought2), 
            color = "black", size = 3,position = position_dodge(.5)) +
  geom_point(size=3,position=pd,alpha=1, pch = 21, aes(fill = drought2), color = "black") +
  scale_fill_manual(values = c("#70A7C3","#A67C2A")) + 
  scale_color_manual(values = c("#70A7C3","#A67C2A")) + 
  theme_bw() + mytheme +
  scale_y_continuous(labels = scales::label_comma(accuracy =0.1), limits = c(-0.62, 0.38)) + 
  labs(x = "Response species",
       y = expression(PSF[competitiveness] ~ "(Ln " ~ frac(CI[home], CI[away]) ~ ")"), 
       fill = NULL, tag = "b") + 
  theme(legend.position = "none",
        legend.key = element_blank(),
        legend.background = element_rect(fill = NA)) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed") -> Fig_2b; Fig_2b

PSF_data_compi = PSF_data ## Storing Data

################################################################################
#PSF_data_compi = PSF_data
colnames(PSF_data_compi)[4] = "PSF_compi"
colnames(PSF_data_growth)[4] = "PSF_growth"
PSF_data_rela = PSF_data_compi %>% left_join(PSF_data_growth)

cor.test(PSF_data_rela$PSF_compi, PSF_data_rela$PSF_growth)

plot(PSF_data_rela$PSF_growth, PSF_data_rela$PSF_compi)

library(BestFitM)
library(ggtrendline)
AAA = subset(PSF_data_rela, drought == "G")
bestFitM2(data= AAA, x= "PSF_growth", y = "PSF_compi")
FitM(data = AAA, x= "PSF_growth", y = "PSF_compi",model = "line3P" )

BBB = subset(PSF_data_rela, drought == "D")
bestFitM2(data= BBB, x= "PSF_growth", y = "PSF_compi")
FitM(data = BBB, x= "PSF_growth", y = "PSF_compi",model = "line2P" )

ggtrendline(AAA$PSF_growth, AAA$PSF_compi, model = "line3P") + 
  geom_point(AAA, mapping = aes(x = PSF_growth, y = PSF_compi))

ggtrendline(BBB$PSF_growth, BBB$PSF_compi, model = "line2P") + 
  geom_point(BBB, mapping = aes(x = PSF_growth, y = PSF_compi))


## ln(HOME/AWAY)
ggplot(data = PSF_data_rela, aes(x = PSF_growth, y = PSF_compi, color = drought2, fill = drought2)) + 
  geom_smooth(data = subset(PSF_data_rela, drought2 == "Ambient"),method = "lm", 
              se = TRUE, aes(fill = drought2), formula = y ~ poly(x, 2, raw=TRUE), alpha = 0.5) +
  geom_smooth(data = subset(PSF_data_rela, drought2 == "Drought"),
              method = "lm", se = TRUE, aes(fill = drought2), alpha = 0.5) +
  scale_color_manual(values = c("#70A7C3","#A67C2A")) + 
  scale_fill_manual(values = c("#B7D3E1","#D2BD94")) + 
  ggnewscale::new_scale_fill() + 
  geom_point(size = 3, color = "black", pch = 21, aes(fill = drought2)) + 
  scale_fill_manual(values = c("#70A7C3","#A67C2A")) + 
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  annotate("text", x = -1.18, y = -1, parse = TRUE, size = 3.5, color = "#70A7C3",
           label = expression(italic(R)^2 == 0.465 ~ "," ~ italic(p) < 0.001)) +
  annotate("text", x = -1.18, y = -1.15, parse = TRUE, size = 3.5, color = "#A67C2A",
           label = expression(italic(R)^2 == 0.408 ~ "," ~ italic(p) < 0.001)) +
  scale_x_continuous(labels = scales::label_comma(accuracy =0.1)) +
  scale_y_continuous(labels = scales::label_comma(accuracy =0.1)) + 
  theme_bw() + mytheme + theme(legend.position = c(0.85,0.90)) + 
  labs(x = expression("PSF "[growth] ~ "estimated by PSF experiment"),
       y = expression("PSF "[competitiveness] ~ "estimated by PSF experiment"), tag = "c") -> Fig_2c; Fig_2c


library(patchwork)
((Fig_2a/Fig_2b)|Fig_2c) + plot_layout(widths = c(0.45,0.55))


