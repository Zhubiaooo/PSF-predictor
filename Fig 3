library(openxlsx)
library(dplyr)
library(phytools)
library(vegan)

# Soil sample grouping information
Field_group = read.xlsx("Coexistence_data.xlsx", sheet = "Pot_conditioning_phase", rowNames = T, colNames = T)
Field_group$Sample_ID = rownames(Field_group)

# Soil sample abundance information
Field_otu_row2 = read.xlsx("Coexistence_data.xlsx", sheet = "fungi_phase", colNames = T, rowNames = T) # row ASVs abundance 
Field_otu_row = Field_otu_row2[,rownames(Field_group)]
Field_otu_row[1:6,1:6]
dim(Field_otu_row)
rownames(Field_group) %in% colnames(Field_otu_row)

## Tax INFORMATION
tax_default <- read.xlsx("Coexistence_data.xlsx", sheet = "fungi_tax", colNames = T, rowNames = T)
head(tax_default)

# Resampled by the minimum number of reads per sample
library(microeco)
library(mecodev)
data_default <- microtable$new(sample_table = Field_group,otu_table = Field_otu_row, tax_table = tax_default)
print(data_default)
data_default$sample_table
data_default$tidy_dataset() 
print(data_default)
data_default$sample_sums()%>% range

View(data_default$otu_table)
sum(data_default$otu_table)

set.seed(1234)
data_default$rarefy_samples(sample.size = 31438)
data_default$sample_sums()%>% range
print(data_default)
fungi_Flattening <- data_default$otu_table
fungi_Flattening[1:5,1:5]

################################################################################
fungi_Flattening = data_default$otu_table
fungi_tax_Flattenning = data_default$tax_table
fungi_tax_Flattenning$OTU_ID = rownames(fungi_tax_Flattenning)
Soil_group = data_default$sample_table

### load phylogenetic tree
library(phytools)
ASV_tree = read.newick("tree.nwk")
to_drop <- ASV_tree$tip.label[!ASV_tree$tip.label %in% fungi_tax_Flattenning$OTU_ID]
ASV_tree_zyl <- drop.tip(as.phylo(ASV_tree), to_drop) 
#plot(ASV_tree_zyl)

### Fungaltraits
################################################################################
OTU_tax = fungi_tax_Flattenning %>% tidyr::separate(col = taxonomy, 
                                                    into = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species"),
                                                    sep = ";\\s*", fill = "right")
head(OTU_tax[,1:6])
OTU_tax$GENUS = sub("g__", "", OTU_tax$Genus)

##### FungalTraits
FungalTraits = read.xlsx("FungalTraits.xlsx", sheet = "FungalTraits", rowNames = F, colNames = T)
head(FungalTraits[,c(2:6,8)])
OTU_tax2 = OTU_tax %>% left_join(FungalTraits[,c("GENUS","primary_lifestyle")], by = "GENUS")
head(OTU_tax2)
####  |Plant Pathogen|
Pathogen_id1 = subset(OTU_tax2, primary_lifestyle == "plant_pathogen")$OTU_ID
#### Fusarium
Pathogen_id2 = subset(OTU_tax, Genus == "g__Fusarium")$OTU_ID
### |Plant Pathogen| & Fusarium
Pathogens = as.data.frame(unique(c(Pathogen_id1, Pathogen_id2)))
Pathogens$guild = "Plant Pathogen"; colnames(Pathogens)[1] = "OTU_ID"

unique(OTU_tax2$primary_lifestyle)
############################ Arbuscular Mycorrhizal ############################
####  Arbuscular Mycorrhizal
AMF_id1 = subset(OTU_tax2, primary_lifestyle == "arbuscular_mycorrhizal")$OTU_ID; length(AMF_id1)
#### Glomeromycota
AMF_id2 = subset(OTU_tax, Phylum == "p__Glomeromycota")$OTU_ID; length(AMF_id2)
### Arbuscular Mycorrhizal & Glomeromycota
AMF = as.data.frame(unique(c(AMF_id1, AMF_id2)))
AMF$guild = "Arbuscular Mycorrhizal"; colnames(AMF)[1] = "OTU_ID"

############################ Saprotroph ############################
####  Saprotroph
Saprotroph_type = c("soil_saprotroph","litter_saprotroph","unspecified_saprotroph","wood_saprotroph","nectar/tap_saprotroph","pollen_saprotroph")
Saprotroph_id = OTU_tax2[OTU_tax2$primary_lifestyle %in% Saprotroph_type,]$OTU_ID; length(Saprotroph_id)
### Arbuscular Mycorrhizal & Glomeromycota
Saprotrophs = as.data.frame(Saprotroph_id)
Saprotrophs$guild = "Saprotroph"; colnames(Saprotrophs)[1] = "OTU_ID"

#### Check if there is overlap
nrow(Pathogens) + nrow(AMF) + nrow(Saprotrophs) ### 10316
length(unique(c(Pathogens$OTU_ID, AMF$OTU_ID, Saprotrophs$OTU_ID)))

#### guild list
guild_list2 = rbind(Pathogens, AMF, Saprotrophs)
unique(guild_list2$guild)

#
Soil_group$drought = as.factor(Soil_group$drought)
Soil_group$focal = as.factor(Soil_group$focal)
Soil_group$block = as.factor(Soil_group$block)
Soil_group$drought2 = ifelse(Soil_group$drought == "G", "Ambient", "Drought")
Soil_group$drought2 = factor(Soil_group$drought2, levels = c("Ambient", "Drought"))
colnames(Soil_group)
Soil_group$bio_sq = sqrt(Soil_group$biomass)
Soil_group = Soil_group %>% 
  mutate(abbrev_focal = recode(focal,`1` = "Bb",`2` = "Ac",`3` = "Car",
                               `4` = "Cal",`5` = "Sp",`6` = "Pb"))

# Permutational analysis of variance (PERMANOVA, 999 permutations, based on weighted Unifrac distance matrix)
# Overall fungi
library(vegan)
library(phyloseq)
species_hel <- as.data.frame(decostand(t(fungi_Flattening), method = 'hellinger'))
species_hel[1:6,1:6]
Soil_group = Soil_group[rownames(species_hel), ]

df <- phyloseq(otu_table(species_hel, taxa_are_rows = F), phy_tree(ASV_tree_zyl))
all_unif_dis=phyloseq::UniFrac(df, weighted = T)

# PERMANOVA
set.seed(1234)
with(Soil_group, adonis2(all_unif_dis ~  drought * focal, 
                         data = Soil_group, permutations = 999, strata = block))

## Fungi richness
soil_richness <- as.data.frame(specnumber(t(fungi_Flattening)))
colnames(soil_richness) = "Richness";  soil_richness$Sample_ID = rownames(soil_richness)
Soil_group2 = Soil_group %>% left_join(soil_richness)
colnames(Soil_group2)
mod = lmer(Richness ~ abbrev_focal * drought2 + (1|block), data = Soil_group2)
anova(mod)
summary(mod)

Soil_group2 %>% group_by(drought2) %>% 
  summarise(mean_SR = mean(Richness),sd_SR = sd(Richness, na.rm = TRUE),       
            se_SR = sd_SR / sqrt(n()), .groups = 'drop')

## plot
# Visualized by plotting axis from principal coordinates analysis (PCoA)
gaw.pco<-cmdscale(all_unif_dis, eig = T)
var<-round(gaw.pco$eig/sum(gaw.pco$eig)*100,1)
eig = gaw.pco$eig
PCOA1<-data.frame(gaw.pco$points[,1])[,1]
PCOA2<-data.frame(gaw.pco$points[,2])[,1]
PCOA<-data.frame(Sample_ID = rownames(gaw.pco$points),PCOA1,PCOA2)
plotdata = PCOA %>% left_join(Soil_group, by = "Sample_ID")
colnames(plotdata)

plotdata <- plotdata %>%
  mutate(abbrev = recode(focal,`1` = "Bb",`2` = "Ac",`3` = "Car",
                         `4` = "Cal",`5` = "Sp",`6` = "Pb"))

###
library(ggplot2)
colnames(plotdata)
df = plotdata %>% group_by(abbrev, drought) %>% 
  summarise(PCOA1_mean = mean(PCOA1), PCOA1_se = sd(PCOA1)/(sqrt(length(PCOA1))),
            PCOA2_mean = mean(PCOA2), PCOA2_se = sd(PCOA2)/(sqrt(length(PCOA2))))

plot_data2 = merge(plotdata, df, by = c("abbrev", "drought"))

plot_data2$drought = ifelse(plot_data2$drought == "D", "Drought", "Ambient")
df$drought = ifelse(df$drought == "D", "Drought", "Ambient")


plot_data2$abbrev = factor(plot_data2$abbrev, levels = c("Ac","Bb","Cal","Car","Pb","Sp"))
df$abbrev = factor(df$abbrev, levels = c("Ac","Bb","Cal","Car","Pb","Sp"))
###
ggplot(df, aes(PCOA1_mean, PCOA2_mean))+
  geom_point(data = plot_data2,mapping = aes(PCOA1, PCOA2,color = abbrev, shape = drought), size = 2, alpha = 0.5, show.legend = F) +
  geom_errorbar(data = df,mapping = aes(ymax = PCOA2_mean+PCOA2_se, ymin=PCOA2_mean-PCOA2_se,color = abbrev),width=0,size=0.8,alpha = 1)+#
  geom_errorbarh(data = df,mapping = aes(xmax=PCOA1_mean+PCOA1_se,xmin=PCOA1_mean-PCOA1_se,color = abbrev),height=0,size=0.8,alpha = 1) +
  geom_point(data = df,mapping = aes(PCOA1_mean, PCOA2_mean, color = abbrev, shape = drought), size = 3.5)+
  scale_color_manual(values = c("#FBB050","#98694E","#C85C19","#4298C5","#004A80","#D2BEA2")) +
  scale_fill_manual(values = c("#FBB050","#98694E","#C85C19","#4298C5","#004A80","#D2BEA2")) +
  scale_shape_manual(values = c(16,21)) + 
  labs(x=paste("PCoA1 (", format(100 * eig[1] / sum(eig), digits=3), "%)", sep=""),
       y=paste("PCoA2 (", format(100 * eig[2] / sum(eig), digits=3), "%)", sep=""), tag = "a") + 
  scale_x_continuous(labels = scales::label_comma(accuracy =0.01)) +
  scale_y_continuous(labels = scales::label_comma(accuracy =0.01), limits = c(-0.032,0.05)) + 
  theme_bw() + mytheme + theme(legend.position = "right") -> Fig_3a; Fig_3a

## Pathogens
path_hel <- as.data.frame(decostand(t(fungi_Flattening[Pathogens$OTU_ID,]), method = 'hellinger'))
library(phyloseq)
df <- phyloseq(otu_table(path_hel, taxa_are_rows = F), phy_tree(ASV_tree_zyl))
path_unif_dis=phyloseq::UniFrac(df, weighted = T)

# PERMANOVA
set.seed(1234)
with(Soil_group, adonis2(path_unif_dis ~ drought * focal, 
                         data = Soil_group, permutations = 999, strata = block))

## Fungi richenss
Path_richness <- as.data.frame(specnumber(t(fungi_Flattening[Pathogens$OTU_ID,])))
colnames(Path_richness) = "Richness";  Path_richness$Sample_ID = rownames(Path_richness)
Soil_group3 = Soil_group %>% left_join(Path_richness)
colnames(Soil_group3)
mod = lmer(Richness ~ abbrev_focal * drought2 + (1|block), data = Soil_group3)
anova(mod)
summary(mod)

emm1 = emmeans(mod, specs = pairwise ~ drought2 * abbrev_focal, type = 'response', adjust = 'tukey')
emm1_multi = multcomp::cld(emm1,alpha=0.05,Letters=letters,adjust="none",decreasing = T)
emm1_multi$.group <- trimws(emm1_multi$.group)

Path_SR_mean = Soil_group3 %>% group_by(drought2, abbrev_focal) %>% 
  summarise(mean_SR = mean(Richness),sd_SR = sd(Richness, na.rm = TRUE),       
            se_SR = sd_SR / sqrt(n()), .groups = 'drop') %>%
  left_join(emm1_multi)

# plot
gaw.pco<-cmdscale(path_unif_dis, eig = T)
var<-round(gaw.pco$eig/sum(gaw.pco$eig)*100,1)
eig = gaw.pco$eig
PCOA1<-data.frame(gaw.pco$points[,1])[,1]
PCOA2<-data.frame(gaw.pco$points[,2])[,1]
PCOA<-data.frame(Sample_ID = rownames(gaw.pco$points),PCOA1,PCOA2)
plotdata = PCOA %>% left_join(Soil_group, by = "Sample_ID")
colnames(plotdata)

plotdata <- plotdata %>%
  mutate(abbrev = recode(focal,`1` = "Bb",`2` = "Ac",`3` = "Car",
                         `4` = "Cal",`5` = "Sp",`6` = "Pb"))

###
library(ggplot2)
colnames(plotdata)
df = plotdata %>% group_by(abbrev, drought2) %>% 
  summarise(PCOA1_mean = mean(PCOA1), PCOA1_se = sd(PCOA1)/(sqrt(length(PCOA1))),
            PCOA2_mean = mean(PCOA2), PCOA2_se = sd(PCOA2)/(sqrt(length(PCOA2))))

plot_data2 = merge(plotdata, df, by = c("abbrev", "drought2"))
plot_data2$drought = ifelse(plot_data2$drought == "D", "Drought", "Ambient")
df$drought = ifelse(df$drought == "D", "Drought", "Ambient")
plot_data2$abbrev = factor(plot_data2$abbrev, levels = c("Ac","Bb","Cal","Car","Pb","Sp"))
df$abbrev = factor(df$abbrev, levels = c("Ac","Bb","Cal","Car","Pb","Sp"))

###
ggplot(df, aes(PCOA1_mean, PCOA2_mean))+
  geom_point(data = plot_data2,mapping = aes(PCOA1, PCOA2,color = abbrev, shape = drought), size = 2, alpha = 0.5, show.legend = F) +
  geom_errorbar(data = df,mapping = aes(ymax = PCOA2_mean+PCOA2_se, ymin=PCOA2_mean-PCOA2_se,color = abbrev),width=0,size=0.8,alpha = 1)+#
  geom_errorbarh(data = df,mapping = aes(xmax=PCOA1_mean+PCOA1_se,xmin=PCOA1_mean-PCOA1_se,color = abbrev),height=0,size=0.8,alpha = 1) +
  geom_point(data = df,mapping = aes(PCOA1_mean, PCOA2_mean, color = abbrev, shape = drought), size = 3.5)+
  scale_color_manual(values = c("#FBB050","#98694E","#C85C19","#4298C5","#004A80","#D2BEA2")) +
  scale_fill_manual(values = c("#FBB050","#98694E","#C85C19","#4298C5","#004A80","#D2BEA2")) +
  scale_shape_manual(values = c(16,21)) + 
  labs(x=paste("PCoA1 (", format(100 * eig[1] / sum(eig), digits=3), "%)", sep=""),
       y=paste("PCoA2 (", format(100 * eig[2] / sum(eig), digits=3), "%)", sep=""), tag = "a") + 
  scale_x_continuous(labels = scales::label_comma(accuracy =0.01)) +
  scale_y_continuous(labels = scales::label_comma(accuracy =0.01), limits = c(-0.032,0.05)) + 
  theme_bw() + mytheme + theme(legend.position = "right")

# Fig 3b
# load PSF growth and PSF competitiveness (pair-wise mean value)
all_predict_data = read.xlsx("all_predict_data.xlsx", rowNames = F, colNames = T)
colnames(all_predict_data)

library(dplyr)
library(tidyr)
dist_pot = as.matrix(path_unif_dis)
dist_pot[1:6,1:6]

# View(all_predict_data)
all_predict_data$drought = ifelse(all_predict_data$drought2 == "Ambient", "G", "D")

drought = unique(all_predict_data$drought)
Code = unique(all_predict_data$Code)
BC_cal_data = NULL

for (i in drought) {
  for (ii in Code) {
    select_data = subset(all_predict_data, drought == i & Code == ii)
    # Split Code Column
    select_data <- select_data %>%
      separate(Code, into = c("sp_A", "sp_B"), sep = "-")
    
    # Creating home and away columns
    select_data <- select_data %>%
      mutate(home = if_else(sp_A == abbrev_focal, sp_A, if_else(sp_B == abbrev_focal, sp_B, NA)),
             away = if_else(sp_A != abbrev_focal & sp_B != abbrev_focal, sp_A, if_else(sp_A == abbrev_focal, sp_B, NA))) %>%
      mutate(home_code = recode(home, "Bb" = "1", "Ac" = "2", "Car" = "3",
                                "Cal" = "4", "Sp" = "5", "Pb" = "6")) %>%
      mutate(away_code = recode(away, "Bb" = "1", "Ac" = "2", "Car" = "3",
                                "Cal" = "4", "Sp" = "5", "Pb" = "6"))
    
    ## Create Sample ID
    Home_ID = paste0("L",i,select_data$home_code,1:5,"W")
    Away_ID = paste0("L",i,select_data$away_code,1:5,"W")
    
    ## Get pairwise dissimilarity matrix
    Select_matrix = dist_pot[Home_ID, Away_ID]
    select_data$mean_BC = mean(Select_matrix)
    BC_cal_data = rbind(BC_cal_data, select_data)
  }
}

library(BestFitM)
AAA = subset(BC_cal_data, drought2 == "Ambient")
BBB = subset(BC_cal_data, drought2 == "Drought")

bestFitM2(data= AAA, x= "mean_BC", y = "mean_PSF_G")
FitM(data = AAA, x= "mean_BC", y = "mean_PSF_G",model = "line2P")

bestFitM2(data= AAA, x= "mean_BC", y = "mean_PSF_CI")
FitM(data = AAA, x= "mean_BC", y = "mean_PSF_CI",model = "line2P")

bestFitM2(data= BBB, x= "mean_BC", y = "mean_PSF_G")
FitM(data = BBB, x= "mean_BC", y = "mean_PSF_G",model = "line2P")

bestFitM2(data= BBB, x= "mean_BC", y = "mean_PSF_CI")
FitM(data = BBB, x= "mean_BC", y = "mean_PSF_CI",model = "line2P")

ggplot(data = BC_cal_data, aes(x = mean_BC, y = mean_PSF_G, color = drought2)) + 
  geom_smooth(data = subset(BC_cal_data, drought2 == "Ambient"), linetype = 2,
              method = "lm", se = TRUE, aes(fill = drought2)) +
  geom_smooth(data = subset(BC_cal_data, drought2 == "Drought"), linetype = 2,
              method = "lm", se = TRUE, aes(fill = drought2)) +
  scale_color_manual(values = c("#70A7C3","#A67C2A")) + 
  scale_fill_manual(values = c("#B7D3E1","#D2BD94")) + 
  ggnewscale::new_scale_fill() + 
  geom_point(size = 3, aes(fill = drought2), pch = 21, color = "black") + 
  scale_fill_manual(values = c("#70A7C3","#A67C2A")) + 
  theme_bw() + mytheme + 
  scale_x_continuous(labels = scales::label_comma(accuracy =0.01)) +
  scale_y_continuous(labels = scales::label_comma(accuracy =0.01)) + 
  labs(x = "PSF competitiveness\nestimated in PSF experiment",
       y = "Competition index\nestimated in field experiment", tag = "a")


ggplot(data = BC_cal_data, aes(x = mean_BC, y = mean_PSF_CI, color = drought2)) + 
  geom_smooth(data = subset(BC_cal_data, drought2 == "Ambient"), linetype = 2,
              method = "lm", se = TRUE, aes(fill = drought2)) +
  geom_smooth(data = subset(BC_cal_data, drought2 == "Drought"), linetype = 1,
              method = "lm", se = TRUE, aes(fill = drought2)) +
  scale_color_manual(values = c("#70A7C3","#A67C2A")) + 
  scale_fill_manual(values = c("#B7D3E1","#D2BD94")) + 
  ggnewscale::new_scale_fill() + 
  geom_point(size = 3, aes(fill = drought2), pch = 21, color = "black") + 
  scale_fill_manual(values = c("#70A7C3","#A67C2A")) + 
  theme_bw() + mytheme + # theme(legend.position = "right") + 
  scale_x_continuous(labels = scales::label_comma(accuracy =0.01)) +
  scale_y_continuous(labels = scales::label_comma(accuracy =0.01)) + 
  annotate("text", x = 0.22, y = -0.31, parse = TRUE, size = 3, color = "#70A7C3",
           label = expression(italic(R)^2 == 0.005 ~ "," ~ italic(p) == 0.703)) +
  annotate("text", x = 0.22, y = -0.40, parse = TRUE, size = 3, color = "#A67C2A",
           label = expression(italic(R)^2 == 0.176 ~ "," ~ italic(p) == 0.021)) +
  labs(x = "Pathogens community dissimilarities\nbetween home and away soils",
       y = expression(PSF[competitiveness] ~ "(Ln " ~ frac(italic(CI)[home], italic(CI)[away]) ~ ")"), tag = "b") -> Fig_3b; Fig_3b

library(patchwork)
Fig_3a|Fig_3b





