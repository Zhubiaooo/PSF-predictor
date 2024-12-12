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

## Effects of conditioning species, drought and their interactions on plant aboveground biomass
Soil_group$drought = as.factor(Soil_group$drought)
Soil_group$focal = as.factor(Soil_group$focal)
Soil_group$block = as.factor(Soil_group$block)
colnames(Soil_group)
Soil_group$bio_sq = sqrt(Soil_group$biomass)
Soil_group = Soil_group %>% 
  mutate(abbrev_focal = recode(focal,`1` = "Bb",`2` = "Ac",`3` = "Car",
                               `4` = "Cal",`5` = "Sp",`6` = "Pb"))

Soil_group$drought2 = ifelse(Soil_group$drought == "G", "Ambient", "Drought")
Soil_group$drought2 = factor(Soil_group$drought2, levels = c("Ambient", "Drought"))

# Table S1
bio_mod = lmer(bio_sq ~ drought2 * abbrev_focal + (1|block), data = Soil_group)
anova(bio_mod, type = 3)

# Fig S2
bio_data_sum = Soil_group %>% group_by(drought, focal) %>% 
  summarise(mean_bio = mean(bio_sq),sd_bio = sd(bio_sq, na.rm = TRUE),       
            se_bio = sd_bio / sqrt(n()), .groups = 'drop') %>%
  mutate(abbrev_focal = recode(focal,`1` = "Bb",`2` = "Ac",`3` = "Car",
                               `4` = "Cal",`5` = "Sp",`6` = "Pb"))


bio_data_sum$drought2 = ifelse(bio_data_sum$drought == "G", "Ambient", "Drought")
bio_data_sum$drought2 = factor(bio_data_sum$drought2, levels = c("Ambient", "Drought"))


###
emm1 = emmeans(bio_mod, specs = pairwise ~ abbrev_focal * drought2, type = 'response', adjust = 'none')
emm1_multi = multcomp::cld(emm1,alpha=0.05,Letters=letters,adjust="none",decreasing = T)
emm1_multi$.group <- trimws(emm1_multi$.group)

bio_data_sum = bio_data_sum %>% left_join(emm1_multi)
bio_data_sum$abbrev_focal = factor(bio_data_sum$abbrev_focal, levels = c("Ac","Bb","Cal","Car","Pb","Sp"))

pd = position_dodge(.8)
library(ggplot2)
ggplot(bio_data_sum, aes(x = abbrev_focal, y = mean_bio)) + 
  geom_errorbar(aes(ymin = mean_bio - se_bio, ymax = mean_bio + se_bio, group = drought2), 
                position=pd, width = 0, color = "black", show.legend = F) +
  geom_text(data = bio_data_sum, mapping = aes(x = abbrev_focal,y = mean_bio+0.5,label = .group, group = drought2), 
            color = "black", size = 3,position = position_dodge(.8)) +
  geom_point(data = Soil_group, aes(x = abbrev_focal, y = bio_sq, color = drought2),
             position = position_jitterdodge(.2), alpha = 0.5, show.legend = F) + 
  geom_point(size=3,position=pd,alpha=1, pch = 21, aes(fill = drought2), color = "black") +
  scale_fill_manual(values = c("#70A7C3","#A67C2A")) + 
  scale_color_manual(values = c("#70A7C3","#A67C2A")) + 
  theme_bw() + mytheme +
  annotate("text", x = 1.4, y = 1.5, parse = TRUE, size = 3.5, 
           label = expression("W × C:" ~ italic(p) < 0.001)) + 
  scale_y_continuous(labels = scales::label_comma(accuracy =0.1)) + 
  labs(x = "Soil conditioning species",y = "Aboverground biomass (g, sqrt)",fill = NULL) + 
  theme(legend.position = c(0.85, 0.13),
        legend.key = element_blank(),
        legend.background = element_rect(fill = NA)) -> Fig_S2; Fig_S2






