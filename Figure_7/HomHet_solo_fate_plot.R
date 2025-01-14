setwd("D:/00_1st_Year_PhD/00_Manuscript_2/Plots/Dispersion/")

library(ggplot2)
library(tidyverse)
library(rstatix)
library(ggpubr)
library(openxlsx)
library(readxl)
library(readr)
library(svglite)

#ALL - no meso = 4
#ALL - no meso + exemeso = 6

Q <- read_excel('Migration_HOMHET_mig_morpho.xlsx', sheet = 4)
theme_set(theme_bw()) 

#Q <- filter(M, Fate %in% c("LV", "Atria"))

#Q <- filter(M, Period=="Morpho")
#Q <- filter(Q, HomHet=="Hom")
Q <- filter(Q, Period=="Morpho")
Q <- filter(Q, Fate %in% c("LV", "Atria", "Endocardium", "Pericardium", "ExEMeso", 'Endothelial'))



Q$Fate <- factor(Q$Fate, levels = c('Atria',
                                    'LV',
                                    'Pericardium',
                                    'Endocardium',
                                    'Endothelial',
                                    'ExEMeso'))

p <- ggplot(Q, aes(y = log(Distance), x = interaction(Fate, Period, lex.order = TRUE), colour = Fate)) +
  geom_boxplot(width = 0.5, lwd =0.25, fatten = 1, outlier.shape = NA, coef=5) +
  geom_jitter(size = 0.2, alpha = 1, width = 0.4, height = 0) +
  #scale_y_continuous(limits = c(0, 150), breaks = seq(0, 150, by = 30)) +
  #scale_x_continuous(limits = c(0, 4), breaks = seq(0, 4, by = 1)) +
  xlab('') +
  ylab('log(Distance)') +
  theme(axis.text.y = element_text(size = 14, face = "bold"), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), 
        axis.ticks.y =element_blank()) +
  theme(axis.text.x = element_text(size = 14, face = "bold")) +
  scale_color_manual(values = c('LV' = '#2CAC80', 'Atria' = '#B9CD35', 'Endocardium' = '#D0B2D5', 'Pericardium' = '#F5AC61','Endothelial' = '#BCE0EF', 'ExEMeso' = '#3C538A'), 
                     label = c('LV' = 'LV', 'Atria' = 'Atria', 'Endocardium' = 'Endocardium', 'Pericardium' = 'Pericardium','Endothelial' = 'Endothelial', 'ExEMeso' = 'Extraembryonic Mesoderm')) 


p


#Mig vs Morpho
# homhet_comp <- list(c("LV.Mig", "LV.Morpho"),c("Atria.Mig", "Atria.Morpho"),
#                     c("Pericardium.Mig", "Pericardium.Morpho"),c("Endocardium.Mig", "Endocardium.Morpho"),
#                     c("ExEMeso.Mig", "ExEMeso.Morpho"), c("Endothelial.Mig", "Endothelial.Morpho"))
# 
# homhet_comp <- list(c("LV.Mig", "LV.Morpho"),c("Atria.Mig", "Atria.Morpho"))


#Migration
# homhet_comp <- list(c("Atria.Mig", "LV.Mig"),c("LV.Mig", "Pericardium.Mig"),
#                     c("LV.Mig", "Endocardium.Mig") ,c("LV.Mig", "ExEMeso.Mig"),
#                     c("ExEMeso.Mig", "Pericardium.Mig"), c("Endocardium.Mig", "ExEMeso.Mig"),
#                     c("Atria.Mig", "ExEMeso.Mig"),c("Endocardium.Mig", "Pericardium.Mig"))

#Morphogenesis
homhet_comp <- list(c("Atria.Morpho", "LV.Morpho"), c("LV.Morpho", "Pericardium.Morpho"),
                    c("LV.Morpho", "Endocardium.Morpho"),c("LV.Morpho", "ExEMeso.Morpho"),
                    c("ExEMeso.Morpho", "Pericardium.Morpho"), c("Endocardium.Morpho", "ExEMeso.Morpho"),
                    c("Atria.Morpho", "ExEMeso.Morpho"),c("Endocardium.Morpho", "Pericardium.Morpho"))


b <- p + stat_compare_means(comparisons=homhet_comp, aes(label = after_stat(p.format)),
                            method = "wilcox.test", ref.group = "0.05", label.x = c(1,2,3))

b


means <- Q %>%
  group_by(Fate, Period) %>%
  summarise(
    mean = mean(Distance),
    sd = sd(Distance),
    n = n() 
  )
print(means)



# ggsave('Hom_Morpho_Fate.svg', b, bg='transparent', width = 7,
#         height = 4, limitsize = FALSE)

