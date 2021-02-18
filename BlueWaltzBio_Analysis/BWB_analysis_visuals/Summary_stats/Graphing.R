library(ggplot2)
require(reshape2)
library(gplots)
require(stats)
library(qwraps2)
library(VennDiagram)
library(tidyverse)

setwd(Users/samuelrapp/GitHub/CALeDNA-NPS-AIS/BWB_analysis_visual)
importedf <- file.choose("gnr_genus_species_NCBI_.csv")
Taxonomy_DF <- read.csv(file = importedf) 
vector <- c()
Columnnames <-Taxonomy_DF %>% colnames()
vector <- c(vector, Columnnames[-c(1)]) #copy everything but the first column (which is spp names)
vector
population.long <- Taxonomy_DF %>% pivot_longer(cols =vector, names_to = "barcodes", values_to = "seq_counts")# %>% arrange(year, city_state)

population.long %>% 
  ggplot(aes(x = barcodes, y = seq_counts, color = X)) +
  geom_line()+
  geom_point()

removezeros <-population.long[population.long[,3] > 0 , ] #removing rows where seq count = 0

removezeros %>%  
  ggplot(aes(x = barcodes, y = seq_counts, color = X)) +
  geom_point()+
  ylim(0,5000)
1+2
removezeros %>%  
  ggplot(aes(x = seq_counts, y = barcodes)) +
  geom_boxplot()+
  ylim(0,5000)

removezeros %>%  
  ggplot(aes(x = X, y = seq_counts)) +
  geom_point()+
  ylim(0,5000)



