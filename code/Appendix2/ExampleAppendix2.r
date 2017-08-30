#load files, sources and libraries#
load("./code/Appendix2/fig2extnd.r")
source("./code/Appendix2/Rao.r")
library(cluster)

#make the species x plot matrix#
usesp <- fig2extnd[,1:4]

#calculate the pairwise dissimilarity#
useD <- daisy(as.data.frame(fig2extnd[,8]), metric="gower")

#calculate Rao diversity
useRaoNJ <- Rao(sample = usesp, dfunc = useD, dphyl = NULL, weight = F, Jost = F, structure = NULL)
useRaoJ <- Rao(sample = usesp, dfunc = useD, dphyl = NULL, weight = F, Jost = T, structure = NULL)

useRaoJ_summary <- data.frame(
  alpha = c(useRaoJ$TD$Mean_Alpha, useRaoJ$FD$Mean_Alpha), 
  beta_prop = c(useRaoJ$TD$Beta_prop, useRaoJ$FD$Beta_prop), 
  gamma = c(useRaoJ$TD$Gamma, useRaoJ$FD$Gamma), 
  effective_beta = c(useRaoJ$TD$Gamma / useRaoJ$TD$Mean_Alpha, useRaoJ$FD$Gamma / useRaoJ$FD$Mean_Alpha))
rownames(useRaoJ_summary) <- c('Taxonomic', 'Functional')
useRaoJ_summary

useRaoNJ_summary <- data.frame(
  alpha = c(useRaoNJ$TD$Mean_Alpha, useRaoNJ$FD$Mean_Alpha), 
  beta_prop = c(useRaoNJ$TD$Beta_prop, useRaoNJ$FD$Beta_prop), 
  gamma = c(useRaoNJ$TD$Gamma, useRaoNJ$FD$Gamma), 
  effective_beta = c(useRaoNJ$TD$Gamma / useRaoNJ$TD$Mean_Alpha, useRaoNJ$FD$Gamma / useRaoNJ$FD$Mean_Alpha))
rownames(useRaoNJ_summary) <- c('Taxonomic', 'Functional')
useRaoNJ_summary

#plot(useRaoNJ$TD$Pairwise_samples$Beta_prop, useRaoJ$TD$Pairwise_samples$Beta_prop)
