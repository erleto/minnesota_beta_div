library(ade4)
library(psych)
library(dplyr)

# Select interesting variables for PCA
units_merged_pca <- dplyr::select(units_merged_forest_freq, censusSUM, 
  Dens_Major:Dens_Other, HabV15:HabV18, HabV26)
units_merged_pca <- dplyr::select(units_merged_forest_freq, censusSUM, 
  Dens_Major:Dens_Other, HabV2:HabV51)

# Print scree plot, showing how many components to include in the PCA
VSS.scree(units_merged_pca, main = "scree plot")

# Perform PCA
human_gradient_pca <- principal(units_merged_pca, nfactors = 10, rotate="varimax", covar = F)
human_gradient_pca