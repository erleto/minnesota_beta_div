library(rgdal)
library(raster)
library(dplyr)

forest_status <- raster('./data/Minnesota_bird_data/MNBBA_Homogenization_GISFiles_8_2015/Forest_Status/forstat_rast.tif')
units_merged <- readOGR('./data/Minnesota_bird_data/MNBBA_Homogenization_Data_9_22_2015/Units Spatial Data/units_merged.shp', 'units_merged')
mnn_bba_points <- readOGR('./data/Minnesota_bird_data/MNBBA_Homogenization_Data_9_22_2015/Bird Data/MNBBA_Surveys_DominantHabitat_Final.shp', 'MNBBA_Surveys_DominantHabitat_Final')

# For the purpose of determining how many forested BBA points there are per unit,
# we read in and reclassify the forest status raster so that forested pixels have 
# a value of 1. We then extract the forest cover values to the BBA point data, 
# and use this later to count the number of BBA points in forested areas per 
# unit.
reclass_matrix <- matrix(c(1, 4, 1), ncol = 3, byrow = TRUE)
forest_cover <- reclassify(forest_status, reclass_matrix)
forest_points <- extract(forest_cover, mnn_bba_points)
forest_points[is.na(forest_points)] <- 0
mnn_bba_points@data$forest <- forest_points

# We continue by calculating how many unique BBA points fall within each unit. 
# It seems like some units have BBA points that have been counted in multiple 
# years. For this, we use the 'block_poin' column in the BBA data.

# Select unique points based on the 'block_poin' column
mnn_bba_points_unique <- mnn_bba_points[which(!duplicated(mnn_bba_points@data$block_poin)), ]

# Calculate the number of unique BBA points within each unit
units_points <- over(mnn_bba_points_unique, units_merged)
units_points_count <- data.frame(table(units_points$unit))
colnames(units_points_count) <- c('unit', 'point_freq')
units_points_count$unit <- as.numeric(as.character(units_points_count$unit))
units_points_freq <- left_join(units_merged@data, units_points_count)
units_points_freq$point_freq[is.na(units_points_freq$point_freq)] <- 0

# Summarise how many unique BBA points units have
point_freq <- as.data.frame(table(units_points_freq$point_freq))
colnames(point_freq) <- c('points per unit', 'freq')
print.data.frame(point_freq)
hist(units_points_freq$point_freq, breaks = 18, xlim = c(0, 20), xlab = 'Number 
  of BBA points', main = 'BBA points per unit')

# We then calculate the number of BBA points in forested areas per unit.
# Select only BBA points in forested pixels
mnn_bba_points_forest <- mnn_bba_points_unique[mnn_bba_points_unique@data$forest == 1, ]

# Calculate the number of BBA points in forested pixels within each unit
units_forest_points <- over(mnn_bba_points_forest, units_merged)
units_forest_points_count <- data.frame(table(units_forest_points$unit))
colnames(units_forest_points_count) <- c('unit', 'forest_point_freq')
units_forest_points_count$unit <- as.numeric(as.character(units_forest_points_count$unit))
units_merged_forest_freq <- left_join(units_merged@data, units_forest_points_count)
units_merged_forest_freq$forest_point_freq[is.na(units_merged_forest_freq$forest_point_freq)] <- 0

# Summarise how many unique BBA points in forest pixels units have
forest_point_freq <- as.data.frame(table(units_merged_forest_freq$forest_point_freq))
colnames(forest_point_freq) <- c('forested points per unit', 'freq')
print.data.frame(forest_point_freq)
hist(units_merged_forest_freq$forest_point_freq, breaks = 15, xlim = c(0, 15), xlab = 'Number 
  of forested BBA points', main = 'Forested BBA points per unit')

# We can then choose units with at leas two BBA points in forested pixels
units_merged_subset <- units_merged_forest_freq[units_merged_forest_freq$forest_point_freq >= 2, ]
dim(units_merged_subset)
hist(units_merged_subset$forest_point_freq)