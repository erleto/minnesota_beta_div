# NOT NEEDED! ALL INFO ALREADY IN UNIT DATA!

# Load necessary libraries
library(raster)

#### Read raster files ####
forest_status <- raster('./data/Minnesota_bird_data/MNBBA_Homogenization_GISFiles_8_2015/Forest_Status/forstat_rast.tif')
forest_status_2 <- raster('./data/Minnesota_bird_data/MNBBA_Homogenization_Data_9_22_2015/Forest Status/forstat_rast2.tif')
land_fire <- raster('./data/Minnesota_bird_data/MNBBA_Homogenization_GISFiles_8_2015/Landcover/landfire.tif')
land_fire_3 <- raster('./data/Minnesota_bird_data/MNBBA_Homogenization_Data_9_22_2015/Landfire/landfire3.tif')
ave_annual_precip <- raster('./data//Minnesota_bird_data/MNBBA_Homogenization_GISFiles_8_2015/PRISM/aveannprecip.tif')
ave_annual_temp <- raster('./data/Minnesota_bird_data/MNBBA_Homogenization_GISFiles_8_2015/PRISM/aveannualtemp2.tif')
census_2010 <- raster('./data/Minnesota_bird_data/MNBBA_Homogenization_GISFiles_8_2015/US_Census/census2010.tif')
#ave_annual_temp <- raster('./data/Minnesota_bird_data/MNBBA_Homogenization_GISFiles_8_2015/PRISM/aveannualtemp.tif')

raster_list <- list(forest_status, land_fire, land_fire_3, ave_annual_precip, ave_annual_temp, census_2010)

# Print out various properties of objects within the list
length(raster_list)
lapply(raster_list, crs)
lapply(raster_list, res)
lapply(raster_list, extent)

# Calculate maximum extent, and use this as the extent for all raster layers 
# in order to be able to stack them
extent_list <- lapply(raster_list, extent)
extent_matrix <- lapply(extent_list, as.matrix)
extent_matrix <- matrix(unlist(extent_matrix), ncol = length(extent_matrix))
rownames(extent_matrix) <- c("xmin", "ymin", "xmax", "ymax")
extent_best <- extent(min(extent_matrix[1, ]), max(extent_matrix[3, ]),
  min(extent_matrix[2, ]), max(extent_matrix[4, ]))

# Create reference raster from the maximum extent, with the same resolution and
# CRS as the original raster layers
reference_raster <- raster(extent_best, res = 30, crs = crs('+proj=utm +zone=15 
  +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'))

# Resample rasters with respect to the reference raster
forest_status <- resample(forest_status, reference_raster, method = 'ngb')
land_fire <- resample(land_fire, reference_raster, method = 'ngb')
land_fire_3 <- resample(land_fire_3, reference_raster, method = 'ngb')
ave_annual_precip <- resample(ave_annual_precip, reference_raster, method = 'ngb')
ave_annual_temp <- resample(ave_annual_temp, reference_raster, method = 'ngb')
census_2010 <- resample(census_2010, reference_raster, method = 'ngb')

# Create forest raster
reclass_matrix <- matrix(c(1, 4, 1), ncol=3, byrow=TRUE)
forest_cover <- reclassify(forest_status, reclass_matrix)

# Standardise layer names
names(forest_status)
names(forest_cover) <- 'forest_cover'
names(ave_annual_temp)

# Stack all rasters and save on disk
raster_list <- list(forest_status, land_fire, land_fire_3, ave_annual_precip, 
  ave_annual_temp, census_2010, forest_cover)
raster_stack <- stack(raster_list)
writeRaster(raster_stack, filename = paste0('./data/', names(raster_stack)), bylayer = T, format="GTiff", overwrite = T)
