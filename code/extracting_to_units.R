library(raster)
library(rgdal)

# Load data
forest_status <- raster('./data/forest_status.tif')
land_fire <- raster('./data/landfire.tif')
land_fire_3 <- raster('./data/landfire3.tif')
ave_annual_precip <- raster('./data/aveannprecip.tif')
ave_annual_temp <- raster('./data/aveannualtemp.tif')
census_2010 <- raster('./data/census2010.tif')
forest_cover <- raster('./data/forest_cover.tif')
raster_list <- list(forest_status, land_fire, land_fire_3, ave_annual_precip, 
  ave_annual_temp, census_2010, forest_cover)
raster_stack <- stack(raster_list)

units_mnn <- readOGR('./data/Minnesota_bird_data/MNBBA_Homogenization_Data_9_22_2015/Units Spatial Data/Units.shp', 'Units')
roads_mnn <- readOGR('./data/Minnesota_bird_data/MNBBA_Homogenization_GISFiles_8_2015/Roads/Roads.shp', 'Roads')

units_values_mean <- extract(raster_stack, units_mnn, fun = mean, na.rm = T)
units_mnn@data <- cbind(units_mnn@data, units_values_mean)


# Print out attributes of rasters in raster stack
raster_stack <- stack('./data/raster_stack.tif')
#land_fire_3@data@attributes
class(raster_stack@layers)
lapply(raster_stack@layers, summary)

raster_names <- c('forest_status_2', 'land_fire_3', 'forest_status', 'land_fire', 
'ave_annual_precip', 'ave_annual_temp_2', 'census_2010', 'forest_cover')

names(raster_stack) <- raster_names

