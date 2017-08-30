library(rgdal)
library(maptools)
library(dplyr)
library(rgeos)

# First, we prepare the vector data by joining all unit-level data together

# Read the different unit-level shapefiles
units <- readOGR('./data/Minnesota_bird_data/MNBBA_Homogenization_Data_9_22_2015/Units Spatial Data/Units.shp', 'Units')
units_census <- readOGR('./data/Minnesota_bird_data/MNBBA_Homogenization_Data_9_22_2015/Units Spatial Data/Units_Census.shp', 'Units_Census')
units_eco_subsection <- readOGR('./data/Minnesota_bird_data/MNBBA_Homogenization_Data_9_22_2015/Units Spatial Data/Units_EcoSubsection.shp', 'Units_EcoSubsection')
units_forest_status <- readOGR('./data/Minnesota_bird_data/MNBBA_Homogenization_Data_9_22_2015/Units Spatial Data/Units_ForestStatus.shp', 'Units_ForestStatus')
units_land_fire <- readOGR('./data/Minnesota_bird_data/MNBBA_Homogenization_Data_9_22_2015/Units Spatial Data/Units_Landfire.shp', 'Units_Landfire')
units_prism <- readOGR('./data/Minnesota_bird_data/MNBBA_Homogenization_Data_9_22_2015/Units Spatial Data/Units_PRISM.shp', 'Units_PRISM')
units_road_density <- readOGR('./data/Minnesota_bird_data/MNBBA_Homogenization_Data_9_22_2015/Units Spatial Data/Units_RoadDensity.shp', 'Units_RoadDensity')

# Join attributes of unit- level shapefiles
units_merged <- units
units_merged@data <- inner_join(units_merged@data, units_eco_subsection@data, by = 'unit')
units_merged@data <- inner_join(units_merged@data, units_census@data, by = 'unit')
units_merged@data <- inner_join(units_merged@data, units_prism@data, by = 'unit')
units_merged@data <- inner_join(units_merged@data, units_forest_status@data, by = 'unit')
units_merged@data <- inner_join(units_merged@data, units_road_density@data, by = 'unit')
units_merged@data <- inner_join(units_merged@data, units_land_fire@data, by = 'unit')
units_merged@data$Dens_Minor <- as.numeric(units_merged@data$Dens_Minor)

# Save the joined data as a shapefile
writeOGR(units_merged, './data/Minnesota_bird_data/MNBBA_Homogenization_Data_9_22_2015/Units Spatial Data/units_merged.shp', 'units_merged', driver = 'ESRI Shapefile', overwrite_layer = T)

# Spatially join block and unit- level data-
#library(sp)
#library(maptools)
#head(blocks@data)
#blocks_units_merged <- over(blocks, units_merged)
#head(blocks_units_merged)
#blocks <- spCbind(blocks, blocks_units_merged

# Group blocks (township sub-blocks) by their group_id to form township blocks. 
# The unionSpatialPolygons function results in a spatialpolygons object, which 
# then needs to have data attached to it. In this case I aggregate the data from 
# the blocks data by group_id, and join it to the spatial data to yield a 
# spatialpolygondataframe.
#blocks <- readOGR('./data/Minnesota_bird_data/MNBBA_Homogenization_GISFiles_8_2015/Minnesota_Blocks/MNBBA_Blocks.shp', 'MNBBA_Blocks')
#township_blocks <- unionSpatialPolygons(blocks, blocks@data$group_id)
#township_blocks_df <- as.data.frame(blocks) %>% arrange(group_id) %>% 
#  group_by(group_id) %>% summarise(TOWN = first(TOWN), RDIR = first(RDIR), 
#    RANG = first(RANG), region = first(region), block_id = first(block_id), 
#    State = first(State))
#row.names(township_blocks_df) <- township_blocks_df$group_id
#township_blocks_df <- as.data.frame(township_blocks_df)
#township_blocks_spdf <- SpatialPolygonsDataFrame(township_blocks, township_blocks_df, match.ID = T)
#writeOGR(township_blocks_spdf, './data/Minnesota_bird_data/MNBBA_Homogenization_GISFiles_8_2015/Minnesota_Blocks/township_blocks.shp', 
#  'township_blocks', driver = 'ESRI Shapefile', overwrite_layer = T)