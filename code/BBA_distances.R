# Points with a block_poin value that os duplicated. Only one per block_poin value!
aaa <- mnn_bba_points[duplicated(mnn_bba_points@data$block_poin), ]

# All points with duplicated block_poin value
mnn_bba_points_unique_block_poin <- mnn_bba_points[duplicated(mnn_bba_points@data$block_poin) | duplicated(mnn_bba_points@data$block_poin, fromLast = TRUE), ]
mnn_bba_points_unique_block_poin <- mnn_bba_points_unique_block_poin[order(mnn_bba_points_unique_block_poin@data$block_poin), ]
#bbb <- bbb[bbb@data$block_poin != 'T120R27a1', ]
# Make separate objects for each individual from each pair
mnn_bba_points_unique_block_poin@data$order <- rep(c(1, 2), length(mnn_bba_points_unique_block_poin) / 2)
mnn_bba_points_unique_block_poin_1 <- mnn_bba_points_unique_block_poin[mnn_bba_points_unique_block_poin@data$order == 1, ]
mnn_bba_points_unique_block_poin_2 <- mnn_bba_points_unique_block_poin[mnn_bba_points_unique_block_poin@data$order == 2, ]
# Calculate the distance between each pair
unique_block_poin_pair_distances <- pointDistance(mnn_bba_points_unique_block_poin_1, mnn_bba_points_unique_block_poin_2)
unique_block_poin_pair_distances <- data.frame(distance = unique_block_poin_pair_distances, block_poin = mnn_bba_points_unique_block_poin_1@data$block_poin)
#unique_block_poin_pair_distances <- sort(unique_block_poin_pair_distances)
min(unique_block_poin_pair_distances$distance)
max(unique_block_poin_pair_distances$distance)
mean(unique_block_poin_pair_distances$distance)
hist(unique_block_poin_pair_distances$distance, breaks = 40)
unique_block_poin_pair_distances_sort <- sort(unique_block_poin_pair_distances$distance)
unique_block_poin_pair_distances[which(unique_block_poin_pair_distances$distance == unique_block_poin_pair_distances_sort[length(unique_block_poin_pair_distances_sort) - 1], arr.ind = T), ]

#######

# Let's calculate the distance between each point with unique coordinates
mnn_bba_points_unique <- mnn_bba_points[which(!duplicated(mnn_bba_points@data[c('x', 'y')])), ]
point_dist <- pointDistance(mnn_bba_points_unique, lonlat = FALSE)
summary(as.vector(point_dist)) 
#point_dist <- dist(mnn_bba_points_unique@data[, 10:11])

point_dist <- as.data.frame(point_dist)
colnames(point_dist) <- mnn_bba_points_unique@data$ID
rownames(point_dist) <- mnn_bba_points_unique@data$ID

# Then we'll calculate the mean nearest neighbour distance between points. Leave 
# zeros out, since they are present in every row and column
point_dist_nn <- sapply(point_dist, FUN = function(x) {min(x[x > 0])})
point_dist_nn <- sort(point_dist_nn)
min(point_dist_nn)
max(point_dist_nn)
point_dist_mnn <- mean(point_dist_nn)

# Perhaps plotting a semivariogram can help use decide if we need a minimum 
# nearest neighbour distance
bird_points_id_wide <- dplyr::select(bird_data_whole, ID, common, Sum_AllBirds, x, y) %>% 
  dplyr::arrange(ID, common)  %>% spread(common, Sum_AllBirds) %>% replace(is.na(.), 0)
bird_points_id_wide$bird_sum  <- rowSums(bird_points_id_wide[4:length(bird_points_id_wide)], na.rm = TRUE)

library(geoR)
bird_point_variogram <- variog(coords = bird_points_id_wide[2:3], data = bird_points_id_wide[4:10])
bird_point_variogram <- variog(coords = bird_points_id_wide[2:3], data = bird_points_id_wide$bird_sum, option = 'cloud')
plot(bird_point_variogram)

library(gstat)
bird_point_variogram <- variogram(as.list(bird_points_id_wide$bird_sum), as.matrix(bird_points_id_wide[2:3]))
aaa <- gstat(id = "bird_sum", formula = bird_sum ~ x + y, location = ~ x + y, data = bird_points_id_wide)
bird_point_variogram <- variogram(aaa, width = ((100) / 15), cutoff = (100))
plot(bird_point_variogram)
########
median(point_dist_nn)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

point_dist_nn_classed <- cut(point_dist_nn, seq.int(0, round(max(point_dist_nn) + 10, digits = -1), 10))
getmode(point_dist_nn_classed)
which(table(point_dist_nn_classed) == max(table(point_dist_nn_classed)))

quantile(point_dist_nn, 0.01)

# Check the identities of the largest nearest neighbour distances
# Position from end. 0 is the last value, 1 is the second last value etc.
pos_from_end <- 0
point_dist_nn[length(point_dist_nn) - pos_from_end]

which(point_dist == point_dist_nn[length(point_dist_nn) - pos_from_end], arr.ind = T)

point_dist_unique <- sort(unique(unlist(point_dist)))
# We can then query the position of, e.g. the second smallest distance
which(point_dist == point_dist_unique[2], arr.ind = T)

hist(aaa, breaks = 100)

######
# Compare how many points are left when duplicate coordinates are removed, then 
# duplicate block_poin values, and vice versa
aaa <- mnn_bba_points[which(!duplicated(mnn_bba_points@data[c('x', 'y')])), ]
bbb <- aaa[which(!duplicated(aaa@data$block_poin)), ]

ccc <- mnn_bba_points[which(!duplicated(mnn_bba_points@data$block_poin)), ]
ddd <- ccc[which(!duplicated(ccc@data[c('x', 'y')])), ]

library(plyr)
#mnn_bba_points_unique_coord <- mnn_bba_points[duplicated(mnn_bba_points@data[c('x', 'y')]) | duplicated(mnn_bba_points@data[c('x', 'y')] , fromLast = TRUE), ]
x_y_counts <- count(mnn_bba_points@data, c('x', 'y'))
# We can see that the vast majority of points have uniqe coordinates, but quite 
# a few have one duplicate, and one is present three times!
table(x_y_counts$freq)

# What about when we check the block_poin values?
block_poin_counts <- count(mnn_bba_points@data, 'block_poin')
table(block_poin_counts$freq)

#mnn_bba_points[!duplicated(mnn_bba_points@data[c('x', 'y')]), ]
#mnn_bba_points
