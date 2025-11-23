######  run niche models

# clean working environment
rm(list = ls(all.names = T))
gc()

# load packages
library(ENMeval)
library(terra)
library(sf)
library(dplyr)


#####  load data
# clim
clim <- rast(list.files(path = 'data/envs/bio/masked/', pattern = '.tif$', full.names = T))
clim <- clim[[c('bio01','bio02','bio03','bio06','bio08','bio14','bio15','bio18')]]
print(clim)
plot(clim[[1]])

# occurrences
occs <- read.csv('data/occs/StagmoSDOccurenceData_thinned.csv') %>% dplyr::select(-1)
head(occs)

# bg
bg <- read.csv('data/bg/stagmo_bg.csv') %>% dplyr::select(-1)
head(bg)


#####  run models
test_enms <- ENMevaluate(taxon.name = 'Stagmomantis_sp',
                         occs = occs,
                         bg = bg,
                         envs = clim,
                         tune.args = list(fc = c('L', 'Q', 'H', 'P', 'LQ', 'LP', 'QH', 'QP', 'HP', 'LQH', 'LQP', 'LQHP', 'LQHPT'),
                                          rm = seq(0.5, 5, by = 0.5)),
                         partitions = 'checkerboard',
                         partition.settings = list(aggregation.factor = c(5,5)),
                         algorithm = 'maxent.jar',
                         doClamp = T,
                         raster.preds = T,
                         updateProgress = T)


#####  select optimal model
