######  prep data for niche modeling

# clean working environment
rm(list = ls(all.names = T))
gc()

# load packages
library(ClimDatDownloadR)
library(ENMwrap)
library(ntbox)
library(SDMtune)
library(terra)
library(sf)
library(dplyr)

# set random seed
set.seed(1111)

# set timeout option for data download
options(timeout = 3600)


####  download current climate data == CHELSA v2.1
# defint clipping extent
ext <- c(-124.0834724458500204, -102.2168058666500201, 21.0831940001500016, 39.4998605931500038)

# get data
#Chelsa.Clim.download(save.location = 'data/envs/', parameter = 'bio', bio.var = c(1:19), version.var = '2.1', clipping = T, clip.extent = ext, save.download.table = T)

# check
clim <- rast(list.files(path = 'data/envs/bio/ChelsaV2.1Climatologies/clipped_2025-11-22_13-48-14/', pattern = '.tif', full.names = T))
print(clim)

# mask to boundary
na_shp <- st_read('data/shp/NAmerica_shp/na_shp.shp')
clim <- mask(clim, na_shp)
plot(clim[[1]])

# fix names
names(clim) = c('bio01','bio02','bio03','bio04','bio05','bio06','bio07','bio08','bio09','bio10','bio11','bio12','bio13','bio14','bio15','bio16','bio17','bio18','bio19')
print(clim)

# export masked
for (i in 1:nlyr(clim)) {
  writeRaster(clim[[i]], paste0('data/envs/bio/masked/', names(clim)[i], '.tif'))
}

#### download future data == CHELSA CMIP6
# download data
#Chelsa.CMIP_6.download(save.location = 'data/envs/future/', parameter = 'bio', bio.var = c(1:19), emission.scenario.var = c('ssp370', 'ssp585'),
#                       time.interval.var = c('2041-2070', '2071-2100'), model.var = 'mri-esm2-0', clipping = T, clip.extent = ext)

# load downloaded data == all scenarions are lumped together
futures <- rast(list.files(path = 'data/envs/future/bio/ChelsaCMIP6Climatologies/', pattern = '.tif$', full.names = T))
names(futures)
plot(futures[[1]])

# clip and mask future layers
futures <- crop(futures, ext)
futures <- mask(futures, na_shp)
plot(futures[[1]])

# separate out 2041-2070 and 2071-2100
futures_2041_2070 <- futures[[grepl('2041-2070', names(futures))]]
print(futures_2041_2070)

futures_2071_2100 <- futures[[grepl('2071-2100', names(futures))]]
print(futures_2071_2100)

# separate out ssp370 and ssp585 for each time interval
futures_2041_2070_370 <- futures_2041_2070[[grepl('ssp370', names(futures_2041_2070))]]
print(futures_2041_2070_370)

futures_2041_2070_585 <- futures_2041_2070[[grepl('ssp585', names(futures_2041_2070))]]
print(futures_2041_2070_585)

futures_2071_2100_370 <- futures_2071_2100[[grepl('ssp370', names(futures_2071_2100))]]
print(futures_2071_2100_370)

futures_2071_2100_585 <- futures_2071_2100[[grepl('ssp585', names(futures_2071_2100))]]
print(futures_2071_2100_585)

# change names to bio01, bio02, etc
names(futures_2041_2070_370) <- sprintf('bio%02d', 1:nlyr(futures_2041_2070_370))
names(futures_2041_2070_585) <- sprintf('bio%02d', 1:nlyr(futures_2041_2070_585))

names(futures_2071_2100_370) <- sprintf('bio%02d', 1:nlyr(futures_2071_2100_370))
names(futures_2071_2100_585) <- sprintf('bio%02d', 1:nlyr(futures_2071_2100_585))

# export processed future layers
# 2041-2070 ssp370
for (i in 1:nlyr(futures_2041_2070_370)) {
  writeRaster(futures_2041_2070_370[[i]], paste0('data/envs/future/bio/mri-esm2/2041_2070/ssp370/', names(futures_2041_2070_370)[i], '.tif'), overwrite = T)
}

# 2041-2070 ssp585
for (i in 1:nlyr(futures_2041_2070_585)) {
  writeRaster(futures_2041_2070_585[[i]], paste0('data/envs/future/bio/mri-esm2/2041_2070/ssp585/', names(futures_2041_2070_585)[i], '.tif'), overwrite = T)
}

# 2071-2100 ssp370
for (i in 1:nlyr(futures_2071_2100_370)) {
  writeRaster(futures_2071_2100_370[[i]], paste0('data/envs/future/bio/mri-esm2/2071_2100/ssp370/', names(futures_2071_2100_370)[i], '.tif'), overwrite = T)
}

# 2071-2100 ssp585
for (i in 1:nlyr(futures_2071_2100_585)) {
  writeRaster(futures_2071_2100_585[[i]], paste0('data/envs/future/bio/mri-esm2/2071_2100/ssp585/', names(futures_2071_2100_585)[i], '.tif'), overwrite = T)
}


####  occurrence thinning
# load occurrence
stagmo <- read.csv('data/occs/StagmoSDOccurenceData.csv')
head(stagmo)

# thin data
stagmo <- thinData(coords = stagmo[, c(3,4)], env = clim, x = 'longitude', y = 'latitude', verbose = T, progress = T)
nrow(stagmo)
head(stagmo)

# change column names
colnames(stagmo) = c('long', 'lat')

# export thinned
write.csv(stagmo, 'data/occs/StagmoSDOccurenceData_thinned.csv')

# visualize on the map
plot(clim[[1]])
points(stagmo)


####  sample background points
# draw 200km buffer around the points
buff <- buff_maker(occs_list = list(stagmo), envs = raster(clim[[1]]), buff_dist = 200000)

# sample bg
bg <- bg_sampler(envs = raster(clim[[1]]), n = 10000, occs_list = list(stagmo), buffer_list = buff, excludep = T, method = 'buffer')
head(bg[[1]])

# plot
plot(clim[[1]])
points(stagmo, col = 'blue')
points(bg[[1]], col = 'yellow')

# export
write.csv(bg[[1]], 'data/bg/stagmo_bg.csv')


#####  select variables
# extract env values
clim_val <- extract(clim, bg) %>% as.data.frame()
head(clim_val)

# make correlation matrix
cor_mat <- cor(clim_val[, c(-1)])
print(cor_mat)

# run correlation finder
correlation_finder(cor_mat = cor_mat, threshold = 0.7, verbose = T)

######  selected == bio01 bio02 bio03 bio06 bio08 bio14 bio15 bio18 
