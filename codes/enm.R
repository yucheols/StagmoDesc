######  run niche models for Stagmomantis sp.

# clean working environment
rm(list = ls(all.names = T))
gc()

# increase java heap space
options(java.parameters = "-Xmx16g")

# load packages
library(ENMeval)
library(ntbox)
library(terra)
library(dplyr)

# alter terra options to prevent std::bad_alloc error
terraOptions(memfrac = 0.5)
terraOptions(todisk = T)


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
# run models
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
                         raster.preds = F,
                         updateProgress = T)

# save model object
saveRDS(test_enms, 'output/models/test_enms.rds')

# load saved model
test_enms <- readRDS('output/models/test_enms.rds')


#####  select optimal model
# get results
test_res <- eval.results(test_enms)
print(test_res)

# select
(opt_mod <- test_res %>% dplyr::filter(or.10p.avg <= 0.1) %>% dplyr::filter(auc.diff.avg == min(auc.diff.avg)) %>%
  dplyr::filter(auc.val.avg == max(auc.val.avg)))


#####  predict under current clim
# make prediction
pred_cur <- predicts::predict(object = eval.models(test_enms)[[opt_mod$tune.args]], clim)
print(pred_cur)
plot(pred_cur)

# export prediction layer
writeRaster(pred_cur, 'output/predictions/current.tif', overwrite = T)


#####  predict under LGM & MH climate
# load LGM layers
lgm <- rast(list.files(path = 'D:/env layers/chelsa_LGM_v1_2B_r30s/30sec', pattern = '.tif$', full.names = T))
lgm <- lgm[[c('bio_1', 'bio_2', 'bio_3', 'bio_6', 'bio_8', 'bio_14', 'bio_15', 'bio_18')]]
lgm <- crop(lgm, ext(clim))

names(lgm) = c('bio01', 'bio02', 'bio03', 'bio06', 'bio08', 'bio14', 'bio15', 'bio18')
lgm[[c('bio01', 'bio02', 'bio03', 'bio06', 'bio08')]] <- lgm[[c('bio01', 'bio02', 'bio03', 'bio06', 'bio08')]] / 10

print(lgm)
plot(lgm[[1]])

# load mid Holocene layers
mh <- rast(list.files(path = 'D:/env layers/MH_v1_2_5m', pattern = '.tif$', full.names = T))
mh <- mh[[c('bio_1', 'bio_2', 'bio_3', 'bio_6', 'bio_8', 'bio_14', 'bio_15', 'bio_18')]]
mh <- crop(mh, ext(clim))
mh <- disagg(mh, fact = 5, method = 'bilinear')

names(mh) = c('bio01', 'bio02', 'bio03', 'bio06', 'bio08', 'bio14', 'bio15', 'bio18')
mh[[c('bio01', 'bio02', 'bio03', 'bio06', 'bio08')]] <- mh[[c('bio01', 'bio02', 'bio03', 'bio06', 'bio08')]] / 10

print(mh)
plot(mh[[1]])

# make prediction to LGM climate
lgm_pred <- predicts::predict(object = eval.models(test_enms)[[opt_mod$tune.args]], lgm)
plot(lgm_pred)

# make prediction to MH climate
mh_pred <- predicts::predict(object = eval.models(test_enms)[[opt_mod$tune.args]], mh)
plot(mh_pred)

# export prediction layers
writeRaster(lgm_pred, 'output/predictions/lgm_pred.tif', overwrite = T)
writeRaster(mh_pred, 'output/predictions/mh_pred.tif', overwrite = T)


#####  conduct mess
# run
lgm_mess <- ntb_mess(M_stack = raster::stack(clim), G_stack = raster::stack(lgm))
mh_mess <- ntb_mess(M_stack = raster::stack(clim), G_stack = raster::stack(mh))

plot(lgm_mess)
plot(mh_mess)

# export mess layers
writeRaster(lgm_mess, 'output/mess/lgm_mess.tif', overwrite = T)
writeRaster(mh_mess, 'output/mess/mh_mess.tif', overwrite = T)

