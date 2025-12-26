######  plotting

# clean working environment
rm(list = ls(all.names = T))
gc()

# load packages
library(ENMwrap)
library(tidyterra)
library(terra)
library(dplyr)
library(ggplot2)
library(ggpubr)


#####  plot predictions
# load current and historical preds
current <- rast('output/predictions/current.tif')
lgm <- rast('output/predictions/lgm_pred.tif')
mh <- rast('output/predictions/mh_pred.tif')

# plot current
(current_plot <- ggplot() +
    geom_spatraster(data = current) +
    coord_sf(expand = F) +
    scale_fill_grass_c(palette = 'inferno',
                       name = 'Suitability',
                       breaks = c(0.1, 0.9),
                       labels = c('Low', 'High')) +
    labs(title = '(A) Current') +
    theme_minimal() +
    theme(panel.border = element_rect(fill = NA),
          axis.ticks = element_line(color = 'black'),
          axis.ticks.length = unit(0.2, 'cm'),
          axis.text.y = element_text(angle = 90, hjust = 0.5),
          axis.text = element_text(size = 14),
          plot.title = element_text(size = 20, face = 'bold'),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 14),
          plot.margin = margin(2, 2, 2, 2)))

# plot mh
(mh_plot <- ggplot() +
    geom_spatraster(data = mh) +
    coord_sf(expand = F) +
    scale_fill_grass_c(palette = 'inferno',
                       name = 'Suitability',
                       breaks = c(0.1, 0.9),
                       labels = c('Low', 'High')) +
    labs(title = '(B) Mid-Holocene') +
    theme_minimal() +
    theme(panel.border = element_rect(fill = NA),
          axis.ticks = element_line(color = 'black'),
          axis.ticks.length = unit(0.2, 'cm'),
          axis.text.y = element_text(angle = 90, hjust = 0.5),
          axis.text = element_text(size = 14),
          plot.title = element_text(size = 20, face = 'bold'),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 14),
          plot.margin = margin(2, 2, 2, 2)))

# plot lgm
(lgm_plot <- ggplot() +
    geom_spatraster(data = lgm) +
    coord_sf(expand = F) +
    scale_fill_grass_c(palette = 'inferno',
                       name = 'Suitability',
                       breaks = c(0.1, 0.9),
                       labels = c('Low', 'High')) +
    labs(title = '(C) LGM', fill = 'Suitability') +
    theme_minimal() +
    theme(panel.border = element_rect(fill = NA),
          axis.ticks = element_line(color = 'black'),
          axis.ticks.length = unit(0.2, 'cm'),
          axis.text.y = element_text(angle = 90, hjust = 0.5),
          axis.text = element_text(size = 14),
          plot.title = element_text(size = 20, face = 'bold'),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 14),
          plot.margin = margin(2, 2, 2, 2)))


# arrange plots
(preds_comb <- ggarrange(current_plot, mh_plot, lgm_plot, 
                         ncol = 3, nrow = 1, align = 'hv',
                         common.legend = T,
                         legend = 'right',
                         widths = c(1, 1, 1)))

# export plots
ggsave('plots/preds.png', width = 28, height = 10, dpi = 800, units = 'cm')


#####  plot future model outputs
# load future preds 
futures_2041_2070_370 <- rast('output/predictions/futures_2041_2070_370.tif')
futures_2041_2070_585 <- rast('output/predictions/futures_2041_2070_585.tif')
futures_2071_2100_370 <- rast('output/predictions/futures_2071_2100_370.tif')
futures_2071_2100_585 <- rast('output/predictions/futures_2071_2100_585.tif')

# combine all 
futures <- c(futures_2041_2070_370, futures_2041_2070_585, futures_2071_2100_370, futures_2071_2100_585)
print(futures)

#





#####  get variable contributions
# load saved model
test_enms <- readRDS('output/models/test_enms.rds')

# get results
test_res <- eval.results(test_enms)
print(test_res)

# find optimal model
(opt_mod <- test_res %>% dplyr::filter(or.10p.avg <= 0.1) %>% dplyr::filter(auc.diff.avg == min(auc.diff.avg)) %>%
    dplyr::filter(auc.val.avg == max(auc.val.avg)))

# get var importance
var.imp <- eval.variable.importance(test_enms)[[opt_mod$tune.args]]
print(var.imp)


#####  plot response curves





#####  plot mess