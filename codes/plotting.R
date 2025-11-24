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
# load preds
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
          axis.text.y = element_text(angle = 90),
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
          axis.text.y = element_text(angle = 90),
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
          axis.text.y = element_text(angle = 90),
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


#####  plot response curves



#####  plot mess