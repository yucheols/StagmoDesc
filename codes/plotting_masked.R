######  plot maps masked to a specific level 3 ecoregion

# clean working environment
rm(list = ls(all.names = T))
gc()

# load packages
library(ENMwrap)
library(tidyterra)
library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(ggpubr)


#####  load ecoregion
ecor <- st_read('data/shp/terr_ecoregions_v2_level_iii_shapefile/NA_TerrEcoregions_III/data/NA_Terrestrial_Ecoregions_v2_level3.shp') %>% 
  dplyr::filter(LEVEL3 == '11.1.1') %>% st_transform(crs = 4326)

plot(st_geometry(ecor))

#####  plot predictions
# load current and historical pred
current <- rast('output/predictions/current.tif') %>% crop(ext(-119.0, -116.5, 32.5, 34.5))
lgm <- rast('output/predictions/lgm_pred.tif') %>% crop(ext(-119.0, -116.5, 32.5, 34.5))
mh <- rast('output/predictions/mh_pred.tif') %>% crop(ext(-119.0, -116.5, 32.5, 34.5))

# plot current
(current_plot <- ggplot() +
    geom_spatraster(data = current) +
    coord_sf(crs = st_crs(4326), default_crs = st_crs(4326),
             xlim = c(-119.0, -116.5), ylim = c(32.5, 34.5), expand = F) +
    scale_x_continuous(breaks = c(-118.5, -118.0, -117.5, -117.0)) +
    scale_y_continuous(breaks = c(33.0, 33.5, 34.0)) +
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
#(mh_plot <- ggplot() +
#    geom_spatraster(data = mh) +
#    coord_sf(expand = F) +
#    scale_fill_grass_c(palette = 'inferno',
#                       name = 'Suitability',
#                       breaks = c(0.1, 0.9),
#                       labels = c('Low', 'High')) +
#    labs(title = '(B) Mid-Holocene') +
#    theme_minimal() +
#    theme(panel.border = element_rect(fill = NA),
#         axis.ticks = element_line(color = 'black'),
#          axis.ticks.length = unit(0.2, 'cm'),
#          axis.text.y = element_text(angle = 90, hjust = 0.5),
#          axis.text = element_text(size = 14),
#          plot.title = element_text(size = 20, face = 'bold'),
#          legend.title = element_text(size = 16),
#          legend.text = element_text(size = 14),
#          plot.margin = margin(2, 2, 2, 2)))

# plot lgm
(lgm_plot <- ggplot() +
    geom_spatraster(data = lgm) +
    coord_sf(crs = st_crs(4326), default_crs = st_crs(4326),
             xlim = c(-119.0, -116.5), ylim = c(32.5, 34.5), expand = F) +
    scale_x_continuous(breaks = c(-118.5, -118.0, -117.5, -117.0)) +
    scale_y_continuous(breaks = c(33.0, 33.5, 34.0)) +
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
(preds_comb <- ggarrange(current_plot, lgm_plot, 
                         ncol = 3, nrow = 1, align = 'hv',
                         common.legend = T,
                         legend = 'right',
                         widths = c(1, 1, 0.1)))

# export plots
ggsave('plots/preds_masked.png', width = 28, height = 10, dpi = 800, units = 'cm')


#####  plot future model outputs
# load future preds 
futures_2041_2070_370 <- rast('output/predictions/futures_2041_2070_370.tif') %>% crop(ext(-119.0, -116.5, 32.5, 34.5))
futures_2041_2070_585 <- rast('output/predictions/futures_2041_2070_585.tif') %>% crop(ext(-119.0, -116.5, 32.5, 34.5))
futures_2071_2100_370 <- rast('output/predictions/futures_2071_2100_370.tif') %>% crop(ext(-119.0, -116.5, 32.5, 34.5))
futures_2071_2100_585 <- rast('output/predictions/futures_2071_2100_585.tif') %>% crop(ext(-119.0, -116.5, 32.5, 34.5))

# combine between time periods
futures_2041_2070 <- c(futures_2041_2070_370, futures_2041_2070_585)
names(futures_2041_2070) = c('SSP 370', 'SSP 585')

futures_2071_2100 <- c(futures_2071_2100_370, futures_2071_2100_585)
names(futures_2071_2100) = c('SSP 370', 'SSP 585')

print(futures_2041_2070)
print(futures_2071_2100)

# plot 2041 - 2070
(plot_2041_2070 <- futures_2041_2070 %>% 
    ggplot() +
    geom_spatraster(data = futures_2041_2070) +
    facet_wrap(~ lyr) +
    coord_sf(crs = st_crs(4326), default_crs = st_crs(4326),
             xlim = c(-119.0, -116.5), ylim = c(32.5, 34.5), expand = F) +
    scale_x_continuous(breaks = c(-118.5, -118.0, -117.5, -117.0)) +
    scale_y_continuous(breaks = c(33.0, 33.5, 34.0)) +
    scale_fill_grass_c(palette = 'inferno',
                       name = 'Suitability',
                       breaks = c(0.1, 0.9),
                       labels = c('Low', 'High')) +
    labs(title = '(A) 2041 - 2071', fill = 'Suitability') +
    theme_bw() +
    theme(panel.border = element_rect(fill = NA),
          strip.text = element_text(size = 16),
          axis.ticks = element_line(color = 'black'),
          axis.ticks.length = unit(0.2, 'cm'),
          axis.text.y = element_text(angle = 90, hjust = 0.5),
          axis.text = element_text(size = 14),
          plot.title = element_text(size = 20, face = 'bold'),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 14),
          legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
          plot.margin = margin(2, 2, 2, 2)))

# plot 2071 - 2100
(plot_2071_2100 <- futures_2071_2100 %>% 
    ggplot() +
    geom_spatraster(data = futures_2071_2100) +
    facet_wrap(~ lyr) +
    coord_sf(crs = st_crs(4326), default_crs = st_crs(4326),
             xlim = c(-119.0, -116.5), ylim = c(32.5, 34.5), expand = F) +
    scale_x_continuous(breaks = c(-118.5, -118.0, -117.5, -117.0)) +
    scale_y_continuous(breaks = c(33.0, 33.5, 34.0)) +
    scale_fill_grass_c(palette = 'inferno',
                       name = 'Suitability',
                       breaks = c(0.1, 0.9),
                       labels = c('Low', 'High')) +
    labs(title = '(B) 2071 - 2100', fill = 'Suitability') +
    theme_bw() +
    theme(panel.border = element_rect(fill = NA),
          strip.text = element_text(size = 16),
          axis.ticks = element_line(color = 'black'),
          axis.ticks.length = unit(0.2, 'cm'),
          axis.text.y = element_text(angle = 90, hjust = 0.5),
          axis.text = element_text(size = 14),
          plot.title = element_text(size = 20, face = 'bold'),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 14),
          legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
          plot.margin = margin(2, 2, 2, 2)))

# arrange
(future_plots <- ggarrange(plot_2041_2070, plot_2071_2100, 
                           ncol = 1, nrow = 2, align = 'hv',
                           common.legend = T,
                           legend = 'right',
                           widths = c(1, 1, 0.1)))

# export plots
ggsave('plots/future_preds.png', width = 21, height = 20, dpi = 800, units = 'cm')

