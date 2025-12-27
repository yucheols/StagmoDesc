# plot future predictions == first transform to a dataframe and then plot == to enable double facetting
# convert to df
futures_df <- as.data.frame(futures, xy = T, na.rm = F) %>%
  pivot_longer(cols = -c(x, y),
               names_to = 'layer',
               values_to = 'value') %>%
  tidyr::extract(layer,
                 into = c('prefix', 'period', 'ssp'),
                 regex = '^(.*)_(\\d{4}_\\d{4})_(\\d+)$',
                 remove = F) %>%
  mutate(period = factor(period, levels = c('2041_2070', '2071_2100')),
         ssp = factor(ssp, levels = c('370', '585'),
                      labels = c('SSP 370', 'SSP 585')))

# recode periods
futures_df$period <- recode_factor(futures_df$period,
                                   '2041_2070' = '2041-2070',
                                   '2071_2100' = '2071-2100')

# plot
ggplot(futures_df) +
  geom_raster(aes(x = x, y = y, fill = value)) +
  coord_equal(expand = F) +
  facet_wrap(period ~ ssp, ncol = 2, nrow = 2)