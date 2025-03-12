#####  decadal range visualization for M. religiosa

# clean working environment
rm(list = ls(all.names = T))
gc()

# load packages
library(terra)
library(sf)
library(tidyterra)
library(dplyr)
library(ggplot2)
library(ggpubr)

##### load a global elevation raster
elev <- rast('E:/env layers/elev_worldclim/wc2.1_30s_elev.tif')
plot(elev)

##### load raw European occurrence points with the year column
r.occs_eu <- read.csv('data/occs/raw/religiosa_europe_raw.csv')
r.occs_eu$X <- NULL
head(r.occs_eu)

unique(r.occs_eu$year)

# create a decade column
r.occs_eu_dec <- r.occs_eu %>% na.omit() %>% dplyr::mutate(decade = floor(year / 10 ) * 10)
head(r.occs_eu_dec)
unique(r.occs_eu_dec$decade)

# split data by decadal interval
r_dec_eu_list <- r.occs_eu_dec %>%
  group_by(decade) %>%
  group_split()

print(r_dec_eu_list)

# check the number of rows per decadal interval == only the last 5 intervals have a sufficient number of occurrence points
for (i in 1:length(r_dec_eu_list)) {
  print(nrow(r_dec_eu_list[[i]]))
}

# bind the last 5 intervals
r_dec_eu_keep <- bind_rows(r_dec_eu_list[10:14]) %>% as.data.frame()
head(r_dec_eu_keep)

# recode the decade names for better visualization
r_dec_eu_keep$decade <- as.character(r_dec_eu_keep$decade)
r_dec_eu_keep$decade <- r_dec_eu_keep$decade %>% recode_factor('1980' = '1980s',
                                                               '1990' = '1990s',
                                                               '2000' = '2000s',
                                                               '2010' = '2010s',
                                                               '2020' = '2020s')

# convert to sf object
r_dec_eu_keep_sf <- st_as_sf(r_dec_eu_keep, coords = c('long', 'lat'), crs = 4326)

##### visualize decadal ranges in Europe
# mask the map to the area of interest
elev_eu <- crop(elev, ext(r_dec_eu_keep_sf))
plot(elev_eu)

# plot == W 2000 & H 400
r_eu_range <- ggplot() +
  geom_spatraster(data = elev_eu) +
  geom_sf(data = r_dec_eu_keep_sf, shape = 21, fill = '#6495ED', stroke = 1) +
  facet_grid(~ decade) +
  scale_fill_wiki_c(na.value = NA) +
  labs(title = '(A) Europe', fill = 'Elevation (m)') +
  coord_sf(expand = F) +
  theme_bw() +
  theme(plot.title = element_text(size = 18, face = 'bold'), 
        strip.text = element_text(size = 14, face = 'bold'),
        legend.title = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 25, hjust = 1))


##### load raw North American occurrence points with the year column
r.occs_na <- read.csv('data/occs/raw/religiosa_northamerica_raw.csv')
r.occs_na$X <- NULL
head(r.occs_na)

# create a decade column
r.occs_na_dec <- r.occs_na %>% na.omit() %>% dplyr::mutate(decade = floor(year / 10) * 10)
head(r.occs_na_dec)
unique(r.occs_na_dec$decade)

# split data by decadal interval
r_dec_na_list <- r.occs_na_dec %>%
  group_by(decade) %>%
  group_split()

print(r_dec_na_list)

# check the number of rows per decadal interval == include all time bins in this case
for (i in 1:length(r_dec_na_list)) {
  print(nrow(r_dec_na_list[[i]]))
}

# recode the decade names for better visualization
r.occs_na_dec$decade <- r.occs_na_dec$decade %>% recode_factor('1970' = '1970s',
                                                               '1980' = '1980s',
                                                               '1990' = '1990s',
                                                               '2000' = '2000s',
                                                               '2010' = '2010s',
                                                               '2020' = '2020s')

# convert to sf object
r_dec_na_sf <- st_as_sf(r.occs_na_dec, coords = c('long', 'lat'), crs = 4326)

##### visualize decadal ranges in North America
# mask the map to the area of interest
elev_na <- crop(elev, ext(r_dec_na_sf))
plot(elev_na)

# plot == W 2000 & H 400
r_na_range <- ggplot() +
  geom_spatraster(data = elev_na) +
  geom_sf(data = r_dec_na_sf, shape = 21, fill = '#6495ED', stroke = 1) +
  facet_grid(~ decade) +
  scale_fill_wiki_c(na.value = NA) +
  labs(title = '(B) North America', fill = 'Elevation (m)') +
  coord_sf(expand = F) +
  theme_bw() +
  theme(plot.title = element_text(size = 18, face = 'bold'),
        strip.text = element_text(size = 14, face = 'bold'),
        legend.title = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 25, hjust = 1))


#### combine == W2000 H700
ggarrange(r_eu_range, r_na_range, ncol = 1, nrow = 2)

# consider saving with ggsave for publication

