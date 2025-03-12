#####  decadal range visualization for I. oratoria

# clean working environment
rm(list = ls(all.names = T))
gc()

# load packages
library(terra)
library(sf)
library(tidyterra)
library(dplyr)

##### load a global elevation raster
elev <- rast('E:/env layers/elev_worldclim/wc2.1_30s_elev.tif')
plot(elev)

##### load raw European occurrence points with the year column
o.occs_eu <- read.csv('data/occs/raw/oratoria_europe_raw.csv')
o.occs_eu$X <- NULL
head(o.occs_eu)

unique(o.occs_eu$year)

# create a decade column
o.occs_eu_dec <- o.occs_eu %>% na.omit() %>% mutate(decade = floor(year / 10) * 10)
head(o.occs_eu_dec)
unique(o.occs_eu_dec$decade)

# split data by decadal interval
o_dec_eu_list <- o.occs_eu_dec %>%
  group_by(decade) %>%
  group_split()

print(o_dec_eu_list)

# check the number of rows per decadal interval 
for (i in 1:length(o_dec_eu_list)) {
  print(nrow(o_dec_eu_list[[i]]))
}

# recode the decade names for better visualization
o.occs_eu_dec$decade <- as.character(o.occs_eu_dec$decade)
o.occs_eu_dec$decade <- o.occs_eu_dec$decade %>% recode_factor('1980' = '1980s',
                                                               '1990' = '1990s',
                                                               '2000' = '2000s',
                                                               '2010' = '2010s',
                                                               '2020' = '2020s')

# convert to sf object
o_dec_eu_sf <- st_as_sf(o.occs_eu_dec, coords = c('long', 'lat'), crs = 4326)

##### visualize decadal ranges in Europe
# mask the map to the area of interest
elev_eu <- crop(elev, ext(o_dec_eu_sf))
plot(elev_eu)

# plot == W 2000 & H 400
o_eu_range <- ggplot() +
  geom_spatraster(data = elev_eu) +
  geom_sf(data = o_dec_eu_sf, shape = 21, fill = '#FF8C00', stroke = 1) +
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
o.occs_na <- read.csv('data/occs/raw/oratoria_northamerica_raw.csv')
o.occs_na$X <- NULL
head(o.occs_na)

unique(o.occs_na$year)

# create a decade column
o.occs_na_dec <- o.occs_na %>% na.omit() %>% mutate(decade = floor(year / 10) * 10)
head(o.occs_na_dec)
unique(o.occs_na_dec$decade)

# split data by decadal interval
o_dec_na_list <- o.occs_na_dec %>%
  group_by(decade) %>%
  group_split()

# check the number of rows per decadal interval == include all time bins in this case
for (i in 1:length(o_dec_na_list)) {
  print(nrow(o_dec_na_list[[i]]))
}

# recode the decade names for better visualization
o.occs_na_dec$decade <- as.character(o.occs_na_dec$decade)
o.occs_na_dec$decade <- o.occs_na_dec$decade %>% recode_factor('2000' = '2000s',
                                                               '2010' = '2010s',
                                                               '2020' = '2020s')

# convert to sf object
o_dec_na_sf <- st_as_sf(o.occs_na_dec, coords = c('long', 'lat'), crs = 4326)

##### visualize decadal ranges in North America
# mask the map to the area of interest
elev_na <- crop(elev, ext(o_dec_na_sf))
plot(elev_na)

# plot == W 2000 & H 400
o_na_range <- ggplot() +
  geom_spatraster(data = elev_na) +
  geom_sf(data = o_dec_na_sf, shape = 21, fill = '#FF8C00', stroke = 1) + 
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
ggarrange(o_eu_range, o_na_range, ncol = 1, nrow = 2)

# consider saving with ggsave for publication
