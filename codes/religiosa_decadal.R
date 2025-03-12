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

##### load a global elevation raster
elev <- rast('E:/env layers/elev_worldclim/wc2.1_30s_elev.tif')
plot(elev)

##### load raw European occurrence points with the year column
r.occs_eu <- read.csv('data/occs/raw/religiosa_europe_raw.csv')
r.occs_eu$X <- NULL
head(r.occs_eu)

unique(r.occs_eu$year)

# create a decade column
r.occs_eu_dec <- r.occs_eu %>% na.omit() %>% mutate(decade = floor(year / 10 ) * 10)
head(r.occs_eu_dec)
unique(r.occs_eu_dec$decade)

# split data by decadal interval
r_dec_list <- r.occs_eu_dec %>%
  group_by(decade) %>%
  group_split()

print(r_dec_list)

# check the number of rows per decadal interval == only the last 5 intervals have a sufficient number of occurrence points
for (i in 1:length(r_dec_list)) {
  print(nrow(r_dec_list[[i]]))
}

# bind the last 5 intervals
r_dec_keep <- bind_rows(r_dec_list[10:14]) %>% as.data.frame()
head(r_dec_keep)

# convert to sf object
r_dec_keep_sf <- st_as_sf(r_dec_keep, coords = c('long', 'lat'), crs = 4326)

##### visualize these ranges
# mask the map to the area of interest
elev <- crop(elev, ext(r_dec_keep_sf))
plot(elev)

# plot == W 2000 & H 400
ggplot() +
  geom_spatraster(data = elev) +
  geom_sf(data = r_dec_keep_sf, shape = 21, fill = '#6495ED', stroke = 1) +
  facet_grid(~ decade) +
  scale_fill_wiki_c(na.value = NA) +
  labs(fill = 'Elevation (m)') +
  coord_sf(expand = F) +
  theme_bw() +
  theme(strip.text = element_text(size = 14, face = 'bold'),
        legend.title = element_text(size = 14, face = 'bold'),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 12))
 