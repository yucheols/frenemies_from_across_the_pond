#####  Ecological niche modeling and niche shift analyses in two invasive mantises == Mantis religiosa, Iris oratoria
#####  process input data for analyses == these include occurrences, environmental data, etc.

# clean the working environment
rm(list = ls(all.names = T))
gc()

# load packages
library(megaSDM)
library(SDMtune)
library(raster) 
library(terra)
library(plyr)
library(dplyr)

# set a random seed for reproducibility
set.seed(123)

############  THIS IS A TEST....the actual setting of the calibration & projection ranges should be modified for the final modeling

##### part 1 ::: collect and process occurrence points -----
# first collect the occurrence points from across the sp ranges 
#OccurrenceCollection(spplist = c('Mantis religiosa', 'Iris oratoria'),
#                     output = 'data/occs/raw/',
#                     trainingarea = c(-180, 180, -90, 90))

# load occurrence points for religiosa == only take human observations, and then select only the columns that are needed
r.occs <- read.csv('data/occs/raw/Mantis_religiosa.csv') 
r.occs <- r.occs %>% filter(basisOfRecord == 'HUMAN_OBSERVATION') %>% select('species', 'decimalLongitude', 'decimalLatitude', 'continent', 'year')
nrow(r.occs)

colnames(r.occs) = c('sp', 'long', 'lat', 'continent', 'year')
head(r.occs)


# load occurrence points for oratoria == only take human observations, and then select only the columns that are needed
o.occs <- read.csv('data/occs/raw/Iris_oratoria.csv')
o.occs <- o.occs %>% filter(basisOfRecord == 'HUMAN_OBSERVATION') %>% select('species', 'decimalLongitude', 'decimalLatitude', 'continent', 'year')
nrow(o.occs)

colnames(o.occs) = colnames(r.occs)
head(o.occs)


##### part 2 ::: process environmental data -----

### raw environmental raster == CHESLA 2.5 arcmin data
envs <- rast(list.files('E:/env layers/CHELSA_cur_V1_2B_r2_5m/2_5min/', pattern = '.tif$', full.names = T))
names(envs) = gsub('_', '', names(envs))
print(envs)


### calibration (native) range for M. religiosa
# plot out the occurrence points on the map first
plot(envs[[1]])
points(r.occs[, c('long', 'lat')])

# filter occurrences recorded in Europe
r.occs_eu <- r.occs %>% filter(continent == 'EUROPE')
head(r.occs_eu)
nrow(r.occs_eu)

points(r.occs_eu[, c('long', 'lat')], col = 'red')

# check temporal coverage == do this prior to thinning because thinning will remove all columns other than the occurrences
unique(r.occs_eu$year)

# export raw data containing only the essential columns
write.csv(r.occs_eu, 'data/occs/raw/religiosa_europe_raw.csv')

# thin occurrences with a thinning distance of 30 km
# first need to resample the base raster from 5km res to 30km. This is 5*n = 30. So the aggregation factor (n) of 6 is needed.
res_30 <- terra::aggregate(envs[[1]], fact = 6)

# thin occurrence
r.occs_eu_thin <- thinData(coords = r.occs_eu, env = res_30, x = 'long', y = 'lat', verbose = T, progress = T)
nrow(r.occs_eu_thin)

# export thinned points
write.csv(r.occs_eu_thin, 'data/occs/thinned/religiosa_europe_thinned_30km.csv')

# generate calibration area for M. religiosa
# transform thinned coordinates to sf object to get an extent of the study area
r.eu_thin_sf <- st_as_sf(r.occs_eu_thin, coords = c('long', 'lat'), crs = 4326)

# define calibration area
r.calib <- crop(envs, ext(r.eu_thin_sf))
plot(r.calib[[1]])

# export calibration extent layers
for (i in 1:nlyr(r.calib)) {
  writeRaster(r.calib[[i]], paste0('data/envs/religiosa/calib/', names(r.calib)[i], '.tif'), overwrite = T)
}


### projection (non-native) range for M. religiosa
# filter occurrences in North America
r.occs_na <- r.occs %>% filter(continent == 'NORTH_AMERICA')
head(r.occs_na)
nrow(r.occs_na)

points(r.occs_na[, c('long', 'lat')], col = 'blue')

# check temporal coverage == do this prior to thinning because thinning will remove all columns other than the occurrences
unique(r.occs_na$year)

# export raw data containing only the essential columns
write.csv(r.occs_na, 'data/occs/raw/religiosa_northamerica_raw.csv')

# thin the non-native range data using the 30km thinning parameter
r.occs_na_thin <- thinData(coords = r.occs_na, env = res_30, x = 'long', y = 'lat', verbose = T, progress = T)
nrow(r.occs_na_thin)

# export thinned points
write.csv(r.occs_na_thin, 'data/occs/thinned/religiosa_northamerica_thinned_30km.csv')


# generate projection area for M. religiosa
# transform thinned coordinates to sf object to get an extent of the study area
r.na_thin_sf <- st_as_sf(r.occs_na_thin, coords = c('long', 'lat'), crs = 4326)

# define projection area
r.proj <- crop(envs, ext(r.na_thin_sf))
plot(r.proj[[1]])

# export projction layers
for (i in 1:nlyr(r.proj)) {
  writeRaster(r.proj[[i]], paste0('data/envs/religiosa/proj/', names(r.proj)[i], '.tif'), overwrite = T)
}


### calibration (native) range for I. oratoria 
# plot out the occurrence points on the map first
plot(envs[[1]])
points(o.occs[, c('long', 'lat')])

# filter occurrences recorded in Europe
o.occs_eu <- o.occs %>% filter(continent == 'EUROPE')
head(o.occs_eu)
nrow(o.occs_eu)

points(o.occs_eu[, c('long', 'lat')], col = 'red')

# check temporal coverage == do this prior to thinning because thinning will remove all columns other than the occurrences
unique(o.occs_eu$year)

# export raw data containing only the essential columns
write.csv(o.occs_eu, 'data/occs/raw/oratoria_europe_raw.csv')

### projection (non-native) range for I. oratoria
# filter occurrences in North America
o.occs_na <- o.occs %>% filter(continent == 'NORTH_AMERICA')
head(o.occs_na)
nrow(o.occs_na)

points(o.occs_na[, c('long', 'lat')], col = 'blue')

# check temporal coverage == do this prior to thinning because thinning will remove all columns other than the occurrences
unique(o.occs_na$year)

# export raw data containing only the essential columns
write.csv(o.occs_na, 'data/occs/raw/oratoria_northamerica_raw.csv')
