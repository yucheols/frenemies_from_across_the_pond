#####  Ecological niche modeling and niche shift analyses in two invasive mantises == Mantis religiosa, Iris oratoria
#####  process input data for analyses == these include occurrences, environmental data, etc.

# clean the working environment
rm(list = ls(all.names = T))
gc()

# load packages
library(megaSDM)
library(raster) 
library(terra)
library(plyr)
library(dplyr)

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

############  THIS IS A TEST

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

### projection (non-native) range for M. religiosa
# filter occurrence points in North America
r.occs_na <- r.occs %>% filter(continent == 'NORTH_AMERICA')
head(r.occs_na)
nrow(r.occs_na)

points(r.occs_na[, c('long', 'lat')], col = 'blue')

### calibration (native) range for I. oratoria 


### projection (non-native) range for I. oratoria