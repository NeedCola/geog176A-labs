---
# Chi Zhang
# 09/03/2020
#
---
library(raster)
library(tidyverse)
library(getlandsat)
library(sf)
library(mapview)
library(osmdata)


bb = read.csv('data/uscities.csv') %>%
  filter(city == "Palo") %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  st_transform(5070) %>%
  st_buffer(5000) %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_as_sf()

bbwgs = bb %>%
  st_transform(4326)

bb = st_bbox(bbwgs)

scenes = lsat_scenes()

down = scenes %>%
  filter(min_lat <= bb$ymin, max_lat >= bb$ymax,
         min_lon <= bb$xmin, max_lon >= bb$xmax,
         as.Date(acquisitionDate) == as.Date("2016-09-26"))

write.csv(down, file = "data/palo-flood.csv", row.names = FALSE)







