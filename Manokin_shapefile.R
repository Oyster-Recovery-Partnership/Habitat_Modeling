# ----------------- #
# Manokin file downloaded from http://data-chesbay.opendata.arcgis.com/datasets/oyster-restoration-tributaries-2018-1?geometry=-76.377%2C38.037%2C-75.362%2C38.226
# ----------------- #

# ----------------- #
# directory
# ----------------- #
dir.in = "C:/Users/kcoleman/Downloads/Oyster_Restoration_Tributaries_2018"
# ----------------- #

# ----------------- #
# load packages
# ----------------- #
library(rgdal)
library(ggplot2)
library(broom)
library(dplyr)
library(raster)
library(rgeos)
# ----------------- #

# ----------------- #
# load data
# ----------------- #
tribs = readOGR(dir.in, "Oyster_Restoration_Tributaries_2018")

# clip to Manokin
gClip <- function(shp, bb){
  if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
  else b_poly <- as(extent(bb), "SpatialPolygons")
  gIntersection(shp, b_poly, byid = T)
}

#clip and plot for verification
b = as.data.frame(bbox(tribs)) %>% mutate(min = c(-76.377,38.037), max = c(-75.362,38.226)) %>% as.matrix()
Manokin <- gClip(tribs, b)
# ggplot() + geom_polygon(data = Manokin, aes(x = long, y = lat, group = group))
# ----------------- #
