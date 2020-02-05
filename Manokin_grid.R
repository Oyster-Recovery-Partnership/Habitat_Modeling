# why hexagons: https://pro.arcgis.com/en/pro-app/tool-reference/spatial-statistics/h-whyhexagons.htm
# how to in sp: https://stackoverflow.com/questions/29374004/how-do-i-generate-a-hexagonal-grid-in-r 

# ----------------- #
# packages
# ----------------- #
library(sp)
library(rgeos)
library(ggplot2)
# ----------------- #

# ----------------- #
# run NFWF_Manokin.R
# ----------------- #
source("G:/1.0 Restoration and Monitoring/8.0 Habitat modeling/NFWF Habitat/2.0 Data/code/NFWF_Manokin.R")
rm(tribs,b)
# ----------------- #

# ----------------- #
# build grid
# ----------------- #
Manokin <- spTransform(Manokin, CRS("+init=epsg:3347")) 
M.large = gBuffer(Manokin, width = 200)
HexPts <-spsample(M.large, type="hexagonal", cellsize=500)
HexPols <- HexPoints2SpatialPolygons(HexPts)

HexPols <- spTransform(HexPols, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) 
Manokin <- spTransform(Manokin, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) 

ggplot() + geom_polygon(data = Manokin, aes(x = long, y = lat, group = group), fill = "lightblue") + 
  geom_polygon(data = HexPols, aes(x = long, y = lat, group = group), fill = NA, col = "black") + 
  theme_bw() + labs(x="Longitude", y="Latitude") + theme(text = element_text(size=20))
# ----------------- #
