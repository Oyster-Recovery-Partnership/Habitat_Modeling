# clip data sources to fit Manokin boundary

# ----------------- #
# load packages
# ----------------- #
library(rgdal)
library(ggplot2)
library(broom)
library(dplyr)
library(raster)
library(rgeos)
library(readr)
# ----------------- #


# ----------------- #
# directory
# ----------------- #
dir.in = "G:/1.0 Restoration and Monitoring/8.0 Habitat modeling/NFWF Habitat/2.0 Data/rawdata"
# ----------------- #


# ----------------- #
# run NFWF_Manokin.R
# ----------------- #
source("G:/1.0 Restoration and Monitoring/8.0 Habitat modeling/NFWF Habitat/2.0 Data/code/NFWF_Manokin.R")
# ----------------- #


# ----------------- #
# load data
# ----------------- #
channels = readOGR(paste(dir.in, "maintainedchannels", sep = "/"), "maintainedchannels")
bathy = readOGR(paste(dir.in, "Maryland_Bathymetry__Chesapeake_Bay_Contours", sep = "/"), "Maryland_Bathymetry__Chesapeake_Bay_Contours")
seabed = readOGR(paste(dir.in, "Seabed_Form/MARCO_data/Marine_Life/Seabed_Form", sep="/"), "seabed_form")
hist_oys = readOGR(paste(dir.in, "historic oyster habitat", sep = "/"), "Maryland_Shellfish__Historic_Oyster_Bottom")
#mobile_sed = readOGR(paste(dir.in, "MAB_mobile_perc/MAB_mobile_perc", sep = "/"), "MAB_mobile_perc") #no points in Manokin

clay = readOGR(paste(dir.in, "cbess_GIS", sep="/"), "clay")
csand = readOGR(paste(dir.in, "cbess_GIS", sep="/"), "clayey_sand")
csilt = readOGR(paste(dir.in, "cbess_GIS", sep="/"), "clayey_silt")
rshore = readOGR(paste(dir.in, "cbess_GIS", sep="/"), "recent_shoreline")
sand = readOGR(paste(dir.in, "cbess_GIS", sep="/"), "sand")
ssclay = readOGR(paste(dir.in, "cbess_GIS", sep="/"), "sand_silt_clay")
sclay = readOGR(paste(dir.in, "cbess_GIS", sep="/"), "sandy_clay")
ssilt = readOGR(paste(dir.in, "cbess_GIS", sep="/"), "sandy_silt")
sed = readOGR(paste(dir.in, "cbess_GIS", sep="/"), "sed_samples")
silt = readOGR(paste(dir.in, "cbess_GIS", sep="/"), "silt")
siclay = readOGR(paste(dir.in, "cbess_GIS", sep="/"), "silty_clay")
sisand = readOGR(paste(dir.in, "cbess_GIS", sep="/"), "silty_sand")

# The input file geodatabase
fgdb <- paste(dir.in, "WrecksAndObstructions/WrecksAndObstructions/WrecksAndObstructions.gdb", sep = "/")

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)

# Read the feature class
wrecks <- readOGR(dsn=fgdb,layer="WrecksAndObstructions_NAD83")

# Determine the FC extent, projection, and attribute information
summary(wrecks)

# change projections
seabed = spTransform(seabed, proj4string(Manokin))
wrecks = spTransform(wrecks, proj4string(Manokin))
clay = spTransform(clay, proj4string(Manokin))
csand = spTransform(csand, proj4string(Manokin))
csilt = spTransform(csilt, proj4string(Manokin))
rshore = spTransform(rshore, proj4string(Manokin))
sand = spTransform(sand, proj4string(Manokin))
ssclay = spTransform(ssclay, proj4string(Manokin))
sclay = spTransform(sclay, proj4string(Manokin))
ssilt = spTransform(ssilt, proj4string(Manokin))
sed = spTransform(sed, proj4string(Manokin))
silt = spTransform(silt, proj4string(Manokin))
siclay = spTransform(siclay, proj4string(Manokin))
sisand = spTransform(siclay, proj4string(Manokin))
# ----------------- #


# ----------------- #
# clip datap
# ----------------- #
channels = raster::intersect(channels, Manokin)  
ggplot() + theme_bw() +
  geom_polygon(data = Manokin, aes(x = long, y = lat, group = group), col = "black", fill = "lightblue") +
  geom_polygon(data = channels, aes(x = long, y = lat, group = group), col = "red")

bathy = raster::intersect(bathy, Manokin)  
# ggplot() + geom_polygon(data = Manokin, aes(x = long, y = lat, group = group)) + geom_polygon(data = bathy, aes(x = long, y = lat, group = group))

seabed = raster::intersect(seabed, Manokin)  
# ggplot() + theme_bw() + 
#   geom_polygon(data = Manokin, aes(x = long, y = lat, group = group), col = "black", fill = "lightblue") + 
#   geom_polygon(data = seabed, aes(x = long, y = lat, group = group, fill = GRID_CODE))

hist_oys = raster::intersect(hist_oys, Manokin) 
ggplot() + theme_bw() + 
  geom_polygon(data = Manokin, aes(x = long, y = lat, group = group), col = "black", fill = "lightblue") + 
  geom_polygon(data = hist_oys, aes(x = long, y = lat, group = group), col = "black", fill = "gold")

#wrecks = raster::intersect(wrecks, Manokin)  

#clay = raster::intersect(clay, Manokin)
#csand = raster::intersect(csand, Manokin)
#csilt = raster::intersect(csilt, Manokin)
#rshore = raster::intersect(rshore, Manokin)
sand = raster::intersect(sand, Manokin)
ssclay = raster::intersect(ssclay, Manokin)
#sclay = raster::intersect(sclay, Manokin)
#ssilt = raster::intersect(ssilt, Manokin)
#sed = raster::intersect(sed, Manokin)
#silt = raster::intersect(silt, Manokin)
siclay = raster::intersect(siclay, Manokin)
sisand = raster::intersect(siclay, Manokin)

ggplot() + theme_bw() + 
  geom_polygon(data = Manokin, aes(x = long, y = lat, group = group), col = "black", fill = "offwhite") +  
  geom_point(data = ssclay, aes(x = long, y = lat), col = "red", fill = "gold") + 
  geom_point(data = ssclay, aes(x = long, y = lat), col = "gold", fill = "green") + 
  geom_point(data = sand, aes(x = long, y = lat), col = "red", fill = "red") + 
  geom_point(data = sisand, aes(x = long, y = lat), col = "navy", fill = "blue") 
# ----------------- #


# ----------------- #
# Eyes on the bay bouy (ET8.1) 
# source: http://data.chesapeakebay.net/WaterQuality
# ----------------- #
buoy <- read_csv(paste(dir.in, "WaterQualityWaterQualityStation.csv", sep = "/"))
buoy2 = buoy %>% dplyr::select(SampleDate, Parameter, MeasureValue) %>% tidyr::spread(Parameter, SampleDate, MeasureValue)
# ----------------- #


