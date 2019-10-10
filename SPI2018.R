# this script is to look at the SPI camera data from 2018

# directory
dir.in = "G:/1.0 Restoration and Monitoring/8.0 Habitat modeling/NFWF Habitat/1.0 GIS/"

# load packages
library(rgdal)
library(ggplot2)
library(broom)

# load data
PT = readOGR(paste(dir.in, "Sample coordinates", sep="/"), "SamplePoints16April")
SPI = readOGR(paste(dir.in, "Waypoints", sep="/"), "SPI_04_28")

# transform
SPI_df = data.frame(SPI)
PT_df = data.frame(PT)

# other
litChop = readOGR("G:/1.0 Restoration and Monitoring/3.0 Little Choptank/1.0 LC Pre_Construction_2018/1.0 GIS/Final Deliverables", "LC_sanc_bound_project")
litChop = spTransform(dat, CRS("+proj=longlat +datum=WGS84"))

dat = readOGR("G:/1.0 Restoration and Monitoring/3.0 Little Choptank/1.0 LC Pre_Construction_2018/1.0 GIS/Reef shapefiles", "Modified_reefs")
dat = spTransform(dat, CRS("+proj=longlat +datum=WGS84"))
plot(dat)

# plots
ggplot() + 
  geom_polygon(data = dat, aes(x = long, y = lat, group = group), fill = "lightgrey") + 
  geom_point(data = PT_df, aes(x = Longitude, y = Latitude), col = "black", pch = 16) + 
  geom_point(data = SPI_df, aes(x = Longitude, y = Latitude), col = "magenta", pch = 15) + 
  theme_bw() + 
  lims(x = c(-76.265,-76.225), y = c(38.522,38.5425))

 
