# Read Eastern Bay file, created from NOAA codes shapefile

# ----------------- #
# directory
# ----------------- #
dir.in = "U:/O365_NEW_ Operations/Monitoring and Assessment/11_Habitat Modeling/Data/rawdata/EasternBay"
# ----------------- #

# ----------------- #
# load packages
# ----------------- #
library(rgdal)
# ----------------- #

# ----------------- #
# load data
# ----------------- #
EB = readOGR(dir.in, "Eastern_Bay")
# ----------------- #

# ggplot()+geom_polygon(data = EB, aes(x = long, y=lat, group = group), fill = "lightblue")
