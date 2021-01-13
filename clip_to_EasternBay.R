# clip to Eastern Bay only

# load packages
#library(rgdal)
library(raster)

# read in shapefile
#eb = readOGR(dsn = "~/Oyster Recovery Partnership, Inc/ORP - Operations/Monitoring and Assessment/11_Habitat Modeling/Data/rawdata/EasternBay/", layer = "Eastern_Bay")
eb = raster::shapefile("~/Oyster Recovery Partnership, Inc/ORP - Operations/Monitoring and Assessment/11_Habitat Modeling/Data/rawdata/EasternBay/Eastern_Bay")
#proj4string(eb) = "+proj=longlat +datum=WGS84 +no_defs"

# define function to clip to Eastern Bay shapefile
clip_to_eb = function(c){
  # turn into a raster
  p = rasterize(eb, c)
  
  # crop
  y = r-p
  #y = raster::intersect(r, eb) #intersect, extract, crop
  return(y)
}

