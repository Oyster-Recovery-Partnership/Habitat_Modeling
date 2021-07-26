# Pick sample points for camera work using patent tong, planting data, and HSI model 

# load packages
library(rgdal)
library(ggplot2)
library(R.matlab)
library(dplyr)
library(readxl)

# READ IN PLANTING AND PT DATA TO USE THESE POINTS
# planting data
# Oyster Recovery Partnership, Inc\ORP - Operations\GIS\Sanctuary Planting Data\2020\Eastern Bay
#track <- readOGR(dsn = "Oyster Recovery Partnership, Inc/ORP - Operations/GIS/Sanctuary Planting Data/2020/Eastern Bay/", layer = "8_6_20_trackline_buffer")
track <- readOGR(dsn = "Oyster Recovery Partnership, Inc/ORP - Operations/GIS/Sanctuary Planting Data/2020/Eastern Bay/", layer = "Eastern_Bay_DNR_Site_Trackline_8.6.20")
track_pts = as.data.frame(coordinates(track))
names(track_pts) = c("long","lat","z")
#write.csv(track_pts, "~/Oyster Recovery Partnership, Inc/ORP - Operations/Monitoring and Assessment/11_Habitat Modeling/Data/planting_track_080620.csv")

track02 <- readOGR(dsn = "~/Oyster Recovery Partnership, Inc/ORP - Operations/Monitoring and Assessment/11_Habitat Modeling/Data/rawdata/EasternBay/", layer = "2002_EBplantings")

# PT data
# Oyster Recovery Partnership, Inc\ORP - Operations\Monitoring and Assessment\11_Habitat Modeling\GIS
PT <- readOGR(dsn = "Oyster Recovery Partnership, Inc/ORP - Operations/Monitoring and Assessment/11_Habitat Modeling/GIS", layer = "10_20_sample_pts")
PT_pts = as.data.frame(coordinates(PT))
names(PT_pts) = c("long","lat")
#write.csv(PT_pts, "~/Oyster Recovery Partnership, Inc/ORP - Operations/Monitoring and Assessment/11_Habitat Modeling/Data/patent_tong_points_102020.csv")

# # plots
# ggplot() + geom_point(data = track_pts, aes(x=long, y=lat)) +
#   geom_point(data = PT_pts, aes(x=long, y=lat), col = "orange") +
#   theme_bw() + labs(x="Longitude",y="Latitude") #+ 
#   #geom_polygon(data = track02, aes(x=long, y=lat))

# #
# patent_tong_points_102020_HSI <- read_csv("Oyster Recovery Partnership, Inc/ORP - Operations/Monitoring and Assessment/11_Habitat Modeling/Data/output/habitat_modeling/patent_tong_points_102020_HSI.csv")
# planting_track_080620_HSI <- read_csv("Oyster Recovery Partnership, Inc/ORP - Operations/Monitoring and Assessment/11_Habitat Modeling/Data/output/habitat_modeling/planting_track_080620_HSI.csv")

# READ IN HSI MODEL POINTS TO PULL SCORES PICK MORE POINTS
HSI_15x <- read_excel("Oyster Recovery Partnership, Inc/ORP - Operations/Monitoring and Assessment/11_Habitat Modeling/Data/output/habitat_modeling/HSI_15x.xlsx")

# ggplot() + geom_point(data = HSI_15x, aes(x=lon, y=lat, color=HSI))+
#   scale_colour_gradientn(colours=rainbow(10)) #+ 
#   #xlim(-76.266, -76.25) + ylim(38.862, 38.877) +
#   #geom_point(data = HSI_pts, aes(x=lon, y=lat), pch=11, color="black")

subHSI = HSI_15x %>% filter(lat>38.87, lat<38.878, lon>(-76.261), lon<(-76.255))
HSI_pts =   subHSI[c(98,99,100,120,121,122,123,124,144,143,142,141,166,165,164,163,186,187,208,209,
                     200,199,125,4,5,6, 12, 13, 16, 17, 28, 29, 30, 38, 39, 177, 130, 131, 260, 259),]
HSI_pts$comments=NA
HSI_pts$comments[1:20] = "close to 2020 data" 
HSI_pts$ID = seq(1:length(HSI_pts$HSI))

# ggplot() + 
#   geom_point(data = HSI_pts, aes(x=lon, y=lat), pch=11, color="black")+
#   #geom_point(data = track_pts, aes(x=long, y=lat)) +
#   geom_point(data = subHSI, aes(x=lon, y=lat, color=HSI)) + 
#   #geom_point(data = HSI_15x, aes(x=lon, y=lat, color=HSI)) + 
#   #xlim(-76.261, -76.255) + ylim(38.87, 38.878) + 
#   #xlim(-76.2593, -76.2571) + ylim(38.873, 38.8751) + 
#   #xlim(-76.266, -76.25) + ylim(38.86, 38.88) +
#   #geom_point(data = PT_pts, aes(x=long, y=lat), col = "orange") +
#   scale_colour_gradientn(colours=rainbow(10)) #+
#   #geom_point(data = subHSI[260,], aes(x=lon, y=lat), pch=11, color="black")


subHSI2 = HSI_15x %>% filter(lat>38.92, lat<38.94, lon>(-76.286), lon<(-76.275))
#subHSI3 = HSI_15x %>% filter(HSI>0.85)
HSI_pts2 =subHSI2[c(654,655,695,696,710,711,1300, 1301, 1010, 1011, 1295, 1294, 1018, 1019, 1020, 1021),]
HSI_pts2$ID = seq(1:length(HSI_pts2$HSI))+40
  
# ggplot() + 
#   #geom_point(data = HSI_15x, aes(x=lon, y=lat, color=HSI)) + 
#   geom_point(data = subHSI2, aes(x=lon, y=lat, col=HSI)) + 
#   #geom_point(data = subHSI3, aes(x=lon, y=lat), col="black") + 
#   #ylim(38.92, 38.94) + xlim(-76.286, -76.275) + 
#   #geom_point(aes(y=38.93655519579814, x=-76.28599505230727),color="gold",pch=11) + 
#   scale_colour_gradientn(colours=rainbow(10)) + 
#   #geom_point(data = subHSI2[1015,], aes(x=lon, y=lat), pch=11, color="black") +
#   geom_point(data = HSI_pts2, aes(x=lon, y=lat), pch=11, color="black")

HSI_pts = bind_rows(HSI_pts, HSI_pts2)  
write.csv(HSI_pts, "Oyster Recovery Partnership, Inc/ORP - Operations/Monitoring and Assessment/11_Habitat Modeling/Data/camera_sample_points_061021.csv")

ggplot() + 
  geom_point(data = HSI_15x, aes(x=lon, y=lat, color=HSI))+
  scale_colour_gradientn(colours=rainbow(10)) +
  geom_point(data = HSI_pts, aes(x=lon, y=lat), pch=1, color="black")+
  labs(x="Latitude",y="longitude") +
  ylim(38.85, 38.95) + xlim(-76.29, -76.25)

#GIS
b = readOGR(dsn = "Oyster Recovery Partnership, Inc/ORP - Operations/Monitoring and Assessment/11_Habitat Modeling/GIS/Oyster_Restoration_Tributaries_2018/", layer = "Oyster_Restoration_Tributaries_2018")
c = readOGR(dsn = "Oyster Recovery Partnership, Inc/ORP - Operations/GIS/Sanctuary Planting Data/2020/Eastern Bay/", layer = "DNR_sampledpts_July2020_project") 
d = readOGR(dsn = "Oyster Recovery Partnership, Inc/ORP - Operations/GIS/Sanctuary Planting Data/2020/Eastern Bay/", layer = "Eastern_Bay_DNR_Site_Trackline_8.6.20")
#e = readOGR(dsn = "Oyster Recovery Partnership, Inc/ORP - Operations/GIS/Sanctuary Planting Data/2020/Eastern Bay/", layer = "Historic_Oys_Bottom_project")
f = readOGR(dsn = "Oyster Recovery Partnership, Inc/ORP - Operations/GIS/Basemaps, NOAA charts/Maryland_Shellfish__Historic_Oyster_Bottom/",layer = "Maryland_Shellfish__Historic_Oyster_Bottom")

ggplot() + 
  geom_polygon(data = f, aes(x=long, y=lat, group=group)) +
  ylim(38.85, 38.95) + xlim(-76.29, -76.25)+
  geom_point(data = HSI_pts, aes(x=lon, y=lat), pch=1, color="black")+
  labs(x="Latitude",y="longitude")


