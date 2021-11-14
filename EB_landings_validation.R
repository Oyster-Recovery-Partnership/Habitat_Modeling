# validate HSI with landings data

# load packages
library(dplyr)
library(ggplot2)
library(rgdal)
library(raster)
library(sp)
library(broom)
library(readxl)
library(landscapetools)
library(ggpubr)

# load data
EB = readOGR(dsn="Oyster Recovery Partnership, Inc/ORP - Operations/Monitoring and Assessment/11_Habitat Modeling/Data/rawdata/EasternBay",layer="Eastern_Bay")
bars = readOGR(dsn="Oyster Recovery Partnership, Inc/ORP - Operations/GIS/Background Data/Basemaps/Maryland_Shellfish__Historic_Oyster_Bottom",layer="Maryland_Shellfish__Historic_Oyster_Bottom")
EB_bars = intersect(EB, bars)
HSI <- read_excel("Oyster Recovery Partnership, Inc/ORP - Operations/Monitoring and Assessment/11_Habitat Modeling/Data/output/habitat_modeling/HSI_15x.xlsx")
HSI = dplyr::filter(HSI, !is.na(HSI))

landings <- read_excel("Downloads/EasternBayHarvestByBar2009-2018 (1).xlsx")
landings_sum = landings %>% group_by(Name, ID) %>% 
  summarize(bushels = mean(TotalBushels,na.rm=T),
            lat = mean(Latitude, na.rm=T),
            lon = mean(Longitude, na.rm=T)) %>% 
  rename(id=ID)

EB_df = broom::tidy(bars, region = "BARCODE") %>%
  left_join(., dplyr::select(landings_sum, -lat,-lon), by = "id") %>%
  dplyr::filter(!is.na(bushels))
test = EB_df
coordinates(test)<- ~long + lat

library(sf)
test <- EB_df %>% st_as_sf(., coords = c("long","lat")) %>% 
  group_by(id) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON") 
#coordinates(HSI) = ~lon+lat
#proj4string(HSI) = CRS("+proj=longlat +datum=WGS84 +no_defs")
HSI2 = st_as_sf(HSI, coords = c("lon","lat")) %>% st_cast("POINT")
# HSI_bars = over(as(test, "Spatial"), as(HSI2, "Spatial"))
# HSI_bars$id = test$id
# HSI_bar_scores = left_join(EB_df, HSI_bars, by = "id")
HSI_bars = st_intersection(test, HSI2) %>%
  group_by(id) %>% summarize(HSI = mean(HSI, na.rm=T))
HSI_bar_scores = left_join(EB_df, HSI_bars, by = "id")


ggplot() +
  geom_polygon(data = EB, aes(x=long, y=lat, group=group), fill = "lightgrey") +
  geom_polygon(data = EB_bars, aes(x=long, y=lat, group=group), fill = "grey", col="black") +
  geom_point(data = landings_sum, aes(x = lon, y = lat, col = bushels, size = bushels)) +
  scale_color_gradient(low = "blue", high = "orange") + theme_bw()

p1 = ggplot() +
  geom_polygon(data = EB, aes(x=long, y=lat, group=group), fill = "lightgrey") +
  geom_polygon(data = EB_df, aes(x=long, y=lat, group=group, fill=bushels)) +
  scale_fill_gradient(low = "blue",  high = "gold") + 
  theme_bw() + 
  labs(x="Longitude",y="Latitude",title="Mean Oyster Bushels Landed")
p1

p2 = ggplot() +
  geom_polygon(data = EB, aes(x=long, y=lat, group=group), fill = "lightgrey") +
  geom_polygon(data = HSI_bar_scores, aes(x=long, y=lat, group=group, fill=HSI)) +
  scale_fill_gradient(low = "blue",  high = "gold") + 
  theme_bw() + 
  labs(x="Longitude",y="Latitude",title="Bar HSI")
p2

p3 = ggarrange(p1, p2, 
          labels = c("A", "B"),
          ncol = 2, nrow = 1)
p3
ggsave("Oyster Recovery Partnership, Inc/ORP - Operations/Monitoring and Assessment/11_Habitat Modeling/Data/output/EB_landings_validation.png", p3)

  
