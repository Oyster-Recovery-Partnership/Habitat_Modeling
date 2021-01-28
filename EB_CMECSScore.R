# ----------------- #
# model habitat suitability for oysters in Eastern Bay
# CMECS
#
# K. Coleman 1/2021
# ----------------- #


# ----------------- #
# load packages
# ----------------- #
library(dplyr)
library(ggplot2)
library(rgeos)
library(raster)
# ----------------- #


# ----------------- #
# load data
# ----------------- #
# clip to Eastern Bay
source("~/Oyster Recovery Partnership, Inc/ORP - Operations/Monitoring and Assessment/11_Habitat Modeling/Data/code/clip_to_EasternBay.R")

# CMECS
eb_bottom = raster::shapefile("~/Oyster Recovery Partnership, Inc/ORP - Operations/Monitoring and Assessment/11_Habitat Modeling/GIS/EasterBay_CMECS_R_Errors_Corrected_01222021_ShapeFile/EasterBay_CMECS_R_Errors_Corrected_01222021")
eb_bottom = spTransform(eb_bottom, CRS("+proj=longlat +datum=WGS84 +no_defs")) #CRS("+proj=utm +zone=18 +datum=NAD83 +units=m +no_defs"))
# ----------------- #


# ----------------- #
# define scores for variables 
# ----------------- #
# make CMECS score curve and look up table
cmecs_curve = as.data.frame(cbind(c("Anthropogenic_Shell_Reef",
                                    "Anthropogenic_Shell_Rubble",
                                    "Biogenic_Oyster_Reef",
                                    "Biogenic_Oyster_Rubble",
                                    "Mud",
                                    "Muddy_Sand",
                                    "Sand",
                                    "Sandy_Mud",
                                    "Unclassified"),
                                  c(1,1,1,1,0.1,0.75,1,0.5,NA)))
names(cmecs_curve) = c("cmecs","score")
cmecs_curve = mutate(cmecs_curve, score = as.numeric(score))
# p=ggplot() + geom_bar(data=cmecs_curve, aes(x=reorder(cmecs, score), y=score),
#                     stat = "identity", width = 0.8) +
#   theme_bw() + theme(text = element_text(size=20),
#         axis.text.x = element_text(angle = 20, hjust = 1)) + 
#   labs(y="Score",x="CMECS Group") 
# p
# ggsave("~/Oyster Recovery Partnership, Inc/ORP - Operations//Monitoring and Assessment/11_Habitat Modeling/Data/output/CMECS_curve.png", p)
# ----------------- #


# ----------------- #
# manipulte data
# ----------------- #
bot1 = gBuffer(eb_bottom, byid=TRUE, width=0) # clean up ring intersection
new_bot = raster::intersect(bot1, eb) #rgeos::gIntersection(eb_bottom, eb, checkValidity = 2L)
new_bot2 = broom::tidy(new_bot, region = 'Group_')  
# p = ggplot() + 
#   geom_polygon(data = new_bot2, aes(x=long, y=lat, group=group, fill = id)) + 
#   labs(y="Latitude",x="Longitude",title="CMECS")
# p
# ggsave("~/Oyster Recovery Partnership, Inc/ORP - Operations//Monitoring and Assessment/11_Habitat Modeling/Data/output/CMECS_rawdata_Map.png", p)

# r = rasterize(new_bot, newr)
# ggplot()+geom_polygon(data = new_bot, aes(x=long, y=lat, group=group, fill = Group_))

# avg CMECS classification for each grid square, weighted by area
new_bot3 = left_join(new_bot2, cmecs_curve %>% rename(id=cmecs), by = "id")

p = ggplot() +
  geom_polygon(data = new_bot3, aes(x=long, y=lat, group=group, fill = score), col="black") +
  labs(y="Latitude",x="Longitude",title="CMECS Score") +
  scale_fill_gradient(low="blue", high="yellow")
p
ggsave("~/Oyster Recovery Partnership, Inc/ORP - Operations//Monitoring and Assessment/11_Habitat Modeling/Data/output/CMECS_scoredata_Map.png", p)

# test = new_bot3
# coordinates(test) = ~long+lat
# proj4string(test) = "+proj=longlat +datum=WGS84 +no_defs"

#t = st_as_sf(new_bot3, coords = c("long", "lat"), fill=T, group=T, crs=4326) 

# tibble to sf
# random_points = new_bot3 %>% 
#   st_as_sf(coords = c("long", "lat")) %>% # set coordinates
#   st_set_crs(4326) # set geographic CRS

# ebot = st_read("~/Oyster Recovery Partnership, Inc/ORP - Operations/Monitoring and Assessment/11_Habitat Modeling/GIS/EasterBay_CMECS_R_Errors_Corrected_01222021_ShapeFile/EasterBay_CMECS_R_Errors_Corrected_01222021.shp")
# ebot = st_transform(ebot, crs = 4326)
# eb2 = st_read("~/Oyster Recovery Partnership, Inc/ORP - Operations/Monitoring and Assessment/11_Habitat Modeling/Data/rawdata/EasternBay/Eastern_Bay.shp")
# ebot = st_crop(ebot, eb2)

# poly to sf
t = st_as_sf(new_bot, crs = 4326)

# join with scores
t = left_join(t, cmecs_curve %>% rename(Group_=cmecs), by = "Group_")


# raster to poly to sf
library(inlmisc)
spdf_1 <- as(newr,'SpatialPolygonsDataFrame')
sf_1 <- st_as_sf(spdf_1)

# test2 = sf_1  %>%
#   st_join(random_points) %>%
#   group_by(V1) %>%
#   summarize(rankings = mean(score, na.rm = TRUE)) %>%
#   dplyr::select(rankings)
# ggplot(test2) + geom_sf(aes(fill=rankings))

test3 = sf_1  %>%
  st_join(t) %>%
  group_by(V1) %>%
  summarize(rankings = mean(score, na.rm = TRUE)) %>%
  dplyr::select(rankings)

p = ggplot(test3) + geom_sf(aes(fill=rankings)) + 
  #geom_polygon(data = eb, aes(x=lon, y=lat)) + 
  labs(x="Longitude", y="Latitude", title="Mean CMECS Score", fill="Score") + 
  scale_fill_gradient(low="blue", high="yellow")
p 
ggsave("~/Oyster Recovery Partnership, Inc/ORP - Operations//Monitoring and Assessment/11_Habitat Modeling/Data/output/CMECSMap.png", p)

f = fasterize(test3, newr)
f[!is.na(f)] = test3$rankings
# doesnt work but is needed to get the right number of cells 

