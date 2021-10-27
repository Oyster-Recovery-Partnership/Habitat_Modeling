# -------------- #
# define ranking curves for variables of interest
# -------------- #

# -------------- #
# load packages
# -------------- #
require(dplyr)
require(ggplot2)
require(rgdal)
require(sf)
library(rgeos)
# -------------- #

# -------------- #
# set dir
# -------------- #
dir.out = "~/Oyster Recovery Partnership, Inc/ORP - Operations//Monitoring and Assessment/11_Habitat Modeling/Data/output/variable_curves"
# -------------- #

# -------------- #
# BOTTOM TYPE
# -------------- #
# # load CMECS data
# fgdb <- "~Oyster Recovery Partnership, Inc/ORP - Operations/GIS/Tributary Blueprints/Manokin_Oyster_Restoration_Blueprint_Geodatabase_09_30_2019/Manokin_River_Oyster_Restoration_BluePrint_GeoDatabase_09302019.gdb"
# 
# # List all feature classes in a file geodatabase
# subset(ogrDrivers(), grepl("GDB", name))
# fc_list <- ogrListLayers(fgdb)
# print(fc_list)
# 
# # Read the feature class
# cmecs <- st_read(dsn=fgdb,layer="FINAL_CMECS_Benthic_Habitat_Characterization_Manokin_River_Sanctuary_10172018")
# 
# # make dataframe
# cmecs_df = as.data.frame(cmecs)
# cmecs_bt = broom::tidy(cmecs, "Group_")
#
## Eastern Bay
#library(raster)
# doesn't cover the whole shapefile
#eb_bottom = raster::shapefile("~/Oyster Recovery Partnership, Inc/ORP - Operations/GIS/Sanctuary Planting Data/2020/Eastern Bay/EasternBayBottomTypes")
#eb_bottom = spTransform(eb_bottom,CRS("+proj=longlat +datum=WGS84 +no_defs"))
#
# eb_bottom = raster::shapefile("~/Oyster Recovery Partnership, Inc/ORP - Operations/Monitoring and Assessment/11_Habitat Modeling/GIS/CMECS_v4")
# eb_bottom = spTransform(eb_bottom,CRS("+proj=longlat +datum=WGS84 +no_defs"))
# new_bot = raster::intersect(eb_bottom, eb)
# new_bot2 = broom::tidy(new_bot, region = "bottom_typ") 
# ggplot()+geom_polygon(data = new_bot2, aes(x=long, y=lat, fill = id, group=group))
#
#
# fgdb <- "~/Oyster Recovery Partnership, Inc/ORP - Operations/GIS/Chesapeake Bay CMECS v4 Substrate Component 01062017/Chesapeake_Bay_CMECS_V4_Substrate_Component_01062017.gdb"
# fc_list <- ogrListLayers(fgdb)
# fc <- readOGR(dsn=fgdb,layer="Chesapeake_Bay_CMECS_V4_Substrate_Component_01062017")
# fc = spTransform(fc, CRS("+proj=longlat +datum=WGS84 +no_defs"))
# new_bot = raster::intersect(fc, eb)
# new_bot = raster::crop(fc, extent(eb))
# new_bot2 = broom::tidy(fc, region = "Group_") 

# eb_bottom = raster::shapefile("~/Oyster Recovery Partnership, Inc/ORP - Operations/Monitoring and Assessment/11_Habitat Modeling/GIS/EB_CMECS_update")
# new_bot2 = broom::tidy(eb_bottom, region = "Group_") 
# ggplot() + geom_polygon(data = eb_bottom, aes(x = long, y = lat, group = group, fill = 'SubClass'))
# test = rgeos::gIntersection(eb_bottom, eb, checkValidity=2L)

eb_bottom = raster::shapefile("~/Oyster Recovery Partnership, Inc/ORP - Operations/Monitoring and Assessment/11_Habitat Modeling/GIS/EasterBay_CMECS_R_Errors_Corrected_01222021_ShapeFile/EasterBay_CMECS_R_Errors_Corrected_01222021")
eb_bottom = spTransform(eb_bottom, CRS("+proj=longlat +datum=WGS84 +no_defs")) #CRS("+proj=utm +zone=18 +datum=NAD83 +units=m +no_defs"))
bot1 = gBuffer(eb_bottom, byid=TRUE, width=0)
new_bot = raster::intersect(bot1, eb) #rgeos::gIntersection(eb_bottom, eb, checkValidity = 2L)
new_bot2 = broom::tidy(new_bot, region = 'Group_')  
ggplot()+geom_polygon(data = new_bot2, aes(x=long, y=lat, group=group, fill = id))

test = rasterize(new_bot, newr)
ggplot()+geom_polygon(data = new_bot, aes(x=long, y=lat, group=group, fill = Group_))


# simp_bot <- gSimplify(eb_bottom, tol = 0.00001)
# simp_bot <- gBuffer(simp_bot, byid=TRUE, width=0)
# sum(gIsValid(simp_bot, byid=TRUE)==FALSE)
# 
# eb.df <- as.data.frame(eb_bottom)
# test = as.data.frame(coordinates(eb_bottom))
# names(test) = c("long","lat")
# eb.df = cbind(eb.df, test)
# names(eb.df) = gsub("_","",names(eb.df))

# eb_bottom = raster::shapefile("~/Oyster Recovery Partnership, Inc/ORP - Operations/Monitoring and Assessment/11_Habitat Modeling/GIS/EB_CMECS")
# eb_bottom = spTransform(eb_bottom, CRS("+proj=longlat +datum=WGS84 +no_defs"))

# fgdb = "~/Oyster Recovery Partnership, Inc/ORP - Operations/Monitoring and Assessment/11_Habitat Modeling/Data/rawdata/Chesapeake_Bay_Benthic_Habitat_CMECS_Geodatabase_12182020/Chesapeake_Bay_Benthic_Habitat_CMECS_Geodatabase_12182020.gdb"
# fc_list <- ogrListLayers(fgdb)
# #fc <- readOGR(dsn=fgdb,layer="CB_Islands_12162020")
# fc <- readOGR(dsn=fgdb,layer="Chesapeake_Bay_CMECS_V4_Substrate_Component_12_18_2020")

# ggplot() + geom_polygon(data = eb, aes(x=long, y=lat, group=group)) + 
#  # geom_polygon(data = new_bot, aes(x=long, y=lat, fill=id, group=group))
#   geom_polygon(data = eb_bottom, aes(x=long, y=lat, group=group), col="blue")
# 
# eb_bottom = dplyr::rename(eb_bottom, id = 'Group_')
# ggplot() + geom_polygon(data = eb_bottom, aes(x=long, y=lat, group=group, fill = 'Group_'))

library(sf)
nc <- st_read("~/Oyster Recovery Partnership, Inc/ORP - Operations/Monitoring and Assessment/11_Habitat Modeling/GIS/EB_CMECS.shp")
test <- gBuffer(nc, byid=TRUE, width=0)
# simplify the polgons a tad (tweak 0.00001 to your liking)
spydf_states <- gSimplify(spydf_states, tol = 0.00001)

# this is a well known R / GEOS hack (usually combined with the above) to 
# deal with "bad" polygons
spydf_states <- gBuffer(spydf_states, byid=TRUE, width=0)

# any bad polys?
sum(gIsValid(spydf_states, byid=TRUE)==FALSE)



# # https://data.imap.maryland.gov/datasets/1e6cf46ed99045bdbe09ac51bd1f66ae_0?geometry=-76.740%2C38.803%2C-75.871%2C38.990
# this shapefile/ geodatabase has a ring self-intersection that makes it impossible to crop/intersect
#
# eb_bottom = raster::shapefile("~/Oyster Recovery Partnership, Inc/ORP - Operations/Monitoring and Assessment/11_Habitat Modeling/Data/rawdata/Maryland_Benthic_Habitat_-_Chesapeake_Bay_Benthic_Habitat-shp/Maryland_Benthic_Habitat_-_Chesapeake_Bay_Benthic_Habitat")
# eb_bottom = spTransform(eb_bottom, CRS("+proj=longlat +datum=WGS84 +no_defs"))
# new_bot = raster::intersect(eb_bottom, eb)
# extent
# 
# fgdb <- "~/Oyster Recovery Partnership, Inc/ORP - Operations/Monitoring and Assessment/11_Habitat Modeling/Data/rawdata/Maryland_Benthic_Habitat_-_Chesapeake_Bay_Benthic_Habitat-fgdb/Maryland_Benthic_Habitat_-_Chesapeake_Bay_Benthic_Habitat.gdb"
# subset(ogrDrivers(), grepl("GDB", name))
# fc_list <- ogrListLayers(fgdb)
#
# library(sf)
# nc <- st_read("~/Oyster Recovery Partnership, Inc/ORP - Operations/Monitoring and Assessment/11_Habitat Modeling/Data/rawdata/Maryland_Benthic_Habitat_-_Chesapeake_Bay_Benthic_Habitat-shp/Maryland_Benthic_Habitat_-_Chesapeake_Bay_Benthic_Habitat.shp")
# nc <- st_transform(nc, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
# test = st_crop(nc, c(xmin=-76.37011, xmax=-76.17721, ymin=38.81691, ymax=38.97435))

# classifications
cmecs_curve = as.data.frame(cbind(c("Mud",
                                    "Sandy Mud",
                                    "Muddy Sand",
                                    "Sand",
                                    "Biogenic Oyster Reef",
                                    "Biogenic Oyster Rubble",
                                    "Anthprogogenic Shell Rubble",
                                    "Anthropogenic Shell Rubble"),
                                  c(0.1, 0.5, 0.75, rep(1,5)),
                                  c(seq(1, 8, by=1))))
names(cmecs_curve) = c("cmecs","ranks","orders")
cmecs_curve = mutate(cmecs_curve, cmecs = as.character(cmecs), ranks = as.numeric(as.character(ranks)), orders = as.numeric(orders))

# plot curve
p1 = ggplot(data = cmecs_curve, aes(x = reorder(cmecs, orders), y = ranks)) + 
  geom_point(stat = "identity", size=2) + 
  theme_bw() + labs(x = "CMECS")+#, y = "Habitat Rank") + 
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle = 20, hjust = 1))# +
  #ggtitle("CMECS")
p1 
ggsave(paste(dir.out,"cmecs_curve.png",sep="/"), p1)

cmecs_curve2 = as.data.frame(cbind(c("M",
                                    "SM",
                                    "MS",
                                    "S",
                                    "BORf",
                                    "BORb",
                                    "ASRf",
                                    "ASRb"),
                                  c(0.1, 0.5, 0.75, rep(1,5)),
                                  c(seq(1, 8, by=1))))
names(cmecs_curve2) = c("cmecs","ranks","orders")
cmecs_curve2 = mutate(cmecs_curve2, cmecs = as.character(cmecs), ranks = as.numeric(as.character(ranks)), orders = as.numeric(orders))

# plot curve
p1b = ggplot(data = cmecs_curve2, aes(x = reorder(cmecs, orders), y = ranks)) + 
  geom_point(stat = "identity", size=2) + 
  theme_bw() + labs(x = "CMECS")+#, y = "Habitat Rank") + 
  theme(text = element_text(size=20))#ggtitle("CMECS")
p1b 

# -------------- #


# -------------- #
# TEMPERATURE 
# -------------- #
# -2°C to 36°C
# Larval 15.5 = 0, 26-31=was 1, 34.5=0; Adult: 10.5=0, 23-30=1, 34.5=0 (Barnes et al. 2007)
# Mean summer: 5=0, 10-30=1, 40=0 (Cake 1983)
# 8=0, 15-18=1, 34=0 (Cho et al. 2012 (gigas))
# temp_curve = as.data.frame(cbind(c(-2,5,10,20,30,33,36),c(0,0.1,0.5,1,1,0.25,0)))
# names(temp_curve)=c("temp","score")
# 
# p = ggplot(data = temp_curve, aes(x = temp, y = score)) + 
#   geom_point()+
#   geom_line()+
#   #geom_area(col="lightgrey") + 
#   theme_bw() + labs(x = "Temperature (C)", y = "Habitat Rank") + 
#   theme(text = element_text(size=20)) +
#   ggtitle("Temperature")
# p 
# ggsave(paste(dir.out,"temp_curve.png",sep="/"), p)
# -------------- #


# -------------- #
# SALINITY
# -------------- #
# salinity extremes
# 5 - 40, optimal 14 - 28
# salt_curve = as.data.frame(cbind(c(5,14,28,40),c(0,1,1,0)))
# names(salt_curve)=c("salinity","score")
# 
# p = ggplot(data = salt_curve, aes(x = salinity, y = score)) + 
#   geom_point()+
#   geom_line()+
#   geom_area(col="lightgrey") + 
#   theme_bw() + labs(x = "Salinity", y = "Habitat Rank") + 
#   theme(text = element_text(size=20)) 
# p 
# ggsave(paste(dir.out,"salt_curve.png",sep="/"), p)

# min
# 2=0, 4=0.05, 8+=1 (Sonait et al. 2013, Hijuelos et al. 2017, Préau et al. 2015)
min_salt_curve = as.data.frame(cbind(c(2,4,8,20),c(0,0.05,1,1)))
names(min_salt_curve)=c("salinity","score")

p2= ggplot(data = min_salt_curve, aes(x = salinity, y = score)) + 
  geom_point()+
  geom_line(lwd=2)+
  #geom_area(col="lightgrey") + 
  theme_bw() + labs(x = "Salinity")+#, y = "Habitat Rank") + 
  theme(text = element_text(size=20)) #+
  #ggtitle("Minimum Salinity")
p2 
ggsave(paste(dir.out,"min_salt_curve.png",sep="/"), p2)

# max
# no other papers used max so modeling off of mean
# max_salt_curve = as.data.frame(cbind(c(14,28,35,40),c(1,1,0.1,0)))
# names(max_salt_curve)=c("salinity","score")
# 
# p = ggplot(data = max_salt_curve, aes(x = salinity, y = score)) + 
#   geom_point()+
#   geom_line()+
#   geom_area(col="lightgrey") + 
#   theme_bw() + labs(x = "Salinity", y = "Habitat Rank") + 
#   theme(text = element_text(size=20)) +
#   ggtitle("Maximum Salinity")
# p 
# ggsave(paste(dir.out,"max_salt_curve.png",sep="/"), p)
# -------------- #


# -------------- #
# DEPTH
# -------------- #
# 6 - 30 feet (~2-10 m)
# 0.5-3 optimal, no curve (Hijuelos et al. 2017)
# 0-3.5=1, 4.5=0.5, 4.6-10=0 (Theuerkauf and Lipscius 2016)
# 0-4=1, 8=0 (Starke et al. 2011)
depth_curve = as.data.frame(cbind(c(0,3.5,10, 25),c(1,1,0, 0)))
names(depth_curve)=c("depth","score")

p3 = ggplot(data = depth_curve, aes(x = depth, y = score)) + 
  geom_point()+
  geom_line(lwd=2)+
  #geom_area(col="lightgrey") + 
  theme_bw() + labs(x = "Depth (m)")+#, y = "Habitat Rank") + 
  theme(text = element_text(size=20)) #+
  #ggtitle("Depth")
p3 
ggsave(paste(dir.out,"depth_curve.png",sep="/"), p3)

# -------------- #


# -------------- #
# CHL A
# -------------- #
# 	                         Spring Threshold       Summer Threshold
# Tidal - fresh	(0-0.4 ppt)  ≤ 14                   ≤ 12
# Oligohaline	(0.5-5 ppt)    ≤ 20.9                 ≤ 9.5
# Mesohaline (5.1-18 ppt)	   ≤ 6.2                  ≤ 7.7                           
# Polyhaline (18.1-30 ppt)	 ≤ 2.8                  ≤ 4.5

# 1=0, 12-50=1, 55=0 (Cho et al. 2012 (gigas))
# 0=0, 100=1 (Theuerkauf et al. 2018)
#chl_curve = as.data.frame(cbind(c(0,12,50,55),c(0,1,1,0)))
#chl_curve = as.data.frame(cbind(c(0,100),c(0,1)))
chl_curve = as.data.frame(cbind(c(0,12,40),c(0,0.75,1)))
names(chl_curve)=c("chl","score")

p4 = ggplot(data = chl_curve, aes(x = chl, y = score)) + 
  geom_point()+
  geom_line(lwd=2)+
  #geom_area(col="lightgrey") + 
  theme_bw() + labs(x = "Chl a")+#, y = "Habitat Rank") + 
  theme(text = element_text(size=20)) #+
 # ggtitle("Chl a")  
p4 
ggsave(paste(dir.out,"chl_curve.png",sep="/"), p4)
# -------------- #


# -------------- #
# O2
# -------------- #
# Open water	≥ 5 (mg/L)
# Deep water seasonal	≥ 3 (mg/L)
# Deep- channel seasonal	≥ 1 (mg/L)
# Perc. Sat. 40=0, 73+=1 (Cho et al. 2012)
# convert perc. sat. to mg/l for Cho, 40=3.3, 73=6.03 
min_o2_curve = as.data.frame(cbind(c(0,3.3,6.03,15),c(0,0,1,1)))
names(min_o2_curve)=c("o2","score")

p5 = ggplot(data = min_o2_curve, aes(x = o2, y = score)) + 
  geom_point()+
  geom_line(lwd=2)+
 # geom_area(col="lightgrey") + 
  theme_bw() + labs(x = "Dissolved Oxygen")+#, y = "Habitat Rank") + 
  theme(text = element_text(size=20)) #+
  #ggtitle("Min. Dissolved Oxygen (mg/L)")
p5 
ggsave(paste(dir.out,"min_o2_curve.png",sep="/"), p5)
# -------------- #


# -------------- #
# INORGANIC SUSPENDED SOLIDS
# -------------- #
# # 0-9=1, 50=0.1 mg/l suspended sediments (Cho et al. 2012)
# ss_curve = as.data.frame(cbind(c(0, 0.5, 5),c(1, 0.5, 0)))
# names(ss_curve)=c("ss","score")
# 
# p = ggplot(data = ss_curve, aes(x = ss, y = score)) + 
#   geom_point()+
#   geom_line()+
#   geom_area(col="lightgrey") + 
#   theme_bw() + labs(x = "ISS (g/L)", y = "Habitat Rank") + 
#   theme(text = element_text(size=20)) +
#   ggtitle("Inorganic Suspended Solids (g/L)")
# p 
# ggsave(paste(dir.out,"ss_curve.png",sep="/"), p)
# -------------- #

library(devtools)
library(ggpubr)

ggarrange(p2 + theme(axis.title.y=element_blank()), 
          p3 + theme(axis.title.y=element_blank()), 
          p4 + theme(axis.title.y=element_blank()), 
          p5 + theme(axis.title.y=element_blank()),
          p1b + theme(axis.title.y=element_blank()),
          labels = c("V1", "V2", "V3","V4","V5"),
          ncol=2, nrow=2)

