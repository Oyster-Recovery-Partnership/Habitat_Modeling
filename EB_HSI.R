# Make Habitat Suitability Index Model for Oysters in Eastern Bay
# HSI is (V1+V2+V3+V4+V5+V6+V7)^1/7

# # run each variable if you haven't already
# # takes some time, each takes a couple minutes
# source("~/Oyster Recovery Partnership, Inc/ORP - Operations//Monitoring and Assessment/11_Habitat Modeling/Data/code/hab_model/EB_ChlaScore.R")
# source("~/Oyster Recovery Partnership, Inc/ORP - Operations//Monitoring and Assessment/11_Habitat Modeling/Data/code/hab_model/EB_DepthScore.R")
# source("~/Oyster Recovery Partnership, Inc/ORP - Operations//Monitoring and Assessment/11_Habitat Modeling/Data/code/hab_model/EB_CMECSScore.R")
# source("~/Oyster Recovery Partnership, Inc/ORP - Operations//Monitoring and Assessment/11_Habitat Modeling/Data/code/hab_model/EB_O2Score.R")
# source("~/Oyster Recovery Partnership, Inc/ORP - Operations//Monitoring and Assessment/11_Habitat Modeling/Data/code/EB_MinSaltScore.R")
# source("~/Oyster Recovery Partnership, Inc/ORP - Operations//Monitoring and Assessment/11_Habitat Modeling/Data/code/EB_ISSScore.R")
# source("~/Oyster Recovery Partnership, Inc/ORP - Operations//Monitoring and Assessment/11_Habitat Modeling/Data/code/EB_TempScore.R")

# if there is a mix of NaNs and NAs, change NaNs to Na just for consistancy 
final.chl.score[is.nan(final.chl.score)] = NA
final.iss.score[is.nan(final.iss.score)] = NA
final.salt.score[is.nan(final.salt.score)] = NA
final.temp.score[is.nan(final.temp.score)] = NA
final.o2.score[is.nan(final.o2.score)] = NA
final.depth.score[is.nan(final.depth.score)] = NA
final.temp.score[is.nan(final.temp.score)] = NA
#final.cmecs.score[is.nan(final.temp.score)] = NA # no NaNs

# finals = rbind(final.chl.score, final.iss.score, final.salt.score, 
#                final.temp.score, final.o2.score, final.depth.score,
#                final.cmecs.score)
# oys_hsi = apply(finals, 2, mean) #,na.rm=T)

oys_hsi = (final.chl.score +
             final.iss.score +
             final.salt.score +
             final.temp.score +
             final.o2.score +
             final.depth.score + 
             final.cmecs.scores) ^ 1/7

hsi_map = newr
values(hsi_map) = oys_hsi
# plot(hsi_map)  

# change raster to spatial pixels data frame to data frame in order to use ggplot
r.spdf <- as(hsi_map, "SpatialPixelsDataFrame")
r.df <- as.data.frame(r.spdf)

p = ggplot() + 
  geom_polygon(data = eb, aes(x=long, y=lat, group=group)) + 
  geom_tile(data = r.df, aes(x=x, y=y, fill=V1)) +
  labs(x="Longitude", y="Latitude", title="Oyster HSI", fill="Score") + 
  scale_fill_gradient(low="blue", high="yellow")
p 
ggsave("~/Oyster Recovery Partnership, Inc/ORP - Operations//Monitoring and Assessment/11_Habitat Modeling/Data/output/HSIMap.png", p)


# # AIC
# oys_dat = 
# m1 = oys_dat ~ (final.chl.score + final.iss.score + final.salt.score + final.temp.score + final.o2.score + final.depth.score + final.cmecs.score)^1/7
# m2 = oys_dat ~ (final.chl.score + final.iss.score + final.salt.score + final.temp.score + final.o2.score + final.depth.score)^1/6
# m3 = oys_dat ~ (final.chl.score + final.iss.score + final.salt.score + final.temp.score + final.o2.score + final.cmecs.score)^1/6
# m4 = oys_dat ~ (final.chl.score + final.iss.score + final.salt.score + final.temp.score + final.depth.score + final.cmecs.score)^1/6
# m5 = oys_dat ~ (final.chl.score + final.iss.score + final.salt.score + final.o2.score + final.depth.score + final.cmecs.score)^1/6
# m6 = oys_dat ~ (final.chl.score + final.iss.score + final.temp.score + final.o2.score + final.depth.score + final.cmecs.score)^1/6
# m7 = oys_dat ~ (final.chl.score + final.salt.score + final.temp.score + final.o2.score + final.depth.score + final.cmecs.score)^1/6
# 
# AIC(m1)
# AIC(m2)
# AIC(m3)
# AIC(m4)
# AIC(m5)
# AIC(m6)
# AIC(m7)

# # test errors
# x = which(is.na(oys_hsi))
# y = which(is.na(final.cmecs.score))
# xy = x[!x %in% y]

# oys_hsi[xy]
# final.chl.score[xy]
# final.iss.score[xy]
# final.salt.score[xy]
# final.temp.score[xy] # only one with NAs
# final.o2.score[xy]
# final.depth.score[xy] 
# final.cmecs.scores[xy]
  
  

