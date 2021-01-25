# ----------------- #
# model habitat suitability for oysters in Eastern Bay
# depth
#
# K. Coleman 1/2021
# ----------------- #

# ----------------- #
# load data
# ----------------- #
# load ChesROMS data from Pierre
source("~/Oyster Recovery Partnership, Inc/ORP - Operations/Monitoring and Assessment/11_Habitat Modeling/Data/code/read_ChesROMS_fromPierre.R")

# run loop to transform data and clip to Eastern Bay
source("~/Oyster Recovery Partnership, Inc/ORP - Operations/Monitoring and Assessment/11_Habitat Modeling/Data/code/clip_to_EasternBay.R")
# ----------------- #


# ----------------- #
# define scores for variables 
# ----------------- #
# make depth score curve and look up table
depth_curve = as.data.frame(cbind(c(0, 3.5, 10, 27), c(1, 1, 0, 0)))
depth_curve = approx(depth_curve[,1], depth_curve[,2], xout = seq(0, 27, by = 1))
depth.df <- data.frame(matrix(unlist(depth_curve), nrow=length(depth_curve[[1]]), byrow=F))
names(depth.df)=c("depth","score")
# ggplot()+geom_line(data = depth.df, aes(x=depth, y=score))
# ----------------- #

  
# ----------------- #
# manipulte data
# ----------------- #
# slide array
ndvi.slice <- eb_h
r <- raster(t(ndvi.slice), xmn=min(eb_lon), xmx=max(eb_lon), ymn=min(eb_lat), ymx=max(eb_lat), crs=CRS("+proj=longlat"))
r <- flip(r, direction='y')
#plot(r, xlim = c(min(eb_lon), max(eb_lon)), ylim=c(min(eb_lat), max(eb_lat)))
  
# clip to Eastern Bay
newr = clip_to_eb(r)
#plot(newr)
# ----------------- #
  
  
# ----------------- #
# create habitat scores based on curves
# ----------------- #
#### depth
# put scores on depth raster
# make a copy of existing raster, find where the oxygen matches that score, then replace values
depth_score = round(newr, digits = 0) # round to closet 0.01 to make score look up table
depth_score[depth_score %in% NaN] = NA

w.list = lapply(depth_score, function(x) {which(as.factor(depth.df$depth) %in% x)}) # need to make factor for some reason
is.na(w.list) <- lengths(w.list) == 0

w.score = unlist(w.list)
final.depth.score = depth.df$score[w.score]
newplot = newr
values(newplot) = final.depth.score

# change raster to spatial pixels data frame to data frame in order to use ggplot
r.spdf <- as(newplot, "SpatialPixelsDataFrame")
r.df <- as.data.frame(r.spdf)
#head(r.df)

p = ggplot()+
  geom_polygon(data=eb, aes(x=long, y=lat, group=group))+
  geom_tile(data=r.df, aes(x=x, y=y, fill = layer)) + 
  labs(x="Longitude", y="Latitude", title="Depth Score", fill="Score") + 
  scale_fill_gradient(low="blue", high="yellow")
p
ggsave("~/Oyster Recovery Partnership, Inc/ORP - Operations//Monitoring and Assessment/11_Habitat Modeling/Data/output/DepthMap.png", p)
