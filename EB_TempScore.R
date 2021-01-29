# ----------------- #
# model habitat suitability for oysters in Eastern Bay
# mean temperature
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
# make salinity score curve and look up table
temp_curve = as.data.frame(cbind(c(-2,5,10,20,30,33,36), c(0,0.1,0.5,1,1,0.25,0)))
temp_curve = approx(temp_curve[,1], temp_curve[,2], xout = seq(-2, 36, by = 0.1))
temp.df <- data.frame(matrix(unlist(temp_curve), nrow=length(temp_curve[[1]]), byrow=F))
names(temp.df)=c("temp","score")
#ggplot()+geom_line(data = temp.df, aes(x=temp, y=score))
# ----------------- #


# ----------------- #
# run through look for all months for all years
# ----------------- #
temp.months = matrix(data = NA, nrow = 120, ncol = 1760)

for(a in 1:120){
  # ----------------- #
  # manipulte data
  # ----------------- #
  # slide array
  ndvi.slice <- eb_temp[, , a]
  r <- raster(t(ndvi.slice), xmn=min(eb_lon), xmx=max(eb_lon), ymn=min(eb_lat), ymx=max(eb_lat), crs=CRS("+proj=longlat"))
  r <- flip(r, direction='y')
  #plot(r, xlim = c(min(eb_lon), max(eb_lon)), ylim=c(min(eb_lat), max(eb_lat)))
  
  # clip to Eastern Bay
  newr = clip_to_eb(r)
  # plot(newr)
  # ----------------- #
  
  
  # ----------------- #
  # create habitat scores based on curves
  # ----------------- #
  #### mean temp
  # put scores on temp raster
  # make a copy of existing raster, find where the temp matches that score, then replace values
  temp_score = round(newr, digits = 1) # round to closet 0.01 to make score look up table
  temp_score[temp_score %in% NaN] = NA
  
  w.list = lapply(temp_score, function(x) {which(as.factor(temp.df$temp) %in% x)}) # need to make factor for some reason
  is.na(w.list) <- lengths(w.list) == 0
  
  w.score = unlist(w.list)
  temp.score = temp.df$score[w.score]
  
  temp.months[a,] = temp.score
  # ----------------- #
}

final.temp.score = apply(temp.months, 2, mean, na.rm=TRUE)
newplot = newr
values(newplot) = final.temp.score
# plot(newplot)

# change raster to spatial pixels data frame to data frame in order to use ggplot
r.spdf <- as(newplot, "SpatialPixelsDataFrame")
r.df <- as.data.frame(r.spdf)
# head(r.df)

p = ggplot()+
  geom_polygon(data=eb, aes(x=long, y=lat, group=group))+
  geom_tile(data=r.df, aes(x=x, y=y, fill = V1)) + 
  labs(x="Longitude", y="Latitude", title="Mean Temperature Score", fill="Score") + 
  scale_fill_gradient(low="blue", high="yellow")
p
ggsave("~/Oyster Recovery Partnership, Inc/ORP - Operations//Monitoring and Assessment/11_Habitat Modeling/Data/output/MeanTempMap.png", p)
