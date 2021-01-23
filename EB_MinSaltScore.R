# ----------------- #
# model habitat suitability for oysters in Eastern Bay
# minimum salinity score 
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
min_salt_curve = as.data.frame(cbind(c(0,2,4,8,20), c(0,0,0.05,1,1)))
min_salt_curve = approx(min_salt_curve[,1], min_salt_curve[,2], xout = seq(0, 20, by = 0.1))
salt.df <- data.frame(matrix(unlist(min_salt_curve), nrow=length(min_salt_curve[[1]]), byrow=F))
names(salt.df)=c("salinity","score")
# ----------------- #


# ----------------- #
# run through look for all months for all years
# ----------------- #
salt.months = matrix(data = NA, nrow = 120, ncol = 1760)

for(a in 1:120){
  # ----------------- #
  # manipulte data
  # ----------------- #
  # slide array
  ndvi.slice <- eb_salt_min[, , a]
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
  #### minimum salinity 
  # put scores on salinity raster
  # make a copy of existing raster, find where the salinity matches that score, then replace values
  min_salt_score = round(newr, digits = 1) # round to closet 0.01 to make score look up table
  min_salt_score[min_salt_score %in% NaN] = NA
  
  w.list = lapply(min_salt_score, function(x) {which(as.factor(salt.df$salinity) %in% x)}) # need to make factor for some reason
  is.na(w.list) <- lengths(w.list) == 0
  
  w.score = unlist(w.list)
  salt.score = salt.df$score[w.score]
  values(min_salt_score) = salt.score
  
  salt.months[a,] = salt.score
  # ----------------- #
}

final.salt.score = apply(salt.months, 2, mean, na..rm=TRUE)
newplot = newr
values(newplot) = final.salt.score

# change raster to spatial pixels data frame to data frame in order to use ggplot
r.spdf <- as(newplot, "SpatialPixelsDataFrame")
r.df <- as.data.frame(r.spdf)
#head(r.df)

ggplot()+
  geom_polygon(data=eb, aes(x=long, y=lat, group=group))+
  geom_tile(data=r.df, aes(x=x, y=y, fill = layer)) + 
  labs(x="Longitude", y="Latitude", title="Minimum Salinity Score", fill="Score") + 
  scale_fill_gradient(low="blue", high="yellow")
