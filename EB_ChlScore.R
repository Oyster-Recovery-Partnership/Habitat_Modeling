# ----------------- #
# model habitat suitability for oysters in Eastern Bay
# chl a 
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
# make chl a score curve and look up table
chl_curve = as.data.frame(cbind(c(0, 12, 50), c(0, 0.75, 1)))
chl_curve = approx(chl_curve[,1], chl_curve[,2], xout = seq(0, 50, by = 0.1))
chl.df <- data.frame(matrix(unlist(chl_curve), nrow=length(chl_curve[[1]]), byrow=F))
names(chl.df)=c("chl","score")
# ggplot()+geom_line(data = chl.df, aes(x=chl, y=score))
# ----------------- #


# ----------------- #
# run through look for all months for all years
# ----------------- #
chl.months = matrix(data = NA, nrow = 120, ncol = 1760)

for(a in 1:120){
  # ----------------- #
  # manipulte data
  # ----------------- #
  # slide array
  ndvi.slice <- eb_chla[, , a]
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
  #### chl a 
  # put scores on chl raster
  # make a copy of existing raster, find where the oxygen matches that score, then replace values
  chl_score = round(newr, digits = 1) # round to closet 0.01 to make score look up table
  chl_score[chl_score %in% NaN] = NA
  
  w.list = lapply(chl_score, function(x) {which(as.factor(chl.df$chl) %in% x)}) # need to make factor for some reason
  is.na(w.list) <- lengths(w.list) == 0
  
  w.score = unlist(w.list)
  chl.score = chl.df$score[w.score]
  
  chl.months[a,] = chl.score
  # ----------------- #
}

final.chl.score = apply(chl.months, 2, mean, na.rm=TRUE)
newplot = newr
values(newplot) = final.chl.score

# change raster to spatial pixels data frame to data frame in order to use ggplot
r.spdf <- as(newplot, "SpatialPixelsDataFrame")
r.df <- as.data.frame(r.spdf)
#head(r.df)

p = ggplot()+
  geom_polygon(data=eb, aes(x=long, y=lat, group=group))+
  geom_tile(data=r.df, aes(x=x, y=y, fill = layer)) + 
  labs(x="Longitude", y="Latitude", title="Chlorophyll a Score", fill="Score") + 
  scale_fill_gradient(low="blue", high="yellow")
p
ggsave("~/Oyster Recovery Partnership, Inc/ORP - Operations//Monitoring and Assessment/11_Habitat Modeling/Data/output/ChlaMap.png", p)
