# ----------------- #
# model habitat suitability for oysters in Eastern Bay
# inorganic suspended solids (Kg/m3)
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
iss_curve = as.data.frame(cbind(c(-1, 0.5, 5, 121), c(1, 0.5, 0, 0)))
iss_curve = approx(iss_curve[,1], iss_curve[,2], xout = seq(-1, 121, by = 0.1))
iss.df <- data.frame(matrix(unlist(iss_curve), nrow=length(iss_curve[[1]]), byrow=F))
names(iss.df)=c("iss","score")
# ggplot()+geom_line(data = iss.df, aes(x=iss, y=score))
# ----------------- #


# ----------------- #
# run through look for all months for all years
# ----------------- #
iss.months = matrix(data = NA, nrow = 120, ncol = 1760)

# convert kg to g
eb_iss2 = eb_iss * 1000

for(a in 1:120){
  # ----------------- #
  # manipulte data
  # ----------------- #
  # slide array
  ndvi.slice <- eb_iss2[, , a]
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
  #### inorganic suspended solids
  # put scores on iss raster
  # make a copy of existing raster, find where the oxygen matches that score, then replace values
  iss_score = round(newr, digits = 1) # round to closet 1 to make score look up table
  iss_score[iss_score %in% NaN] = NA
  
  w.list = lapply(iss_score, function(x) {which(as.factor(iss.df$iss) %in% x)}) # need to make factor for some reason
  is.na(w.list) <- lengths(w.list) == 0
  
  w.score = unlist(w.list)
  iss.score = iss.df$score[w.score]
  
  iss.months[a,] = iss.score
  # ----------------- #
}

final.iss.score = apply(iss.months, 2, mean, na.rm=TRUE)
newplot = newr
values(newplot) = final.iss.score

# change raster to spatial pixels data frame to data frame in order to use ggplot
r.spdf <- as(newplot, "SpatialPixelsDataFrame")
r.df <- as.data.frame(r.spdf)
#head(r.df)

p = ggplot()+
  geom_polygon(data=eb, aes(x=long, y=lat, group=group))+
  geom_tile(data=r.df, aes(x=x, y=y, fill = layer)) + 
  labs(x="Longitude", y="Latitude", title="Inorganic Suspended Solids Score", fill="Score") + 
  scale_fill_gradient(low="blue", high="yellow")
p
ggsave("~/Oyster Recovery Partnership, Inc/ORP - Operations//Monitoring and Assessment/11_Habitat Modeling/Data/output/ISSMap.png", p)
