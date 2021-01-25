# ----------------- #
# model habitat suitability for oysters in Eastern Bay
# min. dissolved oxygen (mmol/m3 in ChesROMS)
# need to convert to mg/l 
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
# 40 % sat = 3.3 mg/L (since Cho 2012 was in % sat. and modeling after his score)
# 73 % sat = 6.03 mg/L
o2_curve = as.data.frame(cbind(c(0,3.3,6.03,14), c(0,0,1,1)))
o2_curve = approx(o2_curve[,1], o2_curve[,2], xout = seq(0, 14, by = 0.01))
o2.df <- data.frame(matrix(unlist(o2_curve), nrow=length(o2_curve[[1]]), byrow=F))
names(o2.df)=c("oxygen","score")
# ggplot()+geom_line(data = o2.df, aes(x=oxygen, y=score))
# ----------------- #


# ----------------- #
# run through look for all months for all years
# ----------------- #
o2.months = matrix(data = NA, nrow = 120, ncol = 1760)

# convert mmol/m3 to mg/L
# O2 has molar mass 32 g/mol (the molecular weight)
# So 1 µmol = 32 µg = 0.032 mg
eb_o2_min2 = eb_o2_min*0.032

for(a in 1:120){
  # ----------------- #
  # manipulte data
  # ----------------- #
  # slide array
  ndvi.slice <- eb_o2_min2[, , a]
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
  #### min DO
  # put scores on o2 raster
  # make a copy of existing raster, find where the oxygen matches that score, then replace values
  o2_score = round(newr, digits = 2) # round to closet 0.01 to make score look up table
  o2_score[o2_score %in% NaN] = NA
  
  w.list = lapply(o2_score, function(x) {which(as.factor(o2.df$oxygen) %in% x)}) # need to make factor for some reason
  is.na(w.list) <- lengths(w.list) == 0
  
  w.score = unlist(w.list)
  o2.score = o2.df$score[w.score]
  
  o2.months[a,] = o2.score
  # ----------------- #
}

final.o2.score = apply(o2.months, 2, mean, na.rm=TRUE)
newplot = newr
values(newplot) = final.o2.score

# change raster to spatial pixels data frame to data frame in order to use ggplot
r.spdf <- as(newplot, "SpatialPixelsDataFrame")
r.df <- as.data.frame(r.spdf)
#head(r.df)

p = ggplot()+
  geom_polygon(data=eb, aes(x=long, y=lat, group=group))+
  geom_tile(data=r.df, aes(x=x, y=y, fill = layer)) + 
  labs(x="Longitude", y="Latitude", title="Minimum Dissolved Oxygen Score", fill="Score") + 
  scale_fill_gradient(low="blue", high="yellow")
p
ggsave("~/Oyster Recovery Partnership, Inc/ORP - Operations//Monitoring and Assessment/11_Habitat Modeling/Data/output/MinDOMap.png", p)
