# ----------- # 
# Find the spatial scale at which we should be modeling Manokin
# ----------- #

# ----------- #
# load packages
# ----------- #
require(dplyr)
require(ggplot2)
library(gstat)
library(sp)
library(raster)
require(rgdal)
library(sf)
library(PBSmapping) 
# ----------- #

# ----------- #
# load data
# ----------- #
# Manokin grid
source("U:/O365_NEW_ Operations/Monitoring and Assessment/11_Habitat Modeling/Data/code/Manokin_grid.R")

# # reefs in Manokin

reefs <- readOGR(dsn="G:/1.0 Restoration and Monitoring/8.0 Habitat modeling/NFWF Habitat/1.0 GIS",layer="Manokin_Blueprint_SHP")
reefs = spTransform(reefs, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# fgdb <- "G:/Tributary Blueprints/Manokin_River_Oyster_Restoration_BluePrint_GeoDatabase_02132020/Manokin_River_Oyster_Restoration_BluePrint_GeoDatabase_02132020.gdb"
# subset(ogrDrivers(), grepl("GDB", name))
# fc_list <- ogrListLayers(fgdb)
# print(fc_list)
# #reefs <- readOGR(dsn=fgdb,layer="DRAFT_Manokin_River_Oyster_Restoration_Blueprint_02132020")
# reefs <- sf::st_read(dsn = fgdb, layer = "DRAFT_Manokin_River_Oyster_Restoration_Blueprint_02132020")
# str(reefs)
# plot(reefs)
# 
# test = reefs[3]
# st_geometry(test)
# glimpse(test)
# test = st_transform(test, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# #test_sp <- as(test, "Spatial")
# test2 = as_Spatial(test, cast = TRUE, IDs = test$Site_ID)
# ggplot() + geom_polygon(data = test, aes(x = long, y = lat, group = group), fill = "lightblue")
# ----------- #

# ----------- #
# define which have reefs (>50% coverage in each square)
# ----------- #
#x = area.poly(intersect(Grid200, reefs)) 

#Ints <- Grid600 + reefs  ## Shorthand for union(p1, p2)

x = raster::intersect(Grid600, reefs)
plot(Grid600[!Grid600$layer %in% x$layer,])
plot(Grid600[Grid600$layer %in% x$layer,], add = TRUE, col = "lightblue")
plot(reefs, add = TRUE, col = "purple")

xx = raster::intersect(Grid200, reefs)
plot(Grid200[!Grid200$layer %in% xx$layer,])
plot(Grid200[Grid200$layer %in% xx$layer,], add = TRUE, col = "lightblue")
plot(reefs, add = TRUE, col = "purple")
# ----------- #

# ----------- #
# run analysis
# example: https://www.r-bloggers.com/exploring-spatial-autocorrelation-in-r/
# https://gwenantell.com/autocorrelation-in-ecology/ 
# https://gwenantell.com/analyses-for-spatial-autocorrelation/ 
# ----------- #
# create autocorrelated grid
gridDim <- 60
xy <- expand.grid(x=1:gridDim, y=1:gridDim)

# Variogram model, with defined sill and range
varioMod <- vgm(psill=0.05, range=10, model='Exp')

# Set up an additional variable from simple kriging
zDummy <- gstat(formula= z~1, 
                locations = ~x+y, 
                dummy=TRUE, 
                beta=1, 
                model=varioMod, 
                nmax=20)

# Generate 2 randomly autocorrelated predictor data fields
set.seed(3)
xyz <- predict(zDummy, newdata=HexPts, nsim=2)

# Generate an autocorrelated response variable:
# Deterministic on predictors, with normal error
e <- rnorm(nrow(xyz), sd=.5)
xyz$resp <- .7*xyz$sim1 + 1.4*xyz$sim2 + e

test <- xyz
gridded(test) = ~x+y
spplot(test[1])

# variogram
spdf <- SpatialPointsDataFrame(xy, data=xyz)
vario <- variogram(resp~1, data=xyz, locations= ~x+y, 
                   cutoff=0.5*gridDim)
plot(vario$dist, vario$gamma)

# decrease autocorrelation
rng <- 15
mxPosition <- floor(gridDim/rng)
keepPosition <- rng*(1:mxPosition)
keepX <- which(xy$x %in% keepPosition)
keepY <- which(xy$y %in% keepPosition)
bigGrain <- xyz[intersect(keepX, keepY),]

# regression
fmla <- as.formula(resp ~ sim1 + sim2)
lmBig <- lm(fmla, data = bigGrain)
summary(lmBig)

lmFine <- lm(fmla, data = xyz)
summary(lmFine)

# compare coarse and fine 
cornrPosition <- 1:length(keepPosition)
keepX <- which(xy$x %in% cornrPosition)
keepY <- which(xy$y %in% cornrPosition)
cornrFine <- xyz[intersect(keepX, keepY),]

lmCornr <- lm(fmla, data = cornrFine)
summary(lmCornr)
# ----------- #
