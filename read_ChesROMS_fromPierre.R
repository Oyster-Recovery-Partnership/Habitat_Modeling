# read the ChesROMS output from Pierre's higher resolution model
# the data are from Eastern Bay and Manokin
# from Jan 2009 to Dec 2018, there are 120 months
# spatially the grid is 40x44
#
# from Pierre: 
# Note that I'm regularly making improvements to this new 600m-resolution model.
# The model results are reasonably good along the "main stem" of the Bay 
# (based on comparisons with in situ data from the Chesapeake Bay Program), 
# but I haven't had time (yet) to evaluate the results in nearshore regions 
# such as the ones you are focusing on.
# Of all the model variables, I would expect the temperature and salinity to be the most accurate.
# The model's dissolved oxygen is possibly biased low, and ISS is possibly biased high.
# Finally, we need to keep in mind that the 600m resolution is a bit coarse when 
# focusing on such small embayments (as you can infer from the size of the "pixels" 
# when plotting the model results).
#
# example on how to work with netcdfs in R: https://rpubs.com/boyerag/297592


# load packages
library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting

# set directories
dir.in = "~/Oyster Recovery Partnership, Inc/ORP - Operations/Monitoring and Assessment/11_Habitat Modeling/Data/rawdata/ChesROMS_model/"
#dir.out = "~/Oyster Recovery Partnership, Inc/ORP - Operations/Monitoring and Assessment/11_Habitat Modeling/Data/output/"

# load data
eb = nc_open(paste(dir.in, "easternbay2009_2018_v20200919.nc", sep=""))
man = nc_open(paste(dir.in, "manokin2009_2018_v20200919.nc", sep=""))
#print(eb)
#print(man)
#names(eb$var)

# mask = land vs water
# h = bathymetry
# iss = inorganic suspended solids
# Cs_r = "S-coordinate stretching curves at RHO-points" ;
# hc = "S-coordinate parameter, critical depth" ;
# zeta = sea surface height

eb_temp <- ncvar_get(eb, "temp_ave")
eb_salt_max <- ncvar_get(eb, "salt_max")
eb_salt_min <- ncvar_get(eb, "salt_min")
eb_o2_max <- ncvar_get(eb, "o2_max")
eb_o2_min <- ncvar_get(eb, "o2_min")
eb_mask <- ncvar_get(eb, "mask_rho")
eb_lat <- ncvar_get(eb, "lat_rho")
eb_lon <- ncvar_get(eb, "lon_rho")
eb_Cs_r <- ncvar_get(eb, "Cs_r")
eb_chla <- ncvar_get(eb, "chla_ave")
eb_h <- ncvar_get(eb, "h")
eb_hc <- ncvar_get(eb, "hc")
eb_iss <- ncvar_get(eb, "iss_ave")
eb_zeta <- ncvar_get(eb, "zeta")

man_temp <- ncvar_get(man, "temp_ave")
man_salt_max <- ncvar_get(man, "salt_max")
man_salt_min <- ncvar_get(man, "salt_min")
man_o2_max <- ncvar_get(man, "o2_max")
man_o2_min <- ncvar_get(man, "o2_min")
man_mask <- ncvar_get(man, "mask_rho")
man_lat <- ncvar_get(man, "lat_rho")
man_lon <- ncvar_get(man, "lon_rho")
man_Cs_r <- ncvar_get(man, "Cs_r")
man_chla <- ncvar_get(man, "chla_ave")
man_h <- ncvar_get(man, "h")
man_hc <- ncvar_get(man, "hc")
man_iss <- ncvar_get(man, "iss_ave")
man_zeta <- ncvar_get(man, "zeta")

# close data once done pulling the variables
nc_close(eb)
nc_close(man)

# # slide array
# x = eb_salt_max
# ndvi.slice <- x[, ,120] 
# dim(ndvi.slice)
# r <- raster(t(ndvi.slice), xmn=min(eb_lon), xmx=max(eb_lon), ymn=min(eb_lat), ymx=max(eb_lat), 
#             crs=CRS("+proj=longlat"))
# r <- flip(r, direction='y')
# plot(r, xlim = c(min(eb_lon), max(eb_lon)), ylim=c(min(eb_lat), max(eb_lat)))




