# read the ChesROMS output from Pierre's higher resolution model
# the data are from Eastern Bay and Manokin
# from 2009 to 2018

# load packages
library(ncdf4)
library(ggplot2)

# set directories
dir.in = "~/Oyster Recovery Partnership, Inc/ORP - Operations/Monitoring and Assessment/11_Habitat Modeling/Data/rawdata/ChesROMS_model/"
dir.out = "~/Oyster Recovery Partnership, Inc/ORP - Operations/Monitoring and Assessment/11_Habitat Modeling/Data/output/"

# load data
eb = nc_open(paste(dir.in, "easternbay2009_2018_v20200919.nc", sep=""))
man = nc_open(paste(dir.in, "manokin2009_2018_v20200919.nc", sep=""))
#print(eb)
#print(man)
#names(eb$var)

# mask = land vs water
# h = bathymetry

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

