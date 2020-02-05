# OPeNDAP for ChesROMS: http://thredds.comt.ioos.us/thredds/dodsC/noaa/ioos/comt/cb_hypoxia.CHESROMS_1termDO_surfsat.ncml.html
# ROMS discussion: https://www.myroms.org/forum/viewtopic.php?t=4126
# thredds R how to with ncdf4: https://github.com/ornldaac/thredds_opendap_r_max_temperature/blob/master/opendap_r_v1.Rmd 
# ncvar_get: https://www.rdocumentation.org/packages/ncdf4/versions/1.17/topics/ncvar_get

library(ncdf4)

dat_url = "http://thredds.comt.ioos.us/thredds/dodsC/noaa/ioos/comt/cb_hypoxia.CHESROMS_1termDO_surfsat.ncml"
dat = nc_open(dat_url)
dat

# ncvar_get(file,'variable', start, count), start and count are X-Y-Z-T (time is last)
# pull specific variables
dat_lats = ncvar_get(dat, "lat_rho")
dat_lons = ncvar_get(dat, "lon_rho")
dat_mask = ncvar_get(dat, "mask_rho")

i = matrix(ncol = 150, nrow = 100, data = 1:100)
j = matrix(ncol = 150, nrow = 100, data = c(rep(1,100),rep(2,100),rep(3,100),rep(4,100),rep(5,100),rep(6,100),rep(7,100),rep(8,100),rep(9,100),rep(10,100),
                                            rep(11,100),rep(12,100),rep(13,100),rep(14,100),rep(15,100),rep(16,100),rep(17,100),rep(18,100),rep(19,100),rep(20,100),
                                            rep(21,100),rep(22,100),rep(23,100),rep(24,100),rep(25,100),rep(26,100),rep(27,100),rep(28,100),rep(29,100),rep(30,100),
                                            rep(31,100),rep(32,100),rep(33,100),rep(34,100),rep(35,100),rep(36,100),rep(37,100),rep(38,100),rep(39,100),rep(40,100),
                                            rep(41,100),rep(42,100),rep(43,100),rep(44,100),rep(45,100),rep(46,100),rep(47,100),rep(48,100),rep(49,100),rep(50,100),
                                            rep(51,100),rep(52,100),rep(53,100),rep(54,100),rep(55,100),rep(56,100),rep(57,100),rep(58,100),rep(59,100),rep(60,100),
                                            rep(61,100),rep(62,100),rep(63,100),rep(64,100),rep(65,100),rep(66,100),rep(67,100),rep(68,100),rep(69,100),rep(70,100),
                                            rep(71,100),rep(72,100),rep(73,100),rep(74,100),rep(75,100),rep(76,100),rep(77,100),rep(78,100),rep(79,100),rep(80,100),
                                            rep(81,100),rep(82,100),rep(83,100),rep(84,100),rep(85,100),rep(86,100),rep(87,100),rep(88,100),rep(89,100),rep(90,100),
                                            rep(91,100),rep(92,100),rep(93,100),rep(94,100),rep(95,100),rep(96,100),rep(97,100),rep(98,100),rep(99,100),rep(100,100),
                                            rep(101,100),rep(102,100),rep(103,100),rep(104,100),rep(105,100),rep(106,100),rep(107,100),rep(108,100),rep(109,100),rep(110,100),
                                            rep(111,100),rep(112,100),rep(113,100),rep(114,100),rep(115,100),rep(116,100),rep(117,100),rep(118,100),rep(119,100),rep(120,100),
                                            rep(121,100),rep(122,100),rep(123,100),rep(124,100),rep(125,100),rep(126,100),rep(127,100),rep(128,100),rep(129,100),rep(130,100),
                                            rep(131,100),rep(132,100),rep(133,100),rep(134,100),rep(135,100),rep(136,100),rep(137,100),rep(138,100),rep(139,100),rep(140,100),
                                            rep(141,100),rep(142,100),rep(143,100),rep(144,100),rep(145,100),rep(146,100),rep(147,100),rep(148,100),rep(149,100),rep(150,100)))


i = as.vector(i)
j = as.vector(j)
dat_df = as.data.frame(cbind(as.vector(dat_lats), as.vector(dat_lons), as.vector(dat_mask), i , j))
names(dat_df) = c("lat", "lon", "mask","i","j")
coordinates(dat_df) <- dat_df[,c(2,1)] 
proj4string(dat_df) = proj4string(Manokin)

ggplot() + geom_point(data = dat_df, aes(x = lon, y = lat, col = mask)) +
  theme_bw()+ theme(text = element_text(size=20)) + labs(x = "Longitude", y = "Latitude")

ggplot() + geom_point(data = dat_df, aes(x = lon, y = lat, col = topo)) +
  theme_bw()+ theme(text = element_text(size=20)) + labs(x = "Longitude", y = "Latitude")

# run NFWF_Manokin.R
# ----------------- #
source("G:/1.0 Restoration and Monitoring/8.0 Habitat modeling/NFWF Habitat/2.0 Data/code/NFWF_Manokin.R")
rm(tribs,b)


ChesRoms_in_Manokin = raster::intersect(dat_df, Manokin)  
ChesRoms_in_Manokin = as.data.frame(ChesRoms_in_Manokin)

ggplot() +
  geom_polygon(data = Manokin, aes(x = long, y = lat, group = group), fill = "lightblue")+ 
  geom_point(data = ChesRoms_in_Manokin, aes(x=lon, y=lat), size = 5) + theme_bw() + 
  labs(x = "Longitude", y = "Latitude") + 
  theme(text = element_text(size=20))
