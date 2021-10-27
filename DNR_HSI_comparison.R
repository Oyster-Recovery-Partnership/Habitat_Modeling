library(readxl)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

# load data
#EB_PD <- read_excel("Oyster Recovery Partnership, Inc/ORP - Operations/Monitoring and Assessment/11_Habitat Modeling/Data/rawdata/DNR/Easterb Bay Survey 2020 A1 PDredge.xlsx")
EB_PT_A1 <- read_excel("Oyster Recovery Partnership, Inc/ORP - Operations/Monitoring and Assessment/11_Habitat Modeling/Data/rawdata/DNR/EasternBay PT 2020 potential restoration sites.xls")
EB_PT_A2 <- read_excel("Oyster Recovery Partnership, Inc/ORP - Operations/Monitoring and Assessment/11_Habitat Modeling/Data/rawdata/DNR/EasternBay PT 2020 potential restoration sites.xls", sheet=2)
HSI <- read_excel("Oyster Recovery Partnership, Inc/ORP - Operations/Monitoring and Assessment/11_Habitat Modeling/Data/output/habitat_modeling/HSI_15x.xlsx")
HSI_sub = HSI %>% dplyr::filter(lon < (-76.253), lon > (-76.266), lat > 38.8625, lat < 38.8775)
  
#names(EB_PD) = gsub(" ","",names(EB_PD))
names(EB_PT_A1) = gsub(" ","",names(EB_PD))
names(EB_PT_A2) = gsub(" ","",names(EB_PD))

#EB_PD = EB_PD %>% dplyr::select(xcoord, ycoord, TotalShellVolume) %>% rename(lat=ycoord, lon=xcoord)
EB_PT_A1 = EB_PT_A1 %>% dplyr::select(xcoord, ycoord, TotalShellVolume) %>% rename(lat=ycoord, lon=xcoord)
EB_PT_A2 = EB_PT_A2 %>% dplyr::select(xcoord, ycoord, TotalShellVolume) %>% rename(lat=ycoord, lon=xcoord)

ggplot() + 
  #geom_point(data = EB_PD, aes(x=lon, y=lat, col=TotalShellVolume)) + 
  geom_point(data = EB_PT_A1, aes(x=lon, y=lat, col=TotalShellVolume)) + 
  geom_point(data = EB_PT_A2, aes(x=lon, y=lat, col=TotalShellVolume))  +
  geom_point(data = HSI_sub, aes(x=lon, y=lat), col = "magenta", pch = 1)

ggplot() + 
  geom_point(data = HSI_sub, aes(x=lon, y=lat, col = HSI)) +
  scale_color_gradientn(colours = rainbow(6)) +
  #geom_point(data = EB_PD, aes(x=lon, y=lat, size=TotalShellVolume)) 
  geom_point(data = EB_PT_A1, aes(x=lon, y=lat, size=TotalShellVolume)) + 
  geom_point(data = EB_PT_A2, aes(x=lon, y=lat, size=TotalShellVolume)) 
  
# pull HSI cells where DNR samples occurred
library(RANN)
#PD_coords <- dplyr::select(EB_PD,lon,lat)
PT_A1_coords <- dplyr::select(EB_PT_A1,lon,lat)
PT_A2_coords <- dplyr::select(EB_PT_A2,lon,lat)
HSIs_coords <- dplyr::select(HSI_sub,lon,lat)

# fast nearest neighbour search
# closest <- nn2(HSIs_coords, PD_coords, k = 1, searchtype = "radius", radius = 0.001)
# closest <- sapply(closest, cbind) %>% as_tibble
# PD_HSIs = HSI_sub[closest$nn.idx,]

closest1 <- nn2(HSIs_coords, PT_A1_coords, k = 1, searchtype = "radius", radius = 0.001)
closest1 <- sapply(closest1, cbind) %>% as_tibble
PT_A1_HSIs = HSI_sub[closest1$nn.idx,]

closest2 <- nn2(HSIs_coords, PT_A2_coords, k = 1, searchtype = "radius", radius = 0.001)
closest2 <- sapply(closest2, cbind) %>% as_tibble
PT_A2_HSIs = HSI_sub[closest2$nn.idx,]

# 
# ggplot() +   geom_point(data = EB_PD, aes(x=lon, y=lat, size=TotalShellVolume)) +
#   geom_point(data = PD_HSIs, aes(x=lon, y=lat, col=HSI)) +
#   scale_color_gradientn(colours = rainbow(2)) + theme_bw()

ggplot() +   geom_point(data = EB_PT_A1, aes(x=lon, y=lat, size=TotalShellVolume)) +
  geom_point(data = PT_A1_HSIs, aes(x=lon, y=lat, col=HSI)) +
  scale_color_gradientn(colours = rainbow(2)) + theme_bw()

ggplot() +   geom_point(data = EB_PT_A2, aes(x=lon, y=lat, size=TotalShellVolume)) +
  geom_point(data = PT_A2_HSIs, aes(x=lon, y=lat, col=HSI)) +
  scale_color_gradientn(colours = rainbow(2)) + theme_bw()

# comparison
library(blandr)
normalized <- function(x) {(x-min(x))/(max(x)-min(x))}

blandr.output.text(normalized(EB_PT_A1$TotalShellVolume), PT_A1_HSIs$HSI, sig.level=0.95 )
p = blandr.draw(EB_PT_A1$TotalShellVolume, PT_A1_HSIs$HSI)
p + theme_bw() + 
  theme(text = element_text(size = 15))

blandr.output.text(normalized(EB_PT_A2$TotalShellVolume), PT_A2_HSIs$HSI, sig.level=0.95 )
p = blandr.draw(EB_PT_A2$TotalShellVolume, PT_A2_HSIs$HSI)
p + theme_bw() + 
  theme(text = element_text(size = 15))


  
  




