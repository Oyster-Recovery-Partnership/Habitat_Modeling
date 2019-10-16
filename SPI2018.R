# this script is to look at the SPI camera data from 2018

# directory
dir.in = "G:/1.0 Restoration and Monitoring/8.0 Habitat modeling/NFWF Habitat/1.0 GIS/"

# load packages
library(rgdal)
library(ggplot2)
library(broom)

# load data
PT = readOGR(paste(dir.in, "Sample coordinates", sep="/"), "SamplePoints16April")
SPI = readOGR(paste(dir.in, "Waypoints", sep="/"), "SPI_04_28")

# transform
SPI_df = data.frame(SPI)
PT_df = data.frame(PT)

# other
litChop = readOGR("G:/1.0 Restoration and Monitoring/3.0 Little Choptank/1.0 LC Pre_Construction_2018/1.0 GIS/Final Deliverables", "LC_sanc_bound_project")
litChop = spTransform(dat, CRS("+proj=longlat +datum=WGS84"))

# The input file geodatabase
fgdb <- "G:/Tributary Blueprints/Little_Choptank_Oyster_Restoration_Blueprint_Geodatabase_07_09_19/Little_Choptank_Oyster_Restoration_Blueprint_Geodatabase_07_09_2019.gdb"

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)
# "LC_NaturalOysterBars_UTM83m_merge"                   
# "LC_Yates_Bars"                                      
# "Little_Choptank_Sanctuary_Boundary"                  
# "Madison_Nav_Channel_150ft_Buffer"                   
# "LC_Nav_Aids_LtList_01_2012_250ft_Buffer_NOB"         
# "LC_marinas2011_250ft_Buffer_NOB"                    
# "Docks_2013"                                          
# "Implementation_2014_L_Choptank_BluePrint_02_19_2015"
# "MD_Grows_Oysters_02272014"                           
# "LC_ORES_and_Control_Sites_06082015"                 
# "LC_KPL_VERSAR_PTong_Merge_2012_IN_NOB"               
# "LC_DNR_PTong_2014"                                  
# "CBF_PLANTINGS_2015_L_Choptank_12282015"              
# "LC_MP_Intepolated_Mean_Salinity_April_Oct_2001_2006"
# "LC_MP_Intepolated_Mean_Bottom_DO_June_Aug_2001_2006" 
# "LC_CBP_WQ_Stations"                                 
# "ORP_PLANTINGS_2014_L_Choptank_01292016"              
# "MD_DNR_FallSurvey_Sites_2015"                       
# "ORP_PLANTINGS_2015_L_Choptank_03152016"              
# "ORP_PLANTINGS_2016_L_Choptank_10122016"             
# "CBF_PLANTINGS_2016_L_Choptank_10122016"              
# "ORP_CBF_PLANTINGS_2017_L_Choptank_12072017"         
# "St_Marys_DNR_PTong_2012"                             
# "ORP_CBF_PLANTINGS_2018_L_Choptank_12032018"         
# "Little_Choptank_BluePrint_07_09_2019"   

# Read the feature class
fc <- readOGR(dsn=fgdb,layer="Little_Choptank_BluePrint_07_09_2019")
fc = spTransform(fc, CRS("+proj=longlat +datum=WGS84"))

# Determine the FC extent, projection, and attribute information
summary(fc)

# View the feature class
plot(fc)

# plots
ggplot() + 
  geom_polygon(data = fc, aes(x = long, y = lat, group = group), fill = "lightgrey") + 
  geom_point(data = PT_df, aes(x = Longitude, y = Latitude), col = "black", pch = 16) + 
  geom_point(data = SPI_df, aes(x = Longitude, y = Latitude), col = "magenta", pch = 15) + 
  theme_bw() + 
  lims(x = c(-76.265,-76.225), y = c(38.522,38.5425))

 
