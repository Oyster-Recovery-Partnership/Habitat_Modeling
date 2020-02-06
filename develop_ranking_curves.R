# -------------- #
# define ranking curves for variables of interest
# -------------- #

# -------------- #
# load packages
# -------------- #
require(dplyr)
require(ggplot2)
require(rgdal)
require(sf)
# -------------- #

# -------------- #
# set dir
# -------------- #
dir.out = "U:/O365_NEW_ Operations/Monitoring and Assessment/11_Habitat Modeling/Data/output/variable_curves"
# -------------- #

# -------------- #
# BOTTOM TYPE
# -------------- #
# # load CMECS data
# fgdb <- "G:/Tributary Blueprints/Manokin_Oyster_Restoration_Blueprint_Geodatabase_09_30_2019/Manokin_River_Oyster_Restoration_BluePrint_GeoDatabase_09302019.gdb"
# 
# # List all feature classes in a file geodatabase
# subset(ogrDrivers(), grepl("GDB", name))
# fc_list <- ogrListLayers(fgdb)
# print(fc_list)
# 
# # Read the feature class
# cmecs <- st_read(dsn=fgdb,layer="FINAL_CMECS_Benthic_Habitat_Characterization_Manokin_River_Sanctuary_10172018")
# 
# # make dataframe
# cmecs_df = as.data.frame(cmecs)
# cmecs_bt = broom::tidy(cmecs, "Group_")

# classifications
cmecs_curve = as.data.frame(cbind(c("Bio_Shell_Reef",
                                    "Bio_Shell_Rubble",
                                    "Anth_Shell_Rubble",
                                    "Bio_Shell_Rubble_Sand",
                                    "Anth_Shell_Rubble_Sand", 
                                    "Bio_Shell_Reef_Mud",
                                    "Bio_Shell_Rubble_Mud", 
                                    "Anth_Shell_Rubble_Mud",
                                    "Sand_Shell",  
                                    "Sand",
                                    "MuddySand_Shell", 
                                    "MuddySand",
                                    "SandyMud",
                                    "Mud_Shell",
                                    "Mud"),
                                  c(1, 0.9, 0.8, 0.8,  
                                    0.75, 0.7, 0.7, 0.7,
                                    0.6, 0.5, 0.3, 0.2,
                                    0.1, 0.1, -0.01),
                                  c(seq(1,15,by=1))))
                                  #c("Biogenic_Oyster_Reef",
                                  #  "Biogenic_Oyster_Rubble",
                                  #  "Anthropogenic_Oyster_Rubble",
                                  #  "Sand",
                                  #  "Muddy_Sand",
                                  #  "Sandy_Mud",
                                  #  "Mud"),
                                  #c(1,0.9,0.8,0.4,0.2,0.1,0),
                                  #c(seq(1,7,by=1))))
names(cmecs_curve) = c("cmecs","ranks","orders")
cmecs_curve = mutate(cmecs_curve, cmecs = as.character(cmecs), ranks = as.numeric(as.character(ranks)), orders = as.numeric(orders))

# plot curve
p = ggplot(data = cmecs_curve, aes(x = reorder(cmecs, ranks), y = ranks)) + 
  geom_bar(stat = "identity", width = 1) + 
  geom_area(col = "darkgrey") + 
  theme_bw() + labs(x = "CMECS Classification", y = "Habitat Rank") + 
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle = 20, hjust = 1)) 
p 
ggsave(paste(dir.out,"cmecs_curve.png",sep="/"), p)
# -------------- #


# -------------- #
# TEMPERATURE 
# -------------- #
# mean summer temp
# mean winter temp
# rate of temp change
# -------------- #


# -------------- #
# SALINITY
# -------------- #
# salinity extremes
# -------------- #


# -------------- #
# DEPTH
# -------------- #
# -------------- #
