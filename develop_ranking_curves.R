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
dir.out = "~/Oyster Recovery Partnership, Inc/ORP - Operations//Monitoring and Assessment/11_Habitat Modeling/Data/output/variable_curves"
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
                                  c(rep(1,9),
                                    0.7, 1, 0.5, 0.5,
                                    1,0.3),
                                  # c(1, 0.9, 0.8, 0.8,  
                                  #   0.75, 0.7, 0.7, 0.7,
                                  #   0.6, 0.5, 0.3, 0.2,
                                  #   0.1, 0.1, -0.01),
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
  theme_bw() + labs(x = "CMECS Classification", y = "Habitat Rank") + 
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle = 20, hjust = 1)) 
p 
ggsave(paste(dir.out,"cmecs_curve.png",sep="/"), p)
# -------------- #


# -------------- #
# TEMPERATURE 
# -------------- #
# -2°C to 36°C

# mean summer temp
# mean winter temp
# rate of temp change
# -------------- #


# -------------- #
# SALINITY
# -------------- #
# salinity extremes
# 5 - 40, optimal 14 - 28
salt_curve = as.data.frame(cbind(c(seq(0, 45, by = 1)),
                                    c(rep(0.25, 5),
                                      rep(0.5, 9),
                                      rep(1, 15),
                                      rep(0.5, 12),
                                      rep(0.25, 5))))
names(salt_curve)=c("salinity","score")

p = ggplot(data = salt_curve, aes(x = salinity, y = score)) + 
  geom_bar(stat = "identity", width = 1) + 
  theme_bw() + labs(x = "Salinity", y = "Habitat Rank") + 
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle = 20, hjust = 1)) 
p 
ggsave(paste(dir.out,"salt_curve.png",sep="/"), p)
# -------------- #


# -------------- #
# DEPTH
# -------------- #
# 6 - 30 feet

# -------------- #


# -------------- #
# CHL A
# -------------- #
# 	                         Spring Threshold       Summer Threshold
# Tidal - fresh	(0-0.4 ppt)  ≤ 14                   ≤ 12
# Oligohaline	(0.5-5 ppt)    ≤ 20.9                 ≤ 9.5
# Mesohaline (5.1-18 ppt)	   ≤ 6.2                  ≤ 7.7                           
# Polyhaline (18.1-30 ppt)	 ≤ 2.8                  ≤ 4.5

# -------------- #


# -------------- #
# O2
# -------------- #
# Open water	≥ 5 (mg/L)
# Deep water seasonal	≥ 3 (mg/L)
# Deep- channel seasonal	≥ 1 (mg/L)

# -------------- #

