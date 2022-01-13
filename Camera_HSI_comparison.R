# compare HSI scores to GoPro camera scores

# --------- #
# load packages
# --------- #
library(dplyr)
library(ggplot2)
library(readxl)
library(blandr) #B-A
# --------- #

# --------- #
# load data
# --------- #
cam_dat <-  read_excel("Oyster Recovery Partnership, Inc/ORP - Operations/Monitoring and Assessment/11_Habitat Modeling/Data/rawdata/EasternBay/Camera_Grid_Join.xls")
names(cam_dat) = gsub(" ","",names(cam_dat))

min_max_norm <- function(x) {
  (x - min(x, na.rm=T)) / (max(x, na.rm=T) - min(x, na.rm=T))
}
cam_dat  = cam_dat %>% 
  mutate(AverageScore1 = as.numeric(AverageScore),
         AverageScore = min_max_norm(AverageScore1))
# --------- #

# --------- #
# plot
# --------- #
p1 = ggplot() + 
  geom_point(data = cam_dat, aes(y = AverageScore, x = HSI), size=3) + 
  geom_smooth(data = cam_dat, aes(y = AverageScore, x = HSI), method = "lm") + 
  theme_bw() + 
  theme(text = element_text(size = 20)) + 
  geom_abline(intercept = 0, linetype="dashed", col="grey", size = 2) +
  labs(y="Normalized GoPro Assessment Score", x = "Habitat Suitability Index")
p1
ggsave("Oyster Recovery Partnership, Inc/ORP - Operations/Monitoring and Assessment/11_Habitat Modeling/Data/output/Camera_HSI_regression.png",p1) 

# linear regression, correlation
fit = lm(AverageScore ~ HSI, cam_dat)
summary(fit)
anova(fit)
plot(residuals(fit))
cor.test(cam_dat$AverageScore,cam_dat$HSI)

# Bland - Altman
#devtools::install_github("deepankardatta/blandr")
blandr.output.text(cam_dat$HSI, cam_dat$AverageScore, sig.level=0.95 )
p2 = blandr.draw(cam_dat$HSI, cam_dat$AverageScore)
p2 + theme_bw() + 
  theme(text = element_text(size = 15))
ggsave("Oyster Recovery Partnership, Inc/ORP - Operations/Monitoring and Assessment/11_Habitat Modeling/Data/output/Camera_HSI_BA.png",p2) 

# library(broom)
# m <- lm(SPI_scores$norm_PT_score ~ SPI_scores$norm_SPI_score)
# df <- augment(m)
# ggplot(df, aes(x = .fitted, y = .resid)) + geom_point()
# --------- #

