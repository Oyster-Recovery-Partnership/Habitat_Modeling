library(dplyr)

library(readxl)
cam_dat <- read_excel("Downloads/Camera_Grid_Join.xls")
names(cam_dat) = gsub(" ","",names(cam_dat))

min_max_norm <- function(x) {
  (x - min(x, na.rm=T)) / (max(x, na.rm=T) - min(x, na.rm=T))
}
cam_dat  = cam_dat %>% mutate(AverageScore = as.numeric(AverageScore),
                              AverageScore = min_max_norm(AverageScore))

ggplot() + 
  geom_point(data = cam_dat, aes(y = AverageScore, x = HSI)) + 
  geom_smooth(data = cam_dat, aes(y = AverageScore, x = HSI), method = "lm") + 
  theme_bw() + 
  theme(text = element_text(size = 20)) + 
  labs(x="Normalized GoPro Assessment Score", y = "Habitat Suitability Index")
 
#devtools::install_github("deepankardatta/blandr")
library(blandr)
blandr.output.text(cam_dat$HSI, cam_dat$AverageScore, sig.level=0.95 )
p = blandr.draw(cam_dat$HSI, cam_dat$AverageScore)
p + theme_bw() + 
  theme(text = element_text(size = 15))

# library(broom)
# m <- lm(SPI_scores$norm_PT_score ~ SPI_scores$norm_SPI_score)
# df <- augment(m)
# ggplot(df, aes(x = .fitted, y = .resid)) + geom_point()
