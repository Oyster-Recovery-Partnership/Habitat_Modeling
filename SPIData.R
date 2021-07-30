# summarize SPI data

library(readr)
library(ggplot2)
library(dplyr)

SPI_scores <- read_csv("Downloads/SPI_scores.csv")
SPI_scores = SPI_scores[,c(1,2)]
names(SPI_scores) = c("Photo","SPI_score")
SPI_scores$PT_score = as.numeric(substr(SPI_scores$Photo, 2, 2))

min_max_norm <- function(x) {
  (x - min(x, na.rm=T)) / (max(x, na.rm=T) - min(x, na.rm=T))
}
SPI_scores$norm_PT_score = min_max_norm(SPI_scores$PT_score)
SPI_scores$norm_SPI_score = min_max_norm(SPI_scores$SPI_score)

pen = read_excel("Downloads/SPIanalysis_Penetration.xlsx")
names(pen) = c("ID","left_penetration_cm","right_penetration_cm","AvgPen")
pen$AvgPen = (pen$left_penetration_cm + pen$right_penetration_cm)/2
max(pen$AvgPen)
min(pen$AvgPen)
median(pen$AvgPen)

ggplot() + 
  geom_point(data = SPI_scores, aes(x = norm_PT_score, y =norm_SPI_score)) + 
  geom_smooth(data = SPI_scores, aes(x = norm_PT_score, y =norm_SPI_score), method = "lm") + 
  theme_bw() + 
  theme(text = element_text(size = 20)) + 
  labs(x="Normalized Patent Tong Score", y = "Normalized SPI Score")
m <- lm(norm_PT_score ~ norm_SPI_score, data = SPI_scores)
m
#devtools::install_github("deepankardatta/blandr")
library(blandr)
blandr.output.text(SPI_scores$norm_PT_score, SPI_scores$norm_SPI_score, sig.level=0.95 )
p = blandr.draw(SPI_scores$norm_PT_score, SPI_scores$norm_SPI_score)
p + theme_bw() + 
  theme(text = element_text(size = 15))
  
SPIdata <- read_excel("Downloads/SPIdata.xlsx")
pen = left_join(dplyr::select(SPIdata, ID, PictureNum), pen, by="ID") %>%
  rename(Photo = PictureNum)
SPI_scores2 = left_join(SPI_scores, pen, by="Photo") %>%
  dplyr::select(Photo, norm_PT_score, norm_SPI_score, AvgPen)
