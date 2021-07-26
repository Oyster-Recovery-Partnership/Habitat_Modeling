# 2015	26066.24	912.3184
# 2016	30199.82	1056.9937
# 2017	33457.33	1171.00655
# 2018	33635.01	1177.22535
# 2019	35749.49	1251.23215
# 2020	15904.97	556.67395
# 2021 (Q1-Q2)	12382.69	433.39415

dat = as.data.frame(cbind(c(2015:2021), 
                          c(912.3184,1056.9937,1171.00655,1177.22535,1251.23215,556.67395,433.39415)))
names(dat) = c("Year","Shell")

library(ggplot2)

ggplot(dat, aes(Year, Shell)) + 
  geom_bar(stat="identity",fill="cornflowerblue", col="black") + 
  theme_bw() + 
  labs(y="Shell (Tons)", title="SRA Shell Collected") + 
  theme(text = element_text(size=20)) + 
  scale_x_continuous(breaks = seq(2015,2021,1))

