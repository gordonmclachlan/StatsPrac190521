library(tidyverse)
library(readxl)

#Import
CRFdata <- read_excel("../Data/CRFcorestudy.xlsx", skip=3)

#Tidy data, restructure,
str(CRFdata)
CRFdata1 <- gather(CRFdata, key=Suction, value = grams_N, c(5,6,7,8))
str(CRFdata)

#visualise
ggplot(CRFdata1, aes(x=Suction, grams_N, colour=Soil)) + geom_boxplot() 
ggplot(CRFdata1, aes(x=Suction, grams_N, colour=Soil)) + geom_point()
ggplot(CRFdata1, aes(x=Days, grams_N, colour=Suction)) + geom_boxplot() 
ggplot(CRFdata1, aes(x=Days, grams_N, colour=Suction)) + geom_point() 
ggplot(CRFdata1, aes(x=Days, grams_N, colour=Suction)) + geom_point() + facet_wrap(~Soil, scales="free")
ggplot(CRFdata1, aes(x=Days, grams_N, colour=Soil)) + geom_point() + facet_wrap(~Suction, scales="free")
ggplot(CRFdata1, aes(x=Days, grams_N, colour=Soil)) + geom_boxplot() + facet_wrap(~Suction, scales="free")