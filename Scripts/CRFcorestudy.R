library(tidyverse)
library(readxl)

#Import
CRFdata <- read_excel("../Data/CRFcorestudy.xlsx", skip=3)

#Tidy data, restructure,
str(CRFdata)
CRFdata1 <- gather(CRFdata, key=Suction, value = grams_N, c(5,6,7,8))
str(CRFdata1)
CRFdata1$Suction <- factor(CRFdata1$Suction, levels = c("Suction_100cm", "Suction_600cm","Suction_5000cm", "Suction_10000cm"))
str(CRFdata1)

#visualise
ggplot(CRFdata1, aes(x=Suction, grams_N, colour=Soil)) + geom_boxplot() 
ggplot(CRFdata1, aes(x=Suction, grams_N, colour=Soil)) + geom_point()
ggplot(CRFdata1, aes(x=Days, grams_N, colour=Suction)) + geom_boxplot() 
ggplot(CRFdata1, aes(x=Days, grams_N, colour=Suction)) + geom_point() 
ggplot(CRFdata1, aes(x=Days, grams_N, colour=Suction)) + geom_point() + facet_wrap(~Soil, scales="free")
ggplot(CRFdata1, aes(x=Days, grams_N, colour=Soil)) + geom_point() + facet_wrap(~Suction, scales="free")
ggplot(CRFdata1, aes(x=Days, grams_N, colour=Soil)) + geom_boxplot() + facet_wrap(~Suction, scales="free")
ggplot(CRFdata1, aes(x=Days, grams_N, colour=Suction)) + geom_point() + facet_wrap(~Product, scales="free")
ggplot(CRFdata1, aes(x=Days, grams_N, colour=Suction)) + geom_boxplot() + facet_wrap(~Product, scales="free")
ggplot(CRFdata1, aes(x=Days, grams_N, colour=Soil)) + geom_point() + geom_smooth() + facet_wrap(~Suction, scales="free")
ggplot(CRFdata1, aes(x=Days, grams_N, colour=Suction)) + geom_boxplot() + geom_smooth() + facet_wrap(~Product, scales="free")
ggplot(CRFdata1, aes(x=Days, grams_N, colour=Suction)) + geom_point() + geom_smooth() + facet_wrap(~Soil, scales="free")

install.packages("emmeans")
library(emmeans)
install.packages("lmerTest")
library(lmerTest)

#model data

CRFlm1 <- lmer(grams_N~Suction + (1|Product), data = CRFdata1)
anova(CRFlm1)
summary(CRFlm1)
plot(CRFlm1)

CRFlm2 <- lmer(grams_N~Soil + (1|Product), data = CRFdata1)
anova(CRFlm2)
summary(CRFlm2)
plot(CRFlm2)

CRFlm3 <- lmer(grams_N~Suction*Days + (1|Product), data = CRFdata1)
anova(CRFlm3)
summary(CRFlm3)
plot(CRFlm3)
