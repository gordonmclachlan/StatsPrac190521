library(tidyverse)
library(readxl)

#Import
MFdata <- read_excel("../Data/MulligansFlatInfiltration.xlsx", skip=5)

#Tidy data, restructure, delete or combine columns, change coulmn structure to factors,
MFdata1 <-  MFdata[-c(2,4)]
names(MFdata1) <- c("Site_ID", "Element",	"Date", "Bulk_Density_1",	"BD1_percent_moisture", "Bulk_Density_2",	"BD2_percent_moisture", "Average_Bulk_Density", "Infiltration_Rate_potential_minus4cm", "Infiltration_Rate_potential_minus1cm", "Infiltration_Rate_potential_plus1cm")
str(MFdata1)
MFdata1 <- mutate_at(MFdata1, vars("Site_ID", "Element"), as.factor)
str(MFdata1)
MFdata2 <-  MFdata1[-c(3,4,5,6,7)] #average BD & infiltration
MFdata2a <- gather(MFdata2, key=Infiltration, value = ml_per_minute, c(4,5,6))
MFdata2a$Infiltration <- factor(MFdata2a$Infiltration, levels = c("Infiltration_Rate_potential_plus1cm", "Infiltration_Rate_potential_minus1cm","Infiltration_Rate_potential_minus4cm"))
str(MFdata2a)
MFdata3 <-  MFdata1[-c(3,8)]
MFdata4 <- gather(MFdata3, key=Bulk_Density, value = grams_per_cubic_cm, c(3,5))
MFdata5 <- gather(MFdata4, key=BD_moisture, value = moisture_percent, c(3,4)) #BD, BDmoisture, & infiltration
MFdata6 <- gather(MFdata5, key=Infiltration, value = ml_per_minute, c(3,4,5)) #tidy but not useful for infiltration
str(MFdata6)
MFdata6$Infiltration <- factor(MFdata6$Infiltration, levels = c("Infiltration_Rate_potential_plus1cm", "Infiltration_Rate_potential_minus1cm","Infiltration_Rate_potential_minus4cm"))
str(MFdata6)

#visualise
ggplot(MFdata2, aes(x=Site_ID, Average_Bulk_Density, colour=Element)) + geom_boxplot() #not so useful
ggplot(MFdata2, aes(x=Site_ID, Average_Bulk_Density, colour=Element)) + geom_point() # useful
ggplot(MFdata2a, aes(x=Site_ID, Average_Bulk_Density, colour=Element)) + geom_point() + facet_wrap(~Infiltration, scales="free") #not useful
ggplot(MFdata2, aes(x=Element, Average_Bulk_Density, colour=Site_ID)) + geom_boxplot() #not useful
ggplot(MFdata2, aes(x=Element, Average_Bulk_Density, colour=Site_ID)) + geom_point() #useful, Graph1

ggplot(MFdata2a, aes(x=Site_ID, ml_per_minute, colour=Element)) + geom_point() + facet_wrap(~Infiltration, scales="free") #useful
#ggplot(MFdata2a, aes(x=Site_ID, ml_per_minute, colour=Element)) + geom_boxplot() + facet_wrap(~Infiltration, scales="free")
ggplot(MFdata2a, aes(x=Average_Bulk_Density, ml_per_minute, colour=Element)) + geom_point() + facet_wrap(~Site_ID, scales="free") #useful?
ggplot(MFdata2a, aes(x=Average_Bulk_Density, ml_per_minute, colour=Element)) + geom_point() + facet_wrap(~Infiltration, scales="free") #useful?
ggplot(MFdata2a, aes(x=Average_Bulk_Density, log(ml_per_minute), colour=Element)) + geom_point() + facet_wrap(~Infiltration, scales="free") #useful, Graph?
ggplot(MFdata2a, aes(x=Average_Bulk_Density, ml_per_minute, colour=Site_ID)) + geom_point() + facet_wrap(~Infiltration, scales="free") #useful?
ggplot(MFdata2a, aes(x=Element, ml_per_minute, colour=Infiltration)) + geom_boxplot() #useful, Graph2
ggplot(MFdata2a, aes(x=Element, log(ml_per_minute), colour=Site_ID)) + geom_point() + facet_wrap(~Infiltration, scales="free") #useful, Graph?
ggplot(MFdata2a, aes(x=Element, ml_per_minute, colour=Infiltration)) + geom_point() #useful
ggplot(MFdata2a, aes(x=Site_ID, ml_per_minute, colour=Infiltration)) + geom_boxplot() #useful, Graph3
ggplot(MFdata2a, aes(x=Site_ID, ml_per_minute, colour=Infiltration)) + geom_point() #useful

ggplot(MFdata6, aes(x=Site_ID, ml_per_minute, colour=Element)) + geom_boxplot() + facet_wrap(~Infiltration, scales="free") #maybe not useful
ggplot(MFdata6, aes(x=Site_ID, grams_per_cubic_cm, colour=Element)) + geom_boxplot() #very useful
ggplot(MFdata6, aes(x=Element, grams_per_cubic_cm, colour=Site_ID)) + geom_boxplot() #very useful Graph4
ggplot(MFdata6, aes(x=Site_ID, moisture_percent, colour=Element)) + geom_boxplot() #useful??
ggplot(MFdata6, aes(x=Element, moisture_percent, colour=Site_ID)) + geom_boxplot() #useful??, Graph5
ggplot(MFdata6, aes(x=Site_ID, ml_per_minute, colour=Element)) + geom_boxplot() #not useful
ggplot(MFdata6, aes(x=Element, ml_per_minute, colour=Site_ID)) + geom_boxplot() #not useful
ggplot(MFdata6, aes(x=grams_per_cubic_cm, ml_per_minute, colour=Element)) + geom_boxplot() #useful, Graph6
ggplot(MFdata6, aes(x=grams_per_cubic_cm, ml_per_minute, colour=Element)) + geom_point() #useful
ggplot(MFdata6, aes(x=grams_per_cubic_cm, ml_per_minute, colour=Site_ID)) + geom_boxplot() #maybe useful, Graph7
ggplot(MFdata6, aes(x=grams_per_cubic_cm, ml_per_minute, colour=Site_ID)) + geom_point() #maybe useful
ggplot(MFdata6, aes(x=grams_per_cubic_cm, moisture_percent, colour=Element)) + geom_boxplot() #useful, Graph 8
ggplot(MFdata6, aes(x=grams_per_cubic_cm, moisture_percent, colour=Element)) + geom_point() #useful?
ggplot(MFdata6, aes(x=grams_per_cubic_cm, moisture_percent, colour=Site_ID)) + geom_boxplot() #useful, Graph 9
ggplot(MFdata6, aes(x=grams_per_cubic_cm, moisture_percent, colour=Site_ID)) + geom_point() #useful?
ggplot(MFdata6, aes(x=moisture_percent, ml_per_minute, colour=Element)) + geom_boxplot() #useful
ggplot(MFdata6, aes(x=moisture_percent, ml_per_minute, colour=Element)) + geom_point() #maybe useful
ggplot(MFdata6, aes(x=moisture_percent, ml_per_minute, colour=Element)) + geom_boxplot() + facet_wrap(~Site_ID, scales="free") #useful?
ggplot(MFdata6, aes(x=moisture_percent, ml_per_minute, colour=Element)) + geom_boxplot() + facet_wrap(~Infiltration, scales="free") #useful, Graph10
ggplot(MFdata6, aes(x=moisture_percent, ml_per_minute, colour=Site_ID)) + geom_boxplot() #useful
ggplot(MFdata6, aes(x=moisture_percent, ml_per_minute, colour=Site_ID)) + geom_point() #Useful?
ggplot(MFdata6, aes(x=moisture_percent, ml_per_minute, colour=Site_ID)) + geom_boxplot() + facet_wrap(~Element, scales="free") #useful
ggplot(MFdata6, aes(x=moisture_percent, ml_per_minute, colour=Site_ID)) + geom_point() + facet_wrap(~Element, scales="free") #useful
ggplot(MFdata6, aes(x=moisture_percent, ml_per_minute, colour=Site_ID)) + geom_boxplot() + facet_wrap(~Infiltration, scales="free") #useful, Graph11
ggplot(MFdata6, aes(x=moisture_percent, ml_per_minute, colour=Infiltration)) + geom_boxplot()  #useful, Graph12
ggplot(MFdata6, aes(x=moisture_percent, ml_per_minute, colour=Infiltration)) + geom_point() # maybe useful
ggplot(MFdata6, aes(x=grams_per_cubic_cm, ml_per_minute, colour=Infiltration)) + geom_boxplot() #useful, Graph13
ggplot(MFdata6, aes(x=grams_per_cubic_cm, ml_per_minute, colour=Infiltration)) + geom_point() # maybe useful
ggplot(MFdata6, aes(x=grams_per_cubic_cm, ml_per_minute, colour=Infiltration)) + geom_boxplot()+ facet_wrap(~Element, scales="free") #useful
ggplot(MFdata6, aes(x=grams_per_cubic_cm, ml_per_minute, colour=Infiltration)) + geom_boxplot()+ facet_wrap(~Site_ID, scales="free") #useful
ggplot(MFdata6, aes(x=grams_per_cubic_cm, ml_per_minute, colour=Site_ID)) + geom_boxplot() + facet_wrap(~Infiltration, scales="free") #useful? Graph14
ggplot(MFdata6, aes(x=grams_per_cubic_cm, ml_per_minute, colour=Element)) + geom_boxplot() + facet_wrap(~Infiltration, scales="free") #useful? Graph15
ggplot(MFdata6, aes(x=Infiltration, log(ml_per_minute), colour=Element)) + geom_boxplot() # very useful  Graph16
ggplot(MFdata6, aes(x=Infiltration, log(ml_per_minute), colour=Site_ID)) + geom_boxplot() # very useful  Graph17
ggplot(MFdata6, aes(x=grams_per_cubic_cm, log(ml_per_minute), colour=Element)) + geom_boxplot() + facet_wrap(~Infiltration, scales="free") #useful, Graph18
ggplot(MFdata6, aes(x=grams_per_cubic_cm, log(ml_per_minute), colour=Site_ID)) + geom_boxplot() + facet_wrap(~Infiltration, scales="free") # maybe useful, Graph19
ggplot(MFdata6, aes(x=moisture_percent, log(ml_per_minute), colour=Element)) + geom_boxplot() + facet_wrap(~Infiltration, scales="free") #useful?, Graph20
ggplot(MFdata6, aes(x=moisture_percent, log(ml_per_minute), colour=Site_ID)) + geom_boxplot() + facet_wrap(~Infiltration, scales="free") #useful?, Graph21
ggplot(MFdata6, aes(x=Site_ID, log(ml_per_minute), colour=Element)) + geom_point() + facet_wrap(~Infiltration, scales="free") #useful?, Graph ?
ggplot(MFdata6, aes(x=Element, log(ml_per_minute), colour=Site_ID)) + geom_point() + facet_wrap(~Infiltration, scales="free") #useful?, Graph22?

#Plot below to big and useless
#ggplot(MFdata6, aes(x=grams_per_cubic_cm, y=ml_per_minute, colour=Infiltration)) + 
# geom_point() + 
# geom_smooth(alpha=.2) +
# facet_wrap(~Site_ID*Element)


install.packages("emmeans")
library(emmeans)
install.packages("lmerTest")
library(lmerTest)

#model data
MFlm1 <- lmer(ml_per_minute~Infiltration*Element + (1|Site_ID), data = MFdata2a)
anova(MFlm1)
summary(MFlm1)
plot(MFlm1)
emmeans(MFlm1, pairwise~Infiltratio|Element, type="response") 
emmeans(MFlm1, revpairwise~Infiltration|Element, type="response")
# don't think I have this right above.




#lm3 <- lmer(Height~MAP_kg_ha+Nitrogen_kg_ha+Treatment+factor(Week)+
 #             Treatment*factor(Week)+ Nitrogen_kg_ha* factor(Week)+
  #            MAP_kg_ha*factor(Week) + (1|Transect) + (1|Plot), data = MFdata2a)

#lm4 <- lmer(Height~factor(Week) * (Nitrogen_kg_ha + Treatment + MAP_kg_ha) +
 #             (1|Transect) +(1|Plot), data=TM2)

#summary(lm3)
#anova(lm4)
#summary(lm4)
#plot(lm3)
#plot(lm4)
#emmeans(lm4, pairwise~Treatment|factor(Week), type="response") 
#emmeans(lm4, revpairwise~Treatment|factor(Week), type="response")
