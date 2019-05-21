library(tidyverse)
library(readxl)

#Import
MFdata <- read_excel("../Data/MulligansFlatInfiltration.xlsx", skip=5)

MFdata1 <-  MFdata[-c(2,4)]
names(MFdata1) <- c("Site_ID", "Element",	"Date", "Bulk_Density_1",	"BD1_percent_moisture", "Bulk_Density_2",	"BD2_percent_moisture", "Average_Bulk_Density", "Infiltration_Rate_potential_minus4cm", "Infiltration_Rate_potential_minus1cm", "Infiltration_Rate_potential_plus1cm")
str(MFdata1)
MFdata1 <- mutate_at(MFdata1, vars("Site_ID", "Element"), as.factor)
str(MFdata1)
MFdata2 <-  MFdata1[-c(3,4,5,6,7)]
MFdata3 <-  MFdata1[-c(3,8)]
MFdata4 <- gather(MFdata3, key=Bulk_Density, value = grams_per_cubic_cm, c(3,5))
MFdata5 <- gather(MFdata4, key=BD_moisture, value = moisture_percent, c(3,4))
MFdata6 <- gather(MFdata5, key=Infiltraton, value = ml_per_minute, c(7,8,9))


#install.packages("emmeans")
#library(emmeans)
#install.packages("lmerTest")
#library(lmerTest)

#Import
#SeijaN <- read_csv("Data/Comparison_Experiment_KCL_v_K2SO4.csv")
#str(SeijaN)

#restructure
#SeijaN2 <- gather(SeijaN, key=N_type, value = amount, c(7,9,11))
#SeijaN2$RepID <- with(SeijaN2, interaction(Treatment,Extractant,Soil))
#SeijaN2$RepID <- str_replace_all(SeijaN2$RepID, ' ', '_')



#visualise
#ggplot(SeijaN, aes(x=Soil, log(NH3N_adjusted), colour=Extractant)) + geom_boxplot() + facet_wrap(~Treatment, scales="free")
#ggplot(SeijaN, aes(x=Treatment, log(NH3N_adjusted), colour=Extractant)) + geom_boxplot() + facet_wrap(~Soil, scales="free")
#ggplot(SeijaN, aes(x=Treatment, log(NH3N_adjusted), colour=Extractant)) + geom_point() + facet_wrap(~Soil, scales="free")


#visualise by tables
#with(TM,table(Plot, Transect)) #table(TM$Plot, TM$Transect)
w#ith(TM,table(MAP_kg_ha, Nitrogen_kg_ha,Treatment))


#visualise
#ggplot(TM2, aes(x=Week, y=Height, colour=Treatment)) + 
 # geom_point() + 
  #geom_smooth(alpha=.2) +
  #facet_wrap(~MAP_kg_ha*Nitrogen_kg_ha)

#model data
#lm3 <- lmer(Height~MAP_kg_ha+Nitrogen_kg_ha+Treatment+factor(Week)+
 #             Treatment*factor(Week)+ Nitrogen_kg_ha* factor(Week)+
  #            MAP_kg_ha*factor(Week) + (1|Transect) + (1|Plot), data = TM2)

#lm4 <- lmer(Height~factor(Week) * (Nitrogen_kg_ha + Treatment + MAP_kg_ha) +
 #             (1|Transect) +(1|Plot), data=TM2)

#summary(lm3)
#anova(lm4)
#summary(lm4)
#plot(lm3)
#plot(lm4)
#emmeans(lm4, pairwise~Treatment|factor(Week), type="response") 
#emmeans(lm4, revpairwise~Treatment|factor(Week), type="response")
