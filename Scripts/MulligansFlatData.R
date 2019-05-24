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
MFdata1$Average_moisture_percent <- ((MFdata1$BD1_percent_moisture + MFdata1$BD2_percent_moisture)/2)
MFdata2 <-  MFdata1[-c(3,4,5,6,7)] #average BD & infiltration
MFdata2a <- gather(MFdata2, key=Infiltration, value = ml_per_minute, c(4,5,6))
MFdata2a$Infiltration <- factor(MFdata2a$Infiltration, levels = c("Infiltration_Rate_potential_plus1cm", "Infiltration_Rate_potential_minus1cm","Infiltration_Rate_potential_minus4cm"))
MFdata2a$ml_per_minute <- replace(MFdata2a$ml_per_minute, MFdata2a$ml_per_minute == 0.00 , 0.000001) #Need to remove zero before log
MFdata2a$log_ml_per_minute <- log(MFdata2a$ml_per_minute)#log infiltration ml_per_minute
str(MFdata2a)
MFdata3 <-  MFdata1[-c(3,8)]
MFdata4 <- gather(MFdata3, key=Bulk_Density, value = grams_per_cubic_cm, c(3,5))
MFdata5 <- gather(MFdata4, key=BD_moisture, value = moisture_percent, c(3,4)) #BD, BDmoisture, & infiltration
MFdata6 <- gather(MFdata5, key=Infiltration, value = ml_per_minute, c(3,4,5)) #tidy but not useful for infiltration
str(MFdata6)
MFdata6$Infiltration <- factor(MFdata6$Infiltration, levels = c("Infiltration_Rate_potential_plus1cm", "Infiltration_Rate_potential_minus1cm","Infiltration_Rate_potential_minus4cm"))
MFdata6$ml_per_minute <- replace(MFdata6$ml_per_minute, MFdata6$ml_per_minute == 0.00 , 0.000001) #Need to remove zero before log
MFdata6$log_ml_per_minute <- log(MFdata6$ml_per_minute)#log infiltration ml_per_minute
str(MFdata6)

#visualise

ggplot(MFdata2, aes(x=Element, Average_Bulk_Density, colour=Site_ID)) + geom_point() #useful, Graph1

ggplot(MFdata2a, aes(x=Element, ml_per_minute, colour=Infiltration)) + geom_boxplot() #useful, Graph2
ggplot(MFdata2a, aes(x=Element, ml_per_minute, colour=Site_ID)) + geom_point() + facet_wrap(~Infiltration, scales="free") #useful, Graph3
ggplot(MFdata2a, aes(x=Site_ID, ml_per_minute, colour=Infiltration)) + geom_boxplot() #useful, Graph4

ggplot(MFdata6, aes(x=Element, grams_per_cubic_cm, colour=Site_ID)) + geom_boxplot() +facet_wrap(~Infiltration, scales="free") #very useful Graph5
ggplot(MFdata6, aes(x=Element, moisture_percent, colour=Site_ID)) + geom_boxplot() #useful??, Graph6
ggplot(MFdata6, aes(x=Element, ml_per_minute, colour=Site_ID)) + geom_boxplot() +facet_wrap(~Infiltration, scales="free") #useful? Graph7
ggplot(MFdata6, aes(x=grams_per_cubic_cm, ml_per_minute, colour=Element)) + geom_boxplot()+facet_wrap(~Infiltration, scales="free") #useful, Graph8
ggplot(MFdata6, aes(x=grams_per_cubic_cm, ml_per_minute, colour=Site_ID)) + geom_boxplot()+facet_wrap(~Infiltration, scales="free") #maybe useful, Graph9
ggplot(MFdata6, aes(x=grams_per_cubic_cm, moisture_percent, colour=Site_ID)) + geom_boxplot() #useful, Graph 11
ggplot(MFdata6, aes(x=moisture_percent, ml_per_minute, colour=Element)) + geom_boxplot() + facet_wrap(~Infiltration, scales="free") #useful,Graph 12
ggplot(MFdata6, aes(x=moisture_percent, ml_per_minute, colour=Site_ID)) + geom_boxplot() + facet_wrap(~Infiltration, scales="free") #useful, Graph 13
ggplot(MFdata6, aes(x=moisture_percent, ml_per_minute, colour=Infiltration)) + geom_boxplot()  #useful, Graph14, why infiltration is a block?
ggplot(MFdata6, aes(x=grams_per_cubic_cm, ml_per_minute, colour=Infiltration)) + geom_boxplot() #useful, Graph15, why infiltration is a block?
ggplot(MFdata6, aes(x=Infiltration, log(ml_per_minute), colour=Element)) + geom_boxplot() # very useful  Graph16
ggplot(MFdata6, aes(x=Infiltration, log(ml_per_minute), colour=Site_ID)) + geom_boxplot() # very useful  Graph17
ggplot(MFdata6, aes(x=grams_per_cubic_cm, log(ml_per_minute), colour=Element)) + geom_boxplot() + facet_wrap(~Infiltration, scales="free") #useful, Graph18
ggplot(MFdata6, aes(x=grams_per_cubic_cm, log(ml_per_minute), colour=Site_ID)) + geom_boxplot() + facet_wrap(~Infiltration, scales="free") # maybe useful, Graph19
ggplot(MFdata6, aes(x=moisture_percent, log(ml_per_minute), colour=Element)) + geom_boxplot() + facet_wrap(~Infiltration, scales="free") #useful?, Graph20
ggplot(MFdata6, aes(x=moisture_percent, log(ml_per_minute), colour=Site_ID)) + geom_boxplot() + facet_wrap(~Infiltration, scales="free") #useful?, Graph21

#ggplot geom_smooth
ggplot(MFdata2a, aes(x=Average_Bulk_Density, ml_per_minute, colour=Element)) + geom_point() + geom_smooth() + facet_wrap(~Infiltration, scales="free") #useful? Graph22
ggplot(MFdata2a, aes(x=Average_moisture_percent, ml_per_minute, colour=Element)) + geom_point() + geom_smooth() + facet_wrap(~Infiltration, scales="free") #useful? Graph23

ggplot(MFdata6, aes(x=moisture_percent, ml_per_minute, colour=Element)) + geom_point() + geom_smooth() +facet_wrap(~Infiltration, scales="free") #useful? Graph24
ggplot(MFdata6, aes(x=grams_per_cubic_cm, ml_per_minute, colour=Element)) + geom_point() + geom_smooth() + facet_wrap(~Infiltration, scales="free") #useful? Graph25
ggplot(MFdata6, aes(x=moisture_percent, log(ml_per_minute), colour=Site_ID)) + geom_point()+ geom_smooth() + facet_wrap(~Infiltration, scales="free") #useful?? Graph26
ggplot(MFdata6, aes(x=moisture_percent, log(ml_per_minute), colour=Element)) + geom_point()+ geom_smooth() + facet_wrap(~Infiltration, scales="free") #useful?? Graph27


#unused graphs
#ggplot(MFdata2, aes(x=Site_ID, Average_Bulk_Density, colour=Element)) + geom_boxplot() #not so useful
#ggplot(MFdata2, aes(x=Site_ID, Average_Bulk_Density, colour=Element)) + geom_point() # useful
#ggplot(MFdata2a, aes(x=Site_ID, Average_Bulk_Density, colour=Element)) + geom_point() + facet_wrap(~Infiltration, scales="free") #not useful
#ggplot(MFdata2, aes(x=Element, Average_Bulk_Density, colour=Site_ID)) + geom_boxplot() #not useful

#ggplot(MFdata2a, aes(x=Site_ID, ml_per_minute, colour=Element)) + geom_point() + facet_wrap(~Infiltration, scales="free") #useful
#ggplot(MFdata2a, aes(x=Site_ID, ml_per_minute, colour=Element)) + geom_boxplot() + facet_wrap(~Infiltration, scales="free")
#ggplot(MFdata2a, aes(x=Average_Bulk_Density, ml_per_minute, colour=Element)) + geom_point() + facet_wrap(~Site_ID, scales="free") #useful?
#ggplot(MFdata2a, aes(x=Average_Bulk_Density, ml_per_minute, colour=Element)) + geom_point() + facet_wrap(~Infiltration, scales="free") #useful?
#ggplot(MFdata2a, aes(x=Average_Bulk_Density, log(ml_per_minute), colour=Element)) + geom_point() + facet_wrap(~Infiltration, scales="free") #useful, Graph?
#ggplot(MFdata2a, aes(x=Average_Bulk_Density, ml_per_minute, colour=Site_ID)) + geom_point() + facet_wrap(~Infiltration, scales="free") #useful?
#ggplot(MFdata2a, aes(x=Element, ml_per_minute, colour=Infiltration)) + geom_point() #useful
#ggplot(MFdata2a, aes(x=Site_ID, ml_per_minute, colour=Infiltration)) + geom_point() #useful
#ggplot(MFdata2a, aes(x=Average_moisture_percent, log(ml_per_minute), colour=Element)) + geom_point() + facet_wrap(~Infiltration, scales="free") # not useful, errors

#ggplot(MFdata6, aes(x=Site_ID, ml_per_minute, colour=Element)) + geom_boxplot() + facet_wrap(~Infiltration, scales="free") #maybe not useful
#ggplot(MFdata6, aes(x=Site_ID, grams_per_cubic_cm, colour=Element)) + geom_boxplot() #very useful
#ggplot(MFdata6, aes(x=Site_ID, moisture_percent, colour=Element)) + geom_boxplot() #useful??
#ggplot(MFdata6, aes(x=Site_ID, ml_per_minute, colour=Element)) + geom_boxplot() #not useful
#ggplot(MFdata6, aes(x=grams_per_cubic_cm, ml_per_minute, colour=Element)) + geom_point() #useful
#ggplot(MFdata6, aes(x=grams_per_cubic_cm, ml_per_minute, colour=Site_ID)) + geom_point() #maybe useful
#ggplot(MFdata6, aes(x=grams_per_cubic_cm, moisture_percent, colour=Element)) + geom_boxplot() #useful, Graph 10
#ggplot(MFdata6, aes(x=grams_per_cubic_cm, moisture_percent, colour=Element)) + geom_point() #useful?
#ggplot(MFdata6, aes(x=grams_per_cubic_cm, moisture_percent, colour=Site_ID)) + geom_point() #useful?
#ggplot(MFdata6, aes(x=moisture_percent, ml_per_minute, colour=Element)) + geom_boxplot() #useful
#ggplot(MFdata6, aes(x=moisture_percent, ml_per_minute, colour=Element)) + geom_point() #maybe useful
#ggplot(MFdata6, aes(x=moisture_percent, ml_per_minute, colour=Element)) + geom_boxplot() + facet_wrap(~Site_ID, scales="free") #useful?
#ggplot(MFdata6, aes(x=moisture_percent, ml_per_minute, colour=Site_ID)) + geom_boxplot() #useful
#ggplot(MFdata6, aes(x=moisture_percent, ml_per_minute, colour=Site_ID)) + geom_point() #Useful?
#ggplot(MFdata6, aes(x=moisture_percent, ml_per_minute, colour=Site_ID)) + geom_boxplot() + facet_wrap(~Element, scales="free") #useful
#ggplot(MFdata6, aes(x=moisture_percent, ml_per_minute, colour=Site_ID)) + geom_point() + facet_wrap(~Element, scales="free") #useful
#ggplot(MFdata6, aes(x=moisture_percent, ml_per_minute, colour=Infiltration)) + geom_point() # maybe useful
#ggplot(MFdata6, aes(x=grams_per_cubic_cm, ml_per_minute, colour=Infiltration)) + geom_point() # maybe useful
#ggplot(MFdata6, aes(x=grams_per_cubic_cm, ml_per_minute, colour=Infiltration)) + geom_boxplot()+ facet_wrap(~Element, scales="free") #useful
#ggplot(MFdata6, aes(x=grams_per_cubic_cm, ml_per_minute, colour=Infiltration)) + geom_boxplot()+ facet_wrap(~Site_ID, scales="free") #useful
#ggplot(MFdata6, aes(x=grams_per_cubic_cm, ml_per_minute, colour=Site_ID)) + geom_boxplot() + facet_wrap(~Infiltration, scales="free") #useful? same as graph9
#ggplot(MFdata6, aes(x=grams_per_cubic_cm, ml_per_minute, colour=Element)) + geom_boxplot() + facet_wrap(~Infiltration, scales="free") #useful? same as graph8
#ggplot(MFdata6, aes(x=Site_ID, log(ml_per_minute), colour=Element)) + geom_point() + facet_wrap(~Infiltration, scales="free") #useful?,
#ggplot(MFdata6, aes(x=Element, log(ml_per_minute), colour=Site_ID)) + geom_point() + facet_wrap(~Infiltration, scales="free") #useful?,
#ggplot(MFdata6, aes(x=moisture_percent, log(ml_per_minute), colour=Site_ID)) + geom_point() + facet_wrap(~Infiltration, scales="free") #useful?


#unused ggplot geom_smooth
#ggplot(MFdata2a, aes(x=Average_Bulk_Density, log(ml_per_minute), colour=Element)) + geom_point() + geom_smooth + facet_wrap(~Infiltration, scales="free") # not useful, errors
#ggplot(MFdata2a, aes(x=Average_Bulk_Density, log(ml_per_minute), colour=Site_ID)) + geom_point() + geom_smooth() + facet_wrap(~Infiltration, scales="free") #useful?
#ggplot(MFdata2a, aes(x=Average_moisture_percent, log(ml_per_minute), colour=Element)) + geom_point() + geom_smooth + facet_wrap(~Infiltration, scales="free") # not useful, errors
#ggplot(MFdata2a, aes(x=Average_moisture_percent, log(ml_per_minute), colour=Site_ID)) + geom_point() + geom_smooth() + facet_wrap(~Infiltration, scales="free") #useful?
#ggplot(MFdata2a, aes(x=Element, ml_per_minute, colour=Infiltration)) + geom_point() + geom_smooth() # not useful, errors
#ggplot(MFdata2a, aes(x=Average_moisture_percent, log(ml_per_minute), colour=Infiltration)) + geom_point() + geom_smooth  # not useful, errors


install.packages("emmeans")
library(emmeans)
install.packages("lmerTest")
library(lmerTest)

#model data

MFlm1 <- lmer(ml_per_minute~Element + (1|Infiltration), data = MFdata2a)
anova(MFlm1)
summary(MFlm1)
plot(MFlm1)
# no significant diferrence between elements for ml_per_minute

MFlm2 <- lmer(ml_per_minute~Site_ID + (1|Infiltration), data = MFdata2a)
anova(MFlm2)
summary(MFlm2)
plot(MFlm2)
# significant difference between sites, 
#especially at the following Site_ID; MF25A-3A, MF27A-1A, MF37-1A, MF38-1A.
#residual plot shows need for log transformation

MFlm3 <- lmer(ml_per_minute~Average_Bulk_Density + (1|Infiltration), data = MFdata2a)
anova(MFlm3)
summary(MFlm3)
plot(MFlm3)
# significant difference between Bulk Density, & Infiltration
# residual plot shows need for log transformation

MFlm4 <- lmer(ml_per_minute~Average_moisture_percent + (1|Infiltration), data = MFdata2a)
anova(MFlm4)
summary(MFlm4)
plot(MFlm4)
# significant difference between Average_moisture_percent
# residual plot shows need for log transformation

MFlm5 <- lmer(ml_per_minute~Element + (1|Infiltration), data = MFdata6)
anova(MFlm5)
summary(MFlm5)
plot(MFlm5)
# significant diferrence between elements for ml_per_minute. 
#Especially ClumpedTop & Old Log
#Why? Maybe increased observations in MFdata6?

MFlm6 <- lmer(ml_per_minute~Site_ID + (1|Infiltration), data = MFdata6)
anova(MFlm6)
summary(MFlm6)
plot(MFlm6)
# significant difference between sites, Why increased Number of sites
#especially at the following Site_ID; MF22AZ-4A, MF25A-3A, MF27A-1A, MF37-1A, MF34-4B, MF37-1A,MF38-1A.
#Why increased significant diferent sites?
#residual plot shows need for log transformation

MFlm7 <- lmer(ml_per_minute~grams_per_cubic_cm + (1|Infiltration), data = MFdata6)
anova(MFlm7)
summary(MFlm7)
plot(MFlm7)
# significant difference between Bulk Density(grams_per_cubic_cm), & Infiltration
# residual plot shows need for log transformation

MFlm8 <- lmer(ml_per_minute~moisture_percent + (1|Infiltration), data = MFdata6)
anova(MFlm8)
summary(MFlm8)
plot(MFlm8)
# significant difference between moisture_percent
# residual plot shows need for log transformation

MFlm9 <- lmer(log_ml_per_minute~Element + (1|Infiltration), data = MFdata2a)
anova(MFlm9)
summary(MFlm9)
plot(MFlm9)
# no significant diferrence between elements for log_ ml_per_minute
# residuals are better

MFlm10 <- lmer(log_ml_per_minute~Site_ID + (1|Infiltration), data = MFdata2a)
anova(MFlm10)
summary(MFlm10)
plot(MFlm10)
# significant difference between sites, 
#especially at the following Site_ID; MF19A-2B, MF22AZ-4A, MF25A-3A, MF27A-1A, MF34-4B, MF37-1A,MF38-1A. MF25A-3A, MF27A-1A, MF37-1A, MF38-1A.
#log_ml_per_minute_ improved residual 

MFlm11 <- lmer(log_ml_per_minute~Average_Bulk_Density + (1|Infiltration), data = MFdata2a)
anova(MFlm11)
summary(MFlm11)
plot(MFlm11)
# no significant difference between Bulk Density, & Infiltration
# log_ml_per_minute_ improved residual 

MFlm12 <- lmer(log_ml_per_minute~Average_moisture_percent + (1|Infiltration), data = MFdata2a)
anova(MFlm12)
summary(MFlm12)
plot(MFlm12)
# no significant difference between Average_moisture_percent
# log_ml_per_minute_ improved residual 

MFlm13 <- lmer(log_ml_per_minute~moisture_percent*grams_per_cubic_cm + (1|Infiltration), data = MFdata6)
anova(MFlm13)
summary(MFlm13)
plot(MFlm13)
# significant difference between moisture_percent, grams_per_cubic_cm, &  moisture_percent*grams_per_cubic_cm

MFlm14 <- lmer(log_ml_per_minute~Average_moisture_percent*Average_Bulk_Density + (1|Infiltration), data = MFdata2a)
anova(MFlm14)
summary(MFlm14)
plot(MFlm14)
# no significant difference between moisture_percent, grams_per_cubic_cm, &  moisture_percent*grams_per_cubic_cm

MFlm15 <- lmer(log_ml_per_minute~Site_ID*Element + (1|Infiltration), data = MFdata6)
anova(MFlm15)
summary(MFlm15)
plot(MFlm15)
#fixed-effect model matrix is rank deficient so dropping 18 columns / coefficients
# significant difference between Site_ID, Element, Site_ID*Element
# especially Site_IDMF19A-2B,Site_IDMF22AZ-4A,Site_IDMF32/1A,Site_IDMF34-4B,Site_IDMF37-1A, Site_IDMF38-1A, Site_IDMF34-4B:ElementClump Top, Site_IDMF27A-1A:ElementOld Log, Site_IDMF34-4B:ElementOld Log, Site_IDMF34-4B:ElementOpen, Site_IDMF34-4B:ElementTree     

MFlm16 <- lmer(log_ml_per_minute~Site_ID*Element + (1|Infiltration), data = MFdata2a)
anova(MFlm16)
summary(MFlm16)
plot(MFlm16)
# fixed-effect model matrix is rank deficient so dropping 18 columns / coefficients
# significant difference between Site_ID,
# especially Site_IDMF34-4B, Site_IDMF34-4B:ElementOpen


#emmeans(MFlm1, pairwise~ml_|Element, type="response") 
#emmeans(MFlm1, revpairwise~Infiltration|Element, type="response")
# don't think I have this right above.


