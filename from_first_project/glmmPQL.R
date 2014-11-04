FACEmp <- read.csv("~/Rdata/nutrients/mean_nutrient_data.csv")

library(MASS)
library(nlme)
library(lme4)
library(ggplot2) # Plotting
#mod<-glmmPQL(log(Zinc)~CO+as.factor(water)+as.factor(nitrogen level),random=~1|paircount,data=   ,subset=croptype==1)
#summary(mod)$tTable[2,1]
FACEmp$WateringRegime<-as.factor(FACEmp$WateringRegime)
FACEmp$Pair_Count<-as.factor(FACEmp$Pair_Count)
FACEmp["pair"]<-as.factor(FACEmp$Pair_Count)
FACEmp["water"]<-ifelse(FACEmp$WateringRegime=="Wet",1,0)
FACEmp["fert"]<-ifelse(FACEmp$NapplicnQ=="Low"& FACEmp$Crop !="Corn" & FACEmp$Crop !="Fieldpeas", 0,(ifelse(FACEmp$NapplicnQ=="Medium",1,2)))
                       
Wheat.Zn.mod<-glmmPQL(log(Zn)~as.factor(CO2_indicator)+as.factor(water)+as.factor(fert),random=~1|pair,data=~FACEmp,subset=Crop=="wheat",family = gaussian)
# 
# Wheat.Yield.mod<-lmer(log(Yield)~CO2treatment+as.factor(Wateringregime)+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="wheat")
# Wheat.Zn.mod<-lmer(log(Zn)~CO2treatment+as.factor(Wateringregime)+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="wheat")
# Wheat.Fe.mod<-lmer(log(Fe)~CO2treatment+as.factor(Wateringregime)+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="wheat")
# Wheat.Ph.mod<-lmer(log(Ph)~CO2treatment+as.factor(Wateringregime)+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="wheat")
# Wheat.N.mod<-lmer(log(N)~CO2treatment+as.factor(Wateringregime)+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="wheat")
# NA
# NA
# NA
# NA
# NA
# NA
# NA
# NA
# 
# Fieldpeas.Yield.mod<-lmer(log(Yield)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")
# Fieldpeas.Zn.mod<-lmer(log(Zn)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")
# Fieldpeas.Fe.mod<-lmer(log(Fe)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")
# Fieldpeas.Ph.mod<-lmer(log(Ph)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")
# Fieldpeas.N.mod<-lmer(log(N)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")
# Fieldpeas.P.mod<-lmer(log(P)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")
# Fieldpeas.K.mod<-lmer(log(K)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")
# Fieldpeas.S.mod<-lmer(log(S)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")
# Fieldpeas.B.mod<-lmer(log(B)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")
# Fieldpeas.Ca.mod<-lmer(log(Ca)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")
# Fieldpeas.Mg.mod<-lmer(log(Mg)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")
# Fieldpeas.Mn.mod<-lmer(log(Mn)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")
# Fieldpeas.Cu.mod<-lmer(log(Cu)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")
# 
# NA
Rice.Zn.mod<-lmer(log(Zn)~CO2treatment+as.factor(Temperature_treatment)+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&NapplicnQ !="Low")
Rice.Fe.mod<-lmer(log(Fe)~CO2treatment+as.factor(Temperature_treatment)+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&NapplicnQ !="Low")
Rice.N.mod<-lmer(log(N)~CO2treatment+as.factor(Temperature_treatment)+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&NapplicnQ !="Low")
Rice.P.mod<-lmer(log(P)~CO2treatment+as.factor(Temperature_treatment)+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&NapplicnQ !="Low")
Rice.K.mod<-lmer(log(K)~CO2treatment+as.factor(Temperature_treatment)+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&NapplicnQ !="Low")
Rice.S.mod<-lmer(log(S)~CO2treatment+as.factor(Temperature_treatment)+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&NapplicnQ !="Low")
Rice.B.mod<-lmer(log(B)~CO2treatment+as.factor(Temperature_treatment)+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&NapplicnQ !="Low")
Rice.Ca.mod<-lmer(log(Ca)~CO2treatment+as.factor(Temperature_treatment)+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&NapplicnQ !="Low")
Rice.Mg.mod<-lmer(log(Mg)~CO2treatment+as.factor(Temperature_treatment)+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&NapplicnQ !="Low")
Rice.Mn.mod<-lmer(log(Mn)~CO2treatment+as.factor(Temperature_treatment)+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&NapplicnQ !="Low")
Rice.Cu.mod<-lmer(log(Cu)~CO2treatment+as.factor(Temperature_treatment)+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&NapplicnQ !="Low")

# Sorghum.Yield.mod<-lmer(log(Yield)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Sorghum")
# Sorghum.Zn.mod<-lmer(log(Zn)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Sorghum")
# Sorghum.Fe.mod<-lmer(log(Fe)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Sorghum")
# NA
# Sorghum.N.mod<-lmer(log(N)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Sorghum")
# Sorghum.P.mod<-lmer(log(P)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Sorghum")
# Sorghum.K.mod<-lmer(log(K)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Sorghum")
# Sorghum.S.mod<-lmer(log(S)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Sorghum")
# Sorghum.B.mod<-lmer(log(B)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Sorghum")
# Sorghum.Ca.mod<-lmer(log(Ca)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Sorghum")
# Sorghum.Mg.mod<-lmer(log(Mg)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Sorghum")
# Sorghum.Mn.mod<-lmer(log(Mn)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Sorghum")
# Sorghum.Cu.mod<-lmer(log(Cu)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Sorghum")
# 
# NA
# soybean.Zn.mod<-lmer(log(Zn)~CO2treatment+(1|pair),data=FACEmp,subset=Crop=="soybean")
# soybean.Fe.mod<-lmer(log(Fe)~CO2treatment+(1|pair),data=FACEmp,subset=Crop=="soybean")
# NA
# soybean.N.mod<-lmer(log(N)~CO2treatment+(1|pair),data=FACEmp,subset=Crop=="soybean")
# soybean.P.mod<-lmer(log(P)~CO2treatment+(1|pair),data=FACEmp,subset=Crop=="soybean")
# soybean.K.mod<-lmer(log(K)~CO2treatment+(1|pair),data=FACEmp,subset=Crop=="soybean")
# soybean.S.mod<-lmer(log(S)~CO2treatment+(1|pair),data=FACEmp,subset=Crop=="soybean")
# soybean.B.mod<-lmer(log(B)~CO2treatment+(1|pair),data=FACEmp,subset=Crop=="soybean")
# soybean.Ca.mod<-lmer(log(Ca)~CO2treatment+(1|pair),data=FACEmp,subset=Crop=="soybean")
# soybean.Mg.mod<-lmer(log(Mg)~CO2treatment+(1|pair),data=FACEmp,subset=Crop=="soybean")
# soybean.Mn.mod<-lmer(log(Mn)~CO2treatment+(1|pair),data=FACEmp,subset=Crop=="soybean")
# soybean.Cu.mod<-lmer(log(Cu)~CO2treatment+(1|pair),data=FACEmp,subset=Crop=="soybean")
# 
# NA
# Corn.Zn.mod<-lmer(log(Zn)~CO2treatment+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Corn")
# Corn.Fe.mod<-lmer(log(Fe)~CO2treatment+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Corn")
# NA
# Corn.N.mod<-lmer(log(N)~CO2treatment+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Corn")
# Corn.P.mod<-lmer(log(P)~CO2treatment+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Corn")
# Corn.K.mod<-lmer(log(K)~CO2treatment+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Corn")
# Corn.S.mod<-lmer(log(S)~CO2treatment+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Corn")
# Corn.B.mod<-lmer(log(B)~CO2treatment+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Corn")
# Corn.Ca.mod<-lmer(log(Ca)~CO2treatment+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Corn")
# Corn.Mg.mod<-lmer(log(Mg)~CO2treatment+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Corn")
# Corn.Mn.mod<-lmer(log(Mn)~CO2treatment+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Corn")
# Corn.Cu.mod<-lmer(log(Cu)~CO2treatment+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Corn")

#I have turned the plusses into multiplies. This is fine when there's only one treatment, for seeing whether the interaction is sig, but for wheat and rice, we need to do it separately for watering/temperature, nitrogen
Wheat.Yield.modN<-lmer(log(Yield)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="wheat")
Wheat.Zn.modN<-lmer(log(Zn)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="wheat")
Wheat.Fe.modN<-lmer(log(Fe)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="wheat")
Wheat.Ph.modN<-lmer(log(Ph)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="wheat")
Wheat.N.modN<-lmer(log(N)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="wheat")
Wheat.Yield.modW<-lmer(log(Yield)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="wheat")
Wheat.Zn.modW<-lmer(log(Zn)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="wheat")
Wheat.Fe.modW<-lmer(log(Fe)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="wheat")
Wheat.Ph.modW<-lmer(log(Ph)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="wheat")
Wheat.N.modW<-lmer(log(N)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="wheat")
NA
NA
NA
NA
NA
NA
NA
NA

Fieldpeas.Yield.mod<-lmer(log(Yield)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")
Fieldpeas.Zn.mod<-lmer(log(Zn)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")
Fieldpeas.Fe.mod<-lmer(log(Fe)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")
Fieldpeas.Ph.mod<-lmer(log(Ph)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")
Fieldpeas.N.mod<-lmer(log(N)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")
Fieldpeas.P.mod<-lmer(log(P)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")
Fieldpeas.K.mod<-lmer(log(K)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")
Fieldpeas.S.mod<-lmer(log(S)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")
Fieldpeas.B.mod<-lmer(log(B)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")
Fieldpeas.Ca.mod<-lmer(log(Ca)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")
Fieldpeas.Mg.mod<-lmer(log(Mg)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")
Fieldpeas.Mn.mod<-lmer(log(Mn)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")
Fieldpeas.Cu.mod<-lmer(log(Cu)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")

NA
Rice.Zn.modN<-lmer(log(Zn)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&NapplicnQ !="Low")
Rice.Fe.modN<-lmer(log(Fe)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&NapplicnQ !="Low")
NA
Rice.N.modN<-lmer(log(N)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&NapplicnQ !="Low")
Rice.P.modN<-lmer(log(P)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&NapplicnQ !="Low")
Rice.K.modN<-lmer(log(K)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&NapplicnQ !="Low")
Rice.S.modN<-lmer(log(S)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&NapplicnQ !="Low")
Rice.B.modN<-lmer(log(B)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&NapplicnQ !="Low")
Rice.Ca.modN<-lmer(log(Ca)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&NapplicnQ !="Low")
Rice.Mg.modN<-lmer(log(Mg)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&NapplicnQ !="Low")
Rice.Mn.modN<-lmer(log(Mn)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&NapplicnQ !="Low")
Rice.Cu.modN<-lmer(log(Cu)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&NapplicnQ !="Low")
NA
Rice.Zn.modT<-lmer(log(Zn)~CO2treatment*as.factor(Temperature_treatment)+(1|pair),data=FACEmp,subset=Crop=="Rice")
Rice.Fe.modT<-lmer(log(Fe)~CO2treatment*as.factor(Temperature_treatment)+(1|pair),data=FACEmp,subset=Crop=="Rice")
NA
Rice.N.modT<-lmer(log(N)~CO2treatment*as.factor(Temperature_treatment)+(1|pair),data=FACEmp,subset=Crop=="Rice")
Rice.P.modT<-lmer(log(P)~CO2treatment*as.factor(Temperature_treatment)+(1|pair),data=FACEmp,subset=Crop=="Rice")
Rice.K.modT<-lmer(log(K)~CO2treatment*as.factor(Temperature_treatment)+(1|pair),data=FACEmp,subset=Crop=="Rice")
Rice.S.modT<-lmer(log(S)~CO2treatment*as.factor(Temperature_treatment)+(1|pair),data=FACEmp,subset=Crop=="Rice")
Rice.B.modT<-lmer(log(B)~CO2treatment*as.factor(Temperature_treatment)+(1|pair),data=FACEmp,subset=Crop=="Rice")
Rice.Ca.modT<-lmer(log(Ca)~CO2treatment*as.factor(Temperature_treatment)+(1|pair),data=FACEmp,subset=Crop=="Rice")
Rice.Mg.modT<-lmer(log(Mg)~CO2treatment*as.factor(Temperature_treatment)+(1|pair),data=FACEmp,subset=Crop=="Rice")
Rice.Mn.modT<-lmer(log(Mn)~CO2treatment*as.factor(Temperature_treatment)+(1|pair),data=FACEmp,subset=Crop=="Rice")
Rice.Cu.modT<-lmer(log(Cu)~CO2treatment*as.factor(Temperature_treatment)+(1|pair),data=FACEmp,subset=Crop=="Rice")

Sorghum.Yield.mod<-lmer(log(Yield)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Sorghum")
Sorghum.Zn.mod<-lmer(log(Zn)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Sorghum")
Sorghum.Fe.mod<-lmer(log(Fe)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Sorghum")
NA
Sorghum.N.mod<-lmer(log(N)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Sorghum")
Sorghum.P.mod<-lmer(log(P)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Sorghum")
Sorghum.K.mod<-lmer(log(K)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Sorghum")
Sorghum.S.mod<-lmer(log(S)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Sorghum")
Sorghum.B.mod<-lmer(log(B)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Sorghum")
Sorghum.Ca.mod<-lmer(log(Ca)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Sorghum")
Sorghum.Mg.mod<-lmer(log(Mg)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Sorghum")
Sorghum.Mn.mod<-lmer(log(Mn)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Sorghum")
Sorghum.Cu.mod<-lmer(log(Cu)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Sorghum")


Sorghum.Yield.modWet<-lmer(log(Yield)~CO2treatment+(1|pair),data=FACEmp,subset=(Crop=="Sorghum"&WateringRegime=="Wet"))
Sorghum.Zn.modWet<-lmer(log(Zn)~CO2treatment+(1|pair),data=FACEmp,subset=(Crop=="Sorghum"&WateringRegime=="Wet"))
Sorghum.Fe.modWet<-lmer(log(Fe)~CO2treatment+(1|pair),data=FACEmp,subset=(Crop=="Sorghum"&WateringRegime=="Wet"))
NA
Sorghum.N.modWet<-lmer(log(N)~CO2treatment+(1|pair),data=FACEmp,subset=(Crop=="Sorghum"&WateringRegime=="Wet"))
Sorghum.P.modWet<-lmer(log(P)~CO2treatment+(1|pair),data=FACEmp,subset=(Crop=="Sorghum"&WateringRegime=="Wet"))
Sorghum.K.modWet<-lmer(log(K)~CO2treatment+(1|pair),data=FACEmp,subset=(Crop=="Sorghum"&WateringRegime=="Wet"))
Sorghum.S.modWet<-lmer(log(S)~CO2treatment+(1|pair),data=FACEmp,subset=(Crop=="Sorghum"&WateringRegime=="Wet"))
Sorghum.B.modWet<-lmer(log(B)~CO2treatment+(1|pair),data=FACEmp,subset=(Crop=="Sorghum"&WateringRegime=="Wet"))
Sorghum.Ca.modWet<-lmer(log(Ca)~CO2treatment+(1|pair),data=FACEmp,subset=(Crop=="Sorghum"&WateringRegime=="Wet"))
Sorghum.Mg.modWet<-lmer(log(Mg)~CO2treatment+(1|pair),data=FACEmp,subset=(Crop=="Sorghum"&WateringRegime=="Wet"))
Sorghum.Mn.modWet<-lmer(log(Mn)~CO2treatment+(1|pair),data=FACEmp,subset=(Crop=="Sorghum"&WateringRegime=="Wet"))
Sorghum.Cu.modWet<-lmer(log(Cu)~CO2treatment+(1|pair),data=FACEmp,subset=(Crop=="Sorghum"&WateringRegime=="Wet"))
Sorghum.Yield.modDry<-lmer(log(Yield)~CO2treatment+(1|pair),data=FACEmp,subset=(Crop=="Sorghum"&WateringRegime=="Dry"))
Sorghum.Zn.modDry<-lmer(log(Zn)~CO2treatment+(1|pair),data=FACEmp,subset=(Crop=="Sorghum"&WateringRegime=="Dry"))
Sorghum.Fe.modDry<-lmer(log(Fe)~CO2treatment+(1|pair),data=FACEmp,subset=(Crop=="Sorghum"&WateringRegime=="Dry"))
NA
Sorghum.N.modDry<-lmer(log(N)~CO2treatment+(1|pair),data=FACEmp,subset=(Crop=="Sorghum"&WateringRegime=="Dry"))
Sorghum.P.modDry<-lmer(log(P)~CO2treatment+(1|pair),data=FACEmp,subset=(Crop=="Sorghum"&WateringRegime=="Dry"))
Sorghum.K.modDry<-lmer(log(K)~CO2treatment+(1|pair),data=FACEmp,subset=(Crop=="Sorghum"&WateringRegime=="Dry"))
Sorghum.S.modDry<-lmer(log(S)~CO2treatment+(1|pair),data=FACEmp,subset=(Crop=="Sorghum"&WateringRegime=="Dry"))
Sorghum.B.modDry<-lmer(log(B)~CO2treatment+(1|pair),data=FACEmp,subset=(Crop=="Sorghum"&WateringRegime=="Dry"))
Sorghum.Ca.modDry<-lmer(log(Ca)~CO2treatment+(1|pair),data=FACEmp,subset=(Crop=="Sorghum"&WateringRegime=="Dry"))
Sorghum.Mg.modDry<-lmer(log(Mg)~CO2treatment+(1|pair),data=FACEmp,subset=(Crop=="Sorghum"&WateringRegime=="Dry"))
Sorghum.Mn.modDry<-lmer(log(Mn)~CO2treatment+(1|pair),data=FACEmp,subset=(Crop=="Sorghum"&WateringRegime=="Dry"))
Sorghum.Cu.modDry<-lmer(log(Cu)~CO2treatment+(1|pair),data=FACEmp,subset=(Crop=="Sorghum"&WateringRegime=="Dry"))



NA
soybean.Zn.mod<-lmer(log(Zn)~CO2treatment+(1|pair),data=FACEmp,subset=Crop=="soybean")
soybean.Fe.mod<-lmer(log(Fe)~CO2treatment+(1|pair),data=FACEmp,subset=Crop=="soybean")
NA
soybean.N.mod<-lmer(log(N)~CO2treatment+(1|pair),data=FACEmp,subset=Crop=="soybean")
soybean.P.mod<-lmer(log(P)~CO2treatment+(1|pair),data=FACEmp,subset=Crop=="soybean")
soybean.K.mod<-lmer(log(K)~CO2treatment+(1|pair),data=FACEmp,subset=Crop=="soybean")
soybean.S.mod<-lmer(log(S)~CO2treatment+(1|pair),data=FACEmp,subset=Crop=="soybean")
soybean.B.mod<-lmer(log(B)~CO2treatment+(1|pair),data=FACEmp,subset=Crop=="soybean")
soybean.Ca.mod<-lmer(log(Ca)~CO2treatment+(1|pair),data=FACEmp,subset=Crop=="soybean")
soybean.Mg.mod<-lmer(log(Mg)~CO2treatment+(1|pair),data=FACEmp,subset=Crop=="soybean")
soybean.Mn.mod<-lmer(log(Mn)~CO2treatment+(1|pair),data=FACEmp,subset=Crop=="soybean")
soybean.Cu.mod<-lmer(log(Cu)~CO2treatment+(1|pair),data=FACEmp,subset=Crop=="soybean")

NA
Corn.Zn.mod<-lmer(log(Zn)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Corn")
Corn.Fe.mod<-lmer(log(Fe)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Corn")
NA
Corn.N.mod<-lmer(log(N)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Corn")
Corn.P.mod<-lmer(log(P)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Corn")
Corn.K.mod<-lmer(log(K)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Corn")
Corn.S.mod<-lmer(log(S)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Corn")
Corn.B.mod<-lmer(log(B)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Corn")
Corn.Ca.mod<-lmer(log(Ca)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Corn")
Corn.Mg.mod<-lmer(log(Mg)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Corn")
Corn.Mn.mod<-lmer(log(Mn)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Corn")
Corn.Cu.mod<-lmer(log(Cu)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Corn")


#Wheat.Fe.mod.output<-data.frame(species = "Wheat", nutrient = "Fe", av = fixef(Wheat.Fe.mod)[1], ci =(2*sqrt(vcov(Wheat.Fe.mod)[1])))
Wheat.Fe.modN.output<-data.frame(species = "Wheat", nutrient = "Fe", av = fixef(Wheat.Fe.modN)[2], ci =vcov(Wheat.Fe.modN)[1])
Wheat.Yield.modN.output<-data.frame(species = "Wheat", nutrient = "Yield", av = fixef(Wheat.Yield.modN)[2], SE =sqrt(vcov(Wheat.Yield.modN)[2,2]))
Wheat.Zn.modN.output<-data.frame(species = "Wheat", nutrient = "Zn", av = fixef(Wheat.Zn.modN)[2], SE =sqrt(vcov(Wheat.Zn.modN)[2,2]))
Wheat.Fe.modN.output<-data.frame(species = "Wheat", nutrient = "Fe", av = fixef(Wheat.Fe.modN)[2], SE =sqrt(vcov(Wheat.Fe.modN)[2,2]))
Wheat.Ph.modN.output<-data.frame(species = "Wheat", nutrient = "Ph", av = fixef(Wheat.Ph.modN)[2], SE =sqrt(vcov(Wheat.Ph.modN)[2,2]))
Wheat.N.modN.output<-data.frame(species = "Wheat", nutrient = "N", av = fixef(Wheat.N.modN)[2], SE =sqrt(vcov(Wheat.N.modN)[2,2]))
Wheat.Fe.modW.output<-data.frame(species = "Wheat", nutrient = "Fe", av = fixef(Wheat.Fe.modW)[2], ci =vcov(Wheat.Fe.modW)[1])
Wheat.Yield.modW.output<-data.frame(species = "Wheat", nutrient = "Yield", av = fixef(Wheat.Yield.modW)[2], SE =sqrt(vcov(Wheat.Yield.modW)[2,2]))
Wheat.Zn.modW.output<-data.frame(species = "Wheat", nutrient = "Zn", av = fixef(Wheat.Zn.modW)[2], SE =sqrt(vcov(Wheat.Zn.modW)[2,2]))
Wheat.Fe.modW.output<-data.frame(species = "Wheat", nutrient = "Fe", av = fixef(Wheat.Fe.modW)[2], SE =sqrt(vcov(Wheat.Fe.modW)[2,2]))
Wheat.Ph.modW.output<-data.frame(species = "Wheat", nutrient = "Ph", av = fixef(Wheat.Ph.modW)[2], SE =sqrt(vcov(Wheat.Ph.modW)[2,2]))
Wheat.N.modW.output<-data.frame(species = "Wheat", nutrient = "N", av = fixef(Wheat.N.modW)[2], SE =sqrt(vcov(Wheat.N.modW)[2,2]))
NA
NA
NA
NA
NA
NA
NA
NA

Fieldpeas.Yield.mod.output<-data.frame(species = "Fieldpeas", nutrient = "Yield", av = fixef(Fieldpeas.Yield.mod)[2], SE =sqrt(vcov(Fieldpeas.Yield.mod)[2,2]))
Fieldpeas.Zn.mod.output<-data.frame(species = "Fieldpeas", nutrient = "Zn", av = fixef(Fieldpeas.Zn.mod)[2], SE =sqrt(vcov(Fieldpeas.Zn.mod)[2,2]))
Fieldpeas.Fe.mod.output<-data.frame(species = "Fieldpeas", nutrient = "Fe", av = fixef(Fieldpeas.Fe.mod)[2], SE =sqrt(vcov(Fieldpeas.Fe.mod)[2,2]))
Fieldpeas.Ph.mod.output<-data.frame(species = "Fieldpeas", nutrient = "Ph", av = fixef(Fieldpeas.Ph.mod)[2], SE =sqrt(vcov(Fieldpeas.Ph.mod)[2,2]))
Fieldpeas.N.mod.output<-data.frame(species = "Fieldpeas", nutrient = "N", av = fixef(Fieldpeas.N.mod)[2], SE =sqrt(vcov(Fieldpeas.N.mod)[2,2]))
Fieldpeas.P.mod.output<-data.frame(species = "Fieldpeas", nutrient = "P", av = fixef(Fieldpeas.P.mod)[2], SE =sqrt(vcov(Fieldpeas.P.mod)[2,2]))
Fieldpeas.K.mod.output<-data.frame(species = "Fieldpeas", nutrient = "K", av = fixef(Fieldpeas.K.mod)[2], SE =sqrt(vcov(Fieldpeas.K.mod)[2,2]))
Fieldpeas.S.mod.output<-data.frame(species = "Fieldpeas", nutrient = "S", av = fixef(Fieldpeas.S.mod)[2], SE =sqrt(vcov(Fieldpeas.S.mod)[2,2]))
Fieldpeas.B.mod.output<-data.frame(species = "Fieldpeas", nutrient = "B", av = fixef(Fieldpeas.B.mod)[2], SE =sqrt(vcov(Fieldpeas.B.mod)[2,2]))
Fieldpeas.Ca.mod.output<-data.frame(species = "Fieldpeas", nutrient = "Ca", av = fixef(Fieldpeas.Ca.mod)[2], SE =sqrt(vcov(Fieldpeas.Ca.mod)[2,2]))
Fieldpeas.Mg.mod.output<-data.frame(species = "Fieldpeas", nutrient = "Mg", av = fixef(Fieldpeas.Mg.mod)[2], SE =sqrt(vcov(Fieldpeas.Mg.mod)[2,2]))
Fieldpeas.Mn.mod.output<-data.frame(species = "Fieldpeas", nutrient = "Mn", av = fixef(Fieldpeas.Mn.mod)[2], SE =sqrt(vcov(Fieldpeas.Mn.mod)[2,2]))
Fieldpeas.Cu.mod.output<-data.frame(species = "Fieldpeas", nutrient = "Cu", av = fixef(Fieldpeas.Cu.mod)[2], SE =sqrt(vcov(Fieldpeas.Cu.mod)[2,2]))

NA
Rice.Zn.modT.output<-data.frame(species = "Rice", nutrient = "Zn", av = fixef(Rice.Zn.modT)[2], SE =sqrt(vcov(Rice.Zn.modT)[2,2]))
Rice.Fe.modT.output<-data.frame(species = "Rice", nutrient = "Fe", av = fixef(Rice.Fe.modT)[2], SE =sqrt(vcov(Rice.Fe.modT)[2,2]))
NA
Rice.N.modT.output<-data.frame(species = "Rice", nutrient = "N", av = fixef(Rice.N.modT)[2], SE =sqrt(vcov(Rice.N.modT)[2,2]))
Rice.P.modT.output<-data.frame(species = "Rice", nutrient = "P", av = fixef(Rice.P.modT)[2], SE =sqrt(vcov(Rice.P.modT)[2,2]))
Rice.K.modT.output<-data.frame(species = "Rice", nutrient = "K", av = fixef(Rice.K.modT)[2], SE =sqrt(vcov(Rice.K.modT)[2,2]))
Rice.S.modT.output<-data.frame(species = "Rice", nutrient = "S", av = fixef(Rice.S.modT)[2], SE =sqrt(vcov(Rice.S.modT)[2,2]))
Rice.B.modT.output<-data.frame(species = "Rice", nutrient = "B", av = fixef(Rice.B.modT)[2], SE =sqrt(vcov(Rice.B.modT)[2,2]))
Rice.Ca.modT.output<-data.frame(species = "Rice", nutrient = "Ca", av = fixef(Rice.Ca.modT)[2], SE =sqrt(vcov(Rice.Ca.modT)[2,2]))
Rice.Mg.modT.output<-data.frame(species = "Rice", nutrient = "Mg", av = fixef(Rice.Mg.modT)[2], SE =sqrt(vcov(Rice.Mg.modT)[2,2]))
Rice.Mn.modT.output<-data.frame(species = "Rice", nutrient = "Mn", av = fixef(Rice.Mn.modT)[2], SE =sqrt(vcov(Rice.Mn.modT)[2,2]))
Rice.Cu.modT.output<-data.frame(species = "Rice", nutrient = "Cu", av = fixef(Rice.Cu.modT)[2], SE =sqrt(vcov(Rice.Cu.modT)[2,2]))

Sorghum.Yield.mod.output<-data.frame(species = "Sorghum", nutrient = "Yield", av = fixef(Sorghum.Yield.mod)[2], SE =sqrt(vcov(Sorghum.Yield.mod)[2,2]))
Sorghum.Zn.mod.output<-data.frame(species = "Sorghum", nutrient = "Zn", av = fixef(Sorghum.Zn.mod)[2], SE =sqrt(vcov(Sorghum.Zn.mod)[2,2]))
Sorghum.Fe.mod.output<-data.frame(species = "Sorghum", nutrient = "Fe", av = fixef(Sorghum.Fe.mod)[2], SE =sqrt(vcov(Sorghum.Fe.mod)[2,2]))
NA
Sorghum.N.mod.output<-data.frame(species = "Sorghum", nutrient = "N", av = fixef(Sorghum.N.mod)[2], SE =sqrt(vcov(Sorghum.N.mod)[2,2]))
Sorghum.P.mod.output<-data.frame(species = "Sorghum", nutrient = "P", av = fixef(Sorghum.P.mod)[2], SE =sqrt(vcov(Sorghum.P.mod)[2,2]))
Sorghum.K.mod.output<-data.frame(species = "Sorghum", nutrient = "K", av = fixef(Sorghum.K.mod)[2], SE =sqrt(vcov(Sorghum.K.mod)[2,2]))
Sorghum.S.mod.output<-data.frame(species = "Sorghum", nutrient = "S", av = fixef(Sorghum.S.mod)[2], SE =sqrt(vcov(Sorghum.S.mod)[2,2]))
Sorghum.B.mod.output<-data.frame(species = "Sorghum", nutrient = "B", av = fixef(Sorghum.B.mod)[2], SE =sqrt(vcov(Sorghum.B.mod)[2,2]))
Sorghum.Ca.mod.output<-data.frame(species = "Sorghum", nutrient = "Ca", av = fixef(Sorghum.Ca.mod)[2], SE =sqrt(vcov(Sorghum.Ca.mod)[2,2]))
Sorghum.Mg.mod.output<-data.frame(species = "Sorghum", nutrient = "Mg", av = fixef(Sorghum.Mg.mod)[2], SE =sqrt(vcov(Sorghum.Mg.mod)[2,2]))
Sorghum.Mn.mod.output<-data.frame(species = "Sorghum", nutrient = "Mn", av = fixef(Sorghum.Mn.mod)[2], SE =sqrt(vcov(Sorghum.Mn.mod)[2,2]))
Sorghum.Cu.mod.output<-data.frame(species = "Sorghum", nutrient = "Cu", av = fixef(Sorghum.Cu.mod)[2], SE =sqrt(vcov(Sorghum.Cu.mod)[2,2]))
# Sorghum.Yield.mod.output<-data.frame(species = "Sorghum", nutrient = "Yield", av = fixef(Sorghum.Yield.mod), SE =sqrt(diag(vcov(Sorghum.Yield.mod))))
# Sorghum.Zn.mod.output<-data.frame(species = "Sorghum", nutrient = "Zn", av = fixef(Sorghum.Zn.mod), SE =sqrt(diag(vcov(Sorghum.Zn.mod))))
# Sorghum.Fe.mod.output<-data.frame(species = "Sorghum", nutrient = "Fe", av = fixef(Sorghum.Fe.mod), SE =sqrt(diag(vcov(Sorghum.Fe.mod))))
# NA
# Sorghum.N.mod.output<-data.frame(species = "Sorghum", nutrient = "N", av = fixef(Sorghum.N.mod), SE =sqrt(diag(vcov(Sorghum.N.mod))))
# Sorghum.P.mod.output<-data.frame(species = "Sorghum", nutrient = "P", av = fixef(Sorghum.P.mod), SE =sqrt(diag(vcov(Sorghum.P.mod))))
# Sorghum.K.mod.output<-data.frame(species = "Sorghum", nutrient = "K", av = fixef(Sorghum.K.mod), SE =sqrt(diag(vcov(Sorghum.K.mod))))
# Sorghum.S.mod.output<-data.frame(species = "Sorghum", nutrient = "S", av = fixef(Sorghum.S.mod), SE =sqrt(diag(vcov(Sorghum.S.mod))))
# Sorghum.B.mod.output<-data.frame(species = "Sorghum", nutrient = "B", av = fixef(Sorghum.B.mod), SE =sqrt(diag(vcov(Sorghum.B.mod))))
# Sorghum.Ca.mod.output<-data.frame(species = "Sorghum", nutrient = "Ca", av = fixef(Sorghum.Ca.mod), SE =sqrt(diag(vcov(Sorghum.Ca.mod))))
# Sorghum.Mg.mod.output<-data.frame(species = "Sorghum", nutrient = "Mg", av = fixef(Sorghum.Mg.mod), SE =sqrt(diag(vcov(Sorghum.Mg.mod))))
# Sorghum.Mn.mod.output<-data.frame(species = "Sorghum", nutrient = "Mn", av = fixef(Sorghum.Mn.mod), SE =sqrt(diag(vcov(Sorghum.Mn.mod))))
# Sorghum.Cu.mod.output<-data.frame(species = "Sorghum", nutrient = "Cu", av = fixef(Sorghum.Cu.mod), SE =sqrt(diag(vcov(Sorghum.Cu.mod))))

Sorghum.Yield.modWet.output<-data.frame(watering="Wet", species = "Sorghum", nutrient = "Yield", av = fixef(Sorghum.Yield.modWet)[2], SE =sqrt(vcov(Sorghum.Yield.modWet)[2,2]))
Sorghum.Zn.modWet.output<-data.frame(watering="Wet", species = "Sorghum", nutrient = "Zn", av = fixef(Sorghum.Zn.modWet)[2], SE =sqrt(vcov(Sorghum.Zn.modWet)[2,2]))
Sorghum.Fe.modWet.output<-data.frame(watering="Wet", species = "Sorghum", nutrient = "Fe", av = fixef(Sorghum.Fe.modWet)[2], SE =sqrt(vcov(Sorghum.Fe.modWet)[2,2]))
NA
Sorghum.N.modWet.output<-data.frame(watering="Wet", species = "Sorghum", nutrient = "N", av = fixef(Sorghum.N.modWet)[2], SE =sqrt(vcov(Sorghum.N.modWet)[2,2]))
Sorghum.P.modWet.output<-data.frame(watering="Wet", species = "Sorghum", nutrient = "P", av = fixef(Sorghum.P.modWet)[2], SE =sqrt(vcov(Sorghum.P.modWet)[2,2]))
Sorghum.K.modWet.output<-data.frame(watering="Wet", species = "Sorghum", nutrient = "K", av = fixef(Sorghum.K.modWet)[2], SE =sqrt(vcov(Sorghum.K.modWet)[2,2]))
Sorghum.S.modWet.output<-data.frame(watering="Wet", species = "Sorghum", nutrient = "S", av = fixef(Sorghum.S.modWet)[2], SE =sqrt(vcov(Sorghum.S.modWet)[2,2]))
Sorghum.B.modWet.output<-data.frame(watering="Wet", species = "Sorghum", nutrient = "B", av = fixef(Sorghum.B.modWet)[2], SE =sqrt(vcov(Sorghum.B.modWet)[2,2]))
Sorghum.Ca.modWet.output<-data.frame(watering="Wet", species = "Sorghum", nutrient = "Ca", av = fixef(Sorghum.Ca.modWet)[2], SE =sqrt(vcov(Sorghum.Ca.modWet)[2,2]))
Sorghum.Mg.modWet.output<-data.frame(watering="Wet", species = "Sorghum", nutrient = "Mg", av = fixef(Sorghum.Mg.modWet)[2], SE =sqrt(vcov(Sorghum.Mg.modWet)[2,2]))
Sorghum.Mn.modWet.output<-data.frame(watering="Wet", species = "Sorghum", nutrient = "Mn", av = fixef(Sorghum.Mn.modWet)[2], SE =sqrt(vcov(Sorghum.Mn.modWet)[2,2]))
Sorghum.Cu.modWet.output<-data.frame(watering="Wet", species = "Sorghum", nutrient = "Cu", av = fixef(Sorghum.Cu.modWet)[2], SE =sqrt(vcov(Sorghum.Cu.modWet)[2,2]))
Sorghum.Yield.modDry.output<-data.frame(watering="Dry", species = "Sorghum", nutrient = "Yield", av = fixef(Sorghum.Yield.modDry)[2], SE =sqrt(vcov(Sorghum.Yield.modDry)[2,2]))
Sorghum.Zn.modDry.output<-data.frame(watering="Dry", species = "Sorghum", nutrient = "Zn", av = fixef(Sorghum.Zn.modDry)[2], SE =sqrt(vcov(Sorghum.Zn.modDry)[2,2]))
Sorghum.Fe.modDry.output<-data.frame(watering="Dry", species = "Sorghum", nutrient = "Fe", av = fixef(Sorghum.Fe.modDry)[2], SE =sqrt(vcov(Sorghum.Fe.modDry)[2,2]))
NA
Sorghum.N.modDry.output<-data.frame(watering="Dry", species = "Sorghum", nutrient = "N", av = fixef(Sorghum.N.modDry)[2], SE =sqrt(vcov(Sorghum.N.modDry)[2,2]))
Sorghum.P.modDry.output<-data.frame(watering="Dry", species = "Sorghum", nutrient = "P", av = fixef(Sorghum.P.modDry)[2], SE =sqrt(vcov(Sorghum.P.modDry)[2,2]))
Sorghum.K.modDry.output<-data.frame(watering="Dry", species = "Sorghum", nutrient = "K", av = fixef(Sorghum.K.modDry)[2], SE =sqrt(vcov(Sorghum.K.modDry)[2,2]))
Sorghum.S.modDry.output<-data.frame(watering="Dry", species = "Sorghum", nutrient = "S", av = fixef(Sorghum.S.modDry)[2], SE =sqrt(vcov(Sorghum.S.modDry)[2,2]))
Sorghum.B.modDry.output<-data.frame(watering="Dry", species = "Sorghum", nutrient = "B", av = fixef(Sorghum.B.modDry)[2], SE =sqrt(vcov(Sorghum.B.modDry)[2,2]))
Sorghum.Ca.modDry.output<-data.frame(watering="Dry", species = "Sorghum", nutrient = "Ca", av = fixef(Sorghum.Ca.modDry)[2], SE =sqrt(vcov(Sorghum.Ca.modDry)[2,2]))
Sorghum.Mg.modDry.output<-data.frame(watering="Dry", species = "Sorghum", nutrient = "Mg", av = fixef(Sorghum.Mg.modDry)[2], SE =sqrt(vcov(Sorghum.Mg.modDry)[2,2]))
Sorghum.Mn.modDry.output<-data.frame(watering="Dry", species = "Sorghum", nutrient = "Mn", av = fixef(Sorghum.Mn.modDry)[2], SE =sqrt(vcov(Sorghum.Mn.modDry)[2,2]))
Sorghum.Cu.modDry.output<-data.frame(watering="Dry", species = "Sorghum", nutrient = "Cu", av = fixef(Sorghum.Cu.modDry)[2], SE =sqrt(vcov(Sorghum.Cu.modDry)[2,2]))



NA
soybean.Zn.mod.output<-data.frame(species = "soybean", nutrient = "Zn", av = fixef(soybean.Zn.mod)[2], SE =sqrt(vcov(soybean.Zn.mod)[2,2]))
soybean.Fe.mod.output<-data.frame(species = "soybean", nutrient = "Fe", av = fixef(soybean.Fe.mod)[2], SE =sqrt(vcov(soybean.Fe.mod)[2,2]))
NA
soybean.N.mod.output<-data.frame(species = "soybean", nutrient = "N", av = fixef(soybean.N.mod)[2], SE =sqrt(vcov(soybean.N.mod)[2,2]))
soybean.P.mod.output<-data.frame(species = "soybean", nutrient = "P", av = fixef(soybean.P.mod)[2], SE =sqrt(vcov(soybean.P.mod)[2,2]))
soybean.K.mod.output<-data.frame(species = "soybean", nutrient = "K", av = fixef(soybean.K.mod)[2], SE =sqrt(vcov(soybean.K.mod)[2,2]))
soybean.S.mod.output<-data.frame(species = "soybean", nutrient = "S", av = fixef(soybean.S.mod)[2], SE =sqrt(vcov(soybean.S.mod)[2,2]))
soybean.B.mod.output<-data.frame(species = "soybean", nutrient = "B", av = fixef(soybean.B.mod)[2], SE =sqrt(vcov(soybean.B.mod)[2,2]))
soybean.Ca.mod.output<-data.frame(species = "soybean", nutrient = "Ca", av = fixef(soybean.Ca.mod)[2], SE =sqrt(vcov(soybean.Ca.mod)[2,2]))
soybean.Mg.mod.output<-data.frame(species = "soybean", nutrient = "Mg", av = fixef(soybean.Mg.mod)[2], SE =sqrt(vcov(soybean.Mg.mod)[2,2]))
soybean.Mn.mod.output<-data.frame(species = "soybean", nutrient = "Mn", av = fixef(soybean.Mn.mod)[2], SE =sqrt(vcov(soybean.Mn.mod)[2,2]))
soybean.Cu.mod.output<-data.frame(species = "soybean", nutrient = "Cu", av = fixef(soybean.Cu.mod)[2], SE =sqrt(vcov(soybean.Cu.mod)[2,2]))

NA
Corn.Zn.mod.output<-data.frame(species = "Corn", nutrient = "Zn", av = fixef(Corn.Zn.mod)[2], SE =sqrt(vcov(Corn.Zn.mod)[2,2]))
Corn.Fe.mod.output<-data.frame(species = "Corn", nutrient = "Fe", av = fixef(Corn.Fe.mod)[2], SE =sqrt(vcov(Corn.Fe.mod)[2,2]))
NA
Corn.N.mod.output<-data.frame(species = "Corn", nutrient = "N", av = fixef(Corn.N.mod)[2], SE =sqrt(vcov(Corn.N.mod)[2,2]))
Corn.P.mod.output<-data.frame(species = "Corn", nutrient = "P", av = fixef(Corn.P.mod)[2], SE =sqrt(vcov(Corn.P.mod)[2,2]))
Corn.K.mod.output<-data.frame(species = "Corn", nutrient = "K", av = fixef(Corn.K.mod)[2], SE =sqrt(vcov(Corn.K.mod)[2,2]))
Corn.S.mod.output<-data.frame(species = "Corn", nutrient = "S", av = fixef(Corn.S.mod)[2], SE =sqrt(vcov(Corn.S.mod)[2,2]))
Corn.B.mod.output<-data.frame(species = "Corn", nutrient = "B", av = fixef(Corn.B.mod)[2], SE =sqrt(vcov(Corn.B.mod)[2,2]))
Corn.Ca.mod.output<-data.frame(species = "Corn", nutrient = "Ca", av = fixef(Corn.Ca.mod)[2], SE =sqrt(vcov(Corn.Ca.mod)[2,2]))
Corn.Mg.mod.output<-data.frame(species = "Corn", nutrient = "Mg", av = fixef(Corn.Mg.mod)[2], SE =sqrt(vcov(Corn.Mg.mod)[2,2]))
Corn.Mn.mod.output<-data.frame(species = "Corn", nutrient = "Mn", av = fixef(Corn.Mn.mod)[2], SE =sqrt(vcov(Corn.Mn.mod)[2,2]))
Corn.Cu.mod.output<-data.frame(species = "Corn", nutrient = "Cu", av = fixef(Corn.Cu.mod)[2], SE =sqrt(vcov(Corn.Cu.mod)[2,2]))



output.table.REMLs<-rbind(  
  Wheat.Fe.mod.output	,
  Wheat.Yield.mod.output	,
  Wheat.Zn.mod.output	,
  Wheat.Fe.mod.output	,
  Wheat.Ph.mod.output	,
  Wheat.N.mod.output	,
  NA	,
  NA	,
  NA	,
  NA	,
  NA	,
  NA	,
  NA	,
  NA	,
  
  Fieldpeas.Yield.mod.output	,
  Fieldpeas.Zn.mod.output	,
  Fieldpeas.Fe.mod.output	,
  NA	,
  Fieldpeas.N.mod.output	,
  Fieldpeas.P.mod.output	,
  Fieldpeas.K.mod.output	,
  Fieldpeas.S.mod.output	,
  Fieldpeas.B.mod.output	,
  Fieldpeas.Ca.mod.output	,
  Fieldpeas.Mg.mod.output	,
  Fieldpeas.Mn.mod.output	,
  Fieldpeas.Cu.mod.output	,
  
  NA	,
  Rice.Zn.mod.output	,
  Rice.Fe.mod.output	,
  NA	,
  Rice.N.mod.output	,
  Rice.P.mod.output	,
  Rice.K.mod.output	,
  Rice.S.mod.output	,
  Rice.B.mod.output	,
  Rice.Ca.mod.output	,
  Rice.Mg.mod.output	,
  Rice.Mn.mod.output	,
  Rice.Cu.mod.output	,

  #sorghum output table only
#sorghum.output<-rbind(
#   NA	,
#   Sorghum.Zn.mod.output	,
#   Sorghum.Fe.mod.output	,
#   NA	,
#   Sorghum.N.mod.output	,
#   Sorghum.P.mod.output	,
#   Sorghum.K.mod.output	,
#   Sorghum.S.mod.output	,
#   Sorghum.B.mod.output	,
#   Sorghum.Ca.mod.output	,
#   Sorghum.Mg.mod.output	,
#   Sorghum.Mn.mod.output	,
#   Sorghum.Cu.mod.output	
  NA  ,
  Sorghum.Zn.modWet.output	,
  Sorghum.Fe.modWet.output	,
  NA	,
  Sorghum.N.modWet.output	,
  Sorghum.P.modWet.output	,
  Sorghum.K.modWet.output	,
  Sorghum.S.modWet.output	,
  Sorghum.B.modWet.output	,
  Sorghum.Ca.modWet.output	,
  Sorghum.Mg.modWet.output	,
  Sorghum.Mn.modWet.output	,
  Sorghum.Cu.modWet.output	,
  NA	,
  Sorghum.Zn.modDry.output	,
  Sorghum.Fe.modDry.output	,
  NA	,
  Sorghum.N.modDry.output	,
  Sorghum.P.modDry.output	,
  Sorghum.K.modDry.output	,
  Sorghum.S.modDry.output	,
  Sorghum.B.modDry.output	,
  Sorghum.Ca.modDry.output	,
  Sorghum.Mg.modDry.output	,
  Sorghum.Mn.modDry.output	,
  Sorghum.Cu.modDry.output	
  
#)
,
  
  NA	,
  soybean.Zn.mod.output	,
  soybean.Fe.mod.output	,
  NA	,
  soybean.N.mod.output	,
  soybean.P.mod.output	,
  soybean.K.mod.output	,
  soybean.S.mod.output	,
  soybean.B.mod.output	,
  soybean.Ca.mod.output	,
  soybean.Mg.mod.output	,
  soybean.Mn.mod.output	,
  soybean.Cu.mod.output	,
  
  NA	,
  Corn.Zn.mod.output	,
  Corn.Fe.mod.output	,
  NA	,
  Corn.N.mod.output	,
  Corn.P.mod.output	,
  Corn.K.mod.output	,
  Corn.S.mod.output	,
  Corn.B.mod.output	,
  Corn.Ca.mod.output	,
  Corn.Mg.mod.output	,
  Corn.Mn.mod.output	,
  Corn.Cu.mod.output	)

write.csv(output.table.REMLs,file="~/Rdata/nutrients/REMLoutputs.csv")
write.csv(sorghum.output,file="~/Rdata/nutrients/sorghumoutput.csv")

Wheat.Yield.modN.drop<-data.frame("Crop"="Wheat", "Nutrient"="Yield",chisq = round(drop1(Wheat.Yield.modN, test = "Chisq")[4],5))
Wheat.Zn.modN.drop<-data.frame("Crop"="Wheat", "Nutrient"="Zn",chisq = round(drop1(Wheat.Zn.modN, test = "Chisq")[4],5))
Wheat.Fe.modN.drop<-data.frame("Crop"="Wheat", "Nutrient"="Fe",chisq = round(drop1(Wheat.Fe.modN, test = "Chisq")[4],5))
Wheat.Ph.modN.drop<-data.frame("Crop"="Wheat", "Nutrient"="Ph",chisq = round(drop1(Wheat.Ph.modN, test = "Chisq")[4],5))
Wheat.N.modN.drop<-data.frame("Crop"="Wheat", "Nutrient"="N",chisq = round(drop1(Wheat.N.modN, test = "Chisq")[4],5))
Wheat.Yield.modW.drop<-data.frame("Crop"="Wheat", "Nutrient"="Yield",chisq = round(drop1(Wheat.Yield.modW, test = "Chisq")[4],5))
Wheat.Zn.modW.drop<-data.frame("Crop"="Wheat", "Nutrient"="Zn",chisq = round(drop1(Wheat.Zn.modW, test = "Chisq")[4],5))
Wheat.Fe.modW.drop<-data.frame("Crop"="Wheat", "Nutrient"="Fe",chisq = round(drop1(Wheat.Fe.modW, test = "Chisq")[4],5))
Wheat.Ph.modW.drop<-data.frame("Crop"="Wheat", "Nutrient"="Ph",chisq = round(drop1(Wheat.Ph.modW, test = "Chisq")[4],5))
Wheat.N.modW.drop<-data.frame("Crop"="Wheat", "Nutrient"="N",chisq = round(drop1(Wheat.N.modW, test = "Chisq")[4],5))
NA
NA
NA
NA
NA
NA
NA
NA

Fieldpeas.Yield.mod.drop<-data.frame("Crop"="Fieldpeas", "Nutrient"="Yield",chisq = round(drop1(Fieldpeas.Yield.mod, test = "Chisq")[4],5))
Fieldpeas.Zn.mod.drop<-data.frame("Crop"="Fieldpeas", "Nutrient"="Zn",chisq = round(drop1(Fieldpeas.Zn.mod, test = "Chisq")[4],5))
Fieldpeas.Fe.mod.drop<-data.frame("Crop"="Fieldpeas", "Nutrient"="Fe",chisq = round(drop1(Fieldpeas.Fe.mod, test = "Chisq")[4],5))
NA
Fieldpeas.N.mod.drop<-data.frame("Crop"="Fieldpeas", "Nutrient"="N",chisq = round(drop1(Fieldpeas.N.mod, test = "Chisq")[4],5))
Fieldpeas.P.mod.drop<-data.frame("Crop"="Fieldpeas", "Nutrient"="P",chisq = round(drop1(Fieldpeas.P.mod, test = "Chisq")[4],5))
Fieldpeas.K.mod.drop<-data.frame("Crop"="Fieldpeas", "Nutrient"="K",chisq = round(drop1(Fieldpeas.K.mod, test = "Chisq")[4],5))
Fieldpeas.S.mod.drop<-data.frame("Crop"="Fieldpeas", "Nutrient"="S",chisq = round(drop1(Fieldpeas.S.mod, test = "Chisq")[4],5))
Fieldpeas.B.mod.drop<-data.frame("Crop"="Fieldpeas", "Nutrient"="B",chisq = round(drop1(Fieldpeas.B.mod, test = "Chisq")[4],5))
Fieldpeas.Ca.mod.drop<-data.frame("Crop"="Fieldpeas", "Nutrient"="Ca",chisq = round(drop1(Fieldpeas.Ca.mod, test = "Chisq")[4],5))
Fieldpeas.Mg.mod.drop<-data.frame("Crop"="Fieldpeas", "Nutrient"="Mg",chisq = round(drop1(Fieldpeas.Mg.mod, test = "Chisq")[4],5))
Fieldpeas.Mn.mod.drop<-data.frame("Crop"="Fieldpeas", "Nutrient"="Mn",chisq = round(drop1(Fieldpeas.Mn.mod, test = "Chisq")[4],5))
Fieldpeas.Cu.mod.drop<-data.frame("Crop"="Fieldpeas", "Nutrient"="Cu",chisq = round(drop1(Fieldpeas.Cu.mod, test = "Chisq")[4],5))

NA
Rice.Zn.modT.drop<-data.frame("Crop"="Rice", "Nutrient"="Zn",chisq = round(drop1(Rice.Zn.modT, test = "Chisq")[4],5))
Rice.Fe.modT.drop<-data.frame("Crop"="Rice", "Nutrient"="Fe",chisq = round(drop1(Rice.Fe.modT, test = "Chisq")[4],5))
NA
Rice.N.modT.drop<-data.frame("Crop"="Rice", "Nutrient"="N",chisq = round(drop1(Rice.N.modT, test = "Chisq")[4],5))
Rice.P.modT.drop<-data.frame("Crop"="Rice", "Nutrient"="P",chisq = round(drop1(Rice.P.modT, test = "Chisq")[4],5))
Rice.K.modT.drop<-data.frame("Crop"="Rice", "Nutrient"="K",chisq = round(drop1(Rice.K.modT, test = "Chisq")[4],5))
Rice.S.modT.drop<-data.frame("Crop"="Rice", "Nutrient"="S",chisq = round(drop1(Rice.S.modT, test = "Chisq")[4],5))
Rice.B.modT.drop<-data.frame("Crop"="Rice", "Nutrient"="B",chisq = round(drop1(Rice.B.modT, test = "Chisq")[4],5))
Rice.Ca.modT.drop<-data.frame("Crop"="Rice", "Nutrient"="Ca",chisq = round(drop1(Rice.Ca.modT, test = "Chisq")[4],5))
Rice.Mg.modT.drop<-data.frame("Crop"="Rice", "Nutrient"="Mg",chisq = round(drop1(Rice.Mg.modT, test = "Chisq")[4],5))
Rice.Mn.modT.drop<-data.frame("Crop"="Rice", "Nutrient"="Mn",chisq = round(drop1(Rice.Mn.modT, test = "Chisq")[4],5))
Rice.Cu.modT.drop<-data.frame("Crop"="Rice", "Nutrient"="Cu",chisq = round(drop1(Rice.Cu.modT, test = "Chisq")[4],5))

NA
Sorghum.Zn.mod.drop<-data.frame("Crop"="Sorghum", "Nutrient"="Zn",chisq = round(drop1(Sorghum.Zn.mod, test = "Chisq")[4],5))
Sorghum.Fe.mod.drop<-data.frame("Crop"="Sorghum", "Nutrient"="Fe",chisq = round(drop1(Sorghum.Fe.mod, test = "Chisq")[4],5))
NA
Sorghum.N.mod.drop<-data.frame("Crop"="Sorghum", "Nutrient"="N",chisq = round(drop1(Sorghum.N.mod, test = "Chisq")[4],5))
Sorghum.P.mod.drop<-data.frame("Crop"="Sorghum", "Nutrient"="P",chisq = round(drop1(Sorghum.P.mod, test = "Chisq")[4],5))
Sorghum.K.mod.drop<-data.frame("Crop"="Sorghum", "Nutrient"="K",chisq = round(drop1(Sorghum.K.mod, test = "Chisq")[4],5))
Sorghum.S.mod.drop<-data.frame("Crop"="Sorghum", "Nutrient"="S",chisq = round(drop1(Sorghum.S.mod, test = "Chisq")[4],5))
Sorghum.B.mod.drop<-data.frame("Crop"="Sorghum", "Nutrient"="B",chisq = round(drop1(Sorghum.B.mod, test = "Chisq")[4],5))
Sorghum.Ca.mod.drop<-data.frame("Crop"="Sorghum", "Nutrient"="Ca",chisq = round(drop1(Sorghum.Ca.mod, test = "Chisq")[4],5))
Sorghum.Mg.mod.drop<-data.frame("Crop"="Sorghum", "Nutrient"="Mg",chisq = round(drop1(Sorghum.Mg.mod, test = "Chisq")[4],5))
Sorghum.Mn.mod.drop<-data.frame("Crop"="Sorghum", "Nutrient"="Mn",chisq = round(drop1(Sorghum.Mn.mod, test = "Chisq")[4],5))
Sorghum.Cu.mod.drop<-data.frame("Crop"="Sorghum", "Nutrient"="Cu",chisq = round(drop1(Sorghum.Cu.mod, test = "Chisq")[4],5))

NA
soybean.Zn.mod.drop<-data.frame("Crop"="soybean", "Nutrient"="Zn",chisq = round(drop1(soybean.Zn.mod, test = "Chisq")[4],5))
soybean.Fe.mod.drop<-data.frame("Crop"="soybean", "Nutrient"="Fe",chisq = round(drop1(soybean.Fe.mod, test = "Chisq")[4],5))
NA
soybean.N.mod.drop<-data.frame("Crop"="soybean", "Nutrient"="N",chisq = round(drop1(soybean.N.mod, test = "Chisq")[4],5))
soybean.P.mod.drop<-data.frame("Crop"="soybean", "Nutrient"="P",chisq = round(drop1(soybean.P.mod, test = "Chisq")[4],5))
soybean.K.mod.drop<-data.frame("Crop"="soybean", "Nutrient"="K",chisq = round(drop1(soybean.K.mod, test = "Chisq")[4],5))
soybean.S.mod.drop<-data.frame("Crop"="soybean", "Nutrient"="S",chisq = round(drop1(soybean.S.mod, test = "Chisq")[4],5))
soybean.B.mod.drop<-data.frame("Crop"="soybean", "Nutrient"="B",chisq = round(drop1(soybean.B.mod, test = "Chisq")[4],5))
soybean.Ca.mod.drop<-data.frame("Crop"="soybean", "Nutrient"="Ca",chisq = round(drop1(soybean.Ca.mod, test = "Chisq")[4],5))
soybean.Mg.mod.drop<-data.frame("Crop"="soybean", "Nutrient"="Mg",chisq = round(drop1(soybean.Mg.mod, test = "Chisq")[4],5))
soybean.Mn.mod.drop<-data.frame("Crop"="soybean", "Nutrient"="Mn",chisq = round(drop1(soybean.Mn.mod, test = "Chisq")[4],5))
soybean.Cu.mod.drop<-data.frame("Crop"="soybean", "Nutrient"="Cu",chisq = round(drop1(soybean.Cu.mod, test = "Chisq")[4],5))

NA
Corn.Zn.mod.drop<-data.frame("Crop"="Corn", "Nutrient"="Zn",chisq = round(drop1(Corn.Zn.mod, test = "Chisq")[4],5))
Corn.Fe.mod.drop<-data.frame("Crop"="Corn", "Nutrient"="Fe",chisq = round(drop1(Corn.Fe.mod, test = "Chisq")[4],5))
NA
Corn.N.mod.drop<-data.frame("Crop"="Corn", "Nutrient"="N",chisq = round(drop1(Corn.N.mod, test = "Chisq")[4],5))
Corn.P.mod.drop<-data.frame("Crop"="Corn", "Nutrient"="P",chisq = round(drop1(Corn.P.mod, test = "Chisq")[4],5))
Corn.K.mod.drop<-data.frame("Crop"="Corn", "Nutrient"="K",chisq = round(drop1(Corn.K.mod, test = "Chisq")[4],5))
Corn.S.mod.drop<-data.frame("Crop"="Corn", "Nutrient"="S",chisq = round(drop1(Corn.S.mod, test = "Chisq")[4],5))
Corn.B.mod.drop<-data.frame("Crop"="Corn", "Nutrient"="B",chisq = round(drop1(Corn.B.mod, test = "Chisq")[4],5))
Corn.Ca.mod.drop<-data.frame("Crop"="Corn", "Nutrient"="Ca",chisq = round(drop1(Corn.Ca.mod, test = "Chisq")[4],5))
Corn.Mg.mod.drop<-data.frame("Crop"="Corn", "Nutrient"="Mg",chisq = round(drop1(Corn.Mg.mod, test = "Chisq")[4],5))
Corn.Mn.mod.drop<-data.frame("Crop"="Corn", "Nutrient"="Mn",chisq = round(drop1(Corn.Mn.mod, test = "Chisq")[4],5))
Corn.Cu.mod.drop<-data.frame("Crop"="Corn", "Nutrient"="Cu",chisq = round(drop1(Corn.Cu.mod, test = "Chisq")[4],5))


output.table.REMLs.drop.wheat.and.riceT<-rbind(  
  Wheat.Yield.modN.drop	,
  Wheat.Zn.modN.drop	,
  Wheat.Fe.modN.drop	,
  Wheat.Ph.modN.drop	,
  Wheat.N.modN.drop,
  Wheat.Yield.modW.drop	,
  Wheat.Zn.modW.drop	,
  Wheat.Fe.modW.drop	,
  Wheat.Ph.modW.drop	,
  Wheat.N.modW.drop,
NA  ,
Rice.Zn.modT.drop	,
Rice.Fe.modT.drop	,
NA	,
Rice.N.modT.drop	,
Rice.P.modT.drop	,
Rice.K.modT.drop	,
Rice.S.modT.drop	,
Rice.B.modT.drop	,
Rice.Ca.modT.drop	,
Rice.Mg.modT.drop	,
Rice.Mn.modT.drop	,
Rice.Cu.modT.drop	)
  
  
  ,
  NA	,
  NA	,
  NA	,
  NA	,
  NA	,
  NA	,
  NA	,
  NA	,
  
  Fieldpeas.Yield.mod.drop	,
  Fieldpeas.Zn.mod.drop	,
  Fieldpeas.Fe.mod.drop	,
  NA	,
  Fieldpeas.N.mod.drop	,
  Fieldpeas.P.mod.drop	,
  Fieldpeas.K.mod.drop	,
  Fieldpeas.S.mod.drop	,
  Fieldpeas.B.mod.drop	,
  Fieldpeas.Ca.mod.drop	,
  Fieldpeas.Mg.mod.drop	,
  Fieldpeas.Mn.mod.drop	,
  Fieldpeas.Cu.mod.drop	,
  

  
  NA	,
  Sorghum.Zn.mod.drop
  ,
  Sorghum.Fe.mod.drop	,
  NA	,
  Sorghum.N.mod.drop	,
  Sorghum.P.mod.drop	,
  Sorghum.K.mod.drop	,
  Sorghum.S.mod.drop	,
  Sorghum.B.mod.drop	,
  Sorghum.Ca.mod.drop	,
  Sorghum.Mg.mod.drop	,
  Sorghum.Mn.mod.drop	,
  Sorghum.Cu.mod.drop	,
  
  NA	,
  soybean.Zn.mod.drop	,
  soybean.Fe.mod.drop	,
  NA	,
  soybean.N.mod.drop	,
  soybean.P.mod.drop	,
  soybean.K.mod.drop	,
  soybean.S.mod.drop	,
  soybean.B.mod.drop	,
  soybean.Ca.mod.drop	,
  soybean.Mg.mod.drop	,
  soybean.Mn.mod.drop	,
  soybean.Cu.mod.drop	,
  
  NA	,
  Corn.Zn.mod.drop	,
  Corn.Fe.mod.drop	,
  NA	,
  Corn.N.mod.drop	,
  Corn.P.mod.drop	,
  Corn.K.mod.drop	,
  Corn.S.mod.drop	,
  Corn.B.mod.drop	,
  Corn.Ca.mod.drop	,
  Corn.Mg.mod.drop	,
  Corn.Mn.mod.drop	,
  Corn.Cu.mod.drop	)

write.csv(output.table.REMLs.drop.wheat.and.riceT,file="~/Rdata/nutrients/REMLPvalswheatriceT.csv")


####yield and uptake#####

FACEmp["Znu"]<-FACEmp$Zn*FACEmp$Yield
FACEmp["Feu"]<-FACEmp$Fe*FACEmp$Yield
FACEmp["Phu"]<-FACEmp$Ph*FACEmp$Yield
FACEmp["Nu"]<-FACEmp$N*FACEmp$Yield
FACEmp["Pu"]<-FACEmp$P*FACEmp$Yield
FACEmp["Ku"]<-FACEmp$K*FACEmp$Yield
FACEmp["Su"]<-FACEmp$S*FACEmp$Yield
FACEmp["Bu"]<-FACEmp$B*FACEmp$Yield
FACEmp["Cau"]<-FACEmp$Ca*FACEmp$Yield
FACEmp["Mgu"]<-FACEmp$Mg*FACEmp$Yield
FACEmp["Mnu"]<-FACEmp$Mn*FACEmp$Yield
FACEmp["Cuu"]<-FACEmp$Cu*FACEmp$Yield

Wheat.Yield.mod<-lmer(log(Yield)~CO2treatment+as.factor(WateringRegime)+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="wheat")
Wheat.Znu.mod<-lmer(log(Znu)~CO2treatment+as.factor(WateringRegime)+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="wheat")
Wheat.Feu.mod<-lmer(log(Feu)~CO2treatment+as.factor(WateringRegime)+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="wheat")
Wheat.Phu.mod<-lmer(log(Phu)~CO2treatment+as.factor(WateringRegime)+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="wheat")
Wheat.Nu.mod<-lmer(log(Nu)~CO2treatment+as.factor(WateringRegime)+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="wheat")

Fieldpeas.Yield.mod<-lmer(log(Yield)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")
Fieldpeas.Znu.mod<-lmer(log(Znu)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")
Fieldpeas.Feu.mod<-lmer(log(Feu)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")
Fieldpeas.Phu.mod<-lmer(log(Phu)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")
Fieldpeas.Nu.mod<-lmer(log(Nu)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")
Fieldpeas.Pu.mod<-lmer(log(Pu)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")
Fieldpeas.Ku.mod<-lmer(log(Ku)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")
Fieldpeas.Su.mod<-lmer(log(Su)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")
Fieldpeas.Bu.mod<-lmer(log(Bu)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")
Fieldpeas.Cau.mod<-lmer(log(Cau)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")
Fieldpeas.Mgu.mod<-lmer(log(Mgu)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")
Fieldpeas.Mnu.mod<-lmer(log(Mnu)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")
Fieldpeas.Cuu.mod<-lmer(log(Cuu)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")

Sorghum.Yield.mod<-lmer(log(Yield)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Sorghum")
Sorghum.Znu.mod<-lmer(log(Znu)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Sorghum")
Sorghum.Feu.mod<-lmer(log(Feu)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Sorghum")
NA
Sorghum.Nu.mod<-lmer(log(Nu)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Sorghum")
Sorghum.Pu.mod<-lmer(log(Pu)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Sorghum")
Sorghum.Ku.mod<-lmer(log(Ku)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Sorghum")
Sorghum.Su.mod<-lmer(log(Su)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Sorghum")
Sorghum.Bu.mod<-lmer(log(Bu)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Sorghum")
Sorghum.Cau.mod<-lmer(log(Cau)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Sorghum")
Sorghum.Mgu.mod<-lmer(log(Mgu)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Sorghum")
Sorghum.Mnu.mod<-lmer(log(Mnu)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Sorghum")
Sorghum.Cuu.mod<-lmer(log(Cuu)~CO2treatment+as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Sorghum")

Rice.Yield.mod<-lmer(log(Yield)~CO2treatment+(1|pair),data=FACEmp,subset=Crop=="Rice")
Rice.Znu.mod<-lmer(log(Znu)~CO2treatment+(1|pair),data=FACEmp,subset=Crop=="Rice")
Rice.Feu.mod<-lmer(log(Feu)~CO2treatment+(1|pair),data=FACEmp,subset=Crop=="Rice")
Rice.Nu.mod<-lmer(log(Nu)~CO2treatment+(1|pair),data=FACEmp,subset=Crop=="Rice")
Rice.Pu.mod<-lmer(log(Pu)~CO2treatment+(1|pair),data=FACEmp,subset=Crop=="Rice")
Rice.Ku.mod<-lmer(log(Ku)~CO2treatment+(1|pair),data=FACEmp,subset=Crop=="Rice")
Rice.Su.mod<-lmer(log(Su)~CO2treatment+(1|pair),data=FACEmp,subset=Crop=="Rice")
Rice.Bu.mod<-lmer(log(Bu)~CO2treatment+(1|pair),data=FACEmp,subset=Crop=="Rice")
Rice.Cau.mod<-lmer(log(Cau)~CO2treatment+(1|pair),data=FACEmp,subset=Crop=="Rice")
Rice.Mgu.mod<-lmer(log(Mgu)~CO2treatment+(1|pair),data=FACEmp,subset=Crop=="Rice")
Rice.Mnu.mod<-lmer(log(Mnu)~CO2treatment+(1|pair),data=FACEmp,subset=Crop=="Rice")
Rice.Cuu.mod<-lmer(log(Cuu)~CO2treatment+(1|pair),data=FACEmp,subset=Crop=="Rice")

##output####
Wheat.Feu.mod.output<-data.frame(species = "Wheat", nutrient = "Feu", av = fixef(Wheat.Feu.mod)[2], ci =vcov(Wheat.Feu.mod)[1])
Wheat.Yield.mod.output<-data.frame(species = "Wheat", nutrient = "Yield", av = fixef(Wheat.Yield.mod)[2], SE =sqrt(vcov(Wheat.Yield.mod)[2,2]))
Wheat.Znu.mod.output<-data.frame(species = "Wheat", nutrient = "Znu", av = fixef(Wheat.Znu.mod)[2], SE =sqrt(vcov(Wheat.Znu.mod)[2,2]))
Wheat.Feu.mod.output<-data.frame(species = "Wheat", nutrient = "Feu", av = fixef(Wheat.Feu.mod)[2], SE =sqrt(vcov(Wheat.Feu.mod)[2,2]))
Wheat.Phu.mod.output<-data.frame(species = "Wheat", nutrient = "Phu", av = fixef(Wheat.Phu.mod)[2], SE =sqrt(vcov(Wheat.Phu.mod)[2,2]))
Wheat.Nu.mod.output<-data.frame(species = "Wheat", nutrient = "Nu", av = fixef(Wheat.Nu.mod)[2], SE =sqrt(vcov(Wheat.Nu.mod)[2,2]))

Fieldpeas.Yield.mod.output<-data.frame(species = "Fieldpeas", nutrient = "Yield", av = fixef(Fieldpeas.Yield.mod)[2], SE =sqrt(vcov(Fieldpeas.Yield.mod)[2,2]))
Fieldpeas.Znu.mod.output<-data.frame(species = "Fieldpeas", nutrient = "Znu", av = fixef(Fieldpeas.Znu.mod)[2], SE =sqrt(vcov(Fieldpeas.Znu.mod)[2,2]))
Fieldpeas.Feu.mod.output<-data.frame(species = "Fieldpeas", nutrient = "Feu", av = fixef(Fieldpeas.Feu.mod)[2], SE =sqrt(vcov(Fieldpeas.Feu.mod)[2,2]))
Fieldpeas.Nu.mod.output<-data.frame(species = "Fieldpeas", nutrient = "Nu", av = fixef(Fieldpeas.Nu.mod)[2], SE =sqrt(vcov(Fieldpeas.Nu.mod)[2,2]))
Fieldpeas.Pu.mod.output<-data.frame(species = "Fieldpeas", nutrient = "Pu", av = fixef(Fieldpeas.Pu.mod)[2], SE =sqrt(vcov(Fieldpeas.Pu.mod)[2,2]))
Fieldpeas.Ku.mod.output<-data.frame(species = "Fieldpeas", nutrient = "Ku", av = fixef(Fieldpeas.Ku.mod)[2], SE =sqrt(vcov(Fieldpeas.Ku.mod)[2,2]))
Fieldpeas.Su.mod.output<-data.frame(species = "Fieldpeas", nutrient = "Su", av = fixef(Fieldpeas.Su.mod)[2], SE =sqrt(vcov(Fieldpeas.Su.mod)[2,2]))
Fieldpeas.Bu.mod.output<-data.frame(species = "Fieldpeas", nutrient = "Bu", av = fixef(Fieldpeas.Bu.mod)[2], SE =sqrt(vcov(Fieldpeas.Bu.mod)[2,2]))
Fieldpeas.Cau.mod.output<-data.frame(species = "Fieldpeas", nutrient = "Cau", av = fixef(Fieldpeas.Cau.mod)[2], SE =sqrt(vcov(Fieldpeas.Cau.mod)[2,2]))
Fieldpeas.Mgu.mod.output<-data.frame(species = "Fieldpeas", nutrient = "Mgu", av = fixef(Fieldpeas.Mgu.mod)[2], SE =sqrt(vcov(Fieldpeas.Mgu.mod)[2,2]))
Fieldpeas.Mnu.mod.output<-data.frame(species = "Fieldpeas", nutrient = "Mnu", av = fixef(Fieldpeas.Mnu.mod)[2], SE =sqrt(vcov(Fieldpeas.Mnu.mod)[2,2]))
Fieldpeas.Cuu.mod.output<-data.frame(species = "Fieldpeas", nutrient = "Cuu", av = fixef(Fieldpeas.Cuu.mod)[2], SE =sqrt(vcov(Fieldpeas.Cuu.mod)[2,2]))

Sorghum.Yield.mod.output<-data.frame(species = "Sorghum", nutrient = "Yield", av = fixef(Sorghum.Yield.mod)[2], SE =sqrt(vcov(Sorghum.Yield.mod)[2,2]))
Sorghum.Znu.mod.output<-data.frame(species = "Sorghum", nutrient = "Znu", av = fixef(Sorghum.Znu.mod)[2], SE =sqrt(vcov(Sorghum.Znu.mod)[2,2]))
Sorghum.Feu.mod.output<-data.frame(species = "Sorghum", nutrient = "Feu", av = fixef(Sorghum.Feu.mod)[2], SE =sqrt(vcov(Sorghum.Feu.mod)[2,2]))
Sorghum.Nu.mod.output<-data.frame(species = "Sorghum", nutrient = "Nu", av = fixef(Sorghum.Nu.mod)[2], SE =sqrt(vcov(Sorghum.Nu.mod)[2,2]))
Sorghum.Pu.mod.output<-data.frame(species = "Sorghum", nutrient = "Pu", av = fixef(Sorghum.Pu.mod)[2], SE =sqrt(vcov(Sorghum.Pu.mod)[2,2]))
Sorghum.Ku.mod.output<-data.frame(species = "Sorghum", nutrient = "Ku", av = fixef(Sorghum.Ku.mod)[2], SE =sqrt(vcov(Sorghum.Ku.mod)[2,2]))
Sorghum.Su.mod.output<-data.frame(species = "Sorghum", nutrient = "Su", av = fixef(Sorghum.Su.mod)[2], SE =sqrt(vcov(Sorghum.Su.mod)[2,2]))
Sorghum.Bu.mod.output<-data.frame(species = "Sorghum", nutrient = "Bu", av = fixef(Sorghum.Bu.mod)[2], SE =sqrt(vcov(Sorghum.Bu.mod)[2,2]))
Sorghum.Cau.mod.output<-data.frame(species = "Sorghum", nutrient = "Cau", av = fixef(Sorghum.Cau.mod)[2], SE =sqrt(vcov(Sorghum.Cau.mod)[2,2]))
Sorghum.Mgu.mod.output<-data.frame(species = "Sorghum", nutrient = "Mgu", av = fixef(Sorghum.Mgu.mod)[2], SE =sqrt(vcov(Sorghum.Mgu.mod)[2,2]))
Sorghum.Mnu.mod.output<-data.frame(species = "Sorghum", nutrient = "Mnu", av = fixef(Sorghum.Mnu.mod)[2], SE =sqrt(vcov(Sorghum.Mnu.mod)[2,2]))
Sorghum.Cuu.mod.output<-data.frame(species = "Sorghum", nutrient = "Cuu", av = fixef(Sorghum.Cuu.mod)[2], SE =sqrt(vcov(Sorghum.Cuu.mod)[2,2]))

Rice.Yield.mod.output<-data.frame(species = "Rice", nutrient = "Yield", av = fixef(Rice.Yield.mod)[2], SE =sqrt(vcov(Rice.Yield.mod)[2,2]))
Rice.Znu.mod.output<-data.frame(species = "Rice", nutrient = "Znu", av = fixef(Rice.Znu.mod)[2], SE =sqrt(vcov(Rice.Znu.mod)[2,2]))
Rice.Feu.mod.output<-data.frame(species = "Rice", nutrient = "Feu", av = fixef(Rice.Feu.mod)[2], SE =sqrt(vcov(Rice.Feu.mod)[2,2]))

Rice.Nu.mod.output<-data.frame(species = "Rice", nutrient = "Nu", av = fixef(Rice.Nu.mod)[2], SE =sqrt(vcov(Rice.Nu.mod)[2,2]))
Rice.Pu.mod.output<-data.frame(species = "Rice", nutrient = "Pu", av = fixef(Rice.Pu.mod)[2], SE =sqrt(vcov(Rice.Pu.mod)[2,2]))
Rice.Ku.mod.output<-data.frame(species = "Rice", nutrient = "Ku", av = fixef(Rice.Ku.mod)[2], SE =sqrt(vcov(Rice.Ku.mod)[2,2]))
Rice.Su.mod.output<-data.frame(species = "Rice", nutrient = "Su", av = fixef(Rice.Su.mod)[2], SE =sqrt(vcov(Rice.Su.mod)[2,2]))
Rice.Bu.mod.output<-data.frame(species = "Rice", nutrient = "Bu", av = fixef(Rice.Bu.mod)[2], SE =sqrt(vcov(Rice.Bu.mod)[2,2]))
Rice.Cau.mod.output<-data.frame(species = "Rice", nutrient = "Cau", av = fixef(Rice.Cau.mod)[2], SE =sqrt(vcov(Rice.Cau.mod)[2,2]))
Rice.Mgu.mod.output<-data.frame(species = "Rice", nutrient = "Mgu", av = fixef(Rice.Mgu.mod)[2], SE =sqrt(vcov(Rice.Mgu.mod)[2,2]))
Rice.Mnu.mod.output<-data.frame(species = "Rice", nutrient = "Mnu", av = fixef(Rice.Mnu.mod)[2], SE =sqrt(vcov(Rice.Mnu.mod)[2,2]))
Rice.Cuu.mod.output<-data.frame(species = "Rice", nutrient = "Cuu", av = fixef(Rice.Cuu.mod)[2], SE =sqrt(vcov(Rice.Cuu.mod)[2,2]))


output.table.REMLsU<-rbind(  
  Wheat.Feu.mod.output	,
  Wheat.Yield.mod.output	,
  Wheat.Znu.mod.output	,
  Wheat.Feu.mod.output	,
  Wheat.Phu.mod.output	,
  Wheat.Nu.mod.output	,
  
  Fieldpeas.Yield.mod.output	,
  Fieldpeas.Znu.mod.output	,
  Fieldpeas.Feu.mod.output	,
  
  Fieldpeas.Nu.mod.output	,
  Fieldpeas.Pu.mod.output	,
  Fieldpeas.Ku.mod.output	,
  Fieldpeas.Su.mod.output	,
  Fieldpeas.Bu.mod.output	,
  Fieldpeas.Cau.mod.output	,
  Fieldpeas.Mgu.mod.output	,
  Fieldpeas.Mnu.mod.output	,
  Fieldpeas.Cuu.mod.output	,
  
  
  Sorghum.Znu.mod.output	,
  Sorghum.Feu.mod.output	,
  
  Sorghum.Nu.mod.output	,
  Sorghum.Pu.mod.output	,
  Sorghum.Ku.mod.output	,
  Sorghum.Su.mod.output	,
  Sorghum.Bu.mod.output	,
  Sorghum.Cau.mod.output	,
  Sorghum.Mgu.mod.output	,
  Sorghum.Mnu.mod.output	,
  Sorghum.Cuu.mod.output	,
  Rice.Znu.mod.output  ,
  Rice.Feu.mod.output	,
  Rice.Nu.mod.output	,
  Rice.Pu.mod.output	,
  Rice.Ku.mod.output	,
  Rice.Su.mod.output	,
  Rice.Bu.mod.output	,
  Rice.Cau.mod.output	,
  Rice.Mgu.mod.output	,
  Rice.Mnu.mod.output	,
  Rice.Cuu.mod.output	
  )

write.csv(output.table.REMLsU,"~/Rdata/nutrients/uptake_CO2_intercepts.csv")

#drop factors
Wheat.Yield.mod.drop<-data.frame("Crop"="Wheat", "Nutrient"="Yield",chisq = round(drop1(Wheat.Yield.mod, test = "Chisq")[4],5))
Wheat.Znu.mod.drop<-data.frame("Crop"="Wheat", "Nutrient"="Znu",chisq = round(drop1(Wheat.Znu.mod, test = "Chisq")[4],5))
Wheat.Feu.mod.drop<-data.frame("Crop"="Wheat", "Nutrient"="Feu",chisq = round(drop1(Wheat.Feu.mod, test = "Chisq")[4],5))
Wheat.Phu.mod.drop<-data.frame("Crop"="Wheat", "Nutrient"="Phu",chisq = round(drop1(Wheat.Phu.mod, test = "Chisq")[4],5))
Wheat.Nu.mod.drop<-data.frame("Crop"="Wheat", "Nutrient"="Nu",chisq = round(drop1(Wheat.Nu.mod, test = "Chisq")[4],5))

Fieldpeas.Yield.mod.drop<-data.frame("Crop"="Fieldpeas", "Nutrient"="Yield",chisq = round(drop1(Fieldpeas.Yield.mod, test = "Chisq")[4],5))
Fieldpeas.Znu.mod.drop<-data.frame("Crop"="Fieldpeas", "Nutrient"="Znu",chisq = round(drop1(Fieldpeas.Znu.mod, test = "Chisq")[4],5))
Fieldpeas.Feu.mod.drop<-data.frame("Crop"="Fieldpeas", "Nutrient"="Feu",chisq = round(drop1(Fieldpeas.Feu.mod, test = "Chisq")[4],5))
Fieldpeas.Nu.mod.drop<-data.frame("Crop"="Fieldpeas", "Nutrient"="Nu",chisq = round(drop1(Fieldpeas.Nu.mod, test = "Chisq")[4],5))
Fieldpeas.Pu.mod.drop<-data.frame("Crop"="Fieldpeas", "Nutrient"="Pu",chisq = round(drop1(Fieldpeas.Pu.mod, test = "Chisq")[4],5))
Fieldpeas.Ku.mod.drop<-data.frame("Crop"="Fieldpeas", "Nutrient"="Ku",chisq = round(drop1(Fieldpeas.Ku.mod, test = "Chisq")[4],5))
Fieldpeas.Su.mod.drop<-data.frame("Crop"="Fieldpeas", "Nutrient"="Su",chisq = round(drop1(Fieldpeas.Su.mod, test = "Chisq")[4],5))
Fieldpeas.Bu.mod.drop<-data.frame("Crop"="Fieldpeas", "Nutrient"="Bu",chisq = round(drop1(Fieldpeas.Bu.mod, test = "Chisq")[4],5))
Fieldpeas.Cau.mod.drop<-data.frame("Crop"="Fieldpeas", "Nutrient"="Cau",chisq = round(drop1(Fieldpeas.Cau.mod, test = "Chisq")[4],5))
Fieldpeas.Mgu.mod.drop<-data.frame("Crop"="Fieldpeas", "Nutrient"="Mgu",chisq = round(drop1(Fieldpeas.Mgu.mod, test = "Chisq")[4],5))
Fieldpeas.Mnu.mod.drop<-data.frame("Crop"="Fieldpeas", "Nutrient"="Mnu",chisq = round(drop1(Fieldpeas.Mnu.mod, test = "Chisq")[4],5))
Fieldpeas.Cuu.mod.drop<-data.frame("Crop"="Fieldpeas", "Nutrient"="Cuu",chisq = round(drop1(Fieldpeas.Cuu.mod, test = "Chisq")[4],5))

Rice.Yield.mod.drop<-data.frame("Crop"="Rice", "Nutrient"="Yield",chisq = round(drop1(Rice.Yield.mod, test = "Chisq")[4],5))
Rice.Znu.mod.drop<-data.frame("Crop"="Rice", "Nutrient"="Znu",chisq = round(drop1(Rice.Znu.mod, test = "Chisq")[4],5))
Rice.Feu.mod.drop<-data.frame("Crop"="Rice", "Nutrient"="Feu",chisq = round(drop1(Rice.Feu.mod, test = "Chisq")[4],5))
Rice.Nu.mod.drop<-data.frame("Crop"="Rice", "Nutrient"="Nu",chisq = round(drop1(Rice.Nu.mod, test = "Chisq")[4],5))
Rice.Pu.mod.drop<-data.frame("Crop"="Rice", "Nutrient"="Pu",chisq = round(drop1(Rice.Pu.mod, test = "Chisq")[4],5))
Rice.Ku.mod.drop<-data.frame("Crop"="Rice", "Nutrient"="Ku",chisq = round(drop1(Rice.Ku.mod, test = "Chisq")[4],5))
Rice.Su.mod.drop<-data.frame("Crop"="Rice", "Nutrient"="Su",chisq = round(drop1(Rice.Su.mod, test = "Chisq")[4],5))
Rice.Bu.mod.drop<-data.frame("Crop"="Rice", "Nutrient"="Bu",chisq = round(drop1(Rice.Bu.mod, test = "Chisq")[4],5))
Rice.Cau.mod.drop<-data.frame("Crop"="Rice", "Nutrient"="Cau",chisq = round(drop1(Rice.Cau.mod, test = "Chisq")[4],5))
Rice.Mgu.mod.drop<-data.frame("Crop"="Rice", "Nutrient"="Mgu",chisq = round(drop1(Rice.Mgu.mod, test = "Chisq")[4],5))
Rice.Mnu.mod.drop<-data.frame("Crop"="Rice", "Nutrient"="Mnu",chisq = round(drop1(Rice.Mnu.mod, test = "Chisq")[4],5))
Rice.Cuu.mod.drop<-data.frame("Crop"="Rice", "Nutrient"="Cuu",chisq = round(drop1(Rice.Cuu.mod, test = "Chisq")[4],5))

Sorghum.Znu.mod.drop<-data.frame("Crop"="Sorghum", "Nutrient"="Znu",chisq = round(drop1(Sorghum.Znu.mod, test = "Chisq")[4],5))
Sorghum.Feu.mod.drop<-data.frame("Crop"="Sorghum", "Nutrient"="Feu",chisq = round(drop1(Sorghum.Feu.mod, test = "Chisq")[4],5))
Sorghum.Nu.mod.drop<-data.frame("Crop"="Sorghum", "Nutrient"="Nu",chisq = round(drop1(Sorghum.Nu.mod, test = "Chisq")[4],5))
Sorghum.Pu.mod.drop<-data.frame("Crop"="Sorghum", "Nutrient"="Pu",chisq = round(drop1(Sorghum.Pu.mod, test = "Chisq")[4],5))
Sorghum.Ku.mod.drop<-data.frame("Crop"="Sorghum", "Nutrient"="Ku",chisq = round(drop1(Sorghum.Ku.mod, test = "Chisq")[4],5))
Sorghum.Su.mod.drop<-data.frame("Crop"="Sorghum", "Nutrient"="Su",chisq = round(drop1(Sorghum.Su.mod, test = "Chisq")[4],5))
Sorghum.Bu.mod.drop<-data.frame("Crop"="Sorghum", "Nutrient"="Bu",chisq = round(drop1(Sorghum.Bu.mod, test = "Chisq")[4],5))
Sorghum.Cau.mod.drop<-data.frame("Crop"="Sorghum", "Nutrient"="Cau",chisq = round(drop1(Sorghum.Cau.mod, test = "Chisq")[4],5))
Sorghum.Mgu.mod.drop<-data.frame("Crop"="Sorghum", "Nutrient"="Mgu",chisq = round(drop1(Sorghum.Mgu.mod, test = "Chisq")[4],5))
Sorghum.Mnu.mod.drop<-data.frame("Crop"="Sorghum", "Nutrient"="Mnu",chisq = round(drop1(Sorghum.Mnu.mod, test = "Chisq")[4],5))
Sorghum.Cuu.mod.drop<-data.frame("Crop"="Sorghum", "Nutrient"="Cuu",chisq = round(drop1(Sorghum.Cuu.mod, test = "Chisq")[4],5))


output.table.REMLsU.drop<-rbind(  
  Wheat.Feu.mod.drop	,
  Wheat.Yield.mod.drop	,
  Wheat.Znu.mod.drop	,
  Wheat.Feu.mod.drop	,
  Wheat.Phu.mod.drop	,
  Wheat.Nu.mod.drop	,
  
  
  Fieldpeas.Yield.mod.drop	,
  Fieldpeas.Znu.mod.drop	,
  Fieldpeas.Feu.mod.drop	,
  Fieldpeas.Nu.mod.drop	,
  Fieldpeas.Pu.mod.drop	,
  Fieldpeas.Ku.mod.drop	,
  Fieldpeas.Su.mod.drop	,
  Fieldpeas.Bu.mod.drop	,
  Fieldpeas.Cau.mod.drop	,
  Fieldpeas.Mgu.mod.drop	,
  Fieldpeas.Mnu.mod.drop	,
  Fieldpeas.Cuu.mod.drop	,
  
  
  Rice.Znu.mod.drop	,
  Rice.Feu.mod.drop	,
  Rice.Nu.mod.drop	,
  Rice.Pu.mod.drop	,
  Rice.Ku.mod.drop	,
  Rice.Su.mod.drop	,
  Rice.Bu.mod.drop	,
  Rice.Cau.mod.drop	,
  Rice.Mgu.mod.drop	,
  Rice.Mnu.mod.drop	,
  Rice.Cuu.mod.drop	,
  
  
  Sorghum.Znu.mod.drop	,
  Sorghum.Feu.mod.drop	,
  Sorghum.Nu.mod.drop	,
  Sorghum.Pu.mod.drop	,
  Sorghum.Ku.mod.drop	,
  Sorghum.Su.mod.drop	,
  Sorghum.Bu.mod.drop	,
  Sorghum.Cau.mod.drop	,
  Sorghum.Mgu.mod.drop	,
  Sorghum.Mnu.mod.drop	,
  Sorghum.Cuu.mod.drop	
)

write.csv(output.table.REMLsU.drop,"~/Rdata/nutrients/uptake_drop_pvals.csv")

#try interactions with nitrogen and water separately for wheat
Wheat.YieldxW.mod<-lmer(log(Yield)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="wheat")
Wheat.ZnuxW.mod<-lmer(log(Znu)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="wheat")
Wheat.FeuxW.mod<-lmer(log(Feu)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="wheat")
Wheat.PhuxW.mod<-lmer(log(Phu)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="wheat")
Wheat.NuxW.mod<-lmer(log(Nu)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="wheat")
Wheat.YieldxN.mod<-lmer(log(Yield)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="wheat")
Wheat.ZnuxN.mod<-lmer(log(Znu)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="wheat")
Wheat.FeuxN.mod<-lmer(log(Feu)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="wheat")
Wheat.PhuxN.mod<-lmer(log(Phu)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="wheat")
Wheat.NuxN.mod<-lmer(log(Nu)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="wheat")

#try interactions with water for sorghum and peas
Fieldpeas.Yieldx.mod<-lmer(log(Yield)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")
Fieldpeas.Znux.mod<-lmer(log(Znu)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")
Fieldpeas.Feux.mod<-lmer(log(Feu)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")
Fieldpeas.Phux.mod<-lmer(log(Phu)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")
Fieldpeas.Nux.mod<-lmer(log(Nu)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")
Fieldpeas.Pux.mod<-lmer(log(Pu)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")
Fieldpeas.Kux.mod<-lmer(log(Ku)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")
Fieldpeas.Sux.mod<-lmer(log(Su)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")
Fieldpeas.Bux.mod<-lmer(log(Bu)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")
Fieldpeas.Caux.mod<-lmer(log(Cau)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")
Fieldpeas.Mgux.mod<-lmer(log(Mgu)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")
Fieldpeas.Mnux.mod<-lmer(log(Mnu)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")
Fieldpeas.Cuux.mod<-lmer(log(Cuu)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Fieldpeas")

Sorghum.Yieldx.mod<-lmer(log(Yield)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Sorghum")
Sorghum.Znux.mod<-lmer(log(Znu)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Sorghum")
Sorghum.Feux.mod<-lmer(log(Feu)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Sorghum")
Sorghum.Nux.mod<-lmer(log(Nu)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Sorghum")
Sorghum.Pux.mod<-lmer(log(Pu)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Sorghum")
Sorghum.Kux.mod<-lmer(log(Ku)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Sorghum")
Sorghum.Sux.mod<-lmer(log(Su)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Sorghum")
Sorghum.Bux.mod<-lmer(log(Bu)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Sorghum")
Sorghum.Caux.mod<-lmer(log(Cau)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Sorghum")
Sorghum.Mgux.mod<-lmer(log(Mgu)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Sorghum")
Sorghum.Mnux.mod<-lmer(log(Mnu)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Sorghum")
Sorghum.Cuux.mod<-lmer(log(Cuu)~CO2treatment*as.factor(WateringRegime)+(1|pair),data=FACEmp,subset=Crop=="Sorghum")

#drop factors
Wheat.YieldxW.mod.output<-data.frame(species = "Wheat", nutrient = "Yield", av = fixef(Wheat.YieldxW.mod), SE =sqrt(diag(vcov(Wheat.YieldxW.mod))))
Wheat.ZnuxW.mod.output<-data.frame(species = "Wheat", nutrient = "Znu", av = fixef(Wheat.ZnuxW.mod), SE =sqrt(diag(vcov(Wheat.ZnuxW.mod))))
Wheat.FeuxW.mod.output<-data.frame(species = "Wheat", nutrient = "Feu", av = fixef(Wheat.FeuxW.mod), SE =sqrt(diag(vcov(Wheat.FeuxW.mod))))
Wheat.PhuxW.mod.output<-data.frame(species = "Wheat", nutrient = "Phu", av = fixef(Wheat.PhuxW.mod), SE =sqrt(diag(vcov(Wheat.PhuxW.mod))))
Wheat.NuxW.mod.output<-data.frame(species = "Wheat", nutrient = "Nu", av = fixef(Wheat.NuxW.mod), SE =sqrt(diag(vcov(Wheat.NuxW.mod))))
Wheat.YieldxN.mod.output<-data.frame(species = "Wheat", nutrient = "Yield", av = fixef(Wheat.YieldxN.mod), SE =sqrt(diag(vcov(Wheat.YieldxN.mod))))
Wheat.ZnuxN.mod.output<-data.frame(species = "Wheat", nutrient = "Znu", av = fixef(Wheat.ZnuxN.mod), SE =sqrt(diag(vcov(Wheat.ZnuxN.mod))))
Wheat.FeuxN.mod.output<-data.frame(species = "Wheat", nutrient = "Feu", av = fixef(Wheat.FeuxN.mod), SE =sqrt(diag(vcov(Wheat.FeuxN.mod))))
Wheat.PhuxN.mod.output<-data.frame(species = "Wheat", nutrient = "Phu", av = fixef(Wheat.PhuxN.mod), SE =sqrt(diag(vcov(Wheat.PhuxN.mod))))
Wheat.NuxN.mod.output<-data.frame(species = "Wheat", nutrient = "Nu", av = fixef(Wheat.NuxN.mod), SE =sqrt(diag(vcov(Wheat.NuxN.mod))))

Fieldpeas.Yieldx.mod.output<-data.frame(species = "Fieldpeas", nutrient = "Yield", av = fixef(Fieldpeas.Yieldx.mod), SE =sqrt(diag(vcov(Fieldpeas.Yieldx.mod))))
Fieldpeas.Znux.mod.output<-data.frame(species = "Fieldpeas", nutrient = "Znu", av = fixef(Fieldpeas.Znux.mod), SE =sqrt(diag(vcov(Fieldpeas.Znux.mod))))
Fieldpeas.Feux.mod.output<-data.frame(species = "Fieldpeas", nutrient = "Feu", av = fixef(Fieldpeas.Feux.mod), SE =sqrt(diag(vcov(Fieldpeas.Feux.mod))))

Fieldpeas.Nux.mod.output<-data.frame(species = "Fieldpeas", nutrient = "Nu", av = fixef(Fieldpeas.Nux.mod), SE =sqrt(diag(vcov(Fieldpeas.Nux.mod))))
Fieldpeas.Pux.mod.output<-data.frame(species = "Fieldpeas", nutrient = "Pu", av = fixef(Fieldpeas.Pux.mod), SE =sqrt(diag(vcov(Fieldpeas.Pux.mod))))
Fieldpeas.Kux.mod.output<-data.frame(species = "Fieldpeas", nutrient = "Ku", av = fixef(Fieldpeas.Kux.mod), SE =sqrt(diag(vcov(Fieldpeas.Kux.mod))))
Fieldpeas.Sux.mod.output<-data.frame(species = "Fieldpeas", nutrient = "Su", av = fixef(Fieldpeas.Sux.mod), SE =sqrt(diag(vcov(Fieldpeas.Sux.mod))))
Fieldpeas.Bux.mod.output<-data.frame(species = "Fieldpeas", nutrient = "Bu", av = fixef(Fieldpeas.Bux.mod), SE =sqrt(diag(vcov(Fieldpeas.Bux.mod))))
Fieldpeas.Caux.mod.output<-data.frame(species = "Fieldpeas", nutrient = "Cau", av = fixef(Fieldpeas.Caux.mod), SE =sqrt(diag(vcov(Fieldpeas.Caux.mod))))
Fieldpeas.Mgux.mod.output<-data.frame(species = "Fieldpeas", nutrient = "Mgu", av = fixef(Fieldpeas.Mgux.mod), SE =sqrt(diag(vcov(Fieldpeas.Mgux.mod))))
Fieldpeas.Mnux.mod.output<-data.frame(species = "Fieldpeas", nutrient = "Mnu", av = fixef(Fieldpeas.Mnux.mod), SE =sqrt(diag(vcov(Fieldpeas.Mnux.mod))))
Fieldpeas.Cuux.mod.output<-data.frame(species = "Fieldpeas", nutrient = "Cuu", av = fixef(Fieldpeas.Cuux.mod), SE =sqrt(diag(vcov(Fieldpeas.Cuux.mod))))


Sorghum.Yieldx.mod.output<-data.frame(species = "Sorghum", nutrient = "Yield", av = fixef(Sorghum.Yieldx.mod), SE =sqrt(diag(vcov(Sorghum.Yieldx.mod))))
Sorghum.Znux.mod.output<-data.frame(species = "Sorghum", nutrient = "Znu", av = fixef(Sorghum.Znux.mod), SE =sqrt(diag(vcov(Sorghum.Znux.mod))))
Sorghum.Feux.mod.output<-data.frame(species = "Sorghum", nutrient = "Feu", av = fixef(Sorghum.Feux.mod), SE =sqrt(diag(vcov(Sorghum.Feux.mod))))

Sorghum.Nux.mod.output<-data.frame(species = "Sorghum", nutrient = "Nu", av = fixef(Sorghum.Nux.mod), SE =sqrt(diag(vcov(Sorghum.Nux.mod))))
Sorghum.Pux.mod.output<-data.frame(species = "Sorghum", nutrient = "Pu", av = fixef(Sorghum.Pux.mod), SE =sqrt(diag(vcov(Sorghum.Pux.mod))))
Sorghum.Kux.mod.output<-data.frame(species = "Sorghum", nutrient = "Ku", av = fixef(Sorghum.Kux.mod), SE =sqrt(diag(vcov(Sorghum.Kux.mod))))
Sorghum.Sux.mod.output<-data.frame(species = "Sorghum", nutrient = "Su", av = fixef(Sorghum.Sux.mod), SE =sqrt(diag(vcov(Sorghum.Sux.mod))))
Sorghum.Bux.mod.output<-data.frame(species = "Sorghum", nutrient = "Bu", av = fixef(Sorghum.Bux.mod), SE =sqrt(diag(vcov(Sorghum.Bux.mod))))
Sorghum.Caux.mod.output<-data.frame(species = "Sorghum", nutrient = "Cau", av = fixef(Sorghum.Caux.mod), SE =sqrt(diag(vcov(Sorghum.Caux.mod))))
Sorghum.Mgux.mod.output<-data.frame(species = "Sorghum", nutrient = "Mgu", av = fixef(Sorghum.Mgux.mod), SE =sqrt(diag(vcov(Sorghum.Mgux.mod))))
Sorghum.Mnux.mod.output<-data.frame(species = "Sorghum", nutrient = "Mnu", av = fixef(Sorghum.Mnux.mod), SE =sqrt(diag(vcov(Sorghum.Mnux.mod))))
Sorghum.Cuux.mod.output<-data.frame(species = "Sorghum", nutrient = "Cuu", av = fixef(Sorghum.Cuux.mod), SE =sqrt(diag(vcov(Sorghum.Cuux.mod))))

output.table.REMLsUx<-rbind(  
  Wheat.YieldxW.mod.output	,
  Wheat.ZnuxW.mod.output	,
  Wheat.FeuxW.mod.output	,
  Wheat.PhuxW.mod.output	,
  Wheat.NuxW.mod.output	,

  Wheat.YieldxN.mod.output	,
  Wheat.ZnuxN.mod.output	,
  Wheat.FeuxN.mod.output	,
  Wheat.PhuxN.mod.output	,
  Wheat.NuxN.mod.output	,  
  
  Fieldpeas.Yieldx.mod.output	,
  Fieldpeas.Znux.mod.output	,
  Fieldpeas.Feux.mod.output	,
  
  Fieldpeas.Nux.mod.output	,
  Fieldpeas.Pux.mod.output	,
  Fieldpeas.Kux.mod.output	,
  Fieldpeas.Sux.mod.output	,
  Fieldpeas.Bux.mod.output	,
  Fieldpeas.Caux.mod.output	,
  Fieldpeas.Mgux.mod.output	,
  Fieldpeas.Mnux.mod.output	,
  Fieldpeas.Cuux.mod.output	,
  
#  SorghWateruptake<-cbind(  
  Sorghum.Znux.mod.output	,
  Sorghum.Feux.mod.output	,
  
  Sorghum.Nux.mod.output	,
  Sorghum.Pux.mod.output	,
  Sorghum.Kux.mod.output	,
  Sorghum.Sux.mod.output	,
  Sorghum.Bux.mod.output	,
  Sorghum.Caux.mod.output	,
  Sorghum.Mgux.mod.output	,
  Sorghum.Mnux.mod.output	,
  Sorghum.Cuux.mod.output	)

#drop
Wheat.YieldxW.mod.drop<-data.frame("Crop"="Wheat", "Nutrient"="Yield",chisq = round(drop1(Wheat.YieldxW.mod, test = "Chisq")[4],5))
Wheat.ZnuxW.mod.drop<-data.frame("Crop"="Wheat", "Nutrient"="Znu",chisq = round(drop1(Wheat.ZnuxW.mod, test = "Chisq")[4],5))
Wheat.FeuxW.mod.drop<-data.frame("Crop"="Wheat", "Nutrient"="Feu",chisq = round(drop1(Wheat.FeuxW.mod, test = "Chisq")[4],5))
Wheat.PhuxW.mod.drop<-data.frame("Crop"="Wheat", "Nutrient"="Phu",chisq = round(drop1(Wheat.PhuxW.mod, test = "Chisq")[4],5))
Wheat.NuxW.mod.drop<-data.frame("Crop"="Wheat", "Nutrient"="Nu",chisq = round(drop1(Wheat.NuxW.mod, test = "Chisq")[4],5))

Wheat.YieldxN.mod.drop<-data.frame("Crop"="Wheat", "Nutrient"="Yield",chisq = round(drop1(Wheat.YieldxN.mod, test = "Chisq")[4],5))
Wheat.ZnuxN.mod.drop<-data.frame("Crop"="Wheat", "Nutrient"="Znu",chisq = round(drop1(Wheat.ZnuxN.mod, test = "Chisq")[4],5))
Wheat.FeuxN.mod.drop<-data.frame("Crop"="Wheat", "Nutrient"="Feu",chisq = round(drop1(Wheat.FeuxN.mod, test = "Chisq")[4],5))
Wheat.PhuxN.mod.drop<-data.frame("Crop"="Wheat", "Nutrient"="Phu",chisq = round(drop1(Wheat.PhuxN.mod, test = "Chisq")[4],5))
Wheat.NuxN.mod.drop<-data.frame("Crop"="Wheat", "Nutrient"="Nu",chisq = round(drop1(Wheat.NuxN.mod, test = "Chisq")[4],5))


Fieldpeas.Yieldx.mod.drop<-data.frame("Crop"="Fieldpeas", "Nutrient"="Yield",chisq = round(drop1(Fieldpeas.Yieldx.mod, test = "Chisq")[4],5))
Fieldpeas.Znux.mod.drop<-data.frame("Crop"="Fieldpeas", "Nutrient"="Znu",chisq = round(drop1(Fieldpeas.Znux.mod, test = "Chisq")[4],5))
Fieldpeas.Feux.mod.drop<-data.frame("Crop"="Fieldpeas", "Nutrient"="Feu",chisq = round(drop1(Fieldpeas.Feux.mod, test = "Chisq")[4],5))

Fieldpeas.Nux.mod.drop<-data.frame("Crop"="Fieldpeas", "Nutrient"="Nu",chisq = round(drop1(Fieldpeas.Nux.mod, test = "Chisq")[4],5))
Fieldpeas.Pux.mod.drop<-data.frame("Crop"="Fieldpeas", "Nutrient"="Pu",chisq = round(drop1(Fieldpeas.Pux.mod, test = "Chisq")[4],5))
Fieldpeas.Kux.mod.drop<-data.frame("Crop"="Fieldpeas", "Nutrient"="Ku",chisq = round(drop1(Fieldpeas.Kux.mod, test = "Chisq")[4],5))
Fieldpeas.Sux.mod.drop<-data.frame("Crop"="Fieldpeas", "Nutrient"="Su",chisq = round(drop1(Fieldpeas.Sux.mod, test = "Chisq")[4],5))
Fieldpeas.Bux.mod.drop<-data.frame("Crop"="Fieldpeas", "Nutrient"="Bu",chisq = round(drop1(Fieldpeas.Bux.mod, test = "Chisq")[4],5))
Fieldpeas.Caux.mod.drop<-data.frame("Crop"="Fieldpeas", "Nutrient"="Cau",chisq = round(drop1(Fieldpeas.Caux.mod, test = "Chisq")[4],5))
Fieldpeas.Mgux.mod.drop<-data.frame("Crop"="Fieldpeas", "Nutrient"="Mgu",chisq = round(drop1(Fieldpeas.Mgux.mod, test = "Chisq")[4],5))
Fieldpeas.Mnux.mod.drop<-data.frame("Crop"="Fieldpeas", "Nutrient"="Mnu",chisq = round(drop1(Fieldpeas.Mnux.mod, test = "Chisq")[4],5))
Fieldpeas.Cuux.mod.drop<-data.frame("Crop"="Fieldpeas", "Nutrient"="Cuu",chisq = round(drop1(Fieldpeas.Cuux.mod, test = "Chisq")[4],5))

Sorghum.Znux.mod.drop<-data.frame("Crop"="Sorghum", "Nutrient"="Znu",chisq = round(drop1(Sorghum.Znux.mod, test = "Chisq")[4],5))
Sorghum.Feux.mod.drop<-data.frame("Crop"="Sorghum", "Nutrient"="Feu",chisq = round(drop1(Sorghum.Feux.mod, test = "Chisq")[4],5))
Sorghum.Nux.mod.drop<-data.frame("Crop"="Sorghum", "Nutrient"="Nu",chisq = round(drop1(Sorghum.Nux.mod, test = "Chisq")[4],5))
Sorghum.Pux.mod.drop<-data.frame("Crop"="Sorghum", "Nutrient"="Pu",chisq = round(drop1(Sorghum.Pux.mod, test = "Chisq")[4],5))
Sorghum.Kux.mod.drop<-data.frame("Crop"="Sorghum", "Nutrient"="Ku",chisq = round(drop1(Sorghum.Kux.mod, test = "Chisq")[4],5))
Sorghum.Sux.mod.drop<-data.frame("Crop"="Sorghum", "Nutrient"="Su",chisq = round(drop1(Sorghum.Sux.mod, test = "Chisq")[4],5))
Sorghum.Bux.mod.drop<-data.frame("Crop"="Sorghum", "Nutrient"="Bu",chisq = round(drop1(Sorghum.Bux.mod, test = "Chisq")[4],5))
Sorghum.Caux.mod.drop<-data.frame("Crop"="Sorghum", "Nutrient"="Cau",chisq = round(drop1(Sorghum.Caux.mod, test = "Chisq")[4],5))
Sorghum.Mgux.mod.drop<-data.frame("Crop"="Sorghum", "Nutrient"="Mgu",chisq = round(drop1(Sorghum.Mgux.mod, test = "Chisq")[4],5))
Sorghum.Mnux.mod.drop<-data.frame("Crop"="Sorghum", "Nutrient"="Mnu",chisq = round(drop1(Sorghum.Mnux.mod, test = "Chisq")[4],5))
Sorghum.Cuux.mod.drop<-data.frame("Crop"="Sorghum", "Nutrient"="Cuu",chisq = round(drop1(Sorghum.Cuux.mod, test = "Chisq")[4],5))

output.table.REMLsux.drop<-rbind(  
  
  Wheat.YieldxW.mod.drop	,
  Wheat.ZnuxW.mod.drop	,
  Wheat.FeuxW.mod.drop	,
  Wheat.PhuxW.mod.drop	,
  Wheat.NuxW.mod.drop	,
  Wheat.YieldxN.mod.drop	,
  Wheat.ZnuxN.mod.drop	,
  Wheat.FeuxN.mod.drop	,
  Wheat.PhuxN.mod.drop	,
  Wheat.NuxN.mod.drop	,
  
  
  
  
  Fieldpeas.Yieldx.mod.drop	,
  Fieldpeas.Znux.mod.drop	,
  Fieldpeas.Feux.mod.drop	,
  
  Fieldpeas.Nux.mod.drop	,
  Fieldpeas.Pux.mod.drop	,
  Fieldpeas.Kux.mod.drop	,
  Fieldpeas.Sux.mod.drop	,
  Fieldpeas.Bux.mod.drop	,
  Fieldpeas.Caux.mod.drop	,
  Fieldpeas.Mgux.mod.drop	,
  Fieldpeas.Mnux.mod.drop	,
  Fieldpeas.Cuux.mod.drop	,
  
  

  Sorghum.Znux.mod.drop	,
  Sorghum.Feux.mod.drop	,

  Sorghum.Nux.mod.drop	,
  Sorghum.Pux.mod.drop	,
  Sorghum.Kux.mod.drop	,
  Sorghum.Sux.mod.drop	,
  Sorghum.Bux.mod.drop	,
  Sorghum.Caux.mod.drop	,
  Sorghum.Mgux.mod.drop	,
  Sorghum.Mnux.mod.drop	,
  Sorghum.Cuux.mod.drop	
)
  
write.csv(output.table.REMLsux.drop,"~/Rdata/nutrients/pvals_uptakeX.csv")

##redoing rice without low N####
Rice.Zn.mod<-lmer(log(Zn)~CO2treatment+as.factor(Temperature_treatment)+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&NapplicnQ !="Low")
Rice.Fe.mod<-lmer(log(Fe)~CO2treatment+as.factor(Temperature_treatment)+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&NapplicnQ !="Low")
Rice.N.mod<-lmer(log(N)~CO2treatment+as.factor(Temperature_treatment)+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&NapplicnQ !="Low")
Rice.P.mod<-lmer(log(P)~CO2treatment+as.factor(Temperature_treatment)+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&NapplicnQ !="Low")
Rice.K.mod<-lmer(log(K)~CO2treatment+as.factor(Temperature_treatment)+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&NapplicnQ !="Low")
Rice.S.mod<-lmer(log(S)~CO2treatment+as.factor(Temperature_treatment)+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&NapplicnQ !="Low")
Rice.B.mod<-lmer(log(B)~CO2treatment+as.factor(Temperature_treatment)+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&NapplicnQ !="Low")
Rice.Ca.mod<-lmer(log(Ca)~CO2treatment+as.factor(Temperature_treatment)+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&NapplicnQ !="Low")
Rice.Mg.mod<-lmer(log(Mg)~CO2treatment+as.factor(Temperature_treatment)+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&NapplicnQ !="Low")
Rice.Mn.mod<-lmer(log(Mn)~CO2treatment+as.factor(Temperature_treatment)+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&NapplicnQ !="Low")
Rice.Cu.mod<-lmer(log(Cu)~CO2treatment+as.factor(Temperature_treatment)+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&NapplicnQ !="Low")

Rice.Zn.modN<-lmer(log(Zn)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&NapplicnQ !="Low")
Rice.Fe.modN<-lmer(log(Fe)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&NapplicnQ !="Low")
Rice.N.modN<-lmer(log(N)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&NapplicnQ !="Low")
Rice.P.modN<-lmer(log(P)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&NapplicnQ !="Low")
Rice.K.modN<-lmer(log(K)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&NapplicnQ !="Low")
Rice.S.modN<-lmer(log(S)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&NapplicnQ !="Low")
Rice.B.modN<-lmer(log(B)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&NapplicnQ !="Low")
Rice.Ca.modN<-lmer(log(Ca)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&NapplicnQ !="Low")
Rice.Mg.modN<-lmer(log(Mg)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&NapplicnQ !="Low")
Rice.Mn.modN<-lmer(log(Mn)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&NapplicnQ !="Low")
Rice.Cu.modN<-lmer(log(Cu)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&NapplicnQ !="Low")


Rice.Zn.modN.output<-data.frame(species = "Rice", nutrient = "Zn", av = fixef(Rice.Zn.modN)[2], SE =sqrt(vcov(Rice.Zn.modN)[2,2]))
Rice.Fe.modN.output<-data.frame(species = "Rice", nutrient = "Fe", av = fixef(Rice.Fe.modN)[2], SE =sqrt(vcov(Rice.Fe.modN)[2,2]))
Rice.N.modN.output<-data.frame(species = "Rice", nutrient = "N", av = fixef(Rice.N.modN)[2], SE =sqrt(vcov(Rice.N.modN)[2,2]))
Rice.P.modN.output<-data.frame(species = "Rice", nutrient = "P", av = fixef(Rice.P.modN)[2], SE =sqrt(vcov(Rice.P.modN)[2,2]))
Rice.K.modN.output<-data.frame(species = "Rice", nutrient = "K", av = fixef(Rice.K.modN)[2], SE =sqrt(vcov(Rice.K.modN)[2,2]))
Rice.S.modN.output<-data.frame(species = "Rice", nutrient = "S", av = fixef(Rice.S.modN)[2], SE =sqrt(vcov(Rice.S.modN)[2,2]))
Rice.B.modN.output<-data.frame(species = "Rice", nutrient = "B", av = fixef(Rice.B.modN)[2], SE =sqrt(vcov(Rice.B.modN)[2,2]))
Rice.Ca.modN.output<-data.frame(species = "Rice", nutrient = "Ca", av = fixef(Rice.Ca.modN)[2], SE =sqrt(vcov(Rice.Ca.modN)[2,2]))
Rice.Mg.modN.output<-data.frame(species = "Rice", nutrient = "Mg", av = fixef(Rice.Mg.modN)[2], SE =sqrt(vcov(Rice.Mg.modN)[2,2]))
Rice.Mn.modN.output<-data.frame(species = "Rice", nutrient = "Mn", av = fixef(Rice.Mn.modN)[2], SE =sqrt(vcov(Rice.Mn.modN)[2,2]))
Rice.Cu.modN.output<-data.frame(species = "Rice", nutrient = "Cu", av = fixef(Rice.Cu.modN)[2], SE =sqrt(vcov(Rice.Cu.modN)[2,2]))



Rice.Zn.modN.drop<-data.frame("Crop"="Rice", "Nutrient"="Zn",chisq = round(drop1(Rice.Zn.modN, test = "Chisq")[4],5))
Rice.Fe.modN.drop<-data.frame("Crop"="Rice", "Nutrient"="Fe",chisq = round(drop1(Rice.Fe.modN, test = "Chisq")[4],5))
NA
Rice.N.modN.drop<-data.frame("Crop"="Rice", "Nutrient"="N",chisq = round(drop1(Rice.N.modN, test = "Chisq")[4],5))
Rice.P.modN.drop<-data.frame("Crop"="Rice", "Nutrient"="P",chisq = round(drop1(Rice.P.modN, test = "Chisq")[4],5))
Rice.K.modN.drop<-data.frame("Crop"="Rice", "Nutrient"="K",chisq = round(drop1(Rice.K.modN, test = "Chisq")[4],5))
Rice.S.modN.drop<-data.frame("Crop"="Rice", "Nutrient"="S",chisq = round(drop1(Rice.S.modN, test = "Chisq")[4],5))
Rice.B.modN.drop<-data.frame("Crop"="Rice", "Nutrient"="B",chisq = round(drop1(Rice.B.modN, test = "Chisq")[4],5))
Rice.Ca.modN.drop<-data.frame("Crop"="Rice", "Nutrient"="Ca",chisq = round(drop1(Rice.Ca.modN, test = "Chisq")[4],5))
Rice.Mg.modN.drop<-data.frame("Crop"="Rice", "Nutrient"="Mg",chisq = round(drop1(Rice.Mg.modN, test = "Chisq")[4],5))
Rice.Mn.modN.drop<-data.frame("Crop"="Rice", "Nutrient"="Mn",chisq = round(drop1(Rice.Mn.modN, test = "Chisq")[4],5))
Rice.Cu.modN.drop<-data.frame("Crop"="Rice", "Nutrient"="Cu",chisq = round(drop1(Rice.Cu.modN, test = "Chisq")[4],5))


Rice.Zn.mod.output<-data.frame(species = "Rice", nutrient = "Zn", av = fixef(Rice.Zn.mod)[2], SE =sqrt(vcov(Rice.Zn.mod)[2,2]))
Rice.Fe.mod.output<-data.frame(species = "Rice", nutrient = "Fe", av = fixef(Rice.Fe.mod)[2], SE =sqrt(vcov(Rice.Fe.mod)[2,2]))
NA
Rice.N.mod.output<-data.frame(species = "Rice", nutrient = "N", av = fixef(Rice.N.mod)[2], SE =sqrt(vcov(Rice.N.mod)[2,2]))
Rice.P.mod.output<-data.frame(species = "Rice", nutrient = "P", av = fixef(Rice.P.mod)[2], SE =sqrt(vcov(Rice.P.mod)[2,2]))
Rice.K.mod.output<-data.frame(species = "Rice", nutrient = "K", av = fixef(Rice.K.mod)[2], SE =sqrt(vcov(Rice.K.mod)[2,2]))
Rice.S.mod.output<-data.frame(species = "Rice", nutrient = "S", av = fixef(Rice.S.mod)[2], SE =sqrt(vcov(Rice.S.mod)[2,2]))
Rice.B.mod.output<-data.frame(species = "Rice", nutrient = "B", av = fixef(Rice.B.mod)[2], SE =sqrt(vcov(Rice.B.mod)[2,2]))
Rice.Ca.mod.output<-data.frame(species = "Rice", nutrient = "Ca", av = fixef(Rice.Ca.mod)[2], SE =sqrt(vcov(Rice.Ca.mod)[2,2]))
Rice.Mg.mod.output<-data.frame(species = "Rice", nutrient = "Mg", av = fixef(Rice.Mg.mod)[2], SE =sqrt(vcov(Rice.Mg.mod)[2,2]))
Rice.Mn.mod.output<-data.frame(species = "Rice", nutrient = "Mn", av = fixef(Rice.Mn.mod)[2], SE =sqrt(vcov(Rice.Mn.mod)[2,2]))
Rice.Cu.mod.output<-data.frame(species = "Rice", nutrient = "Cu", av = fixef(Rice.Cu.mod)[2], SE =sqrt(vcov(Rice.Cu.mod)[2,2]))



Rice.Zn.mod.drop<-data.frame("Crop"="Rice", "Nutrient"="Zn",chisq = round(drop1(Rice.Zn.mod, test = "Chisq")[4],5))
Rice.Fe.mod.drop<-data.frame("Crop"="Rice", "Nutrient"="Fe",chisq = round(drop1(Rice.Fe.mod, test = "Chisq")[4],5))
NA
Rice.N.mod.drop<-data.frame("Crop"="Rice", "Nutrient"="N",chisq = round(drop1(Rice.N.mod, test = "Chisq")[4],5))
Rice.P.mod.drop<-data.frame("Crop"="Rice", "Nutrient"="P",chisq = round(drop1(Rice.P.mod, test = "Chisq")[4],5))
Rice.K.mod.drop<-data.frame("Crop"="Rice", "Nutrient"="K",chisq = round(drop1(Rice.K.mod, test = "Chisq")[4],5))
Rice.S.mod.drop<-data.frame("Crop"="Rice", "Nutrient"="S",chisq = round(drop1(Rice.S.mod, test = "Chisq")[4],5))
Rice.B.mod.drop<-data.frame("Crop"="Rice", "Nutrient"="B",chisq = round(drop1(Rice.B.mod, test = "Chisq")[4],5))
Rice.Ca.mod.drop<-data.frame("Crop"="Rice", "Nutrient"="Ca",chisq = round(drop1(Rice.Ca.mod, test = "Chisq")[4],5))
Rice.Mg.mod.drop<-data.frame("Crop"="Rice", "Nutrient"="Mg",chisq = round(drop1(Rice.Mg.mod, test = "Chisq")[4],5))
Rice.Mn.mod.drop<-data.frame("Crop"="Rice", "Nutrient"="Mn",chisq = round(drop1(Rice.Mn.mod, test = "Chisq")[4],5))
Rice.Cu.mod.drop<-data.frame("Crop"="Rice", "Nutrient"="Cu",chisq = round(drop1(Rice.Cu.mod, test = "Chisq")[4],5))



output.table.REMLs.rice<-rbind(  
  Rice.Zn.mod.drop  ,
  Rice.Fe.mod.drop	,
  Rice.N.mod.drop	,
  Rice.P.mod.drop	,
  Rice.K.mod.drop	,
  Rice.S.mod.drop	,
  Rice.B.mod.drop	,
  Rice.Ca.mod.drop	,
  Rice.Mg.mod.drop	,
  Rice.Mn.mod.drop	,
  Rice.Cu.mod.drop	,
  NA,
  Rice.Zn.modN.drop	,
  Rice.Fe.modN.drop	,
  Rice.N.modN.drop	,
  Rice.P.modN.drop	,
  Rice.K.modN.drop	,
  Rice.S.modN.drop	,
  Rice.B.modN.drop	,
  Rice.Ca.modN.drop	,
  Rice.Mg.modN.drop	,
  Rice.Mn.modN.drop	,
  Rice.Cu.modN.drop	)

##Rice uptake####
Rice.Yield.modNu<-lmer(log(Znu)~CO2treatment+(1|pair),data=FACEmp,subset=NapplicnQ !="Low")
Rice.Zn.modNu<-lmer(log(Znu)~CO2treatment+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&Cultivar =="Koshihikari")
Rice.Fe.modNu<-lmer(log(Feu)~CO2treatment+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&Cultivar =="Koshihikari")
Rice.N.modNu<-lmer(log(Nu)~CO2treatment+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&Cultivar =="Koshihikari")
Rice.P.modNu<-lmer(log(Pu)~CO2treatment+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&Cultivar =="Koshihikari")
Rice.K.modNu<-lmer(log(Ku)~CO2treatment+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&Cultivar =="Koshihikari")
Rice.S.modNu<-lmer(log(Su)~CO2treatment+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&Cultivar =="Koshihikari")
Rice.B.modNu<-lmer(log(Bu)~CO2treatment+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&Cultivar =="Koshihikari")
Rice.Ca.modNu<-lmer(log(Cau)~CO2treatment+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&Cultivar =="Koshihikari")
Rice.Mg.modNu<-lmer(log(Mgu)~CO2treatment+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&Cultivar =="Koshihikari")
Rice.Mn.modNu<-lmer(log(Mnu)~CO2treatment+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&Cultivar =="Koshihikari")
Rice.Cu.modNu<-lmer(log(Cuu)~CO2treatment+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&Cultivar =="Koshihikari")


Rice.Zn.modNux<-lmer(log(Znu)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&Cultivar =="Koshihikari")
Rice.Fe.modNux<-lmer(log(Feu)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&Cultivar =="Koshihikari")
Rice.N.modNux<-lmer(log(Nu)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&Cultivar =="Koshihikari")
Rice.P.modNux<-lmer(log(Pu)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&Cultivar =="Koshihikari")
Rice.K.modNux<-lmer(log(Ku)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&Cultivar =="Koshihikari")
Rice.S.modNux<-lmer(log(Su)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&Cultivar =="Koshihikari")
Rice.B.modNux<-lmer(log(Bu)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&Cultivar =="Koshihikari")
Rice.Ca.modNux<-lmer(log(Cau)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&Cultivar =="Koshihikari")
Rice.Mg.modNux<-lmer(log(Mgu)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&Cultivar =="Koshihikari")
Rice.Mn.modNux<-lmer(log(Mnu)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&Cultivar =="Koshihikari")
Rice.Cu.modNux<-lmer(log(Cuu)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Rice"&Cultivar =="Koshihikari")

Rice.Yield.modNu.drop<-data.frame("Crop"="Rice", "Nutrient"="Znu",chisq = round(drop1(Rice.Yield.modNu, test = "Chisq")[4],5))
Rice.Zn.modNu.drop<-data.frame("Crop"="Rice", "Nutrient"="Znu",chisq = round(drop1(Rice.Zn.modNu, test = "Chisq")[4],5))
Rice.Fe.modNu.drop<-data.frame("Crop"="Rice", "Nutrient"="Feu",chisq = round(drop1(Rice.Fe.modNu, test = "Chisq")[4],5))
Rice.N.modNu.drop<-data.frame("Crop"="Rice", "Nutrient"="Nu",chisq = round(drop1(Rice.N.modNu, test = "Chisq")[4],5))
Rice.P.modNu.drop<-data.frame("Crop"="Rice", "Nutrient"="Pu",chisq = round(drop1(Rice.P.modNu, test = "Chisq")[4],5))
Rice.K.modNu.drop<-data.frame("Crop"="Rice", "Nutrient"="Ku",chisq = round(drop1(Rice.K.modNu, test = "Chisq")[4],5))
Rice.S.modNu.drop<-data.frame("Crop"="Rice", "Nutrient"="Su",chisq = round(drop1(Rice.S.modNu, test = "Chisq")[4],5))
Rice.B.modNu.drop<-data.frame("Crop"="Rice", "Nutrient"="Bu",chisq = round(drop1(Rice.B.modNu, test = "Chisq")[4],5))
Rice.Ca.modNu.drop<-data.frame("Crop"="Rice", "Nutrient"="Cau",chisq = round(drop1(Rice.Ca.modNu, test = "Chisq")[4],5))
Rice.Mg.modNu.drop<-data.frame("Crop"="Rice", "Nutrient"="Mgu",chisq = round(drop1(Rice.Mg.modNu, test = "Chisq")[4],5))
Rice.Mn.modNu.drop<-data.frame("Crop"="Rice", "Nutrient"="Mnu",chisq = round(drop1(Rice.Mn.modNu, test = "Chisq")[4],5))
Rice.Cu.modNu.drop<-data.frame("Crop"="Rice", "Nutrient"="Cuu",chisq = round(drop1(Rice.Cu.modNu, test = "Chisq")[4],5))

Rice.Zn.modNux.drop<-data.frame("Crop"="Rice", "Nutrient"="Znu",chisq = round(drop1(Rice.Zn.modNux, test = "Chisq")[4],5))
Rice.Fe.modNux.drop<-data.frame("Crop"="Rice", "Nutrient"="Feu",chisq = round(drop1(Rice.Fe.modNux, test = "Chisq")[4],5))
Rice.N.modNux.drop<-data.frame("Crop"="Rice", "Nutrient"="Nu",chisq = round(drop1(Rice.N.modNux, test = "Chisq")[4],5))
Rice.P.modNux.drop<-data.frame("Crop"="Rice", "Nutrient"="Pu",chisq = round(drop1(Rice.P.modNux, test = "Chisq")[4],5))
Rice.K.modNux.drop<-data.frame("Crop"="Rice", "Nutrient"="Ku",chisq = round(drop1(Rice.K.modNux, test = "Chisq")[4],5))
Rice.S.modNux.drop<-data.frame("Crop"="Rice", "Nutrient"="Su",chisq = round(drop1(Rice.S.modNux, test = "Chisq")[4],5))
Rice.B.modNux.drop<-data.frame("Crop"="Rice", "Nutrient"="Bu",chisq = round(drop1(Rice.B.modNux, test = "Chisq")[4],5))
Rice.Ca.modNux.drop<-data.frame("Crop"="Rice", "Nutrient"="Cau",chisq = round(drop1(Rice.Ca.modNux, test = "Chisq")[4],5))
Rice.Mg.modNux.drop<-data.frame("Crop"="Rice", "Nutrient"="Mgu",chisq = round(drop1(Rice.Mg.modNux, test = "Chisq")[4],5))
Rice.Mn.modNux.drop<-data.frame("Crop"="Rice", "Nutrient"="Mnu",chisq = round(drop1(Rice.Mn.modNux, test = "Chisq")[4],5))
Rice.Cu.modNux.drop<-data.frame("Crop"="Rice", "Nutrient"="Cuu",chisq = round(drop1(Rice.Cu.modNux, test = "Chisq")[4],5))

#get wheat pvals for nutrients x water (not uptake, just the basics)

Sorghum.Znu.modWet<-lmer(log(Znu)~CO2treatment+(1|pair),data=FACEmp,subset=(Crop=="Sorghum"&WateringRegime=="Wet"))
Sorghum.Feu.modWet<-lmer(log(Feu)~CO2treatment+(1|pair),data=FACEmp,subset=(Crop=="Sorghum"&WateringRegime=="Wet"))
Sorghum.Nu.modWet<-lmer(log(Nu)~CO2treatment+(1|pair),data=FACEmp,subset=(Crop=="Sorghum"&WateringRegime=="Wet"))
Sorghum.Pu.modWet<-lmer(log(Pu)~CO2treatment+(1|pair),data=FACEmp,subset=(Crop=="Sorghum"&WateringRegime=="Wet"))
Sorghum.Ku.modWet<-lmer(log(Ku)~CO2treatment+(1|pair),data=FACEmp,subset=(Crop=="Sorghum"&WateringRegime=="Wet"))
Sorghum.Su.modWet<-lmer(log(Su)~CO2treatment+(1|pair),data=FACEmp,subset=(Crop=="Sorghum"&WateringRegime=="Wet"))
Sorghum.Bu.modWet<-lmer(log(Bu)~CO2treatment+(1|pair),data=FACEmp,subset=(Crop=="Sorghum"&WateringRegime=="Wet"))
Sorghum.Cau.modWet<-lmer(log(Cau)~CO2treatment+(1|pair),data=FACEmp,subset=(Crop=="Sorghum"&WateringRegime=="Wet"))
Sorghum.Mgu.modWet<-lmer(log(Mgu)~CO2treatment+(1|pair),data=FACEmp,subset=(Crop=="Sorghum"&WateringRegime=="Wet"))
Sorghum.Mnu.modWet<-lmer(log(Mnu)~CO2treatment+(1|pair),data=FACEmp,subset=(Crop=="Sorghum"&WateringRegime=="Wet"))
Sorghum.Cuu.modWet<-lmer(log(Cuu)~CO2treatment+(1|pair),data=FACEmp,subset=(Crop=="Sorghum"&WateringRegime=="Wet"))

Sorghum.Znu.modDry<-lmer(log(Znu)~CO2treatment+(1|pair),data=FACEmp,subset=(Crop=="Sorghum"&WateringRegime=="Dry"))
Sorghum.Feu.modDry<-lmer(log(Feu)~CO2treatment+(1|pair),data=FACEmp,subset=(Crop=="Sorghum"&WateringRegime=="Dry"))
Sorghum.Nu.modDry<-lmer(log(Nu)~CO2treatment+(1|pair),data=FACEmp,subset=(Crop=="Sorghum"&WateringRegime=="Dry"))
Sorghum.Pu.modDry<-lmer(log(Pu)~CO2treatment+(1|pair),data=FACEmp,subset=(Crop=="Sorghum"&WateringRegime=="Dry"))
Sorghum.Ku.modDry<-lmer(log(Ku)~CO2treatment+(1|pair),data=FACEmp,subset=(Crop=="Sorghum"&WateringRegime=="Dry"))
Sorghum.Su.modDry<-lmer(log(Su)~CO2treatment+(1|pair),data=FACEmp,subset=(Crop=="Sorghum"&WateringRegime=="Dry"))
Sorghum.Bu.modDry<-lmer(log(Bu)~CO2treatment+(1|pair),data=FACEmp,subset=(Crop=="Sorghum"&WateringRegime=="Dry"))
Sorghum.Cau.modDry<-lmer(log(Cau)~CO2treatment+(1|pair),data=FACEmp,subset=(Crop=="Sorghum"&WateringRegime=="Dry"))
Sorghum.Mgu.modDry<-lmer(log(Mgu)~CO2treatment+(1|pair),data=FACEmp,subset=(Crop=="Sorghum"&WateringRegime=="Dry"))
Sorghum.Mnu.modDry<-lmer(log(Mnu)~CO2treatment+(1|pair),data=FACEmp,subset=(Crop=="Sorghum"&WateringRegime=="Dry"))
Sorghum.Cuu.modDry<-lmer(log(Cuu)~CO2treatment+(1|pair),data=FACEmp,subset=(Crop=="Sorghum"&WateringRegime=="Dry"))



Sorghum.Znu.modWet.output<-data.frame(watering="Wet", species = "Sorghum", nutrient = "Znu", av = fixef(Sorghum.Znu.modWet)[2], SE =sqrt(vcov(Sorghum.Znu.modWet)[2,2]))
Sorghum.Feu.modWet.output<-data.frame(watering="Wet", species = "Sorghum", nutrient = "Feu", av = fixef(Sorghum.Feu.modWet)[2], SE =sqrt(vcov(Sorghum.Feu.modWet)[2,2]))
Sorghum.Nu.modWet.output<-data.frame(watering="Wet", species = "Sorghum", nutrient = "Nu", av = fixef(Sorghum.Nu.modWet)[2], SE =sqrt(vcov(Sorghum.Nu.modWet)[2,2]))
Sorghum.Pu.modWet.output<-data.frame(watering="Wet", species = "Sorghum", nutrient = "Pu", av = fixef(Sorghum.Pu.modWet)[2], SE =sqrt(vcov(Sorghum.Pu.modWet)[2,2]))
Sorghum.Ku.modWet.output<-data.frame(watering="Wet", species = "Sorghum", nutrient = "Ku", av = fixef(Sorghum.Ku.modWet)[2], SE =sqrt(vcov(Sorghum.Ku.modWet)[2,2]))
Sorghum.Su.modWet.output<-data.frame(watering="Wet", species = "Sorghum", nutrient = "Su", av = fixef(Sorghum.Su.modWet)[2], SE =sqrt(vcov(Sorghum.Su.modWet)[2,2]))
Sorghum.Bu.modWet.output<-data.frame(watering="Wet", species = "Sorghum", nutrient = "Bu", av = fixef(Sorghum.Bu.modWet)[2], SE =sqrt(vcov(Sorghum.Bu.modWet)[2,2]))
Sorghum.Cau.modWet.output<-data.frame(watering="Wet", species = "Sorghum", nutrient = "Cau", av = fixef(Sorghum.Cau.modWet)[2], SE =sqrt(vcov(Sorghum.Cau.modWet)[2,2]))
Sorghum.Mgu.modWet.output<-data.frame(watering="Wet", species = "Sorghum", nutrient = "Mgu", av = fixef(Sorghum.Mgu.modWet)[2], SE =sqrt(vcov(Sorghum.Mgu.modWet)[2,2]))
Sorghum.Mnu.modWet.output<-data.frame(watering="Wet", species = "Sorghum", nutrient = "Mnu", av = fixef(Sorghum.Mnu.modWet)[2], SE =sqrt(vcov(Sorghum.Mnu.modWet)[2,2]))
Sorghum.Cuu.modWet.output<-data.frame(watering="Wet", species = "Sorghum", nutrient = "Cuu", av = fixef(Sorghum.Cuu.modWet)[2], SE =sqrt(vcov(Sorghum.Cuu.modWet)[2,2]))

Sorghum.Znu.modDry.output<-data.frame(watering="Dry", species = "Sorghum", nutrient = "Znu", av = fixef(Sorghum.Znu.modDry)[2], SE =sqrt(vcov(Sorghum.Znu.modDry)[2,2]))
Sorghum.Feu.modDry.output<-data.frame(watering="Dry", species = "Sorghum", nutrient = "Feu", av = fixef(Sorghum.Feu.modDry)[2], SE =sqrt(vcov(Sorghum.Feu.modDry)[2,2]))
Sorghum.Nu.modDry.output<-data.frame(watering="Dry", species = "Sorghum", nutrient = "Nu", av = fixef(Sorghum.Nu.modDry)[2], SE =sqrt(vcov(Sorghum.Nu.modDry)[2,2]))
Sorghum.Pu.modDry.output<-data.frame(watering="Dry", species = "Sorghum", nutrient = "Pu", av = fixef(Sorghum.Pu.modDry)[2], SE =sqrt(vcov(Sorghum.Pu.modDry)[2,2]))
Sorghum.Ku.modDry.output<-data.frame(watering="Dry", species = "Sorghum", nutrient = "Ku", av = fixef(Sorghum.Ku.modDry)[2], SE =sqrt(vcov(Sorghum.Ku.modDry)[2,2]))
Sorghum.Su.modDry.output<-data.frame(watering="Dry", species = "Sorghum", nutrient = "Su", av = fixef(Sorghum.Su.modDry)[2], SE =sqrt(vcov(Sorghum.Su.modDry)[2,2]))
Sorghum.Bu.modDry.output<-data.frame(watering="Dry", species = "Sorghum", nutrient = "Bu", av = fixef(Sorghum.Bu.modDry)[2], SE =sqrt(vcov(Sorghum.Bu.modDry)[2,2]))
Sorghum.Cau.modDry.output<-data.frame(watering="Dry", species = "Sorghum", nutrient = "Cau", av = fixef(Sorghum.Cau.modDry)[2], SE =sqrt(vcov(Sorghum.Cau.modDry)[2,2]))
Sorghum.Mgu.modDry.output<-data.frame(watering="Dry", species = "Sorghum", nutrient = "Mgu", av = fixef(Sorghum.Mgu.modDry)[2], SE =sqrt(vcov(Sorghum.Mgu.modDry)[2,2]))
Sorghum.Mnu.modDry.output<-data.frame(watering="Dry", species = "Sorghum", nutrient = "Mnu", av = fixef(Sorghum.Mnu.modDry)[2], SE =sqrt(vcov(Sorghum.Mnu.modDry)[2,2]))
Sorghum.Cuu.modDry.output<-data.frame(watering="Dry", species = "Sorghum", nutrient = "Cuu", av = fixef(Sorghum.Cuu.modDry)[2], SE =sqrt(vcov(Sorghum.Cuu.modDry)[2,2]))

sorghum.uptake.watering <-rbind(
  Sorghum.Znu.modWet.output,
  Sorghum.Feu.modWet.output,
  Sorghum.Nu.modWet.output,
  Sorghum.Pu.modWet.output,
  Sorghum.Ku.modWet.output,
  Sorghum.Su.modWet.output,
  Sorghum.Bu.modWet.output,
  Sorghum.Cau.modWet.output,
  Sorghum.Mgu.modWet.output,
  Sorghum.Mnu.modWet.output,
  Sorghum.Cuu.modWet.output,
  
  Sorghum.Znu.modDry.output,
  Sorghum.Feu.modDry.output,
  Sorghum.Nu.modDry.output,
  Sorghum.Pu.modDry.output,
  Sorghum.Ku.modDry.output,
  Sorghum.Su.modDry.output,
  Sorghum.Bu.modDry.output,
  Sorghum.Cau.modDry.output,
  Sorghum.Mgu.modDry.output,
  Sorghum.Mnu.modDry.output,
  Sorghum.Cuu.modDry.output
  
  
  )

write.csv(sorghum.uptake.watering,"~/Rdata/nutrients/sorghumuptakewatering.csv")



###corn has yield too!####
Corn.Yield.mod<-lmer(log(Yield)~CO2treatment+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Corn")
Corn.Znu.mod<-lmer(log(Znu)~CO2treatment+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Corn")
Corn.Feu.mod<-lmer(log(Feu)~CO2treatment+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Corn")
Corn.Nu.mod<-lmer(log(Nu)~CO2treatment+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Corn")
Corn.Pu.mod<-lmer(log(Pu)~CO2treatment+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Corn")
Corn.Ku.mod<-lmer(log(Ku)~CO2treatment+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Corn")
Corn.Su.mod<-lmer(log(Su)~CO2treatment+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Corn")
Corn.Bu.mod<-lmer(log(Bu)~CO2treatment+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Corn")
Corn.Cau.mod<-lmer(log(Cau)~CO2treatment+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Corn")
Corn.Mgu.mod<-lmer(log(Mgu)~CO2treatment+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Corn")
Corn.Mnu.mod<-lmer(log(Mnu)~CO2treatment+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Corn")
Corn.Cuu.mod<-lmer(log(Cuu)~CO2treatment+as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Corn")

# Corn.Yieldx.mod<-lmer(log(Yield)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Corn")
# Corn.Znux.mod<-lmer(log(Znu)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Corn")
# Corn.Feux.mod<-lmer(log(Feu)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Corn")
# Corn.Nux.mod<-lmer(log(Nu)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Corn")
# Corn.Pux.mod<-lmer(log(Pu)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Corn")
# Corn.Kux.mod<-lmer(log(Ku)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Corn")
# Corn.Sux.mod<-lmer(log(Su)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Corn")
# Corn.Bux.mod<-lmer(log(Bu)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Corn")
# Corn.Caux.mod<-lmer(log(Cau)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Corn")
# Corn.Mgux.mod<-lmer(log(Mgu)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Corn")
# Corn.Mnux.mod<-lmer(log(Mnu)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Corn")
# Corn.Cuux.mod<-lmer(log(Cuu)~CO2treatment*as.factor(NapplicnQ)+(1|pair),data=FACEmp,subset=Crop=="Corn")
Corn.Yieldx.mod<-aov(log(Yield)~CO2treatment*as.factor(NapplicnQ),data=FACEmp,subset=Crop=="Corn")
Corn.Znux.mod<-aov(log(Znu)~CO2treatment*as.factor(NapplicnQ),data=FACEmp,subset=Crop=="Corn")
Corn.Feux.mod<-aov(log(Feu)~CO2treatment*as.factor(NapplicnQ),data=FACEmp,subset=Crop=="Corn")
Corn.Nux.mod<-aov(log(Nu)~CO2treatment*as.factor(NapplicnQ),data=FACEmp,subset=Crop=="Corn")
Corn.Pux.mod<-aov(log(Pu)~CO2treatment*as.factor(NapplicnQ),data=FACEmp,subset=Crop=="Corn")
Corn.Kux.mod<-aov(log(Ku)~CO2treatment*as.factor(NapplicnQ),data=FACEmp,subset=Crop=="Corn")
Corn.Sux.mod<-aov(log(Su)~CO2treatment*as.factor(NapplicnQ),data=FACEmp,subset=Crop=="Corn")
Corn.Bux.mod<-aov(log(Bu)~CO2treatment*as.factor(NapplicnQ),data=FACEmp,subset=Crop=="Corn")
Corn.Caux.mod<-aov(log(Cau)~CO2treatment*as.factor(NapplicnQ),data=FACEmp,subset=Crop=="Corn")
Corn.Mgux.mod<-aov(log(Mgu)~CO2treatment*as.factor(NapplicnQ),data=FACEmp,subset=Crop=="Corn")
Corn.Mnux.mod<-aov(log(Mnu)~CO2treatment*as.factor(NapplicnQ),data=FACEmp,subset=Crop=="Corn")
Corn.Cuux.mod<-aov(log(Cuu)~CO2treatment*as.factor(NapplicnQ),data=FACEmp,subset=Crop=="Corn")



Corn.Yield.mod.output<-data.frame(species = "Corn", nutrient = "Yield", av = fixef(Corn.Yield.mod), SE =sqrt(diag(vcov(Corn.Yield.mod))))
Corn.Znu.mod.output<-data.frame(species = "Corn", nutrient = "Znu", av = fixef(Corn.Znu.mod), SE =sqrt(diag(vcov(Corn.Znu.mod))))
Corn.Feu.mod.output<-data.frame(species = "Corn", nutrient = "Feu", av = fixef(Corn.Feu.mod), SE =sqrt(diag(vcov(Corn.Feu.mod))))
Corn.Nu.mod.output<-data.frame(species = "Corn", nutrient = "Nu", av = fixef(Corn.Nu.mod), SE =sqrt(diag(vcov(Corn.Nu.mod))))
Corn.Pu.mod.output<-data.frame(species = "Corn", nutrient = "Pu", av = fixef(Corn.Pu.mod), SE =sqrt(diag(vcov(Corn.Pu.mod))))
Corn.Ku.mod.output<-data.frame(species = "Corn", nutrient = "Ku", av = fixef(Corn.Ku.mod), SE =sqrt(diag(vcov(Corn.Ku.mod))))
Corn.Su.mod.output<-data.frame(species = "Corn", nutrient = "Su", av = fixef(Corn.Su.mod), SE =sqrt(diag(vcov(Corn.Su.mod))))
Corn.Bu.mod.output<-data.frame(species = "Corn", nutrient = "Bu", av = fixef(Corn.Bu.mod), SE =sqrt(diag(vcov(Corn.Bu.mod))))
Corn.Cau.mod.output<-data.frame(species = "Corn", nutrient = "Cau", av = fixef(Corn.Cau.mod), SE =sqrt(diag(vcov(Corn.Cau.mod))))
Corn.Mgu.mod.output<-data.frame(species = "Corn", nutrient = "Mgu", av = fixef(Corn.Mgu.mod), SE =sqrt(diag(vcov(Corn.Mgu.mod))))
Corn.Mnu.mod.output<-data.frame(species = "Corn", nutrient = "Mnu", av = fixef(Corn.Mnu.mod), SE =sqrt(diag(vcov(Corn.Mnu.mod))))
Corn.Cuu.mod.output<-data.frame(species = "Corn", nutrient = "Cuu", av = fixef(Corn.Cuu.mod), SE =sqrt(diag(vcov(Corn.Cuu.mod))))


Corn.Yieldx.mod.output<-data.frame(species = "Corn", nutrient = "Yield", av = coefficients(Corn.Yieldx.mod)[2], SE =@@@@@)
Corn.Znux.mod.output<-data.frame(species = "Corn", nutrient = "Znu", av = coefficients(Corn.Znux.mod)[2], SE =sqrt(vcov(Corn.Znux.mod)[2,2]))
Corn.Feux.mod.output<-data.frame(species = "Corn", nutrient = "Feu", av = coefficients(Corn.Feux.mod)[2], SE =sqrt(vcov(Corn.Feux.mod)[2,2]))
Corn.Nux.mod.output<-data.frame(species = "Corn", nutrient = "Nu", av = coefficients(Corn.Nux.mod)[2], SE =sqrt(vcov(Corn.Nux.mod)[2,2]))
Corn.Pux.mod.output<-data.frame(species = "Corn", nutrient = "Pu", av = coefficients(Corn.Pux.mod)[2], SE =sqrt(vcov(Corn.Pux.mod)[2,2]))
Corn.Kux.mod.output<-data.frame(species = "Corn", nutrient = "Ku", av = coefficients(Corn.Kux.mod)[2], SE =sqrt(vcov(Corn.Kux.mod)[2,2]))
Corn.Sux.mod.output<-data.frame(species = "Corn", nutrient = "Su", av = coefficients(Corn.Sux.mod)[2], SE =sqrt(vcov(Corn.Sux.mod)[2,2]))
Corn.Bux.mod.output<-data.frame(species = "Corn", nutrient = "Bu", av = coefficients(Corn.Bux.mod)[2], SE =sqrt(vcov(Corn.Bux.mod)[2,2]))
Corn.Caux.mod.output<-data.frame(species = "Corn", nutrient = "Cau", av = coefficients(Corn.Caux.mod[2]), SE =sqrt(vcov(Corn.Caux.mod)[2,2]))
Corn.Mgux.mod.output<-data.frame(species = "Corn", nutrient = "Mgu", av = coefficients(Corn.Mgux.mod)[2], SE =sqrt(vcov(Corn.Mgux.mod)[2,2]))
Corn.Mnux.mod.output<-data.frame(species = "Corn", nutrient = "Mnu", av = coefficients(Corn.Mnux.mod)[2], SE =sqrt(vcov(Corn.Mnux.mod)[2,2]))
Corn.Cuux.mod.output<-data.frame(species = "Corn", nutrient = "Cuu", av = coefficients(Corn.Cuux.mod)[2], SE =sqrt(vcov(Corn.Cuux.mod)[2,2]))
# Corn.Yieldx.mod.output<-data.frame(species = "Corn", nutrient = "Yield", av = fixef(Corn.Yieldx.mod)[2], SE =sqrt(vcov(Corn.Yieldx.mod)[2,2]))
# Corn.Znux.mod.output<-data.frame(species = "Corn", nutrient = "Znu", av = fixef(Corn.Znux.mod)[2], SE =sqrt(vcov(Corn.Znux.mod)[2,2]))
# Corn.Feux.mod.output<-data.frame(species = "Corn", nutrient = "Feu", av = fixef(Corn.Feux.mod)[2], SE =sqrt(vcov(Corn.Feux.mod)[2,2]))
# Corn.Nux.mod.output<-data.frame(species = "Corn", nutrient = "Nu", av = fixef(Corn.Nux.mod)[2], SE =sqrt(vcov(Corn.Nux.mod)[2,2]))
# Corn.Pux.mod.output<-data.frame(species = "Corn", nutrient = "Pu", av = fixef(Corn.Pux.mod)[2], SE =sqrt(vcov(Corn.Pux.mod)[2,2]))
# Corn.Kux.mod.output<-data.frame(species = "Corn", nutrient = "Ku", av = fixef(Corn.Kux.mod)[2], SE =sqrt(vcov(Corn.Kux.mod)[2,2]))
# Corn.Sux.mod.output<-data.frame(species = "Corn", nutrient = "Su", av = fixef(Corn.Sux.mod)[2], SE =sqrt(vcov(Corn.Sux.mod)[2,2]))
# Corn.Bux.mod.output<-data.frame(species = "Corn", nutrient = "Bu", av = fixef(Corn.Bux.mod)[2], SE =sqrt(vcov(Corn.Bux.mod)[2,2]))
# Corn.Caux.mod.output<-data.frame(species = "Corn", nutrient = "Cau", av = fixef(Corn.Caux.mod[2]), SE =sqrt(vcov(Corn.Caux.mod)[2,2]))
# Corn.Mgux.mod.output<-data.frame(species = "Corn", nutrient = "Mgu", av = fixef(Corn.Mgux.mod)[2], SE =sqrt(vcov(Corn.Mgux.mod)[2,2]))
# Corn.Mnux.mod.output<-data.frame(species = "Corn", nutrient = "Mnu", av = fixef(Corn.Mnux.mod)[2], SE =sqrt(vcov(Corn.Mnux.mod)[2,2]))
# Corn.Cuux.mod.output<-data.frame(species = "Corn", nutrient = "Cuu", av = fixef(Corn.Cuux.mod)[2], SE =sqrt(vcov(Corn.Cuux.mod)[2,2]))

Corn.Znu.mod.drop<-data.frame("Crop"="Corn", "Nutrient"="Znu",chisq = round(drop1(Corn.Znu.mod, test = "Chisq")[4],5))
Corn.Feu.mod.drop<-data.frame("Crop"="Corn", "Nutrient"="Feu",chisq = round(drop1(Corn.Feu.mod, test = "Chisq")[4],5))
Corn.Nu.mod.drop<-data.frame("Crop"="Corn", "Nutrient"="Nu",chisq = round(drop1(Corn.Nu.mod, test = "Chisq")[4],5))
Corn.Pu.mod.drop<-data.frame("Crop"="Corn", "Nutrient"="Pu",chisq = round(drop1(Corn.Pu.mod, test = "Chisq")[4],5))
Corn.Ku.mod.drop<-data.frame("Crop"="Corn", "Nutrient"="Ku",chisq = round(drop1(Corn.Ku.mod, test = "Chisq")[4],5))
Corn.Su.mod.drop<-data.frame("Crop"="Corn", "Nutrient"="Su",chisq = round(drop1(Corn.Su.mod, test = "Chisq")[4],5))
Corn.Bu.mod.drop<-data.frame("Crop"="Corn", "Nutrient"="Bu",chisq = round(drop1(Corn.Bu.mod, test = "Chisq")[4],5))
Corn.Cau.mod.drop<-data.frame("Crop"="Corn", "Nutrient"="Cau",chisq = round(drop1(Corn.Cau.mod, test = "Chisq")[4],5))
Corn.Mgu.mod.drop<-data.frame("Crop"="Corn", "Nutrient"="Mgu",chisq = round(drop1(Corn.Mgu.mod, test = "Chisq")[4],5))
Corn.Mnu.mod.drop<-data.frame("Crop"="Corn", "Nutrient"="Mnu",chisq = round(drop1(Corn.Mnu.mod, test = "Chisq")[4],5))
Corn.Cuu.mod.drop<-data.frame("Crop"="Corn", "Nutrient"="Cuu",chisq = round(drop1(Corn.Cuu.mod, test = "Chisq")[4],5))

Corn.Znux.mod.drop<-data.frame("Crop"="Corn", "Nutrient"="Znu",chisq = round(drop1(Corn.Znux.mod, test = "Chisq")[4],5))
Corn.Feux.mod.drop<-data.frame("Crop"="Corn", "Nutrient"="Feu",chisq = round(drop1(Corn.Feux.mod, test = "Chisq")[4],5))
Corn.Nux.mod.drop<-data.frame("Crop"="Corn", "Nutrient"="Nu",chisq = round(drop1(Corn.Nux.mod, test = "Chisq")[4],5))
Corn.Pux.mod.drop<-data.frame("Crop"="Corn", "Nutrient"="Pu",chisq = round(drop1(Corn.Pux.mod, test = "Chisq")[4],5))
Corn.Kux.mod.drop<-data.frame("Crop"="Corn", "Nutrient"="Ku",chisq = round(drop1(Corn.Kux.mod, test = "Chisq")[4],5))
Corn.Sux.mod.drop<-data.frame("Crop"="Corn", "Nutrient"="Su",chisq = round(drop1(Corn.Sux.mod, test = "Chisq")[4],5))
Corn.Bux.mod.drop<-data.frame("Crop"="Corn", "Nutrient"="Bu",chisq = round(drop1(Corn.Bux.mod, test = "Chisq")[4],5))
Corn.Caux.mod.drop<-data.frame("Crop"="Corn", "Nutrient"="Cau",chisq = round(drop1(Corn.Caux.mod, test = "Chisq")[4],5))
Corn.Mgux.mod.drop<-data.frame("Crop"="Corn", "Nutrient"="Mgu",chisq = round(drop1(Corn.Mgux.mod, test = "Chisq")[4],5))
Corn.Mnux.mod.drop<-data.frame("Crop"="Corn", "Nutrient"="Mnu",chisq = round(drop1(Corn.Mnux.mod, test = "Chisq")[4],5))
Corn.Cuux.mod.drop<-data.frame("Crop"="Corn", "Nutrient"="Cuu",chisq = round(drop1(Corn.Cuux.mod, test = "Chisq")[4],5))

corn.yield.pvals <-rbind(
  Corn.Znu.mod.drop,
  Corn.Feu.mod.drop,
  Corn.Nu.mod.drop,
  Corn.Pu.mod.drop,
  Corn.Ku.mod.drop,
  Corn.Su.mod.drop,
  Corn.Bu.mod.drop,
  Corn.Cau.mod.drop,
  Corn.Mgu.mod.drop,
  Corn.Mnu.mod.drop,
  Corn.Cuu.mod.drop,
  
  Corn.Znux.mod.drop,
  Corn.Feux.mod.drop,
  Corn.Nux.mod.drop,
  Corn.Pux.mod.drop,
  Corn.Kux.mod.drop,
  Corn.Sux.mod.drop,
  Corn.Bux.mod.drop,
  Corn.Caux.mod.drop,
  Corn.Mgux.mod.drop,
  Corn.Mnux.mod.drop,
  Corn.Cuux.mod.drop
  )

write.csv(PairedFACEdata,"~/Rdata/nutrients/pairedfacedata.csv")
