DIS_Par<-read.csv("3_intake/outputs/FBS_Distribution_parameters.csv")
load("4_EAR/outputs/EAR_weighted.Rdata")

names(DIS_Par)
names(EAR_weighted)

DIS_Par_EAR<-merge(DIS_Par,EAR_weighted[c(1,2,8)], by="countrycode")

names(DIS_Par_EAR)
DIS_Par_EAR$Def_amb_HHS_EAR2<-plnorm(DIS_Par_EAR$EAR_2_BFP,DIS_Par_EAR$Amb_mean_HHS,DIS_Par_EAR$SD_HHS)
DIS_Par_EAR$Def_amb_SOFI_EAR2<-plnorm(DIS_Par_EAR$EAR_2_BFP,DIS_Par_EAR$Amb_mean_SOFI,DIS_Par_EAR$SD_SOFI)

DIS_Par_EAR$Def_CO2_HHS_EAR2<-plnorm(DIS_Par_EAR$EAR_2_BFP,DIS_Par_EAR$CO2_mean_HHS,DIS_Par_EAR$SD_HHS)
DIS_Par_EAR$Def_CO2_SOFI_EAR2<-plnorm(DIS_Par_EAR$EAR_2_BFP,DIS_Par_EAR$CO2_mean_SOFI,DIS_Par_EAR$SD_SOFI)

DIS_Par_EAR$Difference_HHS2<-DIS_Par_EAR$Def_CO2_HHS_EAR2-DIS_Par_EAR$Def_amb_HHS_EAR2
DIS_Par_EAR$Difference_SOFI2<-DIS_Par_EAR$Def_CO2_SOFI_EAR2-DIS_Par_EAR$Def_amb_SOFI_EAR2


totalpop11<-read.csv("2_countries/inputs/pop from fbs 2011.csv")
names(totalpop11)
DIS_Par_EAR<-merge(DIS_Par_EAR,totalpop11[12:14],by="countrycode") #note I didn't all=TRUE it. i could. I just don't trust the seychelles or Burundi, otherwise why would they have been excluded from the 2011 FBS?

DIS_Par_EAR$People_made_deficient_HHS2<-DIS_Par_EAR$Difference_HHS2*DIS_Par_EAR$pop
DIS_Par_EAR$People_made_deficient_SOFI2<-DIS_Par_EAR$Difference_SOFI2*DIS_Par_EAR$pop

sum_People_made_deficient_HHS2<-sum(DIS_Par_EAR$People_made_deficient_HHS2, na.rm=TRUE)
sum_People_made_deficient_SOFI2<-sum(DIS_Par_EAR$People_made_deficient_SOFI2, na.rm=TRUE)

save(DIS_Par_EAR, file="5_deficiency/DIS_Par_EAR.Rdata")
#EAR1 sure is a bit broken.

#you need to do the same for +- 1.96 SD