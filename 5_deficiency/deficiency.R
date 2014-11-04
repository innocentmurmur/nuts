DIS_Par<-read.csv("3_intake/outputs/FBS_Distribution_parameters.csv")

load("4_EAR/outputs/EAR_weighted.Rdata")
load("4_EAR/outputs/EAR_weighted50.Rdata")
load("4_EAR/outputs/EAR_weighted75.Rdata")


names(DIS_Par)
names(EAR_weighted)

DIS_Par_EAR<-merge(DIS_Par,EAR_weighted[c(1,3,5)], by="countrycode",all=TRUE)
levels(DIS_Par_EAR$countrycode)[1]

names(DIS_Par_EAR)

#50% EAR
DIS_Par_EAR50<-merge(DIS_Par,EAR_weighted50, by="countrycode",all=TRUE)
#75% EAR
DIS_Par_EAR75<-merge(DIS_Par,EAR_weighted75, by="countrycode",all=TRUE)

#veg 
DIS_ParVeg<-read.csv("3_intake/outputs/FBS_Distribution_parametersVeg.csv")
Dis_ParVeg_EAR<-merge(DIS_ParVeg,EAR_weighted, by="countrycode",all=TRUE)



###plnorm####
DIS_Par_EAR$Def_amb_HHS_EAR2<-plnorm(DIS_Par_EAR$EAR_2_BFP,DIS_Par_EAR$Amb_mean_HHS,DIS_Par_EAR$SD_HHS)
DIS_Par_EAR$Def_amb_SOFI_EAR2<-plnorm(DIS_Par_EAR$EAR_2_BFP,DIS_Par_EAR$Amb_mean_SOFI,DIS_Par_EAR$SD_SOFI)

DIS_Par_EAR$Def_CO2_HHS_EAR2<-plnorm(DIS_Par_EAR$EAR_2_BFP,DIS_Par_EAR$CO2_mean_HHS,DIS_Par_EAR$SD_HHS)
DIS_Par_EAR$Def_CO2_SOFI_EAR2<-plnorm(DIS_Par_EAR$EAR_2_BFP,DIS_Par_EAR$CO2_mean_SOFI,DIS_Par_EAR$SD_SOFI)

DIS_Par_EAR$Def_CO2u_HHS_EAR2<-plnorm(DIS_Par_EAR$EAR_2_BFP,DIS_Par_EAR$CO2_CIu_HHS,DIS_Par_EAR$SD_HHS)
DIS_Par_EAR$Def_CO2u_SOFI_EAR2<-plnorm(DIS_Par_EAR$EAR_2_BFP,DIS_Par_EAR$CO2_CIu_SOFI,DIS_Par_EAR$SD_SOFI)
DIS_Par_EAR$Def_CO2l_HHS_EAR2<-plnorm(DIS_Par_EAR$EAR_2_BFP,DIS_Par_EAR$CO2_CIl_HHS,DIS_Par_EAR$SD_HHS)
DIS_Par_EAR$Def_CO2l_SOFI_EAR2<-plnorm(DIS_Par_EAR$EAR_2_BFP,DIS_Par_EAR$CO2_CIl_SOFI,DIS_Par_EAR$SD_SOFI)


DIS_Par_EAR$Difference_HHS2<-DIS_Par_EAR$Def_CO2_HHS_EAR2-DIS_Par_EAR$Def_amb_HHS_EAR2
DIS_Par_EAR$Difference_SOFI2<-DIS_Par_EAR$Def_CO2_SOFI_EAR2-DIS_Par_EAR$Def_amb_SOFI_EAR2

DIS_Par_EAR$DifferenceCIu_HHS2<-DIS_Par_EAR$Def_CO2u_HHS_EAR2-DIS_Par_EAR$Def_amb_HHS_EAR2
DIS_Par_EAR$DifferenceCIu_SOFI2<-DIS_Par_EAR$Def_CO2u_SOFI_EAR2-DIS_Par_EAR$Def_amb_SOFI_EAR2
DIS_Par_EAR$DifferenceCIl_HHS2<-DIS_Par_EAR$Def_CO2l_HHS_EAR2-DIS_Par_EAR$Def_amb_HHS_EAR2
DIS_Par_EAR$DifferenceCIl_SOFI2<-DIS_Par_EAR$Def_CO2l_SOFI_EAR2-DIS_Par_EAR$Def_amb_SOFI_EAR2

#50
DIS_Par_EAR50$Def_amb_HHS_EAR2<-plnorm(DIS_Par_EAR50$EAR_2_BFP,DIS_Par_EAR50$Amb_mean_HHS,DIS_Par_EAR50$SD_HHS)
DIS_Par_EAR50$Def_CO2_HHS_EAR2<-plnorm(DIS_Par_EAR50$EAR_2_BFP,DIS_Par_EAR50$CO2_mean_HHS,DIS_Par_EAR50$SD_HHS)
DIS_Par_EAR50$Def_CO2u_HHS_EAR2<-plnorm(DIS_Par_EAR50$EAR_2_BFP,DIS_Par_EAR50$CO2_CIu_HHS,DIS_Par_EAR50$SD_HHS)
DIS_Par_EAR50$Def_CO2l_HHS_EAR2<-plnorm(DIS_Par_EAR50$EAR_2_BFP,DIS_Par_EAR50$CO2_CIl_HHS,DIS_Par_EAR50$SD_HHS)
DIS_Par_EAR50$Difference_HHS2<-DIS_Par_EAR50$Def_CO2_HHS_EAR2-DIS_Par_EAR50$Def_amb_HHS_EAR2
DIS_Par_EAR50$DifferenceCIu_HHS2<-DIS_Par_EAR50$Def_CO2u_HHS_EAR2-DIS_Par_EAR50$Def_amb_HHS_EAR2
DIS_Par_EAR50$DifferenceCIl_HHS2<-DIS_Par_EAR50$Def_CO2l_HHS_EAR2-DIS_Par_EAR50$Def_amb_HHS_EAR2

#75
DIS_Par_EAR75$Def_amb_HHS_EAR2<-plnorm(DIS_Par_EAR75$EAR_2_BFP,DIS_Par_EAR75$Amb_mean_HHS,DIS_Par_EAR75$SD_HHS)
DIS_Par_EAR75$Def_CO2_HHS_EAR2<-plnorm(DIS_Par_EAR75$EAR_2_BFP,DIS_Par_EAR75$CO2_mean_HHS,DIS_Par_EAR75$SD_HHS)
DIS_Par_EAR75$Def_CO2u_HHS_EAR2<-plnorm(DIS_Par_EAR75$EAR_2_BFP,DIS_Par_EAR75$CO2_CIu_HHS,DIS_Par_EAR75$SD_HHS)
DIS_Par_EAR75$Def_CO2l_HHS_EAR2<-plnorm(DIS_Par_EAR75$EAR_2_BFP,DIS_Par_EAR75$CO2_CIl_HHS,DIS_Par_EAR75$SD_HHS)
DIS_Par_EAR75$Difference_HHS2<-DIS_Par_EAR75$Def_CO2_HHS_EAR2-DIS_Par_EAR75$Def_amb_HHS_EAR2
DIS_Par_EAR75$DifferenceCIu_HHS2<-DIS_Par_EAR75$Def_CO2u_HHS_EAR2-DIS_Par_EAR75$Def_amb_HHS_EAR2
DIS_Par_EAR75$DifferenceCIl_HHS2<-DIS_Par_EAR75$Def_CO2l_HHS_EAR2-DIS_Par_EAR75$Def_amb_HHS_EAR2


#Veg
Dis_ParVeg_EAR$Def_amb_HHS_EAR2<-plnorm(Dis_ParVeg_EAR$EAR_2_BFP,Dis_ParVeg_EAR$Amb_mean_HHS,Dis_ParVeg_EAR$SD_HHS)
Dis_ParVeg_EAR$Def_CO2_HHS_EAR2<-plnorm(Dis_ParVeg_EAR$EAR_2_BFP,Dis_ParVeg_EAR$CO2_mean_HHS,Dis_ParVeg_EAR$SD_HHS)
Dis_ParVeg_EAR$Def_CO2u_HHS_EAR2<-plnorm(Dis_ParVeg_EAR$EAR_2_BFP,Dis_ParVeg_EAR$CO2_CIu_HHS,Dis_ParVeg_EAR$SD_HHS)
Dis_ParVeg_EAR$Def_CO2l_HHS_EAR2<-plnorm(Dis_ParVeg_EAR$EAR_2_BFP,Dis_ParVeg_EAR$CO2_CIl_HHS,Dis_ParVeg_EAR$SD_HHS)
Dis_ParVeg_EAR$Difference_HHS2<-Dis_ParVeg_EAR$Def_CO2_HHS_EAR2-Dis_ParVeg_EAR$Def_amb_HHS_EAR2
Dis_ParVeg_EAR$DifferenceCIu_HHS2<-Dis_ParVeg_EAR$Def_CO2u_HHS_EAR2-Dis_ParVeg_EAR$Def_amb_HHS_EAR2
Dis_ParVeg_EAR$DifferenceCIl_HHS2<-Dis_ParVeg_EAR$Def_CO2l_HHS_EAR2-Dis_ParVeg_EAR$Def_amb_HHS_EAR2




totalpop11<-read.csv("2_countries/inputs/pop from fbs 2011.csv")
totalpop11<-totalpop11[,12:14]
load("2_countries/outputs/continentpops.Rdata")
totalpop11<-rbind(totalpop11,continentpops)

DIS_Par_EAR<-merge(DIS_Par_EAR[!DIS_Par_EAR$countrycode=="",],totalpop11[!totalpop11$countrycode=="",],by="countrycode", all=TRUE)
DIS_Par_EAR50<-merge(DIS_Par_EAR50[!DIS_Par_EAR50$countrycode=="",],totalpop11[!totalpop11$countrycode=="",],by="countrycode",all=TRUE)
DIS_Par_EAR75<-merge(DIS_Par_EAR75[!DIS_Par_EAR75$countrycode=="",],totalpop11[!totalpop11$countrycode=="",],by="countrycode",all=TRUE)
Dis_ParVeg_EAR<-merge(Dis_ParVeg_EAR[!Dis_ParVeg_EAR$countrycode=="",],totalpop11[!totalpop11$countrycode=="",],by="countrycode",all=TRUE)



View(DIS_Par_EAR)
names(DIS_Par_EAR)

DIS_Par_EAR$People_already_deficient_HHS2<-DIS_Par_EAR$Def_amb_HHS_EAR2*DIS_Par_EAR$pop

DIS_Par_EAR$People_made_deficient_HHS2<-DIS_Par_EAR$Difference_HHS2*DIS_Par_EAR$pop
DIS_Par_EAR$People_made_deficient_SOFI2<-DIS_Par_EAR$Difference_SOFI2*DIS_Par_EAR$pop

DIS_Par_EAR$People_made_deficientCIu_HHS2<-DIS_Par_EAR$DifferenceCIu_HHS2*DIS_Par_EAR$pop
DIS_Par_EAR$People_made_deficientCIu_SOFI2<-DIS_Par_EAR$DifferenceCIu_SOFI2*DIS_Par_EAR$pop
DIS_Par_EAR$People_made_deficientCIl_HHS2<-DIS_Par_EAR$DifferenceCIl_HHS2*DIS_Par_EAR$pop
DIS_Par_EAR$People_made_deficientCIl_SOFI2<-DIS_Par_EAR$DifferenceCIl_SOFI2*DIS_Par_EAR$pop
#50
DIS_Par_EAR50$People_already_deficient_HHS2<-DIS_Par_EAR50$Def_amb_HHS_EAR2*DIS_Par_EAR50$pop

DIS_Par_EAR50$People_made_deficient_HHS2<-DIS_Par_EAR50$Difference_HHS2*DIS_Par_EAR50$pop

DIS_Par_EAR50$People_made_deficientCIu_HHS2<-DIS_Par_EAR50$DifferenceCIu_HHS2*DIS_Par_EAR50$pop
DIS_Par_EAR50$People_made_deficientCIl_HHS2<-DIS_Par_EAR50$DifferenceCIl_HHS2*DIS_Par_EAR50$pop
#75
DIS_Par_EAR75$People_already_deficient_HHS2<-DIS_Par_EAR75$Def_amb_HHS_EAR2*DIS_Par_EAR75$pop
DIS_Par_EAR75$People_made_deficient_HHS2<-DIS_Par_EAR75$Difference_HHS2*DIS_Par_EAR75$pop

DIS_Par_EAR75$People_made_deficientCIu_HHS2<-DIS_Par_EAR75$DifferenceCIu_HHS2*DIS_Par_EAR75$pop
DIS_Par_EAR75$People_made_deficientCIl_HHS2<-DIS_Par_EAR75$DifferenceCIl_HHS2*DIS_Par_EAR75$pop
#veg
Dis_ParVeg_EAR$People_made_deficient_HHS2<-Dis_ParVeg_EAR$Difference_HHS2*Dis_ParVeg_EAR$pop

Dis_ParVeg_EAR$People_made_deficientCIu_HHS2<-Dis_ParVeg_EAR$DifferenceCIu_HHS2*Dis_ParVeg_EAR$pop
Dis_ParVeg_EAR$People_made_deficientCIl_HHS2<-Dis_ParVeg_EAR$DifferenceCIl_HHS2*Dis_ParVeg_EAR$pop




sum_People_made_deficient_HHS2<-sum(DIS_Par_EAR$People_made_deficient_HHS2, na.rm=TRUE)
sum_People_made_deficient_SOFI2<-sum(DIS_Par_EAR$People_made_deficient_SOFI2, na.rm=TRUE)

sum_People_made_deficientCIu_HHS2<-sum(DIS_Par_EAR$People_made_deficientCIu_HHS2, na.rm=TRUE)
sum_People_made_deficientCIu_SOFI2<-sum(DIS_Par_EAR$People_made_deficientCIu_SOFI2, na.rm=TRUE)
sum_People_made_deficientCIl_HHS2<-sum(DIS_Par_EAR$People_made_deficientCIl_HHS2, na.rm=TRUE)
sum_People_made_deficientCIl_SOFI2<-sum(DIS_Par_EAR$People_made_deficientCIl_SOFI2, na.rm=TRUE)


sum_People_made_50_deficient_HHS2<-sum(DIS_Par_EAR50$People_made_deficient_HHS2, na.rm=TRUE)
sum_People_made_50_deficientCIu_HHS2<-sum(DIS_Par_EAR50$People_made_deficientCIu_HHS2, na.rm=TRUE)
sum_People_made_50_deficientCIl_HHS2<-sum(DIS_Par_EAR50$People_made_deficientCIl_HHS2, na.rm=TRUE)


sum_People_made_75_deficient_HHS2<-sum(DIS_Par_EAR75$People_made_deficient_HHS2, na.rm=TRUE)
sum_People_made_75_deficientCIu_HHS2<-sum(DIS_Par_EAR75$People_made_deficientCIu_HHS2, na.rm=TRUE)
sum_People_made_75_deficientCIl_HHS2<-sum(DIS_Par_EAR75$People_made_deficientCIl_HHS2, na.rm=TRUE)


save(DIS_Par_EAR, file="5_deficiency/DIS_Par_EAR.Rdata")
write.csv(DIS_Par_EAR,"5_deficiency/outputs/DIS_Par_EAR.csv")
save(DIS_Par_EAR50, file="5_deficiency/DIS_Par_EAR50.Rdata")
write.csv(DIS_Par_EAR50,"5_deficiency/outputs/DIS_Par_EAR50.csv")
save(DIS_Par_EAR75, file="5_deficiency/DIS_Par_EAR75.Rdata")
write.csv(DIS_Par_EAR75,"5_deficiency/outputs/DIS_Par_EAR75.csv")
save(Dis_ParVeg_EAR, file="5_deficiency/Dis_ParVeg_EAR.Rdata")
write.csv(Dis_ParVeg_EAR,"5_deficiency/outputs/Dis_ParVeg_EAR.csv")


plot(DIS_Par_EAR$Difference_HHS2~DIS_Par_EAR$Difference_SOFI2)
lm(DIS_Par_EAR$Difference_HHS2~DIS_Par_EAR$Difference_SOFI2)
noGINI<-subset(DIS_Par_EAR,is.na(DIS_Par_EAR$SD_HHS))
View(noGINI)
noGINIbutSOFI<-subset(noGINI,!is.na(noGINI$SD_SOFI))
View(noGINIbutSOFI)
