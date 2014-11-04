DIS_Par<-read.csv("3_intake/outputs/FBS_Distribution_parameters.csv")
load("4_EAR/outputs/EAR_weighted.Rdata")

names(DIS_Par)
names(EAR_weighted)

DIS_Par_EAR<-merge(DIS_Par,EAR_weighted[c(1,2,8)], by="countrycode")

names(DIS_Par_EAR)
DIS_Par_EAR$Def_amb_HHS_EAR2<-plnorm(DIS_Par_EAR$EAR_2_BFP,DIS_Par_EAR$Amb_mean_HHS,DIS_Par_EAR$SD_HHS)
DIS_Par_EAR$Def_amb_SOFI_EAR2<-plnorm(DIS_Par_EAR$EAR_2_BFP,DIS_Par_EAR$Amb_mean_SOFI,DIS_Par_EAR$SD_SOFI)

DIS_Par_EAR$Def_CO2_lo_HHS_EAR2<-plnorm(DIS_Par_EAR$EAR_2_BFP,DIS_Par_EAR$CO2_mean_lo_HHS,DIS_Par_EAR$SD_HHS)
DIS_Par_EAR$Def_CO2_lo_SOFI_EAR2<-plnorm(DIS_Par_EAR$EAR_2_BFP,DIS_Par_EAR$CO2_mean_lo_SOFI,DIS_Par_EAR$SD_SOFI)

DIS_Par_EAR$Def_CO2_med_HHS_EAR2<-plnorm(DIS_Par_EAR$EAR_2_BFP,DIS_Par_EAR$CO2_mean_med_HHS,DIS_Par_EAR$SD_HHS)
DIS_Par_EAR$Def_CO2_med_SOFI_EAR2<-plnorm(DIS_Par_EAR$EAR_2_BFP,DIS_Par_EAR$CO2_mean_med_SOFI,DIS_Par_EAR$SD_SOFI)

DIS_Par_EAR$Def_CO2_hi_HHS_EAR2<-plnorm(DIS_Par_EAR$EAR_2_BFP,DIS_Par_EAR$CO2_mean_hi_HHS,DIS_Par_EAR$SD_HHS)
DIS_Par_EAR$Def_CO2_hi_SOFI_EAR2<-plnorm(DIS_Par_EAR$EAR_2_BFP,DIS_Par_EAR$CO2_mean_hi_SOFI,DIS_Par_EAR$SD_SOFI)



DIS_Par_EAR$Difference_HHS_lo2<-DIS_Par_EAR$Def_CO2_lo_HHS_EAR2-DIS_Par_EAR$Def_amb_HHS_EAR2
DIS_Par_EAR$Difference_HHS_med2<-DIS_Par_EAR$Def_CO2_med_HHS_EAR2-DIS_Par_EAR$Def_amb_HHS_EAR2
DIS_Par_EAR$Difference_HHS_hi2<-DIS_Par_EAR$Def_CO2_hi_HHS_EAR2-DIS_Par_EAR$Def_amb_HHS_EAR2

DIS_Par_EAR$Difference_SOFI_lo2<-DIS_Par_EAR$Def_CO2_lo_SOFI_EAR2-DIS_Par_EAR$Def_amb_SOFI_EAR2
DIS_Par_EAR$Difference_SOFI_med2<-DIS_Par_EAR$Def_CO2_med_SOFI_EAR2-DIS_Par_EAR$Def_amb_SOFI_EAR2
DIS_Par_EAR$Difference_SOFI_hi2<-DIS_Par_EAR$Def_CO2_hi_SOFI_EAR2-DIS_Par_EAR$Def_amb_SOFI_EAR2


totalpop11<-read.csv("2_countries/inputs/pop from fbs 2011.csv")
names(totalpop11)
DIS_Par_EAR<-merge(DIS_Par_EAR,totalpop11[12:14],by="countrycode") #note I didn't all=TRUE it. i could. I just don't trust the seychelles or Burundi, otherwise why would they have been excluded from the 2011 FBS?

DIS_Par_EAR$People_made_deficient_HHS_lo2<-DIS_Par_EAR$Difference_HHS_lo2*DIS_Par_EAR$pop
DIS_Par_EAR$People_made_deficient_HHS_med2<-DIS_Par_EAR$Difference_HHS_med2*DIS_Par_EAR$pop
DIS_Par_EAR$People_made_deficient_HHS_hi2<-DIS_Par_EAR$Difference_HHS_hi2*DIS_Par_EAR$pop

DIS_Par_EAR$People_made_deficient_SOFI_lo2<-DIS_Par_EAR$Difference_SOFI_lo2*DIS_Par_EAR$pop
DIS_Par_EAR$People_made_deficient_SOFI_med2<-DIS_Par_EAR$Difference_SOFI_med2*DIS_Par_EAR$pop
DIS_Par_EAR$People_made_deficient_SOFI_hi2<-DIS_Par_EAR$Difference_SOFI_hi2*DIS_Par_EAR$pop

sum_People_made_deficient_SOFI_lo2<-sum(DIS_Par_EAR$People_made_deficient_SOFI_lo2, na.rm=TRUE)
sum_People_made_deficient_SOFI_med2<-sum(DIS_Par_EAR$People_made_deficient_SOFI_med2, na.rm=TRUE)
sum_People_made_deficient_SOFI_hi2<-sum(DIS_Par_EAR$People_made_deficient_SOFI_hi2, na.rm=TRUE)

#############go over to the fertiliser.R script if you believe the FAO rather than the world bank


fert<-read.csv("5_deficiency/inputs/WBD_fertiliser.csv")
names(fert)
fert<-subset(fert, !is.na(fert$fert_qual))
DIS_Par_EAR_fert<-merge(DIS_Par_EAR,fert,by="countrycode")
DIS_Par_EAR_fert$People_deficient_fert_SOFI<-rep(0)
DIS_Par_EAR_fert$People_deficient_Nfert_SOFI<-rep(0)

DIS_Par_EAR_fert$Fertilizer_consumption_kilograms_per_hectare_of_arable_land[is.na(DIS_Par_EAR_fert$Fertilizer_consumption_kilograms_per_hectare_of_arable_land)] <- median(DIS_Par_EAR_fert$Fertilizer_consumption_kilograms_per_hectare_of_arable_land, na.rm=TRUE)
DIS_Par_EAR_fert$KgperHa[is.na(DIS_Par_EAR_fert$KgperHa)] <- median(DIS_Par_EAR_fert$KgperHa, na.rm=TRUE)

names(DIS_Par_EAR_fert)

#total fertiliser inputs from world bank
for(rownum in 1:length(rownames(DIS_Par_EAR_fert)))
{
  if(DIS_Par_EAR_fert$Fertilizer_consumption_kilograms_per_hectare_of_arable_land[rownum]<median(DIS_Par_EAR_fert$Fertilizer_consumption_kilograms_per_hectare_of_arable_land))
    DIS_Par_EAR_fert$People_deficient_fert_SOFI[rownum]<-DIS_Par_EAR_fert$People_made_deficient_SOFI_lo2[rownum]
  if(DIS_Par_EAR_fert$Fertilizer_consumption_kilograms_per_hectare_of_arable_land[rownum]>median(DIS_Par_EAR_fert$Fertilizer_consumption_kilograms_per_hectare_of_arable_land))
    DIS_Par_EAR_fert$People_deficient_fert_SOFI[rownum]<-DIS_Par_EAR_fert$People_made_deficient_SOFI_hi2[rownum]
#   else
#     DIS_Par_EAR$People_deficient_fert_SOFI[rownum]<-DIS_Par_EAR$People_made_deficient_SOFI_med2[rownum]
}


#N fertiliser inputs from FAO bank
for(rownum in 1:length(rownames(DIS_Par_EAR_fert)))
{
  if(DIS_Par_EAR_fert$KgperHa[rownum]<median(DIS_Par_EAR_fert$KgperHa))
    DIS_Par_EAR_fert$People_deficient_Nfert_SOFI[rownum]<-DIS_Par_EAR_fert$People_made_deficient_SOFI_lo2[rownum]
  if(DIS_Par_EAR_fert$KgperHa[rownum]>median(DIS_Par_EAR_fert$KgperHa))
    DIS_Par_EAR_fert$People_deficient_Nfert_SOFI[rownum]<-DIS_Par_EAR_fert$People_made_deficient_SOFI_hi2[rownum]
}


names(DIS_Par_EAR_fert)

DIS_Par_EAR_fert<-DIS_Par_EAR_fert[c("countrycode","People_deficient_fert_SOFI","People_deficient_Nfert_SOFI")]
save(DIS_Par_EAR_fert, file="5_deficiency/DIS_Par_EAR_fert.Rdata")

save(DIS_Par_EAR, file="5_deficiency/DIS_Par_EAR.Rdata")
#EAR1 sure is a bit broken.