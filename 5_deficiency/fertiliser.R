AgLand_Ha1000<-read.csv("5_deficiency/inputs/AgLand_Hax1000.csv")

NperCountry<-read.csv("5_deficiency/inputs/N_tonnes_perCountry.csv")
names(NperCountry)
AgLandN<-merge(AgLand_Ha1000,NperCountry[c(4,11)], by="AreaName")


names(AgLandN)
AgLandN<-AgLandN[5:204,c(1,11,14)]
names(AgLandN)<-c("country","Ha1000","NFertiliser")
AgLandN$KgperHa<-AgLandN$NFertiliser/(AgLandN$Ha1000)

countrycodes<-read.csv("0_labels/order_FBS.csv")
names(countrycodes)[2]<-"country"

AgLandN<-merge(AgLandN, countrycodes, by="country", all=TRUE)
View(AgLandN)

plot(AgLandN$KgperHa)
median(AgLandN$KgperHa)

fert<-read.csv("5_deficiency/inputs/WBD_fertiliser.csv")
names(fert)
fert<-subset(fert, !is.na(fert$fert_qual))

names(AgLandN)
fert<-merge(AgLandN[c(4,6)], fert, by="countrycode", all=TRUE)

DIS_Par_EAR_fert<-merge(DIS_Par_EAR,fert,by="countrycode")



