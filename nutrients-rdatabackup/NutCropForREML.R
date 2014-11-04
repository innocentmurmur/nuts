FACEdata <- read.csv("~/Rdata/nutrients/Raw_nutrient_data.csv")
FACEdata["LocYrCrWNTCCO2"]<- paste(FACEdata$Location,FACEdata$Year.of.study,FACEdata$Crop,FACEdata$WateringRegime,FACEdata$NapplicnQ,FACEdata$TOS,FACEdata$Cultivar,FACEdata$CO2treatment)

###soy doesnt have any N or watering treatments####
soyFACEdata <-subset(FACEdata, Crop=="soybean")

###peas####
peaFACEdata <-subset(FACEdata, Crop=="Field_peas")
peaFACEdata["WCCO2"]<- paste(peaFACEdata$TotH2O,peaFACEdata$Cultivar,peaFACEdata$CO2treatment)
peaMeanByWCCO2y <- data.frame(aggregate(cbind(Yield, Zn, Fe, N, P, K, S, B, Ca, Mg, Mn, Cu, protein) ~ WCCO2, FUN=mean, data=peaFACEdata))
peaLabels <- matrix(unlist(strsplit(peaMeanByWCCO2y$WCCO2," ")),ncol = 3, byrow = TRUE)
peaMeanByWCCO2y["CO2"] <- peaLabels[,3]
peaMeanByWCCO2y["watering"] <- peaLabels[,1]
peaMeanByWCCO2y["WC"] <- peaMeanByWCCO2y["WCCO2"]
peaMeanByWCCO2y$WC <- gsub("....$", "", peaMeanByWCCO2y$WC)
peaMeanByWCCO2ye <- subset(peaMeanByWCCO2y, CO2=="eCO2")
peaMeanByWCCO2ya <- subset(peaMeanByWCCO2y, CO2=="aCO2")
peaPMFy <- data.frame(merge(peaMeanByWCCO2ye,peaMeanByWCCO2ya,by="WC"))
peaPMFy["RRYield"]<-peaPMFy$Yield.x/peaPMFy$Yield.y
peaPMFy["RRZn"]<-peaPMFy$Zn.x/peaPMFy$Zn.y
peaPMFy["RRFe"]<-peaPMFy$Fe.x/peaPMFy$Fe.y
peaPMFy["RRN"]<-peaPMFy$N.x/peaPMFy$N.y
peaPMFy["RRP"]<-peaPMFy$P.x/peaPMFy$P.y
peaPMFy["RRK"]<-peaPMFy$K.x/peaPMFy$K.y
peaPMFy["RRS"]<-peaPMFy$S.x/peaPMFy$S.y
peaPMFy["RRB"]<-peaPMFy$B.x/peaPMFy$B.y
peaPMFy["RRCa"]<-peaPMFy$Ca.x/peaPMFy$Ca.y
peaPMFy["RRMg"]<-peaPMFy$Mg.x/peaPMFy$Mg.y
peaPMFy["RRMn"]<-peaPMFy$Mn.x/peaPMFy$Mn.y
peaPMFy["RRCu"]<-peaPMFy$Cu.x/peaPMFy$Cu.y
peaPMFy["RRprotein"]<-peaPMFy$protein.x/peaPMFy$protein.y

peaPMFy$RRFeu<-peaPMFy$RRFe*peaPMFy$RRYield
peaPMFy$RRZnu<-peaPMFy$RRZn*peaPMFy$RRYield
peaPMFy$RRNu<-peaPMFy$RRN*peaPMFy$RRYield
peaPMFy$RRSu<-peaPMFy$RRS*peaPMFy$RRYield
peaPMFy$RRBu<-peaPMFy$RRB*peaPMFy$RRYield
peaPMFy$RRCau<-peaPMFy$RRCa*peaPMFy$RRYield
peaPMFy$RRMgu<-peaPMFy$RRMg*peaPMFy$RRYield
peaPMFy$RRCuu<-peaPMFy$RRCu*peaPMFy$RRYield
peaPMFy$RRMnu<-peaPMFy$RRMn*peaPMFy$RRYield
peaPMFy$RRPu<-peaPMFy$RRP*peaPMFy$RRYield
peaPMFy$RRKu<-peaPMFy$RRK*peaPMFy$RRYield



###pea stats####
watering.aov.peaFe<-aov(RRFe~watering.y,peaPMFy)
summary(watering.aov.peaFe)
#P =0.0853
plot(watering.aov.peaFe)

watering.aov.peaP<-aov(RRP~watering.y,peaPMFy)
summary(watering.aov.peaP)
#P= 0.948
plot(watering.aov.peaP)

watering.aov.peaK<-aov(RRK~watering.y,peaPMFy)
summary(watering.aov.peaK)
#P=0.493
plot(watering.aov.peaK)

watering.aov.peaS<-aov(RRS~watering.y,peaPMFy)
summary(watering.aov.peaS)
#P= 0.572
plot(watering.aov.peaS)

watering.aov.peaB<-aov(RRB~watering.y,peaPMFy)
summary(watering.aov.peaB)
#P=0.296
plot(watering.aov.peaB)

watering.aov.peaCa<-aov(RRCa~watering.y,peaPMFy)
summary(watering.aov.peaCa)
#P=0.802
plot(watering.aov.peaCa)

watering.aov.peaMg<-aov(RRMg~watering.y,peaPMFy)
summary(watering.aov.peaMg)
#P=0.581
plot(watering.aov.peaMg)

watering.aov.peaMn<-aov(RRMn~watering.y,peaPMFy)
summary(watering.aov.peaMn)
#P=0.96
plot(watering.aov.peaMn)

watering.aov.peaCu<-aov(RRCu~watering.y,peaPMFy)
summary(watering.aov.peaCu)
#actually sig: p=0.037
plot(watering.aov.peaCu)

watering.aov.peaZn<-aov(RRZn~watering.y,peaPMFy)
summary(watering.aov.peaZn)
#also sig-ish: p=0.0499
plot(watering.aov.peaZn)


watering.aov.peaprotein<-aov(RRprotein~watering.y,peaPMFy)
summary(watering.aov.peaprotein)
#P=0.722
plot(watering.aov.peaprotein)

watering.aov.peaYield<-aov(RRYield~watering.y,peaPMFy)
summary(watering.aov.peaYield)
#p=0.811
plot(watering.aov.peaYield)


write.csv(peaPMFy, file="~/Rdata/nutrients/peaPMFy.csv")

###Rice####

riceFACEdata <-subset(FACEdata, Crop=="Rice")
riceFACEdata["NCCO2"]<- paste(riceFACEdata$NapplicnQ,riceFACEdata$Cultivar,riceFACEdata$CO2treatment)
riceMeanByNCCO2 <- data.frame(aggregate(cbind(Zn, Fe, N, P, K, S, B, Ca, Mg, Mn, Cu) ~ NCCO2, FUN=mean, data=riceFACEdata))
riceMeanByNCCO2["NC"] <- riceMeanByNCCO2["NCCO2"]
riceMeanByNCCO2$NC <- gsub("....$", "", riceMeanByNCCO2$NC)
riceLabels <- matrix(unlist(strsplit(riceMeanByNCCO2$NCCO2," ")),ncol = 3, byrow = TRUE)
riceMeanByNCCO2["CO2"] <- riceLabels[,3]
riceMeanByNCCO2["cultivar"] <- riceLabels[,2]
riceMeanByNCCO2["fert"] <- riceLabels[,1]
riceMeanByNCCO2e <- subset(riceMeanByNCCO2, CO2=="eCO2")
riceMeanByNCCO2a <- subset(riceMeanByNCCO2, CO2=="aCO2")
ricePMF <- data.frame(merge(riceMeanByNCCO2e,riceMeanByNCCO2a,by="NC"))
ricePMF["RRZn"]<-ricePMF$Zn.x/ricePMF$Zn.y
ricePMF["RRFe"]<-ricePMF$Fe.x/ricePMF$Fe.y
ricePMF["RRN"]<-ricePMF$N.x/ricePMF$N.y
ricePMF["RRP"]<-ricePMF$P.x/ricePMF$P.y
ricePMF["RRK"]<-ricePMF$K.x/ricePMF$K.y
ricePMF["RRS"]<-ricePMF$S.x/ricePMF$S.y
ricePMF["RRB"]<-ricePMF$B.x/ricePMF$B.y
ricePMF["RRCa"]<-ricePMF$Ca.x/ricePMF$Ca.y
ricePMF["RRMg"]<-ricePMF$Mg.x/ricePMF$Mg.y
ricePMF["RRMn"]<-ricePMF$Mn.x/ricePMF$Mn.y
ricePMF["RRCu"]<-ricePMF$Cu.x/ricePMF$Cu.y


riceyield<- data.frame(
  NC=c("Medium Aikoku ",
       "Medium Akidawara ",
       "Medium Akihikari ",
       "Medium Akita63 ",
       "Medium Akitakomachi ",
       "Medium Koshihikari ",
       "Medium Norin8 ",
       "Medium Takanari ",
       "Low Koshihikari ",
       "Medium Koshihikari ",
       "Low Koshihikari ",
       "Medium Koshihikari ",
       "Low Akitakomachi ",
       "Medium Akitakomachi ",
       "High Akitakomachi ",
       "Low Akitakomachi ",
       "Medium Akitakomachi ",
       "High Akitakomachi ",
       "Low Akitakomachi ",
       "Medium Akitakomachi ",
       "High Akitakomachi ",
       "Medium Kirara397 ",
       "Medium Kakehashi ",
       "Medium Akitakomachi ",
       "Medium Hitomebore ",
       "Medium Kirara397 ",
       "Medium Kakehashi ",
       "Medium Akitakomachi ",
       "Medium Hitomebore "),
  yield=c(26.51356994,
          26.1208577,
          2.752293578,
          19.75736568,
          4.142011834,
          16.30434783,
          35.96287703,
          21.2338594,
          13.75,
          16,
          7,
          16,
          11.4,
          15.8,
          21.1,
          4.250617108,
          13.4564796,
          11.39747146,
          6.5,
          14.6,
          13.1,
          13,
          3,
          6,
          17,
          23,
          3,
          17.33333333,
          15))

avriceyield <-data.frame(aggregate(yield ~ NC, FUN=mean, data=riceyield))

ricePMFy<-merge(ricePMF,avriceyield,by="NC")

ricePMFy$RRFeu<-ricePMFy$RRFe*((100+ricePMFy$yield)/100)
ricePMFy$RRZnu<-ricePMFy$RRZn*((100+ricePMFy$yield)/100)
ricePMFy$RRNu<-ricePMFy$RRN*((100+ricePMFy$yield)/100)
ricePMFy$RRSu<-ricePMFy$RRS*((100+ricePMFy$yield)/100)
ricePMFy$RRBu<-ricePMFy$RRB*((100+ricePMFy$yield)/100)
ricePMFy$RRCau<-ricePMFy$RRCa*((100+ricePMFy$yield)/100)
ricePMFy$RRMgu<-ricePMFy$RRMg*((100+ricePMFy$yield)/100)
ricePMFy$RRCuu<-ricePMFy$RRCu*((100+ricePMFy$yield)/100)
ricePMFy$RRMnu<-ricePMFy$RRMn*((100+ricePMFy$yield)/100)
ricePMFy$RRPu<-ricePMFy$RRP*((100+ricePMFy$yield)/100)
ricePMFy$RRKu<-ricePMFy$RRK*((100+ricePMFy$yield)/100)

###rice stats####
fert.aov.rice<-aov(RRN~fert.y,ricePMF)
summary(fert.aov.rice)
plot(fert.aov.rice)
#P = 0.158, therefore no interaction

fert.aov.riceZn<-aov(RRZn~fert.y,ricePMF)
summary(fert.aov.riceZn)
plot(fert.aov.riceZn)
#P = 0.158, therefore no interaction

fert.aov.riceZn<-aov(RRZn~fert.y,ricePMF)
summary(fert.aov.riceZn)
plot(fert.aov.riceZn)
# P = 0.0506

fert.aov.riceFe<-aov(RRFe~fert.y,ricePMF)
summary(fert.aov.riceFe)
plot(fert.aov.riceFe)
#P = 0.72

fert.aov.riceP<-aov(RRP~fert.y,ricePMF)
summary(fert.aov.riceP)
plot(fert.aov.riceP)
#P = 0.0509

fert.aov.riceK<-aov(RRK~fert.y,ricePMF)
summary(fert.aov.riceK)
plot(fert.aov.riceK)
#P = 0.055

fert.aov.riceS<-aov(RRS~fert.y,ricePMF)
summary(fert.aov.riceS)
plot(fert.aov.riceS)
#P = 0.327

fert.aov.riceB<-aov(RRB~fert.y,ricePMF)
summary(fert.aov.riceB)
plot(fert.aov.riceB)
#P= 0.02

fert.aov.riceCa<-aov(RRCa~fert.y,ricePMF)
summary(fert.aov.riceCa)
plot(fert.aov.riceCa)
#P= 0.5

fert.aov.riceMg<-aov(RRMg~fert.y,ricePMF)
summary(fert.aov.riceMg)
plot(fert.aov.riceMg)
#P = 0.122

fert.aov.riceMn<-aov(RRMn~fert.y,ricePMF)
summary(fert.aov.riceMn)
plot(fert.aov.riceMn)
#P = 0.229

fert.aov.riceCu<-aov(RRCu~fert.y,ricePMF)
summary(fert.aov.riceCu)
plot(fert.aov.riceCu)
#P = 0.43


write.csv(ricePMF, file="~/Rdata/nutrients/ricePMF.csv")

###Maize####
maizeFACEdata <-subset(FACEdata, Crop=="Corn")
summary(maizeFACEdata)
#It's got a nitrogen treatment but no watering treatment. and two different cultivars

maizeFACEdata["NCCO2"]<- paste(maizeFACEdata$NapplicnQ,maizeFACEdata$Cultivar,maizeFACEdata$CO2treatment)
maizeMeanByNCCO2 <- data.frame(aggregate(cbind(Zn, Fe, N, P, K, S, B, Ca, Mg, Mn, Cu) ~ NCCO2, FUN=mean, data=maizeFACEdata))
maizeMeanByNCCO2["NC"] <- maizeMeanByNCCO2["NCCO2"]
maizeMeanByNCCO2$NC <- gsub("....$", "", maizeMeanByNCCO2$NC)
maizeLabels <- matrix(unlist(strsplit(maizeMeanByNCCO2$NCCO2," ")), ncol = 3, byrow = TRUE)
maizeMeanByNCCO2["CO2"] <- maizeLabels[,3]
maizeMeanByNCCO2["Cv"] <- maizeLabels[,2]
maizeMeanByNCCO2["fert"] <- maizeLabels[,1]
maizeMeanByNCCO2e <- subset(maizeMeanByNCCO2, CO2=="eCO2")
maizeMeanByNCCO2a <- subset(maizeMeanByNCCO2, CO2=="aCO2")
maizePMF <- data.frame(merge(maizeMeanByNCCO2e,maizeMeanByNCCO2a,by="NC"))
maizePMF["RRZn"]<-maizePMF$Zn.x/maizePMF$Zn.y
maizePMF["RRFe"]<-maizePMF$Fe.x/maizePMF$Fe.y
maizePMF["RRN"]<-maizePMF$N.x/maizePMF$N.y
maizePMF["RRP"]<-maizePMF$P.x/maizePMF$P.y
maizePMF["RRK"]<-maizePMF$K.x/maizePMF$K.y
maizePMF["RRS"]<-maizePMF$S.x/maizePMF$S.y
maizePMF["RRB"]<-maizePMF$B.x/maizePMF$B.y
maizePMF["RRCa"]<-maizePMF$Ca.x/maizePMF$Ca.y
maizePMF["RRMg"]<-maizePMF$Mg.x/maizePMF$Mg.y
maizePMF["RRMn"]<-maizePMF$Mn.x/maizePMF$Mn.y
maizePMF["RRCu"]<-maizePMF$Cu.x/maizePMF$Cu.y

fert.aov.maizeN<-aov(RRN~fert.y,maizePMF)
summary(fert.aov.maizeN)
plot(fert.aov.maizeN)
#P = 0.0544 ALMOST

fert.aov.maizeZn<-aov(RRZn~fert.y,maizePMF)
summary(fert.aov.maizeZn)
plot(fert.aov.maizeZn)
#P = 0.54

fert.aov.maizeFe<-aov(RRFe~fert.y,maizePMF)
summary(fert.aov.maizeFe)
plot(fert.aov.maizeFe)
#P = 0.132

fert.aov.maizeP<-aov(RRP~fert.y,maizePMF)
summary(fert.aov.maizeP)
plot(fert.aov.maizeP)
#P = 0.428

fert.aov.maizeK<-aov(RRK~fert.y,maizePMF)
summary(fert.aov.maizeK)
plot(fert.aov.maizeK)
#P = 0.874

fert.aov.maizeS<-aov(RRS~fert.y,maizePMF)
summary(fert.aov.maizeS)
plot(fert.aov.maizeS)
#P = 0.0801

fert.aov.maizeB<-aov(RRB~fert.y,maizePMF)
summary(fert.aov.maizeB)
plot(fert.aov.maizeB)
#P= 0.906

fert.aov.maizeCa<-aov(RRCa~fert.y,maizePMF)
summary(fert.aov.maizeCa)
plot(fert.aov.maizeCa)
#P= 0.277

fert.aov.maizeMg<-aov(RRMg~fert.y,maizePMF)
summary(fert.aov.maizeMg)
plot(fert.aov.maizeMg)
#P = 0.689

fert.aov.maizeMn<-aov(RRMn~fert.y,maizePMF)
summary(fert.aov.maizeMn)
plot(fert.aov.maizeMn)
#P = 0.502

fert.aov.maizeCu<-aov(RRCu~fert.y,maizePMF)
summary(fert.aov.maizeCu)
plot(fert.aov.maizeCu)
#P = 0.116


write.csv(maizePMF, file="~/Rdata/nutrients/maizePMF.csv")

####sorghum####
sorghumFACEdata <-subset(FACEdata, Crop=="Sorghum")
summary(sorghumFACEdata)

#it has watering, two years, no other variables.
sorghumFACEdata["YrWCO2"]<- paste(sorghumFACEdata$Year.of.study,sorghumFACEdata$TotH2O,sorghumFACEdata$CO2treatment)
sorghumMeanByYrWCO2y <- data.frame(aggregate(cbind(Zn, Fe, N, P, K, S, B, Ca, Mg, Mn, Cu) ~ YrWCO2, FUN=mean, data=sorghumFACEdata))
sorghumLabels <- matrix(unlist(strsplit(sorghumMeanByYrWCO2y$YrWCO2," ")),ncol = 3, byrow = TRUE)
sorghumMeanByYrWCO2y["CO2"] <- sorghumLabels[,3]
sorghumMeanByYrWCO2y["watering"] <- sorghumLabels[,2]
sorghumMeanByYrWCO2y$watering <-as.numeric(sorghumMeanByYrWCO2y$watering)
#the ANOVAs below turn it back into a factor, but if I want to do REMLs, which I do, keep it as numeric AND DONT DO THE ANOVAS in case they convert it!
sorghumMeanByYrWCO2y["Year"] <- sorghumLabels[,1]
sorghumMeanByYrWCO2y["YrW"] <- sorghumMeanByYrWCO2y["YrWCO2"]
sorghumMeanByYrWCO2y$YrW <- gsub("....$", "", sorghumMeanByYrWCO2y$YrW)
sorghumMeanByYrWCO2ye <- subset(sorghumMeanByYrWCO2y, CO2=="eCO2")
sorghumMeanByYrWCO2ya <- subset(sorghumMeanByYrWCO2y, CO2=="aCO2")
sorghumPMFy <- data.frame(merge(sorghumMeanByYrWCO2ye,sorghumMeanByYrWCO2ya,by="YrW"))
sorghumPMFy["RRZn"]<-sorghumPMFy$Zn.x/sorghumPMFy$Zn.y
sorghumPMFy["RRFe"]<-sorghumPMFy$Fe.x/sorghumPMFy$Fe.y
sorghumPMFy["RRN"]<-sorghumPMFy$N.x/sorghumPMFy$N.y
sorghumPMFy["RRP"]<-sorghumPMFy$P.x/sorghumPMFy$P.y
sorghumPMFy["RRK"]<-sorghumPMFy$K.x/sorghumPMFy$K.y
sorghumPMFy["RRS"]<-sorghumPMFy$S.x/sorghumPMFy$S.y
sorghumPMFy["RRB"]<-sorghumPMFy$B.x/sorghumPMFy$B.y
sorghumPMFy["RRCa"]<-sorghumPMFy$Ca.x/sorghumPMFy$Ca.y
sorghumPMFy["RRMg"]<-sorghumPMFy$Mg.x/sorghumPMFy$Mg.y
sorghumPMFy["RRMn"]<-sorghumPMFy$Mn.x/sorghumPMFy$Mn.y
sorghumPMFy["RRCu"]<-sorghumPMFy$Cu.x/sorghumPMFy$Cu.y

SorgYield <-data.frame(YrW = c("1998 474 ","1998 1218 ",  "1999 491 ",  "1999 1047 " ),
                       yield=c(17.16101695,1.044776119,33.96226415,-10.73684211))

sorghumPMFy<-merge(sorghumPMFy,SorgYield,by="YrW")

sorghumPMFy$RRFeu<-sorghumPMFy$RRFe*((100+sorghumPMFy$yield)/100)
sorghumPMFy$RRZnu<-sorghumPMFy$RRZn*((100+sorghumPMFy$yield)/100)
sorghumPMFy$RRNu<-sorghumPMFy$RRN*((100+sorghumPMFy$yield)/100)
sorghumPMFy$RRSu<-sorghumPMFy$RRS*((100+sorghumPMFy$yield)/100)
sorghumPMFy$RRBu<-sorghumPMFy$RRB*((100+sorghumPMFy$yield)/100)
sorghumPMFy$RRCau<-sorghumPMFy$RRCa*((100+sorghumPMFy$yield)/100)
sorghumPMFy$RRMgu<-sorghumPMFy$RRMg*((100+sorghumPMFy$yield)/100)
sorghumPMFy$RRCuu<-sorghumPMFy$RRCu*((100+sorghumPMFy$yield)/100)
sorghumPMFy$RRMnu<-sorghumPMFy$RRMn*((100+sorghumPMFy$yield)/100)
sorghumPMFy$RRPu<-sorghumPMFy$RRP*((100+sorghumPMFy$yield)/100)
sorghumPMFy$RRKu<-sorghumPMFy$RRK*((100+sorghumPMFy$yield)/100)


###sorghum stats####
watering.aov.sorghumFe<-aov(RRFe~watering.y,sorghumPMFy)
summary(watering.aov.sorghumFe)
#P =0.0195
plot(watering.aov.sorghumFe)

watering.aov.sorghumP<-aov(RRP~watering.y,sorghumPMFy)
summary(watering.aov.sorghumP)
#P= 0.0271
plot(watering.aov.sorghumP)

watering.aov.sorghumK<-aov(RRK~watering.y,sorghumPMFy)
summary(watering.aov.sorghumK)
#P=0.393
plot(watering.aov.sorghumK)

watering.aov.sorghumS<-aov(RRS~watering.y,sorghumPMFy)
summary(watering.aov.sorghumS)
#P= 0.128
plot(watering.aov.sorghumS)

watering.aov.sorghumB<-aov(RRB~watering.y,sorghumPMFy)
summary(watering.aov.sorghumB)
#P=0.599
plot(watering.aov.sorghumB)

watering.aov.sorghumCa<-aov(RRCa~watering.y,sorghumPMFy)
summary(watering.aov.sorghumCa)
#P=0.128
plot(watering.aov.sorghumCa)

watering.aov.sorghumMg<-aov(RRMg~watering.y,sorghumPMFy)
summary(watering.aov.sorghumMg)
#P=0.0298
plot(watering.aov.sorghumMg)

watering.aov.sorghumMn<-aov(RRMn~watering.y,sorghumPMFy)
summary(watering.aov.sorghumMn)
#P=0.199
plot(watering.aov.sorghumMn)

watering.aov.sorghumCu<-aov(RRCu~watering.y,sorghumPMFy)
summary(watering.aov.sorghumCu)
#actually sig: p=0.0441
plot(watering.aov.sorghumCu)

watering.aov.sorghumZn<-aov(RRZn~watering.y,sorghumPMFy)
summary(watering.aov.sorghumZn)
#also sig-ish: p=0.071
plot(watering.aov.sorghumZn)

#. so only in Maize did N go from non-limiting to limiting? Which direction did the interaction occur in? I think it might've been the wrong direction, from memory
plot(maizePMF$RRN~maizePMF$fert.y)
maizePMF$fert.y <- factor(maizePMF$fert.y)


####Wheat####
wheatFACEdata <-subset(FACEdata, Crop=="wheat")
wheatFACEdata$Znu<-wheatFACEdata$Zn/10000*wheatFACEdata$Yield
wheatFACEdata$Feu<-wheatFACEdata$Fe/10000*wheatFACEdata$Yield
wheatFACEdata$proteinu<-(wheatFACEdata$protein/100)*wheatFACEdata$Yield

#units are grams

#I have changed "WateringRegime" to "TotH2O" in preparation for doing a GLM. Please change back for ANOVAs. I've done the same with the peas above, but it doesn't matter because there were only two groups
wheatFACEdata["LocYrWNTCCO2"]<-paste(wheatFACEdata$Location, wheatFACEdata$Year.of.study, wheatFACEdata$TotH2O, wheatFACEdata$NapplicnQ, wheatFACEdata$TOS, wheatFACEdata$Cultivar, wheatFACEdata$CO2treatment)
wheatMeanByLocYrWNTCCO2y <- data.frame(aggregate(cbind(Yield, Zn, Fe, protein,Znu,Feu,proteinu) ~ LocYrWNTCCO2, FUN=mean, data=wheatFACEdata))
wheatLabels <- matrix(unlist(strsplit(wheatMeanByLocYrWNTCCO2y$LocYrWNTCCO2," ")),ncol = 7, byrow = TRUE)
wheatMeanByLocYrWNTCCO2y["CO2"] <- wheatLabels[,7]
wheatMeanByLocYrWNTCCO2y["watering"] <- wheatLabels[,3]
wheatMeanByLocYrWNTCCO2y$watering <-as.numeric(wheatMeanByLocYrWNTCCO2y$watering)
wheatMeanByLocYrWNTCCO2y["fert"] <- wheatLabels[,4]
wheatMeanByLocYrWNTCCO2y["Loc"] <- wheatLabels[,1]
wheatMeanByLocYrWNTCCO2y["Yr"] <- wheatLabels[,2]
wheatMeanByLocYrWNTCCO2y["TOS"] <- wheatLabels[,5]
wheatMeanByLocYrWNTCCO2y["Cv"] <- wheatLabels[,6]
wheatMeanByLocYrWNTCCO2y["LocYrWNTC"] <- wheatMeanByLocYrWNTCCO2y["LocYrWNTCCO2"]
wheatMeanByLocYrWNTCCO2y$LocYrWNTC <- gsub("....$", "", wheatMeanByLocYrWNTCCO2y$LocYrWNTC)
wheatMeanByLocYrWNTCCO2ye <- subset(wheatMeanByLocYrWNTCCO2y, CO2=="eCO2")
wheatMeanByLocYrWNTCCO2ya <- subset(wheatMeanByLocYrWNTCCO2y, CO2=="aCO2")
wheatPMFy <- data.frame(merge(wheatMeanByLocYrWNTCCO2ye,wheatMeanByLocYrWNTCCO2ya,by="LocYrWNTC"))
wheatPMFy["RRYield"]<-wheatPMFy$Yield.x/wheatPMFy$Yield.y
wheatPMFy["RRZn"]<-wheatPMFy$Zn.x/wheatPMFy$Zn.y
wheatPMFy["RRFe"]<-wheatPMFy$Fe.x/wheatPMFy$Fe.y
wheatPMFy["RRprotein"]<-wheatPMFy$protein.x/wheatPMFy$protein.y

wheatPMFy["RRZnu"]<-wheatPMFy$Znu.x/wheatPMFy$Znu.y
wheatPMFy["RRFeu"]<-wheatPMFy$Feu.x/wheatPMFy$Feu.y
wheatPMFy["RRproteinu"]<-wheatPMFy$proteinu.x/wheatPMFy$proteinu.y



write.csv(wheatPMFy, file="~/Rdata/nutrients/wheatPMFy.csv")
#### wheat stats####

test.glm.wheat <- glm(Zn~CO2*watering,family=gaussian,wheatMeanByLocYrWNTCCO2y )

test.aov.wheat <-aov(Fe~CO2*watering*Loc*Yr*TOS*Cv,wheatMeanByLocYrWNTCCO2y)
summary(test.aov.wheat)
print(test.aov.wheat)
test3.wheat <- aov(Zn~CO2*fert + Loc + Yr + Cv + TOS, wheatMeanByLocYrWNTCCO2y)
summary(test3.wheat)

test4.wheat <- lmer(Zn ~ CO2*watering + (1|Loc)+(1|Yr)+(1|Cv), wheatMeanByLocYrWNTCCO2y)
#I don't understand whether it's supposed to be (1|Loc) etc or (CO2|Loc)
print(test4.wheat)
test4.wheat.null <- lmer(Zn ~ 1 + (1|Loc)+(1|Yr), wheatMeanByLocYrWNTCCO2y)
test4.wheatco2 <- lmer(Zn ~ CO2 + (1|Loc)+(1|Yr), wheatMeanByLocYrWNTCCO2y)
#comparing this with test4.wheat

lr.test4wheat<-anova(test4.wheat,test4.wheat.null)
lr.test4wheat2 <-anova(test4.wheat,test4.wheatco2)
lr.test4wheat3 <-anova(test4.wheatco2,test4.wheat.null)
#; the lower the BIC the better the model fits the data (and more negative is even better)
#                 Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
#test4.wheat.null  4 740.76 752.17 -366.38   732.76                             
#test4.wheat       7 727.05 747.01 -356.52   713.05 19.712      3  0.0001947
#summary(test2.wheat)

#Linear mixed model fit by REML ['lmerMod']
#Formula: Zn ~ CO2 * watering + (CO2 | Loc) + (CO2 | Yr) 
#Data: wheatMeanByLocYrWNTCCO2y 

#REML criterion at convergence: 703.1678 

#Random effects:
#  Groups   Name        Variance Std.Dev. Corr
#Yr       (Intercept)  0.08132 0.2852       
#CO2eCO2      1.83491 1.3546   1.00
#Loc      (Intercept)  4.04921 2.0123       
#CO2eCO2      2.04461 1.4299   1.00
#Residual             14.58855 3.8195       
#Number of obs: 128, groups: Yr, 3; Loc, 2

##The variance estimates are of interest here because we can add them together to find the total variance (of the random effects) and then divide that total by each random effect to see what proportion of the random effect variance is attributable to each random effect
#http://www.unt.edu/rss/class/Jon/Benchmarks/LinearMixedModels_JDS_Dec2010.pdf
#and if the % of variance is small, you can ditch it. If all of them are small, you have to do a GLM instead of a REML

#Fixed effects:
#  Estimate Std. Error t value
#(Intercept)          30.1857     1.6661  18.117
#CO2eCO2              -4.1549     1.6609  -2.502
#wateringWet           0.1723     0.9635   0.179
#CO2eCO2:wateringWet   1.0290     1.3556   0.759

#Correlation of Fixed Effects:
#  (Intr) CO2eCO2 wtrngW
#CO2eCO2      0.453               
#wateringWet -0.337  0.280        
#CO2CO2:wtrW  0.199 -0.444  -0.694
#I think this means that the estimated effect of watering crosses 0, as does the interaction, but the CO2 effect is sig
#better read on, in that pdf above, to understand this bit, but i think it's time to go to work

watering.aov.wheatFe<-aov(RRFe~watering.y,wheatPMFy)
summary(watering.aov.wheatFe)
#P =.423
plot(watering.aov.wheatFe)

watering.aov.wheatZn<-aov(RRZn~watering.y,wheatPMFy)
summary(watering.aov.wheatZn)
#p=.568
plot(watering.aov.wheatZn)

watering.aov.wheatprotein<-aov(RRprotein~watering.y,wheatPMFy)
summary(watering.aov.wheatprotein)
#P=0.145
plot(watering.aov.wheatprotein)

watering.aov.wheatYield<-aov(RRYield~watering.y,wheatPMFy)
summary(watering.aov.wheatYield)
#p=0.0938
plot(watering.aov.wheatYield)

#with fertilisation
fert.aov.wheatFe<-aov(RRFe~fert.y,wheatPMFy)
summary(fert.aov.wheatFe)
#P =.36
plot(fert.aov.wheatFe)

fert.aov.wheatZn<-aov(RRZn~fert.y,wheatPMFy)
summary(fert.aov.wheatZn)
#p=0.637
plot(fert.aov.wheatZn)

fert.aov.wheatprotein<-aov(RRprotein~fert.y,wheatPMFy)
summary(fert.aov.wheatprotein)
#P=0.215
plot(fert.aov.wheatprotein)

fert.aov.wheatYield<-aov(RRYield~fert.y,wheatPMFy)
summary(fert.aov.wheatYield)
#p=0.745
plot(fert.aov.wheatYield)

Zn.co2.wheat <- lmer(Zn ~ CO2 + (1|Loc)+(1|Yr)+(1|Cv)+(1|TOS), wheatMeanByLocYrWNTCCO2y)
Zn.co2.wheat.null <- lmer(Zn ~ 1 + (1|Loc)+(1|Yr)+(1|Cv)+(1|TOS), wheatMeanByLocYrWNTCCO2y)

lr.Zn.co2wheat<-anova(Zn.co2.wheat,Zn.co2.wheat.null)
testzn<-aggregate(Zn~Loc*Cv, FUN=mean, data=wheatMeanByLocYrWNTCCO2y)


Zn.co2.wheat2 <- lmer(Zn ~ CO2 + (1|TOS), wheatMeanByLocYrWNTCCO2y)
to_predict <- expand.grid(CO2=c("aCO2","eCO2"),TOS=c("TOS_1","TOS_2"))
testZn2 <- ezPredict(Zn.co2.wheat2,to_predict=to_predict,boot=F)

Zn.co2.wheat3 <- lmer(Zn ~ CO2*TOS*Yr + (1|Cv), wheatMeanByLocYrWNTCCO2y)
wheatMeanByLocYrWNTCCO2y$Yr<-as.factor(wheatMeanByLocYrWNTCCO2y$Yr)
to_predict <- expand.grid(CO2=c("aCO2","eCO2"),TOS=c("TOS_1","TOS_2"),Yr=c("2007","2008","2009"))
testZn3 <- ezPredict(Zn.co2.wheat3,boot=F)


library(lme4)
library(ggplot2) # Plotting
fm1 <- lmer(Formula = Zn ~ CO2*fert + (CO2|Cv), data = wheatMeanByLocYrWNTCCO2y)
newdat <- expand.grid(CO2=c("eCO2","aCO2"),fert=c("Low","Medium"),Zn = 0)
mm <- model.matrix(terms(fm1),newdat)
newdat$Zn <- mm %*% fixef(fm1)
pvar1 <- diag(mm %*% tcrossprod(vcov(fm1),mm))
tvar1 <- pvar1+VarCorr(fm1)$Cv[1]  ## must be adapted for more complex models
newdat <- data.frame(
  newdat
  , plo = newdat$Zn-2*sqrt(pvar1)
  , phi = newdat$Zn+2*sqrt(pvar1)
  , tlo = newdat$Zn-2*sqrt(tvar1)
  , thi = newdat$Zn+2*sqrt(tvar1)
)
#plot confidence
g0 <- ggplot(newdat, aes(x=fert, y=Zn, colour=CO2))+geom_point()
g0 + geom_errorbar(aes(ymin = plo, ymax = phi))+
  opts(title="CI based on fixed-effects uncertainty ONLY")
#plot prediction
g0 + geom_errorbar(aes(ymin = tlo, ymax = thi))+
  opts(title="CI based on FE uncertainty + RE variance")

newdat
#CO2   fert       Zn      plo      phi      tlo      thi
#1 eCO2    Low 31.11962 29.73022 32.50903 28.48718 33.75207
#2 aCO2    Low 28.23365 26.92635 29.54095 25.64360 30.82370
#3 eCO2 Medium 33.42889 30.72038 36.13740 29.91672 36.94106
#4 aCO2 Medium 29.54191 26.95753 32.12629 26.12455 32.95927

##same on pmfs
###Zn wheat####
ZnW <- lmer(RRZn ~ fert.x + (fert.x|Cv.x)+(fert.x|TOS.x)+(fert.x|Loc.x)+(fert.x|Yr.x), data = wheatPMFy)
summary(ZnW)
ZnW1 <-lmer(RRZn ~ fert.x + (fert.x|Loc.x),data = wheatPMFy)
summary(ZnW1)
anova(ZnW,ZnW1)
ZnW2 <-lmer(RRZn ~ fert.x + (1|Loc.x),data = wheatPMFy)
anova(ZnW1,ZnW2)
ZnWn <-lmer(RRZn~1+(1|Loc.x),data = wheatPMFy)
anova(ZnWn,ZnW2)
#Df     AIC     BIC logLik deviance  Chisq Chi Df Pr(>Chisq)
#ZnW1  6 -63.477 -50.524 37.738  -75.477                         
#ZnW  15 -58.950 -26.567 44.475  -88.950 13.473      9     0.1423

newdat.ZnW <- expand.grid(fert.x=c("Low","Medium"),RRZn = 0)
mm.ZnW <- model.matrix(terms(ZnW2),newdat.ZnW)
newdat.ZnW$RRZn <- mm.ZnW %*% fixef(ZnW2)
pvar1.ZnW <- diag(mm.ZnW %*% tcrossprod(vcov(ZnW2),mm.ZnW))
tvar1.ZnW <- pvar1.ZnW+VarCorr(ZnW2)$Loc[1]  ## must be adapted for more complex models
newdat.ZnW <- data.frame(
  newdat.ZnW
  , plo = newdat.ZnW$RRZn-2*sqrt(pvar1.ZnW)
  , phi = newdat.ZnW$RRZn+2*sqrt(pvar1.ZnW)
  , tlo = newdat.ZnW$RRZn-2*sqrt(tvar1.ZnW)
  , thi = newdat.ZnW$RRZn+2*sqrt(tvar1.ZnW)
)

newdat.ZnW
#fert.x      RRZn       plo       phi       tlo      thi
#1    Low 0.9032192 0.8233926 0.9830458 0.7876675 1.018771
#2 Medium 0.8757970 0.7677842 0.9838099 0.7392441 1.012350

ZnW.output<-data.frame(species = "wheat",nutrient = "Zn", av = fixef(ZnWn)[1], ci =(2*sqrt(vcov(ZnWn)))[1],pi =(2*sqrt(vcov(ZnWn)+(VarCorr(ZnW2)$Loc[1])))[1])
#av         ci        pi
#(Intercept) 0.9020013 0.06985293 0.1089007

###Fe wheat####
FeW <- lmer(RRFe ~ fert.x + (fert.x|Cv.x)+(fert.x|TOS.x)+(fert.x|Loc.x)+(fert.x|Yr.x), data = wheatPMFy)
summary(FeW)
FeW1 <-lmer(RRFe ~ fert.x + (fert.x|Yr.x),data = wheatPMFy)
summary(FeW1)
anova(FeW,FeW1)
FeW2 <-lmer(RRFe ~ fert.x + (1|Yr.x),data = wheatPMFy)
anova(FeW1,FeW2)
FeWn <-lmer(RRFe~1+(1|Yr.x),data = wheatPMFy)
anova(FeWn,FeW2)
#ns
FeW.output<-data.frame(species = "wheat", nutrient = "Fe", av = fixef(FeWn)[1], ci =(2*sqrt(vcov(FeWn)))[1],pi =(2*sqrt(vcov(FeWn)+(VarCorr(FeW2)$Yr[1])))[1])
FeW.output

###Protein####
proteinW <- lmer(RRprotein ~ fert.x + (fert.x|Cv.x)+(fert.x|TOS.x)+(fert.x|Loc.x)+(fert.x|Yr.x), data = wheatPMFy)
summary(proteinW)
proteinW1 <-lmer(RRprotein ~ fert.x + (fert.x|Yr.x),data = wheatPMFy)
summary(proteinW1)
anova(proteinW,proteinW1)
proteinW2 <-lmer(RRprotein ~ fert.x + (1|Yr.x),data = wheatPMFy)
anova(proteinW1,proteinW2)
proteinWn <-lmer(RRprotein~1+(1|Yr.x),data = wheatPMFy)
anova(proteinWn,proteinW2)
proteinWhaaaaaa <-lmer(RRprotein~1+(1|Cv.x)+(1|TOS.x)+(1|Loc.x)+(1|Yr.x),wheatPMFy)
#I give up for the moment because the models I'm using aren't really sensible. I don't think it's the easiest or best way to estimate confidence intervals.
#and I don't know what sort of error structure to use. 


output.avgs<-data.frame(rbind(ZnW.output,FeW.output))


###Wheat averages with yield: aim is to find the intercept####
ZnYW <-lm(RRZn~RRYield,wheatPMFy)
> summary(ZnYW)


Call:
  lm(formula = RRZn ~ RRYield, data = wheatPMFy)

Residuals:
  Min       1Q   Median       3Q      Max 
-0.32842 -0.06310 -0.00159  0.07603  0.16446 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)  1.33873    0.05965  22.444  < 2e-16 ***
  RRYield     -0.32472    0.04465  -7.272 7.26e-10 ***
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

> FeYW <-lm(RRFe~RRYield,wheatPMFy)
> summary(FeYW)

Call:
  lm(formula = RRFe ~ RRYield, data = wheatPMFy)

Residuals:
  Min        1Q    Median        3Q       Max 
-0.102829 -0.036263  0.004577  0.038492  0.099640 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)  1.07615    0.02913  36.943  < 2e-16 ***
  RRYield     -0.09620    0.02181  -4.411 4.17e-05 ***
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

Residual standard error: 0.04903 on 62 degrees of freedom
Multiple R-squared: 0.2389,  Adjusted R-squared: 0.2266 
F-statistic: 19.46 on 1 and 62 DF,  p-value: 4.174e-05

Residual standard error: 0.1004 on 62 degrees of freedom
Multiple R-squared: 0.4603,  Adjusted R-squared: 0.4516 
F-statistic: 52.88 on 1 and 62 DF,  p-value: 7.263e-10

> PrYW <-lm(RRprotein~RRYield,wheatPMFy)
> summary(PrYW)

Call:
  lm(formula = RRprotein ~ RRYield, data = wheatPMFy)

Residuals:
  Min        1Q    Median        3Q       Max 
-0.086729 -0.025458  0.004234  0.024693  0.089620 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)  1.07147    0.02156  49.696  < 2e-16 ***
  RRYield     -0.10237    0.01614  -6.342 2.91e-08 ***
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

Residual standard error: 0.03629 on 62 degrees of freedom
Multiple R-squared: 0.3935,  Adjusted R-squared: 0.3837 
F-statistic: 40.23 on 1 and 62 DF,  p-value: 2.913e-08 

###Peas LM with yield####
ZnYP <-lm(RRZn~RRYield,peaPMFy)
summary(ZnYP)

lm(formula = RRZn ~ RRYield, data = peaPMFy)

Residuals:
  Min        1Q    Median        3Q       Max 
-0.077189 -0.020260  0.001686  0.033485  0.065496
Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)  1.09562    0.12793   8.564 0.000139 ***
  RRYield     -0.12291    0.09843  -1.249 0.258290    
---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

Residual standard error: 0.0506 on 6 degrees of freedom
Multiple R-squared: 0.2063,  Adjusted R-squared: 0.07398 
F-statistic: 1.559 on 1 and 6 DF,  p-value: 0.2583

FeYP <-lm(RRFe~RRYield,peaPMFy)
summary(FeYP)
NYP <-lm(RRN~RRYield,peaPMFy)
summary(NYP)
SYP<-lm(RRS~RRYield,peaPMFy)
BYP<-lm(RRB~RRYield,peaPMFy)
CaYP<-lm(RRCa~RRYield,peaPMFy)
MgYP<-lm(RRMg~RRYield,peaPMFy)
CuYP<-lm(RRCu~RRYield,peaPMFy)
MnYP<-lm(RRMn~RRYield,peaPMFy)
PYP<-lm(RRP~RRYield,peaPMFy)
KYP<-lm(RRK~RRYield,peaPMFy)

YPinterceptsmaybe <-data.frame(y=c("Fe","Zn","N","S","B","Ca","Mg","Cu","Mn","P","K"),
                               rbind(summary(FeYP)$coefficients[1,1:2],
                                     summary(ZnYP)$coefficients[1,1:2],
                                     summary(NYP)$coefficients[1,1:2],
                                     summary(SYP)$coefficients[1,1:2],
                                     summary(BYP)$coefficients[1,1:2],
                                     summary(CaYP)$coefficients[1,1:2],
                                     summary(MgYP)$coefficients[1,1:2],
                                     summary(CuYP)$coefficients[1,1:2],
                                     summary(MnYP)$coefficients[1,1:2],
                                     summary(PYP)$coefficients[1,1:2],
                                     summary(KYP)$coefficients[1,1:2]))

YPslopes <-data.frame(y=c("Fe","Zn","N","S","B","Ca","Mg","Cu","Mn","P","K"),
                      rbind(summary(FeYP)$coefficients[2,1:2],
                            summary(ZnYP)$coefficients[2,1:2],
                            summary(NYP)$coefficients[2,1:2],
                            summary(SYP)$coefficients[2,1:2],
                            summary(BYP)$coefficients[2,1:2],
                            summary(CaYP)$coefficients[2,1:2],
                            summary(MgYP)$coefficients[2,1:2],
                            summary(CuYP)$coefficients[2,1:2],
                            summary(MnYP)$coefficients[2,1:2],
                            summary(PYP)$coefficients[2,1:2],
                            summary(KYP)$coefficients[2,1:2]))
> YPinterceptsmaybe
y  Estimate Std..Error
1  Fe 1.1346734 0.10747992
2  Zn 1.0956221 0.12793107
3   N 1.0890077 0.07977656
4   S 1.0963525 0.04091172
5   B 0.9311570 0.09754592
6  Ca 0.8103678 0.11726025
7  Mg 1.0102682 0.05996066
8  Cu 1.0015030 0.11670573
9  Mn 1.0568509 0.07126260
10  P 1.1753237 0.12522959
11  K 1.1321782 0.04834603
                               
YPslopes
> YPslopes
y     Estimate Std..Error
1  Fe -0.138607792 0.08269402
2  Zn -0.122907394 0.09842894
3   N -0.082975560 0.06137932
4   S -0.093134033 0.03147709
5   B  0.036797331 0.07505090
6  Ca  0.131474421 0.09021892
7  Mg -0.004853156 0.04613316
8  Cu -0.019752764 0.08979227
9  Mn -0.059871429 0.05482877
10  P -0.166569311 0.09635045
11  K -0.090162845 0.03719698

#mean and intercept but no SE
rbind(coef(FeYP)[1,1:2],
      coef(ZnYP),
      coef(NYP),
      coef(SYP),
      coef(BYP),
      coef(CaYP),
      coef(MgYP),
      coef(CuYP),
      coef(MnYP),
      coef(PYP),
      coef(KYP)))

> YPinterceptsmaybe
y X.Intercept.      RRYield
1  Fe    1.1346734 -0.138607792
2  Zn    1.0956221 -0.122907394
3   N    1.0890077 -0.082975560
4   S    1.0963525 -0.093134033
5   B    0.9311570  0.036797331
6  Ca    0.8103678  0.131474421
7  Mg    1.0102682 -0.004853156
8  Cu    1.0015030 -0.019752764
9  Mn    1.0568509 -0.059871429
10  P    1.1753237 -0.166569311
11  K    1.1321782 -0.090162845

FeYSgh <-lm(RRFe~yield,sorghumPMFy)
NYSgh <-lm(RRN~yield,sorghumPMFy)
SYSgh<-lm(RRS~yield,sorghumPMFy)
BYSgh<-lm(RRB~yield,sorghumPMFy)
CaYSgh<-lm(RRCa~yield,sorghumPMFy)
MgYSgh<-lm(RRMg~yield,sorghumPMFy)
CuYSgh<-lm(RRCu~yield,sorghumPMFy)
MnYSgh<-lm(RRMn~yield,sorghumPMFy)
PYSgh<-lm(RRP~yield,sorghumPMFy)
KYSgh<-lm(RRK~yield,sorghumPMFy)
ZnYSgh<-lm(RRZn~yield,sorghumPMFy)

YSghinterceptsmaybe <-data.frame(y=c("Fe","Zn","N","S","B","Ca","Mg","Cu","Mn","P","K"),
                                 rbind(summary(FeYSgh)$coefficients[1,1:2],
                                       summary(ZnYSgh)$coefficients[1,1:2],
                                       summary(NYSgh)$coefficients[1,1:2],
                                       summary(SYSgh)$coefficients[1,1:2],
                                       summary(BYSgh)$coefficients[1,1:2],
                                       summary(CaYSgh)$coefficients[1,1:2],
                                       summary(MgYSgh)$coefficients[1,1:2],
                                       summary(CuYSgh)$coefficients[1,1:2],
                                       summary(MnYSgh)$coefficients[1,1:2],
                                       summary(PYSgh)$coefficients[1,1:2],
                                       summary(KYSgh)$coefficients[1,1:2]))

YSghslopes <-data.frame(y=c("Fe","Zn","N","S","B","Ca","Mg","Cu","Mn","P","K"),
                        rbind(summary(FeYSgh)$coefficients[2,1:2],
                              summary(ZnYSgh)$coefficients[2,1:2],
                              summary(NYSgh)$coefficients[2,1:2],
                              summary(SYSgh)$coefficients[2,1:2],
                              summary(BYSgh)$coefficients[2,1:2],
                              summary(CaYSgh)$coefficients[2,1:2],
                              summary(MgYSgh)$coefficients[2,1:2],
                              summary(CuYSgh)$coefficients[2,1:2],
                              summary(MnYSgh)$coefficients[2,1:2],
                              summary(PYSgh)$coefficients[2,1:2],
                              summary(KYSgh)$coefficients[2,1:2]))
YSghinterceptsmaybe
YSghslopes
# > YSghinterceptsmaybe
# y Estimate  Std..Error
# 1  Fe 1.065302 0.018262645
# 2  Zn 1.017992 0.018163264
# 3   N 1.028720 0.015594827
# 4   S 1.028708 0.004469187
# 5   B 1.038385 0.051795040
# 6  Ca 1.225562 0.029460863
# 7  Mg 1.031029 0.008575531
# 8  Cu 1.005499 0.022693582
# 9  Mn 1.050182 0.028759919
# 10  P 1.028265 0.013686076
# 11  K 1.056568 0.028116765
# > YSghslopes
# y     Estimate   Std..Error
# 1  Fe -0.004063225 0.0009234910
# 2  Zn -0.002553299 0.0009184655
# 3   N -0.002459336 0.0007885869
# 4   S -0.002767455 0.0002259943
# 5   B -0.003263176 0.0026191305
# 6  Ca -0.009352227 0.0014897536
# 7  Mg -0.002703551 0.0004336407
# 8  Cu -0.002383369 0.0011475511
# 9  Mn -0.002734348 0.0014543088
# 10  P -0.002182874 0.0006920666
# 11  K -0.002485755 0.0014217863

YWheatslopes <-data.frame(y=c("Fe","Zn","Protein"),
                        rbind(summary(FeYW)$coefficients[2,1:2],
                              summary(ZnYW)$coefficients[2,1:2],
                              summary(PrYW)$coefficients[2,1:2]
                              ))

YWheatint <-data.frame(y=c("Fe","Zn","Protein"),
                          rbind(summary(FeYW)$coefficients[1,1:2],
                                summary(ZnYW)$coefficients[1,1:2],
                                summary(PrYW)$coefficients[1,1:2]
                          ))
YWheatint
YWheatslopes

###rice LM Yield####
FeYrice <-lm(RRFe~yield,ricePMFy)
NYrice <-lm(RRN~yield,ricePMFy)
SYrice<-lm(RRS~yield,ricePMFy)
BYrice<-lm(RRB~yield,ricePMFy)
CaYrice<-lm(RRCa~yield,ricePMFy)
MgYrice<-lm(RRMg~yield,ricePMFy)
CuYrice<-lm(RRCu~yield,ricePMFy)
MnYrice<-lm(RRMn~yield,ricePMFy)
PYrice<-lm(RRP~yield,ricePMFy)
KYrice<-lm(RRK~yield,ricePMFy)
ZnYrice<-lm(RRZn~yield,ricePMFy)

Yriceinterceptsmaybe <-data.frame(y=c("Fe","Zn","N","S","B","Ca","Mg","Cu","Mn","P","K"),
                                  rbind(summary(FeYrice)$coefficients[1,],
                                        summary(ZnYrice)$coefficients[1,],
                                        summary(NYrice)$coefficients[1,],
                                        summary(SYrice)$coefficients[1,],
                                        summary(BYrice)$coefficients[1,],
                                        summary(CaYrice)$coefficients[1,],
                                        summary(MgYrice)$coefficients[1,],
                                        summary(CuYrice)$coefficients[1,],
                                        summary(MnYrice)$coefficients[1,],
                                        summary(PYrice)$coefficients[1,],
                                        summary(KYrice)$coefficients[1,]))

Yriceslopes <-data.frame(y=c("Fe","Zn","N","S","B","Ca","Mg","Cu","Mn","P","K"),
                         rbind(summary(FeYrice)$coefficients[2,],
                               summary(ZnYrice)$coefficients[2,],
                               summary(NYrice)$coefficients[2,],
                               summary(SYrice)$coefficients[2,],
                               summary(BYrice)$coefficients[2,],
                               summary(CaYrice)$coefficients[2,],
                               summary(MgYrice)$coefficients[2,],
                               summary(CuYrice)$coefficients[2,],
                               summary(MnYrice)$coefficients[2,],
                               summary(PYrice)$coefficients[2,],
                               summary(KYrice)$coefficients[2,]))
                  
Yriceinterceptsmaybe
# y  Estimate Std..Error
# 1  Fe 0.9759757 0.02996594
# 2  Zn 1.0178739 0.02066482
# 3   N 0.9576501 0.01696558
# 4   S 0.9386329 0.01254824
# 5   B 1.0517979 0.04817798
# 6  Ca 1.2298639 0.24867375
# 7  Mg 0.9845326 0.02432824
# 8  Cu 0.7706299 0.05524336
# 9  Mn 0.9671316 0.06517569
# 10  P 1.0007876 0.02321780
# 11  K 0.9960735 0.02105199
Yriceslopes
# y      Estimate   Std..Error
# 1  Fe -0.0019112975 0.0014151191
# 2  Zn -0.0039554692 0.0009758808
# 3   N -0.0019104963 0.0008011869
# 4   S -0.0011791086 0.0005925814
# 5   B -0.0013863732 0.0022751689
# 6  Ca -0.0035223896 0.0117434312
# 7  Mg -0.0006387942 0.0011488827
# 8  Cu  0.0058075494 0.0026088262
# 9  Mn -0.0048267221 0.0030778731
# 10  P -0.0016727714 0.0010964433
# 11  K -0.0002711924 0.0009941644



