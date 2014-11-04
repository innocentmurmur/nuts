FACEdata <- read.csv("~/Rdata/nutrients/Raw_nutrient_data.csv")

#FACEdata["CropCO2"] <- paste(FACEdata$Crop,FACEdata$CO2treatment)
#FACEdata$CropCO2 <- factor(FACEdata$CropCO2)
#NutMeans <- c(aggregate(cbind(Zn, Fe, N, P, K, S, B, Ca, Mg, Mn, Cu) ~ CropCO2, FUN=mean, data=FACEdata))
#NutSD <- c(aggregate(cbind(Zn, Fe, N, P, K, S, B, Ca, Mg, Mn, Cu) ~ CropCO2, FUN=sd, data=FACEdata))
#NutN <- c(aggregate(cbind(Zn, Fe, N, P, K, S, B, Ca, Mg, Mn, Cu) ~ CropCO2, FUN=length, data=FACEdata))
#namesNutN <- paste("Num",names(NutN))
#names(NutN)<-namesNutN
#NutSE <-data.frame(NutSD$CropCO2,NutSD[,2:12]/sqrt(NutN[,2:12]))
#namesNutSE <- paste("SE",names(NutSE))
#names(NutSE)<-namesNutSE
#write.csv(cbind(NutMeans,NutSE), file="~/Rdata/nutrients/NutMeans.csv")

#wheatFACEdata <-subset(FACEdata, Crop=="wheat")
#wheatFACEdata$CO2 <- factor(wheatFACEdata$CO2)
#wheatNutMeans <- data.frame(c(aggregate(cbind(Zn, Fe, protein) ~ CO2, FUN=mean, data=wheatFACEdata)))
#wheatNutSD <- data.frame(c(aggregate(cbind(Zn, Fe, protein) ~ CO2, FUN=sd, data=wheatFACEdata)))
#wheatNutN <- data.frame(c(aggregate(cbind(Zn, Fe, protein) ~ CO2, FUN=length, data=wheatFACEdata)))
#nameswheatNutN <- paste("Num",names(wheatNutN))
#names(wheatNutN)<-nameswheatNutN
#wheatNutSE <-data.frame(wheatNutSD$CO2,wheatNutSD[,2:4]/sqrt(wheatNutN[,2:4]))
#nameswheatNutSE <- paste("SE",names(wheatNutSE))
#names(wheatNutSE)<-nameswheatNutSE
#write.csv(cbind(wheatNutMeans,wheatNutSE), file="~/Rdata/nutrients/wheatNutMeans.csv")

#trying to make pairs of the data
#FACEdata["LocYrCrL1L2L3WNTC"] <- paste(FACEdata$Location,FACEdata$Year.of.study,FACEdata$Crop,FACEdata$Level1,FACEdata$Level2,FACEdata$Level3,FACEdata$WateringRegime,FACEdata$NapplicnQ,FACEdata$TOS,FACEdata$Cultivar)
#FACEdata["LocYrCrL1L2WNTC"] <- paste(FACEdata$Location,FACEdata$Year.of.study,FACEdata$Crop,FACEdata$Level1,FACEdata$Level2,FACEdata$WateringRegime,FACEdata$NapplicnQ,FACEdata$TOS,FACEdata$Cultivar)
#FACEdata["LocYrCrL1WNTC"] <- paste(FACEdata$Location,FACEdata$Year.of.study,FACEdata$Crop,FACEdata$Level1,FACEdata$WateringRegime,FACEdata$NapplicnQ,FACEdata$TOS,FACEdata$Cultivar)
#FACEdata["LocYrCrWNTC"] <- paste(FACEdata$Location,FACEdata$Year.of.study,FACEdata$Crop,FACEdata$WateringRegime,FACEdata$NapplicnQ,FACEdata$TOS,FACEdata$Cultivar)
FACEdata["LocYrCrWNTCCO2"]<- paste(FACEdata$Location,FACEdata$Year.of.study,FACEdata$Crop,FACEdata$WateringRegime,FACEdata$NapplicnQ,FACEdata$TOS,FACEdata$Cultivar,FACEdata$CO2treatment)


#ToPair <- matrix(c(FACEdata$LocYrCrWNTC,FACEdata$CO2treatment))
#write.csv(FACEdata, file="~/Rdata/nutrients/Raw_nutrient_data.csv")

#As it turns out, LocYrCrL1WNTC contains one value of A and one of E for each treatment plant combo



#now i'm splitting them into ambient and elevated
FACEdataE <- subset(FACEdata, CO2treatment=="eCO2")
FACEdataA <- subset(FACEdata, CO2treatment=="aCO2")
PairedFACEdata <- data.frame(merge(FACEdataA,FACEdataE,by="LocYrCrL1WNTC"))


#now I want to calculate individual response ratios
RRyield <-PairedFACEdata$Yield.y/PairedFACEdata$Yield.x
RRZn <-PairedFACEdata$Zn.y/PairedFACEdata$Zn.x
RRFe <-PairedFACEdata$Fe.y/PairedFACEdata$Fe.x
RRN <-PairedFACEdata$N.y/PairedFACEdata$N.x
RRP <-PairedFACEdata$P.y/PairedFACEdata$P.x
RRK <-PairedFACEdata$K.y/PairedFACEdata$K.x
RRS <-PairedFACEdata$S.y/PairedFACEdata$S.x
RRB <-PairedFACEdata$B.y/PairedFACEdata$B.x
RRCa <-PairedFACEdata$Ca.y/PairedFACEdata$Ca.x
RRMg<-PairedFACEdata$Mg.y/PairedFACEdata$Mg.x
RRMn<-PairedFACEdata$Mn.y/PairedFACEdata$Mn.x
RRCu <-PairedFACEdata$Cu.y/PairedFACEdata$Cu.x

PairedFACE <-subset(PairedFACEdata, select = c(PairedFACEdata$Study.x, 
                                               PairedFACEdata$Location.x, 
                                               PairedFACEdata$Year.of.study.x, 
                                               PairedFACEdata$Crop.x, 
                                               PairedFACEdata$WateringRegime.x, 
                                               PairedFACEdata$TotH2O.x, 
                                               PairedFACEdata$NapplicnQ.x, 
                                               PairedFACEdata$Napplicn.x, 
                                               PairedFACEdata$TOS.x, 
                                               PairedFACEdata$Cultivar.x, 
                                               PairedFACEdata$LocYrCrWNTC.x, 
                                               PairedFACEdata$LocYrCrL1WNTC))

#removing all the columns that end in .y
PairedFACE <- PairedFACEdata[ , -grep("\\.y$", names(PairedFACEdata)) ]
#selecting only the columns with factors in them
PairedFACE <- PairedFACE[,9:26]
#putting the response ratios on... and location because I missed it before
PairedFACE <- cbind(PairedFACE,PairedFACEdata$Location.x, RRB, RRCa, RRCu, RRFe, RRK, RRMg, RRMn, RRN, RRP, RRP, RRS, RRZn)
#renaming the location column
names(PairedFACE)[31] <- "Location"
names(PairedFACE)[1] <-"Year"
#getting rid of the .x on the end of the column names
names(PairedFACE) <- sub(".x$", "", names(PairedFACE))

write.csv(PairedFACE, file="~/Rdata/nutrients/NutRR.csv")

RRFACE <-cbind(RRB, RRCa, RRCu, RRFe, RRK, RRMg, RRMn, RRN, RRP, RRS, RRZn)

#OK now we're ready for principle components analysis
CorrMatrixRR <-round(cor(RRFACE, use = "na.or.complete"), 2)
CorrMatrixRR
#Or we could've done it on PairedFACE as CorrMatrixRR <-round(cor(PairedFACE[,19:30], use = "na.or.complete"), 2)
RRFACE_pca <- prcomp(na.omit(RRFACE), scale = TRUE)
print(RRFACE_pca)
summary(RRFACE_pca)
ax1 <- RRFACE_pca$rotation[,1]
ax1


predict(mRRFACE_pca)[,1]
####same thing on means####
#get means for every combination across levels
MeanByLocYrCrWNTCCO2y <- data.frame(aggregate(cbind(Yield,N,P,S,K,Mg,Mn,Zn,Fe,Cu,Ca,B, protein) ~ LocYrCrWNTCCO2, FUN=mean, data=FACEdata,na.action=NULL))

#Making a column that's made out of the last few characters of the other column
MeanByLocYrCrWNTCCO2y["co2"] <-substring (MeanByLocYrCrWNTCCO2y$LocYrCrWNTCCO2, nchar(MeanByLocYrCrWNTCCO2y$LocYrCrWNTCCO2) - 4, nchar(MeanByLocYrCrWNTCCO2y$LocYrCrWNTCCO2))

#copying the column
MeanByLocYrCrWNTCCO2y["LocYrCrWNTC"] <- MeanByLocYrCrWNTCCO2y["LocYrCrWNTCCO2"]

#taking off the end of it
MeanByLocYrCrWNTCCO2y$LocYrCrWNTC <- gsub("....$", "", MeanByLocYrCrWNTCCO2y$LocYrCrWNTC)


#now i'm splitting them into ambient and elevated
MeanByLocYrCrWNTCCO2ye <- subset(MeanByLocYrCrWNTCCO2y, co2==" eCO2")
MeanByLocYrCrWNTCCO2ya <- subset(MeanByLocYrCrWNTCCO2y, co2==" aCO2")
PairedMeanFACEdatay <- data.frame(merge(MeanByLocYrCrWNTCCO2ya,MeanByLocYrCrWNTCCO2ye,by="LocYrCrWNTC"))

#now I want to calculate individual response ratios
mRRYield <-PairedMeanFACEdatay$Yield.y/PairedMeanFACEdatay$Yield.x
mRRZn <-PairedMeanFACEdatay$Zn.y/PairedMeanFACEdatay$Zn.x
mRRFe <-PairedMeanFACEdatay$Fe.y/PairedMeanFACEdatay$Fe.x
mRRprotein <-PairedMeanFACEdatay$protein.y/PairedMeanFACEdatay$protein.x
mRRN <-PairedMeanFACEdatay$N.y/PairedMeanFACEdatay$N.x
mRRP <-PairedMeanFACEdatay$P.y/PairedMeanFACEdatay$P.x
mRRK <-PairedMeanFACEdatay$K.y/PairedMeanFACEdatay$K.x
mRRS <-PairedMeanFACEdatay$S.y/PairedMeanFACEdatay$S.x
mRRB <-PairedMeanFACEdatay$B.y/PairedMeanFACEdatay$B.x
mRRCa <-PairedMeanFACEdatay$Ca.y/PairedMeanFACEdatay$Ca.x
mRRMg<-PairedMeanFACEdatay$Mg.y/PairedMeanFACEdatay$Mg.x
mRRMn<-PairedMeanFACEdatay$Mn.y/PairedMeanFACEdatay$Mn.x
mRRCu <-PairedMeanFACEdatay$Cu.y/PairedMeanFACEdatay$Cu.x

#putting the response ratios on
PairedMeanFACEy <- data.frame(cbind(PairedMeanFACEdatay$LocYrCrWNTC, 
                                       mRRYield,
                                       mRRFe, 
                                       mRRprotein, 
                                       mRRZn,
                                       mRRB, 
                                       mRRCa, 
                                       mRRCu, 
                                       mRRK, 
                                       mRRMg, 
                                       mRRMn, 
                                       mRRN, 
                                       mRRP, 
                                       mRRS))

PairedMeanFACEy.Labels <- matrix(unlist(strsplit(PairedMeanFACEdatay$LocYrCrWNTC," ")),ncol = 7, byrow = TRUE)
PairedMeanFACEy.Labels <-data.frame(PairedMeanFACEy.Labels)
names(PairedMeanFACEy.Labels)<-c("Location","Year","Crop","Watering","Fert","TOS","Cv")
PairedMeanFACEy<-cbind(PairedMeanFACEy,PairedMeanFACEy.Labels)

#extracting all the plants that haven't had wacky CO2-response-altering treatments... except the warming treatment in rice yet. It's probably a bit late for that.
controlFACEf <-subset(PairedMeanFACEy,Fert %in% c("Medium","High"))
legumesFACE<-subset(PairedMeanFACEy,Crop %in% c("soybean","Field_peas"))
controlFACEfl <-rbind(controlFACEf,legumesFACE)
controlFACEflw <-subset(controlFACEfl,Watering=="Wet")
soyfaceFACE<-subset(controlFACEfl,Location=="Champaign__Illinois")
controlFACE <-rbind(controlFACEflw,soyfaceFACE)


write.csv(controlFACE, file="~/Rdata/nutrients/controlFACE.csv")
controlFACE <- read.csv("~/Rdata/nutrients/controlFACE.csv")
#I have no idea why the below averages and SDs are so wrong. They're in the 20s rather than 1.something. ControlFACE is alright. 
controlFACEav <- data.frame(aggregate(cbind(mRRYield,mRRZn, mRRprotein,mRRFe, mRRN, mRRP, mRRK, mRRS, mRRB, mRRCa, mRRMg, mRRMn, mRRCu) ~ Crop, FUN=mean, data=controlFACE, na.action=NULL))
controlFACElg <- data.frame(aggregate(cbind(mRRYield,mRRZn,mRRprotein, mRRFe, mRRN, mRRP, mRRK, mRRS, mRRB, mRRCa, mRRMg, mRRMn, mRRCu) ~ Crop, FUN=length, data=controlFACE, na.action=NULL))
controlFACEsd <- data.frame(aggregate(cbind(mRRYield,mRRZn,mRRprotein, mRRFe, mRRN, mRRP, mRRK, mRRS, mRRB, mRRCa, mRRMg, mRRMn, mRRCu) ~ Crop, FUN=sd, data=controlFACE, na.action=NULL))
controlFACEse <-data.frame(controlFACEav$Crop,controlFACEsd[,2:14]/sqrt(controlFACElg[,2:14]))

controlFACEci<-data.frame(
  Crop=c("wheat","Sorghum","Rice","Field_peas","Corn","soybean"),
  yieldlow=c(
    (t.test(subset(controlFACE,Crop=="wheat")$mRRYield,alternative="two.sided", na.action=omit)$conf.int[1]),
    NA,
    NA,
    (t.test(subset(controlFACE,Crop=="Field_peas")$mRRYield,alternative="two.sided", na.action=omit)$conf.int[1]),
    NA,
    NA
  ),
  yieldhi=c(
    (t.test(subset(controlFACE,Crop=="wheat")$mRRYield,alternative="two.sided", na.action=omit)$conf.int[2]),
    NA,
    NA,
    (t.test(subset(controlFACE,Crop=="Field_peas")$mRRYield,alternative="two.sided", na.action=omit)$conf.int[2]),
    NA,
    NA
  ),
  Znlow=c(
    (t.test(subset(controlFACE,Crop=="wheat")$mRRZn,alternative="two.sided", na.action=omit)$conf.int[1]),
    (t.test(subset(controlFACE,Crop=="Sorghum")$mRRZn,alternative="two.sided", na.action=omit)$conf.int[1]),
    (t.test(subset(controlFACE,Crop=="Rice")$mRRZn,alternative="two.sided", na.action=omit)$conf.int[1]),
    (t.test(subset(controlFACE,Crop=="Field_peas")$mRRZn,alternative="two.sided", na.action=omit)$conf.int[1]),
    (t.test(subset(controlFACE,Crop=="Corn")$mRRZn,alternative="two.sided", na.action=omit)$conf.int[1]),
    (t.test(subset(controlFACE,Crop=="soybean")$mRRZn,alternative="two.sided", na.action=omit)$conf.int[1])
  ),
  Znhi=c(
    (t.test(subset(controlFACE,Crop=="wheat")$mRRZn,alternative="two.sided", na.action=omit)$conf.int[2]),
    (t.test(subset(controlFACE,Crop=="Sorghum")$mRRZn,alternative="two.sided", na.action=omit)$conf.int[2]),
    (t.test(subset(controlFACE,Crop=="Rice")$mRRZn,alternative="two.sided", na.action=omit)$conf.int[2]),
    (t.test(subset(controlFACE,Crop=="Field_peas")$mRRZn,alternative="two.sided", na.action=omit)$conf.int[2]),
    (t.test(subset(controlFACE,Crop=="Corn")$mRRZn,alternative="two.sided", na.action=omit)$conf.int[2]),
    (t.test(subset(controlFACE,Crop=="soybean")$mRRZn,alternative="two.sided", na.action=omit)$conf.int[2])
  ),
  Protlow=c(
    (t.test(subset(controlFACE,Crop=="wheat")$mRRprotein,alternative="two.sided", na.action=omit)$conf.int[1]),
    NA,
    NA,
    (t.test(subset(controlFACE,Crop=="Field_peas")$mRRprotein,alternative="two.sided", na.action=omit)$conf.int[1]),
    NA,
    NA
  ),
  Prothi=c(
    (t.test(subset(controlFACE,Crop=="wheat")$mRRprotein,alternative="two.sided", na.action=omit)$conf.int[2]),
    NA,
    NA,
    (t.test(subset(controlFACE,Crop=="Field_peas")$mRRprotein,alternative="two.sided", na.action=omit)$conf.int[2]),
    NA,
    NA
    
  ),
  Felow=c(
    (t.test(subset(controlFACE,Crop=="wheat")$mRRFe,alternative="two.sided", na.action=omit)$conf.int[1]),
    (t.test(subset(controlFACE,Crop=="Sorghum")$mRRFe,alternative="two.sided", na.action=omit)$conf.int[1]),
    (t.test(subset(controlFACE,Crop=="Rice")$mRRFe,alternative="two.sided", na.action=omit)$conf.int[1]),
    (t.test(subset(controlFACE,Crop=="Field_peas")$mRRFe,alternative="two.sided", na.action=omit)$conf.int[1]),
    (t.test(subset(controlFACE,Crop=="Corn")$mRRFe,alternative="two.sided", na.action=omit)$conf.int[1]),
    (t.test(subset(controlFACE,Crop=="soybean")$mRRFe,alternative="two.sided", na.action=omit)$conf.int[1])
  ),
  Fehi=c(
    (t.test(subset(controlFACE,Crop=="wheat")$mRRFe,alternative="two.sided", na.action=omit)$conf.int[2]),
    (t.test(subset(controlFACE,Crop=="Sorghum")$mRRFe,alternative="two.sided", na.action=omit)$conf.int[2]),
    (t.test(subset(controlFACE,Crop=="Rice")$mRRFe,alternative="two.sided", na.action=omit)$conf.int[2]),
    (t.test(subset(controlFACE,Crop=="Field_peas")$mRRFe,alternative="two.sided", na.action=omit)$conf.int[2]),
    (t.test(subset(controlFACE,Crop=="Corn")$mRRFe,alternative="two.sided", na.action=omit)$conf.int[2]),
    (t.test(subset(controlFACE,Crop=="soybean")$mRRFe,alternative="two.sided", na.action=omit)$conf.int[2])
    
  ),
  Nlow=c(
    NA,
    (t.test(subset(controlFACE,Crop=="Sorghum")$mRRN,alternative="two.sided", na.action=omit)$conf.int[1]),
    (t.test(subset(controlFACE,Crop=="Rice")$mRRN,alternative="two.sided", na.action=omit)$conf.int[1]),
    (t.test(subset(controlFACE,Crop=="Field_peas")$mRRN,alternative="two.sided", na.action=omit)$conf.int[1]),
    (t.test(subset(controlFACE,Crop=="Corn")$mRRN,alternative="two.sided", na.action=omit)$conf.int[1]),
    (t.test(subset(controlFACE,Crop=="soybean")$mRRN,alternative="two.sided", na.action=omit)$conf.int[1])
  ),
  Nhi=c(
    NA,
    (t.test(subset(controlFACE,Crop=="Sorghum")$mRRN,alternative="two.sided", na.action=omit)$conf.int[2]),
    (t.test(subset(controlFACE,Crop=="Rice")$mRRN,alternative="two.sided", na.action=omit)$conf.int[2]),
    (t.test(subset(controlFACE,Crop=="Field_peas")$mRRN,alternative="two.sided", na.action=omit)$conf.int[2]),
    (t.test(subset(controlFACE,Crop=="Corn")$mRRN,alternative="two.sided", na.action=omit)$conf.int[2]),
    (t.test(subset(controlFACE,Crop=="soybean")$mRRN,alternative="two.sided", na.action=omit)$conf.int[2])
    
  ),
  Plow=c(
    NA,
    (t.test(subset(controlFACE,Crop=="Sorghum")$mRRP,alternative="two.sided", na.action=omit)$conf.int[1]),
    (t.test(subset(controlFACE,Crop=="Rice")$mRRP,alternative="two.sided", na.action=omit)$conf.int[1]),
    (t.test(subset(controlFACE,Crop=="Field_peas")$mRRP,alternative="two.sided", na.action=omit)$conf.int[1]),
    (t.test(subset(controlFACE,Crop=="Corn")$mRRP,alternative="two.sided", na.action=omit)$conf.int[1]),
    (t.test(subset(controlFACE,Crop=="soybean")$mRRP,alternative="two.sided", na.action=omit)$conf.int[1])
  ),
  Phi=c(
    NA,
    (t.test(subset(controlFACE,Crop=="Sorghum")$mRRP,alternative="two.sided", na.action=omit)$conf.int[2]),
    (t.test(subset(controlFACE,Crop=="Rice")$mRRP,alternative="two.sided", na.action=omit)$conf.int[2]),
    (t.test(subset(controlFACE,Crop=="Field_peas")$mRRP,alternative="two.sided", na.action=omit)$conf.int[2]),
    (t.test(subset(controlFACE,Crop=="Corn")$mRRP,alternative="two.sided", na.action=omit)$conf.int[2]),
    (t.test(subset(controlFACE,Crop=="soybean")$mRRP,alternative="two.sided", na.action=omit)$conf.int[2])
  ),
  Klow=c(
    NA,
    (t.test(subset(controlFACE,Crop=="Sorghum")$mRRK,alternative="two.sided", na.action=omit)$conf.int[1]),
    (t.test(subset(controlFACE,Crop=="Rice")$mRRK,alternative="two.sided", na.action=omit)$conf.int[1]),
    (t.test(subset(controlFACE,Crop=="Field_peas")$mRRK,alternative="two.sided", na.action=omit)$conf.int[1]),
    (t.test(subset(controlFACE,Crop=="Corn")$mRRK,alternative="two.sided", na.action=omit)$conf.int[1]),
    (t.test(subset(controlFACE,Crop=="soybean")$mRRK,alternative="two.sided", na.action=omit)$conf.int[1])
  ),
  Khi=c(
    NA,
    (t.test(subset(controlFACE,Crop=="Sorghum")$mRRK,alternative="two.sided", na.action=omit)$conf.int[2]),
    (t.test(subset(controlFACE,Crop=="Rice")$mRRK,alternative="two.sided", na.action=omit)$conf.int[2]),
    (t.test(subset(controlFACE,Crop=="Field_peas")$mRRK,alternative="two.sided", na.action=omit)$conf.int[2]),
    (t.test(subset(controlFACE,Crop=="Corn")$mRRK,alternative="two.sided", na.action=omit)$conf.int[2]),
    (t.test(subset(controlFACE,Crop=="soybean")$mRRK,alternative="two.sided", na.action=omit)$conf.int[2])
  ),
  Slow=c(
    NA,
    (t.test(subset(controlFACE,Crop=="Sorghum")$mRRS,alternative="two.sided", na.action=omit)$conf.int[1]),
    (t.test(subset(controlFACE,Crop=="Rice")$mRRS,alternative="two.sided", na.action=omit)$conf.int[1]),
    (t.test(subset(controlFACE,Crop=="Field_peas")$mRRS,alternative="two.sided", na.action=omit)$conf.int[1]),
    (t.test(subset(controlFACE,Crop=="Corn")$mRRS,alternative="two.sided", na.action=omit)$conf.int[1]),
    (t.test(subset(controlFACE,Crop=="soybean")$mRRS,alternative="two.sided", na.action=omit)$conf.int[1])
  ),
  Shi=c(
    NA,
    (t.test(subset(controlFACE,Crop=="Sorghum")$mRRS,alternative="two.sided", na.action=omit)$conf.int[2]),
    (t.test(subset(controlFACE,Crop=="Rice")$mRRS,alternative="two.sided", na.action=omit)$conf.int[2]),
    (t.test(subset(controlFACE,Crop=="Field_peas")$mRRS,alternative="two.sided", na.action=omit)$conf.int[2]),
    (t.test(subset(controlFACE,Crop=="Corn")$mRRS,alternative="two.sided", na.action=omit)$conf.int[2]),
    (t.test(subset(controlFACE,Crop=="soybean")$mRRS,alternative="two.sided", na.action=omit)$conf.int[2])
  ),
  Blow=c(
    NA,
    (t.test(subset(controlFACE,Crop=="Sorghum")$mRRB,alternative="two.sided", na.action=omit)$conf.int[1]),
    (t.test(subset(controlFACE,Crop=="Rice")$mRRB,alternative="two.sided", na.action=omit)$conf.int[1]),
    (t.test(subset(controlFACE,Crop=="Field_peas")$mRRB,alternative="two.sided", na.action=omit)$conf.int[1]),
    (t.test(subset(controlFACE,Crop=="Corn")$mRRB,alternative="two.sided", na.action=omit)$conf.int[1]),
    (t.test(subset(controlFACE,Crop=="soybean")$mRRB,alternative="two.sided", na.action=omit)$conf.int[1])
  ),
  Bhi=c(
    NA,
    (t.test(subset(controlFACE,Crop=="Sorghum")$mRRB,alternative="two.sided", na.action=omit)$conf.int[2]),
    (t.test(subset(controlFACE,Crop=="Rice")$mRRB,alternative="two.sided", na.action=omit)$conf.int[2]),
    (t.test(subset(controlFACE,Crop=="Field_peas")$mRRB,alternative="two.sided", na.action=omit)$conf.int[2]),
    (t.test(subset(controlFACE,Crop=="Corn")$mRRB,alternative="two.sided", na.action=omit)$conf.int[2]),
    (t.test(subset(controlFACE,Crop=="soybean")$mRRB,alternative="two.sided", na.action=omit)$conf.int[2])
  ),
  Calow=c(
    NA,
    (t.test(subset(controlFACE,Crop=="Sorghum")$mRRCa,alternative="two.sided", na.action=omit)$conf.int[1]),
    (t.test(subset(controlFACE,Crop=="Rice")$mRRCa,alternative="two.sided", na.action=omit)$conf.int[1]),
    (t.test(subset(controlFACE,Crop=="Field_peas")$mRRCa,alternative="two.sided", na.action=omit)$conf.int[1]),
    (t.test(subset(controlFACE,Crop=="Corn")$mRRCa,alternative="two.sided", na.action=omit)$conf.int[1]),
    (t.test(subset(controlFACE,Crop=="soybean")$mRRCa,alternative="two.sided", na.action=omit)$conf.int[1])
  ),
  Cahi=c(
    NA,
    (t.test(subset(controlFACE,Crop=="Sorghum")$mRRCa,alternative="two.sided", na.action=omit)$conf.int[2]),
    (t.test(subset(controlFACE,Crop=="Rice")$mRRCa,alternative="two.sided", na.action=omit)$conf.int[2]),
    (t.test(subset(controlFACE,Crop=="Field_peas")$mRRCa,alternative="two.sided", na.action=omit)$conf.int[2]),
    (t.test(subset(controlFACE,Crop=="Corn")$mRRCa,alternative="two.sided", na.action=omit)$conf.int[2]),
    (t.test(subset(controlFACE,Crop=="soybean")$mRRCa,alternative="two.sided", na.action=omit)$conf.int[2])
  ),
  Mglow=c(
    NA,
    (t.test(subset(controlFACE,Crop=="Sorghum")$mRRMg,alternative="two.sided", na.action=omit)$conf.int[1]),
    (t.test(subset(controlFACE,Crop=="Rice")$mRRMg,alternative="two.sided", na.action=omit)$conf.int[1]),
    (t.test(subset(controlFACE,Crop=="Field_peas")$mRRMg,alternative="two.sided", na.action=omit)$conf.int[1]),
    (t.test(subset(controlFACE,Crop=="Corn")$mRRMg,alternative="two.sided", na.action=omit)$conf.int[1]),
    (t.test(subset(controlFACE,Crop=="soybean")$mRRMg,alternative="two.sided", na.action=omit)$conf.int[1])
  ),
  Mghi=c(
    NA,
    (t.test(subset(controlFACE,Crop=="Sorghum")$mRRMg,alternative="two.sided", na.action=omit)$conf.int[2]),
    (t.test(subset(controlFACE,Crop=="Rice")$mRRMg,alternative="two.sided", na.action=omit)$conf.int[2]),
    (t.test(subset(controlFACE,Crop=="Field_peas")$mRRMg,alternative="two.sided", na.action=omit)$conf.int[2]),
    (t.test(subset(controlFACE,Crop=="Corn")$mRRMg,alternative="two.sided", na.action=omit)$conf.int[2]),
    (t.test(subset(controlFACE,Crop=="soybean")$mRRMg,alternative="two.sided", na.action=omit)$conf.int[2])
  ),
  Mnlow=c(
    NA,
    (t.test(subset(controlFACE,Crop=="Sorghum")$mRRMn,alternative="two.sided", na.action=omit)$conf.int[1]),
    (t.test(subset(controlFACE,Crop=="Rice")$mRRMn,alternative="two.sided", na.action=omit)$conf.int[1]),
    (t.test(subset(controlFACE,Crop=="Field_peas")$mRRMn,alternative="two.sided", na.action=omit)$conf.int[1]),
    (t.test(subset(controlFACE,Crop=="Corn")$mRRMn,alternative="two.sided", na.action=omit)$conf.int[1]),
    (t.test(subset(controlFACE,Crop=="soybean")$mRRMn,alternative="two.sided", na.action=omit)$conf.int[1])
  ),
  Mnhi=c(
    NA,
    (t.test(subset(controlFACE,Crop=="Sorghum")$mRRMn,alternative="two.sided", na.action=omit)$conf.int[2]),
    (t.test(subset(controlFACE,Crop=="Rice")$mRRMn,alternative="two.sided", na.action=omit)$conf.int[2]),
    (t.test(subset(controlFACE,Crop=="Field_peas")$mRRMn,alternative="two.sided", na.action=omit)$conf.int[2]),
    (t.test(subset(controlFACE,Crop=="Corn")$mRRMn,alternative="two.sided", na.action=omit)$conf.int[2]),
    (t.test(subset(controlFACE,Crop=="soybean")$mRRMn,alternative="two.sided", na.action=omit)$conf.int[2])
  ),
  Culow=c(
    NA,
    (t.test(subset(controlFACE,Crop=="Sorghum")$mRRCu,alternative="two.sided", na.action=omit)$conf.int[1]),
    (t.test(subset(controlFACE,Crop=="Rice")$mRRCu,alternative="two.sided", na.action=omit)$conf.int[1]),
    (t.test(subset(controlFACE,Crop=="Field_peas")$mRRCu,alternative="two.sided", na.action=omit)$conf.int[1]),
    (t.test(subset(controlFACE,Crop=="Corn")$mRRCu,alternative="two.sided", na.action=omit)$conf.int[1]),
    (t.test(subset(controlFACE,Crop=="soybean")$mRRCu,alternative="two.sided", na.action=omit)$conf.int[1])
  ),
  Cuhi=c(
    NA,
    (t.test(subset(controlFACE,Crop=="Sorghum")$mRRCu,alternative="two.sided", na.action=omit)$conf.int[2]),
    (t.test(subset(controlFACE,Crop=="Rice")$mRRCu,alternative="two.sided", na.action=omit)$conf.int[2]),
    (t.test(subset(controlFACE,Crop=="Field_peas")$mRRCu,alternative="two.sided", na.action=omit)$conf.int[2]),
    (t.test(subset(controlFACE,Crop=="Corn")$mRRCu,alternative="two.sided", na.action=omit)$conf.int[2]),
    (t.test(subset(controlFACE,Crop=="soybean")$mRRCu,alternative="two.sided", na.action=omit)$conf.int[2])
  ))


namescontrolFACEse <- paste("SE",names(controlFACEse))
names(controlFACEse)<-namescontrolFACEse
controlFACEsummary <- cbind(controlFACEav,controlFACEse)
controlFACEsummary$Crop<-as.factor(controlFACEsummary$Crop)
controlFACEci$Crop<-as.factor(controlFACEci$Crop)
controlFACEsummary <-merge(controlFACEsummary,controlFACEci,by="Crop")

#((((separate thought experiment))) this is for comparing whether grains with high nutrients have higher response ratios
PMFy <- cbind(PairedMeanFACEy,PairedMeanFACEdatay)
write.csv(PMFy, file="~/Rdata/nutrients/PMFy.csv")


#renaming the forst column
#not working: 
#names(PairedMeanFACE[,1]) <-"LocYrCrWNTC"
#LocYrCrWNTC <- c(PairedMeanFACE$LocYrCrWNTC)
#split the column by spaces into a data frame... which is at the moment the wrong direction
#split_text <- as.data.frame(strsplit(PairedMeanFACE$LocYrCrWNTC," "))
#Descrcols <- as.data.frame(t(split_text))
#names(Descrcols) <-c("location","year","crop","water","fertiliser","TOS","cultivar")
#PairedMeanFACEcol <-cbind(Descrcols, PairedMeanFACE)
write.csv(PairedMeanFACE, file="~/Rdata/nutrients/MeanNutRRwheat.csv")


mRRFACE <-as.data.frame(cbind(mRRB, mRRCa, mRRCu, mRRFe, mRRK, mRRMg, mRRMn, mRRN, mRRP, mRRS, mRRZn))
mRRFACE["desc"]<-PairedMeanFACE[,1]
####OK now we're ready for principle components analysis####
desc <- which(colnames(mRRFACE) == "desc")
CorrMatrixmRR <-round(cor(mRRFACE[,-desc], use = "na.or.complete"), 2)

#Or we could've done it on PairedMeanFACE as 
#CorrMatrixmRR <-round(cor(PairedMeanFACE[,2:13], use = "na.or.complete"),2)
#but I can't seem to get it to work with other columns of text in the doc
mRRFACE_pca <- prcomp(mRRFACE[,-1], scale = TRUE)
print(mRRFACE_pca)
summary(mRRFACE_pca)
a1 <- mRRFACE_pca$rotation[,1]
a1
a2 <- mRRFACE_pca$rotation[,2]
a2
center <- mRRFACE_pca$center
scale <- mRRFACE_pca$scale

fm <- as.matrix(mRRFACE[,-desc])

predict(mRRFACE_pca)[,1]

####OH well, let's try a cluster analysis####
#I'll use PairedMeanFACEcol
PairedMeanFACEcol <-na.omit(PairedMeanFACEcol)
mRRFACEn <- na.omit(mRRFACE)
mRRFACEn <-scale(mRRFACEn[,1:11])
# Model Based Clustering
library(mclust)
fit <- Mclust(na.omit(mRRFACEn))
plot(fit) # plot results 
summary(fit) # display the best model

#or... a simpler version... these are coming from here http://www.statmethods.net/advstats/cluster.html
wss <- (nrow(mRRFACEn)-1)*sum(apply(mRRFACEn,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mRRFACEn,centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
fit <- kmeans(mRRFACEn, 5)
aggregate(mRRFACEn,by=list(fit$cluster),FUN=mean)
mRRFACEn <- data.frame(mRRFACEn, fit$cluster, mRRFACE$desc)
#I have no idea how to interpret that.
