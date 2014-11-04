FACEdata <- read.csv("~/Rdata/nutrients/Raw_nutrient_data.csv")
FACEdata["CropCO2"] <- paste(FACEdata$Crop,FACEdata$CO2treatment)
FACEdata$CropCO2 <- factor(FACEdata$CropCO2)
NutMeans <- c(aggregate(cbind(Zn, Fe, N, P, K, S, B, Ca, Mg, Mn, Cu) ~ CropCO2, FUN=mean, data=FACEdata))
NutSD <- c(aggregate(cbind(Zn, Fe, N, P, K, S, B, Ca, Mg, Mn, Cu) ~ CropCO2, FUN=sd, data=FACEdata))
NutN <- c(aggregate(cbind(Zn, Fe, N, P, K, S, B, Ca, Mg, Mn, Cu) ~ CropCO2, FUN=length, data=FACEdata))
namesNutN <- paste("Num",names(NutN))
names(NutN)<-namesNutN
NutSE <-data.frame(NutSD$CropCO2,NutSD[,2:12]/sqrt(NutN[,2:12]))
namesNutSE <- paste("SE",names(NutSE))
names(NutSE)<-namesNutSE
write.csv(cbind(NutMeans,NutSE), file="~/Rdata/nutrients/NutMeans.csv")

wheatFACEdata <-subset(FACEdata, Crop=="wheat")
wheatFACEdata$CO2 <- factor(wheatFACEdata$CO2)
wheatNutMeans <- data.frame(c(aggregate(cbind(Zn, Fe, protein) ~ CO2, FUN=mean, data=wheatFACEdata)))
wheatNutSD <- data.frame(c(aggregate(cbind(Zn, Fe, protein) ~ CO2, FUN=sd, data=wheatFACEdata)))
wheatNutN <- data.frame(c(aggregate(cbind(Zn, Fe, protein) ~ CO2, FUN=length, data=wheatFACEdata)))
nameswheatNutN <- paste("Num",names(wheatNutN))
names(wheatNutN)<-nameswheatNutN
wheatNutSE <-data.frame(wheatNutSD$CO2,wheatNutSD[,2:4]/sqrt(wheatNutN[,2:4]))
nameswheatNutSE <- paste("SE",names(wheatNutSE))
names(wheatNutSE)<-nameswheatNutSE
write.csv(cbind(wheatNutMeans,wheatNutSE), file="~/Rdata/nutrients/wheatNutMeans.csv")


#trying to make pairs of the data
FACEdata["LocYrCrL1L2L3WNTC"] <- paste(FACEdata$Location,FACEdata$Year.of.study,FACEdata$Crop,FACEdata$Level1,FACEdata$Level2,FACEdata$Level3,FACEdata$WateringRegime,FACEdata$NapplicnQ,FACEdata$TOS,FACEdata$Cultivar)
FACEdata["LocYrCrL1L2WNTC"] <- paste(FACEdata$Location,FACEdata$Year.of.study,FACEdata$Crop,FACEdata$Level1,FACEdata$Level2,FACEdata$WateringRegime,FACEdata$NapplicnQ,FACEdata$TOS,FACEdata$Cultivar)
FACEdata["LocYrCrL1WNTC"] <- paste(FACEdata$Location,FACEdata$Year.of.study,FACEdata$Crop,FACEdata$Level1,FACEdata$WateringRegime,FACEdata$NapplicnQ,FACEdata$TOS,FACEdata$Cultivar)
FACEdata["LocYrCrWNTC"] <- paste(FACEdata$Location,FACEdata$Year.of.study,FACEdata$Crop,FACEdata$WateringRegime,FACEdata$NapplicnQ,FACEdata$TOS,FACEdata$Cultivar)
FACEdata["LocYrCrWNTCCO2"]<- paste(FACEdata$Location,FACEdata$Year.of.study,FACEdata$Crop,FACEdata$WateringRegime,FACEdata$NapplicnQ,FACEdata$TOS,FACEdata$Cultivar,FACEdata$CO2treatment)

ToPair <- matrix(c(FACEdata$LocYrCrWNTC,FACEdata$CO2treatment))
write.csv(FACEdata, file="~/Rdata/nutrients/Raw_nutrient_data.csv")

#As it turns out, LocYrCrL1WNTC contains one value of A and one of E for each treatment plant combo



#now i'm splitting them into ambient and elevated
FACEdataE <- subset(FACEdata, CO2treatment=="eCO2")
FACEdataA <- subset(FACEdata, CO2treatment=="aCO2")
PairedFACEdata <- data.frame(merge(FACEdataA,FACEdataE,by="LocYrCrL1WNTC"))

#now I want to calculate individual response ratios
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
MeanByLocYrCrWNTCCO2 <- data.frame(aggregate(cbind(Zn, Fe, N, P, K, S, B, Ca, Mg, Mn, Cu) ~ LocYrCrWNTCCO2, FUN=mean, data=FACEdata))
#Making a column that's made out of the last few characters of the other column
MeanByLocYrCrWNTCCO2["co2"] <-substring (MeanByLocYrCrWNTCCO2$LocYrCrWNTCCO2, nchar(MeanByLocYrCrWNTCCO2$LocYrCrWNTCCO2) - 4, nchar(MeanByLocYrCrWNTCCO2$LocYrCrWNTCCO2))
#copying the column
MeanByLocYrCrWNTCCO2["LocYrCrWNTC"] <- MeanByLocYrCrWNTCCO2["LocYrCrWNTCCO2"]
#taking off the end of it
MeanByLocYrCrWNTCCO2$LocYrCrWNTC <- gsub("....$", "", MeanByLocYrCrWNTCCO2$LocYrCrWNTC)


#now i'm splitting them into ambient and elevated
MeanByLocYrCrWNTCCO2e <- subset(MeanByLocYrCrWNTCCO2, co2==" eCO2")
MeanByLocYrCrWNTCCO2a <- subset(MeanByLocYrCrWNTCCO2, co2==" aCO2")
PairedMeanFACEdata <- data.frame(merge(MeanByLocYrCrWNTCCO2a,MeanByLocYrCrWNTCCO2e,by="LocYrCrWNTC"))

#now I want to calculate individual response ratios
mRRZn <-PairedMeanFACEdata$Zn.y/PairedMeanFACEdata$Zn.x
mRRFe <-PairedMeanFACEdata$Fe.y/PairedMeanFACEdata$Fe.x
mRRN <-PairedMeanFACEdata$N.y/PairedMeanFACEdata$N.x
mRRP <-PairedMeanFACEdata$P.y/PairedMeanFACEdata$P.x
mRRK <-PairedMeanFACEdata$K.y/PairedMeanFACEdata$K.x
mRRS <-PairedMeanFACEdata$S.y/PairedMeanFACEdata$S.x
mRRB <-PairedMeanFACEdata$B.y/PairedMeanFACEdata$B.x
mRRCa <-PairedMeanFACEdata$Ca.y/PairedMeanFACEdata$Ca.x
mRRMg<-PairedMeanFACEdata$Mg.y/PairedMeanFACEdata$Mg.x
mRRMn<-PairedMeanFACEdata$Mn.y/PairedMeanFACEdata$Mn.x
mRRCu <-PairedMeanFACEdata$Cu.y/PairedMeanFACEdata$Cu.x


#putting the response ratios on
PairedMeanFACE <- as.data.frame(cbind(PairedMeanFACEdata$LocYrCrWNTC, mRRB, mRRCa, mRRCu, mRRFe, mRRK, mRRMg, mRRMn, mRRN, mRRP, mRRS, mRRZn))

#((((separate thought experiment))) this is for comparing whether grains with high nutrients have higher response ratios
PMF <- cbind(PairedMeanFACE,PairedMeanFACEdata)
write.csv(PMF, file="~/Rdata/nutrients/PMF.csv")
#renaming the forst column
names(PairedMeanFACE[,1]) <-"LocYrCrWNTC"
LocYrCrWNTC <- c(PairedMeanFACE$LocYrCrWNTC)
#split the column by spaces into a data frame... which is at the moment the wrong direction
split_text <- as.data.frame(strsplit(PairedMeanFACE[,1]," "))
Descrcols <- as.data.frame(t(split_text))
names(Descrcols) <-c("location","year","crop","water","fertiliser","TOS","cultivar")
PairedMeanFACEcol <-cbind(Descrcols, PairedMeanFACE)
write.csv(PairedMeanFACEcol, file="~/Rdata/nutrients/MeanNutRR.csv")


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