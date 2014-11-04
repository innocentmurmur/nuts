shizFACEdata <-subset(FACEdata, Location =="Shizukuishi__Japan")
shizFACEdata["NCCO2"]<- paste(shizFACEdata$NapplicnQ,shizFACEdata$Cultivar,shizFACEdata$CO2treatment)
shizMeanByNCCO2 <- data.frame(aggregate(cbind(Zn, Fe, N, P, K, S, B, Ca, Mg, Mn, Cu) ~ NCCO2, FUN=mean, data=shizFACEdata))
shizMeanByNCCO2["NC"] <- shizMeanByNCCO2["NCCO2"]
shizMeanByNCCO2$NC <- gsub("....$", "", shizMeanByNCCO2$NC)
shizLabels <- matrix(unlist(strsplit(shizMeanByNCCO2$NCCO2," ")),ncol = 3, byrow = TRUE)
shizMeanByNCCO2["CO2"] <- shizLabels[,3]
shizMeanByNCCO2["cultivar"] <- shizLabels[,2]
shizMeanByNCCO2["fert"] <- shizLabels[,1]
shizMeanByNCCO2e <- subset(shizMeanByNCCO2, CO2=="eCO2")
shizMeanByNCCO2a <- subset(shizMeanByNCCO2, CO2=="aCO2")
shizPMF <- data.frame(merge(shizMeanByNCCO2e,shizMeanByNCCO2a,by="NC"))
shizPMF["RRZn"]<-shizPMF$Zn.x/shizPMF$Zn.y
shizPMF["RRFe"]<-shizPMF$Fe.x/shizPMF$Fe.y
shizPMF["RRN"]<-shizPMF$N.x/shizPMF$N.y
shizPMF["RRP"]<-shizPMF$P.x/shizPMF$P.y
shizPMF["RRK"]<-shizPMF$K.x/shizPMF$K.y
shizPMF["RRS"]<-shizPMF$S.x/shizPMF$S.y
shizPMF["RRB"]<-shizPMF$B.x/shizPMF$B.y
shizPMF["RRCa"]<-shizPMF$Ca.x/shizPMF$Ca.y
shizPMF["RRMg"]<-shizPMF$Mg.x/shizPMF$Mg.y
shizPMF["RRMn"]<-shizPMF$Mn.x/shizPMF$Mn.y
shizPMF["RRCu"]<-shizPMF$Cu.x/shizPMF$Cu.y

shizav <- data.frame(location="shiz", aggregate(cbind(RRZn, RRFe, RRN, RRP, RRK, RRS, RRB, RRCa, RRMg, RRMn, RRCu) ~ CO2.x, FUN=mean, data=shizPMF, na.action=NULL))

write.csv(shizav, file="~/Rdata/nutrients/shizav.csv")
