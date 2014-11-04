require("metafor")

metaplants<-read.csv("1_CO2/inputs/meta_plants.csv")
metaplants$difference<-metaplants$elevated-metaplants$ambient
metaplantsEXC<-subset(metaplants,exclude=="ok")


#metaplantsc550<-subset(metaplantsEXC, elevated> 399 & elevated< 701)
metaplantsc550<-subset(metaplantsEXC, elevated> 499 & elevated< 701)
# metaplantsc550<-subset(metaplantsEXC, difference> 78.22 & difference< 278.22)
#metaplantsc550<-subset(metaplantsEXC, elevated>= (1.5*ambient) & elevated<=(2.5*ambient))

# metaplantsc550<-metaplantsEXC

####C3####
metaplantsC3<-subset(metaplantsc550,type=="C3_cereal")

mr.metaplantsC3<-rma.mv(yi = percent_decrease_N,
                        V = Var,
                        W = replicate_facilities,
                        random = ~1|Sitecode/as.factor(yearcode),
                        data=metaplantsC3)
summary(mr.metaplantsC3)



C3out<-data.frame(
  "pred"=mr.metaplantsC3$b,
  "se"=mr.metaplantsC3$se,
  "p"=mr.metaplantsC3$pval,
  "elevated"=mean(metaplantsC3$elevated),
  "fieldpots"= mean(metaplantsC3[,"field01"], na.rm=TRUE),
  "experiments"=length(metaplantsC3$percent_decrease_N))


####C4####
metaplantsC4<-subset(metaplantsc550,type=="C4_cereal")

mr.metaplantsC4<-rma.mv(yi = percent_decrease_N,
                        V = Var,
                        W = replicate_facilities,
                        random = ~1|Sitecode/as.factor(yearcode),
                        data=metaplantsC4)

summary(mr.metaplantsC4)


C4out<-data.frame(
  "pred"=mr.metaplantsC4$b,
  "se"=mr.metaplantsC4$se,
  "p"=mr.metaplantsC4$pval,
  "elevated"=mean(metaplantsC4$elevated),
  "fieldpots"= mean(metaplantsC4[,"field01"], na.rm=TRUE),
  "experiments"=length(metaplantsC4$percent_decrease_N))




####Grain####
metaplantsGrain<-subset(metaplantsc550,type=="C3_cereal" | type=="C4_cereal")


mr.metaplantsGrain<-rma.mv(yi = percent_decrease_N,
                           V = Var,
                           W = replicate_facilities,random = ~1|Sitecode/as.factor(yearcode),
                           data=metaplantsGrain)

summary(mr.metaplantsGrain)


Grainout<-data.frame(
  "pred"=mr.metaplantsGrain$b,
  "se"=mr.metaplantsGrain$se,
  "p"=mr.metaplantsGrain$pval,
  "elevated"=mean(metaplantsGrain$elevated),
  "fieldpots"= mean(metaplantsGrain[,"field01"], na.rm=TRUE),
  "experiments"=length(metaplantsGrain$percent_decrease_N))




####Root####
metaplantsRoot<-subset(metaplantsc550,type=="root_vegetable")


mr.metaplantsRoot<-rma.mv(yi = percent_decrease_N,
                          V = Var,
                          W = replicate_facilities, random = ~1|Sitecode/as.factor(yearcode),
                          data=metaplantsRoot)


summary(mr.metaplantsRoot)


Rootout<-data.frame(
  "pred"=mr.metaplantsRoot$b,
  "se"=mr.metaplantsRoot$se,
  "p"=mr.metaplantsRoot$pval,
  "elevated"=mean(metaplantsRoot$elevated),
  "fieldpots"= mean(metaplantsRoot[,"field01"], na.rm=TRUE),
  "experiments"=length(metaplantsRoot$percent_decrease_N))




####pulse####
metaplantsPulse<-subset(metaplantsc550,type=="pulses")

mr.metaplantsPulse<-rma.mv(yi = percent_decrease_N,
                           V = Var,
                           W = replicate_facilities, random = ~1|Sitecode/as.factor(yearcode),
                           data=metaplantsPulse)

summary(mr.metaplantsPulse)

Pulseout<-data.frame(
  "pred"=mr.metaplantsPulse$b,
  "se"=mr.metaplantsPulse$se,
  "p"=mr.metaplantsPulse$pval,
  "elevated"=mean(metaplantsPulse$elevated),
  "fieldpots"= mean(metaplantsPulse[,"field01"], na.rm=TRUE),
  "experiments"=length(metaplantsPulse$percent_decrease_N))



####oilcrops####
metaplantsOil<-subset(metaplantsc550,type=="oilcrop")


mr.metaplantsOil<-rma.mv(yi = percent_decrease_N,
                         V = Var,
                         W = replicate_facilities,random = ~1|Sitecode/as.factor(yearcode),
                         data=metaplantsOil)

summary(mr.metaplantsOil)

Oilout<-data.frame(
  "pred"=mr.metaplantsOil$b,
  "se"=mr.metaplantsOil$se,
  "p"=mr.metaplantsOil$pval,
  "elevated"=mean(metaplantsOil$elevated),
  "fieldpots"= mean(metaplantsOil[,"field01"], na.rm=TRUE),
  "experiments"=length(metaplantsOil$percent_decrease_N))


####Veg####
metaplantsVeg<-subset(metaplantsc550,type=="vegetable")

mr.metaplantsVeg<-rma.mv(yi = percent_decrease_N,
                         V = Var,
                         W = replicate_facilities,random = ~1|Sitecode/as.factor(yearcode),
                         data=metaplantsVeg)

summary(mr.metaplantsVeg)


Vegout<-data.frame(
  "pred"=mr.metaplantsVeg$b,
  "se"=mr.metaplantsVeg$se,
  "p"=mr.metaplantsVeg$pval,
  "elevated"=mean(metaplantsVeg$elevated),
  "fieldpots"= mean(metaplantsVeg[,"field01"], na.rm=TRUE),
  "experiments"=length(metaplantsVeg$percent_decrease_N))


###############Plant order######################


metaplantsWheat<-subset(metaplantsc550,plantorder==1)

aggregate(metaplantsWheat$percent_decrease_N,by=metaplantsWheat$Sitecode, FUN=mean)
wheatavbysite<- data.frame(aggregate(metaplantsWheat$percent_decrease_N~ metaplantsWheat$Sitecode, FUN=mean, na.action=NULL))
wheatSEbysite<- data.frame(aggregate(metaplantsWheat$SE~ metaplantsWheat$Sitecode, FUN=mean, na.action=NULL))

samdata<-subset(metaplantsWheat,Sitecode=="Hors"|Sitecode=="Walp")

mr.metaplantsSam<-rma.mv(yi = percent_decrease_N,
                           V = Var,
                           W = replicate_facilities, random = ~1|Sitecode/cultivar,
                           data=samdata)
summary(mr.metaplantsSam)

  
  
  
mr.metaplantsWheat<-rma.mv(yi = percent_decrease_N,
                           V = Var,
                           W = replicate_facilities, random = ~1|Sitecode/as.factor(yearcode),
                           data=metaplantsWheat)
summary(mr.metaplantsWheat)

summary(lm(log(percent_decrease_N)~as.factor(Sitecode):as.factor(yearcode),data=metaplantsWheat))

Wheatout<-data.frame(
  "pred"=mr.metaplantsWheat$b,
  "se"=mr.metaplantsWheat$se,
  "p"=mr.metaplantsWheat$pval,
  "elevated"=mean(metaplantsWheat$elevated),
  "fieldpots"= mean(metaplantsWheat[,"field01"], na.rm=TRUE),
  "experiments"=length(metaplantsWheat$percent_decrease_N))



####Rice####
metaplantsRice<-subset(metaplantsc550,plantorder==2)

mr.metaplantsRice<-rma.mv(yi = percent_decrease_N,
                          V = Var,
                          W = replicate_facilities,random = ~1|Sitecode/as.factor(yearcode),
                          data=metaplantsRice)

summary(mr.metaplantsRice)
Riceout<-data.frame(
  "pred"=mr.metaplantsRice$b,
  "se"=mr.metaplantsRice$se,
  "p"=mr.metaplantsRice$pval,
  "elevated"=mean(metaplantsRice$elevated),
  "fieldpots"= mean(metaplantsRice[,"field01"], na.rm=TRUE),
  "experiments"=length(metaplantsRice$percent_decrease_N))

####Barley####
metaplantsBarley<-subset(metaplantsc550,plantorder==3)

mr.metaplantsBarley<-rma.mv(yi = percent_decrease_N,
                            V = Var,
                            W = replicate_facilities, random = ~1|Sitecode/as.factor(yearcode),
                            data=metaplantsBarley)


summary(mr.metaplantsBarley)


Barleyout<-data.frame(
  "pred"=mr.metaplantsBarley$b,
  "se"=mr.metaplantsBarley$se,
  "p"=mr.metaplantsBarley$pval,
  "elevated"=mean(metaplantsBarley$elevated),
  "fieldpots"= mean(metaplantsBarley[,"field01"], na.rm=TRUE),
  "experiments"=length(metaplantsBarley$percent_decrease_N))

Barleyout
####maize####
metaplantsMaize<-subset(metaplantsc550,plantorder==4)

mr.metaplantsMaize<-rma.mv(yi = percent_decrease_N,
                           V = Var,
#                            W = replicate_facilities,
                           W = replicate_facilities, random = ~1|Sitecode/as.factor(yearcode),
                           data=metaplantsMaize)
summary(mr.metaplantsMaize)



Maizeout<-data.frame(
  "pred"=mr.metaplantsMaize$b,
  "se"=mr.metaplantsMaize$se,
  "p"=mr.metaplantsMaize$pval,
  "elevated"=mean(metaplantsMaize$elevated),
  "fieldpots"= mean(metaplantsMaize[,"field01"], na.rm=TRUE),
  "experiments"=length(metaplantsMaize$percent_decrease_N))


####Sorghum####
metaplantsSorghum<-subset(metaplantsc550,plantorder==7)

mr.metaplantsSorghum<-rma.mv(yi = percent_decrease_N,
                             V = Var,
                             W = replicate_facilities,
                             random = ~1|Sitecode/as.factor(yearcode),
                             data=metaplantsSorghum)

summary(mr.metaplantsSorghum)

Sorghumout<-data.frame(
  "pred"=mr.metaplantsSorghum$b,
  "se"=mr.metaplantsSorghum$se,
  "p"=mr.metaplantsSorghum$pval,
  "elevated"=mean(metaplantsSorghum$elevated),
  "fieldpots"= mean(metaplantsSorghum[,"field01"], na.rm=TRUE),
  "experiments"=length(metaplantsSorghum$percent_decrease_N))



###Potato####
metaplantsPotato<-subset(metaplantsc550,plantorder==10)

mr.metaplantsPotato<-rma.mv(yi = percent_decrease_N,
                            V = Var,
                            W = replicate_facilities,
                            random = ~1|Sitecode/as.factor(yearcode),
                            data=metaplantsPotato)

summary(mr.metaplantsPotato)

Potatoout<-data.frame(
  "pred"=mr.metaplantsPotato$b,
  "se"=mr.metaplantsPotato$se,
  "p"=mr.metaplantsPotato$pval,
  "elevated"=mean(metaplantsPotato$elevated),
  "fieldpots"= mean(metaplantsPotato[,"field01"], na.rm=TRUE),
  "experiments"=length(metaplantsPotato$percent_decrease_N))



###BEETS####
metaplantsSugarbeet<-subset(metaplantsc550,plantorder==11)


# doesn't work once exclude data
mr.metaplantsSugarbeet<-rma.mv(yi = percent_decrease_N,
                               V = Var,
                               W = replicate_facilities,
                               random = ~1|Sitecode/as.factor(yearcode),
                               data=metaplantsSugarbeet)

summary(mr.metaplantsSugarbeet)

Sugarbeetout<-data.frame(
  "pred"=mr.metaplantsSugarbeet$b,
  "se"=mr.metaplantsSugarbeet$se,
  "p"=mr.metaplantsSugarbeet$pval,
  "elevated"=mean(metaplantsSugarbeet$elevated),
  "fieldpots"= mean(metaplantsSugarbeet[,"field01"], na.rm=TRUE),
  "experiments"=length(metaplantsSugarbeet$percent_decrease_N))

###BEANS####
metaplantsBeans<-subset(metaplantsc550,plantorder==12)

mr.metaplantsBeans<-rma.mv(yi = percent_decrease_N,
                           V = Var,
                           W = replicate_facilities,
                           random = ~1|Sitecode/as.factor(yearcode),
                           data=metaplantsBeans)

summary(mr.metaplantsBeans)

Beansout<-data.frame(
  "pred"=mr.metaplantsBeans$b,
  "se"=mr.metaplantsBeans$se,
  "p"=mr.metaplantsBeans$pval,
  "elevated"=mean(metaplantsBeans$elevated),
  "fieldpots"= mean(metaplantsBeans[,"field01"], na.rm=TRUE),
  "experiments"=length(metaplantsBeans$percent_decrease_N))

###PEAS####
metaplantsPeas<-subset(metaplants,plantorder==13)

mr.metaplantsPeas<-rma.mv(yi = percent_decrease_N,
                          V = Var,
                          W = replicate_facilities,
#                           mods = ~difference+field01, 
                          random = ~1|Sitecode/as.factor(yearcode),
                          data=metaplantsPeas)

summary(mr.metaplantsPeas)
# 
# predict.mr.d.metaplantsPeas.lowN<-predict(mr.metaplantsPeas, newmods=c(178.22,1))
# predict.mr.d.metaplantsPeas.moderateN<-predict(mr.metaplantsPeas, newmods=c(178.22,1))
# predict.mr.d.metaplantsPeas.highN<-predict(mr.metaplantsPeas, newmods=c(178.22,1))
# predictPeas<-rbind(predict.mr.d.metaplantsPeas.lowN,predict.mr.d.metaplantsPeas.moderateN,predict.mr.d.metaplantsPeas.highN)
# 

Peasout<-data.frame(
    "pred"=mr.metaplantsPeas$b,
    "se"=mr.metaplantsPeas$se,
    "p"=mr.metaplantsPeas$pval,
    "elevated"=mean(metaplantsPeas$elevated),
    "fieldpots"= mean(metaplantsPeas[,"field01"], na.rm=TRUE),
    "experiments"=length(metaplantsPeas$percent_decrease_N))
  #   "pred"=predict.mr.d.metaplantsPeas.moderateN$pred,
#   "se"=predict.mr.d.metaplantsPeas.moderateN$se,
#   "p"="d and f <0.1",
#   "elevated"=mean(metaplantsPeas$elevated),
#   "fieldpots"= mean(metaplantsPeas[,"field01"], na.rm=TRUE),
#   "experiments"=length(metaplantsPeas$percent_decrease_N))



###chickpea####
metaplantsChickpea<-subset(metaplants,plantorder==14)
mr.metaplantsChickpea<-rma.mv(yi = percent_decrease_N,
                              V = Var,
                              W = replicate_facilities,
                              random = ~1|Sitecode/as.factor(yearcode),
                              data=metaplantsChickpea)

summary(mr.metaplantsChickpea)

Chickpeaout<-data.frame(
  "pred"=mr.metaplantsChickpea$b,
  "se"=mr.metaplantsChickpea$se,
  "p"=mr.metaplantsChickpea$pval,
  "elevated"=mean(metaplantsChickpea$elevated),
  "fieldpots"= mean(metaplantsChickpea[,"field01"], na.rm=TRUE),
  "experiments"=length(metaplantsChickpea$percent_decrease_N))

###soy####
metaplantsSoy<-subset(metaplantsc550,plantorder==15)

mr.metaplantsSoy<-rma.mv(yi = percent_decrease_N,
                         V = Var,
                         W = replicate_facilities,                         
                         random = ~1|Sitecode/as.factor(yearcode),
                         data=metaplantsSoy)

summary(mr.metaplantsSoy)

Soyout<-data.frame(
  "pred"=mr.metaplantsSoy$b,
  "se"=mr.metaplantsSoy$se,
  "p"=mr.metaplantsSoy$pval,
  "elevated"=mean(metaplantsSoy$elevated),
  "fieldpots"= mean(metaplantsSoy[,"field01"], na.rm=TRUE),
  "experiments"=length(metaplantsSoy$percent_decrease_N))


# metaplantsCassava<-subset(metaplantsc550,plantorder==9)
# mr.metaplantsCassava<-rma.mv(yi = percent_decrease_N,
#                              V = Var,
#                              W = replicate_facilities,
# #                              random = ~1|Sitecode/as.factor(yearcode),
#                              data=metaplantsCassava)
#                              
# summary(mr.metaplantsCassava)
#                              
# Cassavaout<-data.frame(
#   "pred"=mr.metaplantsCassava$b,
#   "se"=mr.metaplantsCassava$se,
#   "p"=mr.metaplantsCassava$pval,
#   "elevated"=mean(metaplantsCassava$elevated),
#                                "fieldpots"= mean(metaplantsCassava[,"field01"], na.rm=TRUE),
#                                "experiments"=length(metaplantsCassava$percent_decrease_N))
#                              
# metaplantsPeanut<-subset(metaplantsc550,plantorder==16)
# mr.metaplantsPeanut<-rma.mv(yi = percent_decrease_N,
#                             V = Var,
#                             W = replicate_facilities,
#                             random = ~1|Sitecode/as.factor(yearcode),
#                             data=metaplantsPeanut)
#                             
#                             summary(mr.metaplantsPeanut)
#                             
#                             Peanutout<-data.frame(
#                               "pred"=mr.metaplantsPeanut$b,
#                               "se"=mr.metaplantsPeanut$se,
#                               "p"=mr.metaplantsPeanut$pval,
#                               "elevated"=mean(metaplantsPeanut$elevated),
#                               "fieldpots"= mean(metaplantsPeanut[,"field01"], na.rm=TRUE),
#                               "experiments"=length(metaplantsPeanut$percent_decrease_N))

###mustard####
metaplantsMustard<-subset(metaplants,plantorder==18)

mr.metaplantsMustard<-rma.mv(yi = percent_decrease_N,
                             V = Var,
                             W = replicate_facilities,
#                              mods = ~difference+nitrogen,
                             random = ~1|Sitecode/as.factor(yearcode),
                             data=metaplantsMustard,
#                              control=list(optimizer="nlminb", iter.max=500)
                             )

summary(mr.metaplantsMustard)

# predict.mr.d.metaplantsMustard.lowN<-predict(mr.metaplantsMustard, newmods=c(178.22,1))
# predict.mr.d.metaplantsMustard.moderateN<-predict(mr.metaplantsMustard, newmods=c(178.22,2))
# predict.mr.d.metaplantsMustard.highN<-predict(mr.metaplantsMustard, newmods=c(178.22,3))
# predictMustard<-rbind(predict.mr.d.metaplantsMustard.lowN,predict.mr.d.metaplantsMustard.moderateN,predict.mr.d.metaplantsMustard.highN)
# # View(predictMustard)

# predictMustarddf<-data.frame(matrix(unlist(predictMustard), nrow=3, byrow=F))
# names(predictMustarddf)[1:2]<-c("pred","se")



# Mustardout<-data.frame(
#   "pred"=predictMustarddf[2,1],
#   "se"=predictMustarddf[2,2],
#   "p"="fert p<0.1, difference p<0.001, intercept p <0.001",
#   "elevated"=mean(metaplantsMustard$elevated),
#   "fieldpots"= mean(metaplantsMustard[,"field01"], na.rm=TRUE),
#   "experiments"=length(metaplantsMustard$percent_decrease_N))

Mustardout<-data.frame(
  "pred"=mr.metaplantsMustard$b,
  "se"=mr.metaplantsMustard$se,
  "p"=mr.metaplantsMustard$pval,
  "elevated"=mean(metaplantsMustard$elevated),
  "fieldpots"= mean(metaplantsMustard[,"field01"], na.rm=TRUE),
  "experiments"=length(metaplantsMustard$percent_decrease_N))

output_only_sig<-data.frame(
  rbind(Wheatout,
        Riceout, 
        Barleyout,
       Maizeout,
        Sorghumout,
        C3out, 
        C4out,
        Grainout,
        Rootout,
        Potatoout,
#        Cassavaout, 
        Sugarbeetout,
        Pulseout,
        Peasout,
        Beansout,
        Chickpeaout, 
        Oilout,
        Soyout,
        #                        Peanutout,
        Mustardout,
        Vegout))

output_only_sig$plant= c("Wheat","Rice", "Barley",
                         "Maize",
                         "Sorghum","C3", "C4","Grain","Root","Potato",
                         #            "Cassava", 
                         "Sugarbeet","Pulse","Peas","Beans","Chickpea", "Oil","Soy",
                         # "Peanut",
                         "Mustard","Veg")



predictions.type<-output_only_sig
predictions.forexport<-data.frame(lapply(predictions.type, function(predictions) factor(unlist(predictions))))
write.csv(predictions.forexport,"1_CO2/outputs/predictionsType_21Oct2014.csv")


Nfixers<-rbind(metaplantsSoy,metaplantsPulse, metaplantsMustard)

mr.metaplantsNfixers<-rma.mv(yi = percent_decrease_N,
                            V = Var,
                            W = replicate_facilities,
                            random = ~1|Sitecode,
                            data=Nfixers)

summary(mr.metaplantsNfixers)



