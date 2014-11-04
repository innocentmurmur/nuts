require("metafor")
# metaplants<-read.csv("1_CO2/inputs/meta_plants_long.csv")
metaplants<-read.csv("1_CO2/inputs/meta_plants.csv")
metaplantsEXC<-subset(metaplants,exclude=="ok")
metaplants$difference<-metaplants$elevated-metaplants$ambient
# metaplants$logdifference<-log(metaplants$elevated-metaplants$ambient)
# metaplantsc550<-subset(metaplants, elevated> 399 & elevated< 701)
# metaplantsc550<-subset(metaplantsEXC, elevated> 399 & elevated< 701)
# metaplantsc550<-subset(metaplants, CO2_quant> 399 & CO2_quant< 701)
metaplantsc550<-metaplantsEXC

#A: FULL MODEL
# metaplantsC3<-subset(metaplants,type=="C3_cereal")
metaplantsC3<-subset(metaplantsc550,type=="C3_cereal")
length(metaplantsC3$percent_decrease_N) #if it's >4, continue, otherwise... B
range(metaplantsC3$difference) #if it has 2 numbers and they're sane, continue, otherwise...
range(metaplantsC3$nitrogen) #if it has 2 numbers and they're sane, continue, otherwise...

# mr.metaplantsC3<-rma.mv(yi = percent_decrease_N,
#                      V = Var,
#                      W = replicate_facilities,
#                       mods = ~difference,
# #                    mods = ~difference+field01+nitrogen, 
#                      #dogg random = ~1|Sitecode,
#                      data=metaplantsC3)


mr.metaplantsC3<-rma.mv(yi = N_est,
                        V = Var,
                        W = replicate_facilities,
                        mods = ~CO2_quant,
#                         mods = ~difference+field01+nitrogen, 
                        random = ~1|Sitecode+1|paircount,
                        data=metaplantsC3)
summary(mr.metaplantsC3)

# 
# 
# predict.mr.d.metaplantsC3.lowN<-predict(mr.metaplantsC3, newmods=c(178.22,1,1))
# predict.mr.d.metaplantsC3.moderateN<-predict(mr.metaplantsC3, newmods=c(178.22,1,2))
# predict.mr.d.metaplantsC3.highN<-predict(mr.metaplantsC3, newmods=c(178.22,1,3))
# 
# predictC3<-rbind(predict.mr.d.metaplantsC3.lowN,predict.mr.d.metaplantsC3.moderateN,predict.mr.d.metaplantsC3.highN)
# View(predictC3)

C3out<-data.frame(
  "pred"=mr.metaplantsC3$b,
  "se"=mr.metaplantsC3$se,
  "p"=mr.metaplantsC3$pval,
  "elevated"=mean(metaplantsC3$elevated),
  "fieldpots"= mean(metaplantsC3[,"field01"], na.rm=TRUE),
  "experiments"=length(metaplantsC3$percent_decrease_N))


#B
metaplantsC4<-subset(metaplantsc550,type=="C4_cereal")
length(metaplantsC4$percent_decrease_N) #if it's >4, continue, otherwise... 
range(metaplantsC4$difference) #if it has 2 numbers and they're sane, continue, otherwise...
range(metaplantsC4$nitrogen) #if it has 2 numbers and they're sane, continue, otherwise...
range(metaplantsC4$field01) #0 and 1 otherwise B
mr.metaplantsC4<-rma.mv(yi = percent_decrease_N,
                        V = Var,
                        W = replicate_facilities,
#                         mods = ~difference+nitrogen, 
#                         mods = ~difference,
                        
                        #dogg random = ~1|Sitecode,
                        data=metaplantsC4)

summary(mr.metaplantsC4)
# 
# predict.mr.d.metaplantsC4.lowN<-predict(mr.metaplantsC4, newmods=c(178.22,1))
# predict.mr.d.metaplantsC4.moderateN<-predict(mr.metaplantsC4, newmods=c(178.22,2))
# predict.mr.d.metaplantsC4.highN<-predict(mr.metaplantsC4, newmods=c(178.22,3))
# predictC4<-rbind(predict.mr.d.metaplantsC4.lowN,predict.mr.d.metaplantsC4.moderateN,predict.mr.d.metaplantsC4.highN)
# View(predictC4)

C4out<-data.frame(
  "pred"=mr.metaplantsC4$b,
  "se"=mr.metaplantsC4$se,
  "p"=mr.metaplantsC4$pval,
  "elevated"=mean(metaplantsC4$elevated),
  "fieldpots"= mean(metaplantsC4[,"field01"], na.rm=TRUE),
  "experiments"=length(metaplantsC4$percent_decrease_N))




#A
metaplantsGrain<-subset(metaplantsc550,type=="C3_cereal" | type=="C4_cereal")
length(metaplantsGrain$percent_decrease_N) #if it's >4, continue, otherwise... B
range(metaplantsGrain$difference) #if it has 2 numbers and they're sane, continue, otherwise...
range(metaplantsGrain$nitrogen) #if it has 2 numbers and they're sane, continue, otherwise...
range(metaplantsGrain$field01) #0 and 1 otherwise B


mr.metaplantsGrain<-rma.mv(yi = percent_decrease_N,
                           V = Var,
                           W = replicate_facilities,
#                           mods = ~difference,
#                          mods = ~difference  +field01+nitrogen, 
                           #dogg random = ~1|Sitecode,
                           data=metaplantsGrain)

summary(mr.metaplantsGrain)

# predict.mr.d.metaplantsGrain.lowN<-predict(mr.metaplantsGrain, newmods=c(178.22,1,1))
# predict.mr.d.metaplantsGrain.moderateN<-predict(mr.metaplantsGrain, newmods=c(178.22,1,2))
# predict.mr.d.metaplantsGrain.highN<-predict(mr.metaplantsGrain, newmods=c(178.22,1,3))
# predictGrain<-rbind(predict.mr.d.metaplantsGrain.lowN,predict.mr.d.metaplantsGrain.moderateN,predict.mr.d.metaplantsGrain.highN)
# View(predictGrain)

Grainout<-data.frame(
  "pred"=mr.metaplantsGrain$b,
  "se"=mr.metaplantsGrain$se,
  "p"=mr.metaplantsGrain$pval,
  "elevated"=mean(metaplantsGrain$elevated),
  "fieldpots"= mean(metaplantsGrain[,"field01"], na.rm=TRUE),
  "experiments"=length(metaplantsGrain$percent_decrease_N))





#A
metaplantsRoot<-subset(metaplantsc550,type=="root_vegetable")

length(metaplantsRoot$percent_decrease_N) #if it's >4, continue, otherwise... B
range(metaplantsRoot$difference) #if it has 2 numbers and they're sane, continue, otherwise...
range(metaplantsRoot$nitrogen) #if it has 2 numbers and they're sane, continue, otherwise...
range(metaplantsRoot$field01) #0 and 1 otherwise B


mr.metaplantsRoot<-rma.mv(yi = percent_decrease_N,
                          V = Var,
                          W = replicate_facilities,
#                           mods = ~difference,
#                           mods = ~difference+field01+nitrogen, 
                          #dogg random = ~1|Sitecode,
                          data=metaplantsRoot)


summary(mr.metaplantsRoot)
# 
# 
# predict.mr.d.metaplantsRoot.lowN<-predict(mr.metaplantsRoot, newmods=c(178.22,1,1))
# predict.mr.d.metaplantsRoot.moderateN<-predict(mr.metaplantsRoot, newmods=c(178.22,1,2))
# predict.mr.d.metaplantsRoot.highN<-predict(mr.metaplantsRoot, newmods=c(178.22,1,3))
# predictRoot<-rbind(predict.mr.d.metaplantsRoot.lowN,predict.mr.d.metaplantsRoot.moderateN,predict.mr.d.metaplantsRoot.highN)
# View(predictRoot)

Rootout<-data.frame(
  "pred"=mr.metaplantsRoot$b,
  "se"=mr.metaplantsRoot$se,
  "p"=mr.metaplantsRoot$pval,
  "elevated"=mean(metaplantsRoot$elevated),
  "fieldpots"= mean(metaplantsRoot[,"field01"], na.rm=TRUE),
  "experiments"=length(metaplantsRoot$percent_decrease_N))





#A however it doesn't really make sense to fertilise pulses
metaplantsPulse<-subset(metaplantsc550,type=="pulses")
length(metaplantsPulse$percent_decrease_N) #if it's >4, continue, otherwise... B
range(metaplantsPulse$difference) #if it has 2 numbers and they're sane, continue, otherwise...
range(metaplantsPulse$nitrogen) #if it has 2 numbers and they're sane, continue, otherwise...
range(metaplantsPulse$field01) #0 and 1 otherwise B

mr.metaplantsPulse<-rma.mv(yi = percent_decrease_N,
                           V = Var,
                           W = replicate_facilities,
#                            mods = ~difference+field01, 
#                            mods = ~difference, 
#                            #dogg random = ~1|Sitecode,
                           data=metaplantsPulse)

summary(mr.metaplantsPulse)

Pulseout<-data.frame(
  "pred"=mr.metaplantsPulse$b,
  "se"=mr.metaplantsPulse$se,
  "p"=mr.metaplantsPulse$pval,
  "elevated"=mean(metaplantsPulse$elevated),
  "fieldpots"= mean(metaplantsPulse[,"field01"], na.rm=TRUE),
  "experiments"=length(metaplantsPulse$percent_decrease_N))



# # doesn't work once excluding the data with no SE
# # predict.mr.d.metaplantsPulse.lowN<-predict(mr.metaplantsPulse, newmods=c(178.22,1,1))
# # predict.mr.d.metaplantsPulse.moderateN<-predict(mr.metaplantsPulse, newmods=c(178.22,1,2))
# # predict.mr.d.metaplantsPulse.highN<-predict(mr.metaplantsPulse, newmods=c(178.22,1,3))
# 
# predict.mr.d.metaplantsPulse.lowN<-predict(mr.metaplantsPulse, newmods=c(178.22,1))
# predict.mr.d.metaplantsPulse.moderateN<-predict(mr.metaplantsPulse, newmods=c(178.22,1))
# predict.mr.d.metaplantsPulse.highN<-predict(mr.metaplantsPulse, newmods=c(178.22,1))

# predictPulse<-rbind(predict.mr.d.metaplantsPulse.lowN,predict.mr.d.metaplantsPulse.moderateN,predict.mr.d.metaplantsPulse.highN)
# View(predictPulse)


#A
metaplantsOil<-subset(metaplantsc550,type=="oilcrop")
length(metaplantsOil$percent_decrease_N) #if it's >4, continue, otherwise... B
range(metaplantsOil$difference) #if it has 2 numbers and they're sane, continue, otherwise...
range(metaplantsOil$nitrogen) #if it has 2 numbers and they're sane, continue, otherwise...
range(metaplantsOil$field01) #0 and 1 otherwise B


mr.metaplantsOil<-rma.mv(yi = percent_decrease_N,
                         V = Var,
                         W = replicate_facilities,
#                          mods = ~difference, 
#                          mods = ~difference+field01, 
#                          mods = ~difference+field01+nitrogen, 
                         #dogg random = ~1|Sitecode,
                         data=metaplantsOil)

summary(mr.metaplantsOil)

Oilout<-data.frame(
  "pred"=mr.metaplantsOil$b,
  "se"=mr.metaplantsOil$se,
  "p"=mr.metaplantsOil$pval,
  "elevated"=mean(metaplantsOil$elevated),
  "fieldpots"= mean(metaplantsOil[,"field01"], na.rm=TRUE),
  "experiments"=length(metaplantsOil$percent_decrease_N))

# predict.mr.d.metaplantsOil.lowN<-predict(mr.metaplantsOil, newmods=c(178.22,1,1))
# predict.mr.d.metaplantsOil.moderateN<-predict(mr.metaplantsOil, newmods=c(178.22,1,2))
# predict.mr.d.metaplantsOil.highN<-predict(mr.metaplantsOil, newmods=c(178.22,1,3))
# predictOil<-rbind(predict.mr.d.metaplantsOil.lowN,predict.mr.d.metaplantsOil.moderateN,predict.mr.d.metaplantsOil.highN)
# View(predictOil)

#A
metaplantsVeg<-subset(metaplantsc550,type=="vegetable")
length(metaplantsVeg$percent_decrease_N) #if it's >4, continue, otherwise... B
range(metaplantsVeg$difference) #if it has 2 numbers and they're sane, continue, otherwise...
range(metaplantsVeg$nitrogen) #if it has 2 numbers and they're sane, continue, otherwise...
range(metaplantsVeg$field01) #0 and 1 otherwise B

mr.metaplantsVeg<-rma.mv(yi = percent_decrease_N,
                         V = Var,
                         W = replicate_facilities,
#                          mods = ~difference+field01+nitrogen, 
#                          mods = ~difference, 
                         #dogg random = ~1|Sitecode,
                         data=metaplantsVeg)

summary(mr.metaplantsVeg)


Vegout<-data.frame(
  "pred"=mr.metaplantsVeg$b,
  "se"=mr.metaplantsVeg$se,
  "p"=mr.metaplantsVeg$pval,
  "elevated"=mean(metaplantsVeg$elevated),
  "fieldpots"= mean(metaplantsVeg[,"field01"], na.rm=TRUE),
  "experiments"=length(metaplantsVeg$percent_decrease_N))

# predict.mr.d.metaplantsVeg.lowN<-predict(mr.metaplantsVeg, newmods=c(178.22,1,1))
# predict.mr.d.metaplantsVeg.moderateN<-predict(mr.metaplantsVeg, newmods=c(178.22,1,2))
# predict.mr.d.metaplantsVeg.highN<-predict(mr.metaplantsVeg, newmods=c(178.22,1,3))
# predictVeg<-rbind(predict.mr.d.metaplantsVeg.lowN,predict.mr.d.metaplantsVeg.moderateN,predict.mr.d.metaplantsVeg.highN)
# View(predictVeg)


###############Plant order######################


metaplantsWheat<-subset(metaplantsc550,plantorder==1)

length(metaplantsWheat$percent_decrease_N) #if it's >4, continue, otherwise... B
range(metaplantsWheat$difference) #if it has 2 numbers and they're sane, continue, otherwise...
range(metaplantsWheat$nitrogen) #if it has 2 numbers and they're sane, continue, otherwise...

mr.metaplantsWheat<-rma.mv(yi = percent_decrease_N,
                           V = Var,
                           W = replicate_facilities,
#                            mods = ~difference+field01+nitrogen, 
                         #dogg random = ~1|Sitecode,
                           #mods = ~logdifference, 
#                                                      
                           data=metaplantsWheat)
summary(mr.metaplantsWheat)



# predict.mr.d.metaplantsWheat.lowN<-predict(mr.metaplantsWheat, newmods=c(178.22,1,1))
# predict.mr.d.metaplantsWheat.moderateN<-predict(mr.metaplantsWheat, newmods=c(178.22,1,2))
# predict.mr.d.metaplantsWheat.highN<-predict(mr.metaplantsWheat, newmods=c(178.22,1,3))
# predictWheat<-rbind(predict.mr.d.metaplantsWheat.lowN,predict.mr.d.metaplantsWheat.moderateN,predict.mr.d.metaplantsWheat.highN)
# View(predictWheat)

Wheatout<-data.frame(
  "pred"=mr.metaplantsWheat$b,
  "se"=mr.metaplantsWheat$se,
  "p"=mr.metaplantsWheat$pval,
  "elevated"=mean(metaplantsWheat$elevated),
  "fieldpots"= mean(metaplantsWheat[,"field01"], na.rm=TRUE),
  "experiments"=length(metaplantsWheat$percent_decrease_N))




metaplantsRice<-subset(metaplantsc550,plantorder==2)
#A: FULL MODEL
length(metaplantsRice$percent_decrease_N) #if it's >4, continue, otherwise... B
range(metaplantsRice$difference) #if it has 2 numbers and they're sane, continue, otherwise...
range(metaplantsRice$nitrogen) #if it has 2 numbers and they're sane, continue, otherwise...

mr.metaplantsRice<-rma.mv(yi = percent_decrease_N,
                          V = Var,
                          W = replicate_facilities,
#                           mods = ~difference+field01+nitrogen, 
#                           mods = ~difference+field01, 
#                           mods = ~difference, 
                          
                          #dogg random = ~1|Sitecode,
                          data=metaplantsRice)

summary(mr.metaplantsRice)
Riceout<-data.frame(
  "pred"=mr.metaplantsRice$b,
  "se"=mr.metaplantsRice$se,
  "p"=mr.metaplantsRice$pval,
  "elevated"=mean(metaplantsRice$elevated),
  "fieldpots"= mean(metaplantsRice[,"field01"], na.rm=TRUE),
  "experiments"=length(metaplantsRice$percent_decrease_N))

# predict.mr.d.metaplantsRice.lowN<-predict(mr.metaplantsRice, newmods=c(178.22,1,1))
# predict.mr.d.metaplantsRice.moderateN<-predict(mr.metaplantsRice, newmods=c(178.22,1,2))
# predict.mr.d.metaplantsRice.highN<-predict(mr.metaplantsRice, newmods=c(178.22,1,3))
# predictRice<-rbind(predict.mr.d.metaplantsRice.lowN,predict.mr.d.metaplantsRice.moderateN,predict.mr.d.metaplantsRice.highN)
# View(predictRice)


metaplantsBarley<-subset(metaplantsc550,plantorder==3)
#A: FULL MODEL
length(metaplantsBarley$percent_decrease_N) #if it's >4, continue, otherwise... B
range(metaplantsBarley$difference) #if it has 2 numbers and they're sane, continue, otherwise...
range(metaplantsBarley$nitrogen) #if it has 2 numbers and they're sane, continue, otherwise...

mr.metaplantsBarley<-rma.mv(yi = percent_decrease_N,
                            V = Var,
                            W = replicate_facilities,
#                             mods = ~difference+field01+nitrogen, 
                            #dogg random = ~1|Sitecode,
                            data=metaplantsBarley)


summary(mr.metaplantsBarley)

# predict.mr.d.metaplantsBarley.lowN<-predict(mr.metaplantsBarley, newmods=c(178.22,1,1))
# predict.mr.d.metaplantsBarley.moderateN<-predict(mr.metaplantsBarley, newmods=c(178.22,1,2))
# predict.mr.d.metaplantsBarley.highN<-predict(mr.metaplantsBarley, newmods=c(178.22,1,3))
# predictBarley<-rbind(predict.mr.d.metaplantsBarley.lowN,predict.mr.d.metaplantsBarley.moderateN,predict.mr.d.metaplantsBarley.highN)
# View(predictBarley)

Barleyout<-data.frame(
  "pred"=mr.metaplantsBarley$b,
  "se"=mr.metaplantsBarley$se,
  "p"=mr.metaplantsBarley$pval,
  "elevated"=mean(metaplantsBarley$elevated),
  "fieldpots"= mean(metaplantsBarley[,"field01"], na.rm=TRUE),
  "experiments"=length(metaplantsBarley$percent_decrease_N))


metaplantsMaize<-subset(metaplantsc550,plantorder==4)
length(metaplantsMaize$percent_decrease_N) #if it's >=4, continue, otherwise... B
range(metaplantsMaize$difference) #if it has 2 numbers and they're sane, continue, otherwise...
#ONLY ONE LEVEL

range(metaplantsMaize$nitrogen) #if it has 2 numbers and they're sane, continue, otherwise...
range(metaplantsMaize$field01) #0 and 1 otherwise B

mr.metaplantsMaize<-rma.mv(yi = percent_decrease_N,
                           V = Var,
                           W = replicate_facilities,
#                            mods = ~nitrogen, 
                           # random = ~1|yearcode, #isn't working because there's only one year allegedly, and only one site. Whatevs.
                           data=metaplantsMaize)
summary(mr.metaplantsMaize)

# predict.mr.d.metaplantsMaize.lowN<-predict(mr.metaplantsMaize, newmods=1)
# predict.mr.d.metaplantsMaize.moderateN<-predict(mr.metaplantsMaize, newmods=2)
# predict.mr.d.metaplantsMaize.highN<-predict(mr.metaplantsMaize, newmods=2) #don't feel comfortable extrapolating, as moderate N might have been quite high really
# predictMaize<-rbind(predict.mr.d.metaplantsMaize.lowN,predict.mr.d.metaplantsMaize.moderateN,predict.mr.d.metaplantsMaize.highN)
# View(predictMaize)

Maizeout<-data.frame(
  "pred"=mr.metaplantsMaize$b,
  "se"=mr.metaplantsMaize$se,
  "p"=mr.metaplantsMaize$pval,
  "elevated"=mean(metaplantsMaize$elevated),
  "fieldpots"= mean(metaplantsMaize[,"field01"], na.rm=TRUE),
  "experiments"=length(metaplantsMaize$percent_decrease_N))


#metaplants5<-Rye, n = 1
#metaplants6<-Oats, n = 1

metaplantsSorghum<-subset(metaplantsc550,plantorder==7)
#A: FULL MODEL
length(metaplantsSorghum$percent_decrease_N) #if it's >4, continue, otherwise... B
range(metaplantsSorghum$difference) #if it has 2 numbers and they're sane, continue, otherwise...
range(metaplantsSorghum$nitrogen) #if it has 2 numbers and they're sane, continue, otherwise...
#only one N treatment
range(metaplantsSorghum$field01) #0 and 1 otherwise B
#only in the field

mr.metaplantsSorghum<-rma.mv(yi = percent_decrease_N,
                             V = Var,
                             W = replicate_facilities,
#                              mods = ~difference, 
#                              #dogg random = ~1|Sitecode,
                             data=metaplantsSorghum)

summary(mr.metaplantsSorghum)

Sorghumout<-data.frame(
  "pred"=mr.metaplantsSorghum$b,
  "se"=mr.metaplantsSorghum$se,
  "p"=mr.metaplantsSorghum$pval,
  "elevated"=mean(metaplantsSorghum$elevated),
  "fieldpots"= mean(metaplantsSorghum[,"field01"], na.rm=TRUE),
  "experiments"=length(metaplantsSorghum$percent_decrease_N))


# predict.mr.d.metaplantsSorghum.lowN<-predict(mr.metaplantsSorghum, newmods=178.22)
# predict.mr.d.metaplantsSorghum.moderateN<-predict(mr.metaplantsSorghum, newmods=178.22)
# predict.mr.d.metaplantsSorghum.highN<-predict(mr.metaplantsSorghum, newmods=178.22,1)
# predictSorghum<-rbind(predict.mr.d.metaplantsSorghum.lowN,predict.mr.d.metaplantsSorghum.moderateN,predict.mr.d.metaplantsSorghum.highN)
# View(predictSorghum)

#metaplants<-subset(metaplants,plantorder==8)

# metaplantsCassava<-subset(metaplantsc550,plantorder==9)
# 
# length(metaplantsCassava$percent_decrease_N) #if it's >4, continue, otherwise... B
# range(metaplantsCassava$difference) #if it has 2 numbers and they're sane, continue, otherwise...
# range(metaplantsCassava$nitrogen) #if it has 2 numbers and they're sane, continue, otherwise...
# range(metaplantsCassava$field01) #0 and 1 otherwise B



#only in pots
# 
# predict.mr.d.metaplantsCassava.lowN<-predict.mr.d.metaplantsRoot.lowN
# predict.mr.d.metaplantsCassava.moderateN<-predict.mr.d.metaplantsRoot.moderateN
# predict.mr.d.metaplantsCassava.highN<-predict.mr.d.metaplantsRoot.highN
# 

# doesn't work once excluded plants with no sample size or SE
# mr.metaplantsCassava<-rma.mv(yi = percent_decrease_N,
#                              V = Var,
#                              W = replicate_facilities,
# #                              mods = ~difference+nitrogen, 
# # mods = ~difference, 
# #                              #dogg random = ~1|Sitecode,
#                              data=metaplantsCassava)
# 
# summary(mr.metaplantsCassava)
# 
# Cassavaout<-data.frame(
#   "pred"=mr.metaplantsCassava$b,
#   "se"=mr.metaplantsCassava$se,
#   "p"=mr.metaplantsCassava$pval,
#   "elevated"=mean(metaplantsCassava$elevated),
#   "fieldpots"= mean(metaplantsCassava[,"field01"], na.rm=TRUE),
#   "experiments"=length(metaplantsCassava$percent_decrease_N))
# 

# predict.mr.d.metaplantsCassava.lowN<-predict(mr.metaplantsCassava, newmods=c(178.22,1))
# predict.mr.d.metaplantsCassava.moderateN<-predict(mr.metaplantsCassava, newmods=c(178.22,2))
# predict.mr.d.metaplantsCassava.highN<-predict(mr.metaplantsCassava, newmods=c(178.22,3))
#  predictCassava<-rbind(predict.mr.d.metaplantsCassava.lowN,predict.mr.d.metaplantsCassava.moderateN,predict.mr.d.metaplantsCassava.highN)
# View(predictCassava)



metaplantsPotato<-subset(metaplantsc550,plantorder==10)
length(metaplantsPotato$percent_decrease_N) #if it's >4, continue, otherwise... B
range(metaplantsPotato$difference) #if it has 2 numbers and they're sane, continue, otherwise...
range(metaplantsPotato$nitrogen) #if it has 2 numbers and they're sane, continue, otherwise...
#only well fertilised... UNLIKELY but whatever
range(metaplantsPotato$field01) #0 and 1 otherwise B

mr.metaplantsPotato<-rma.mv(yi = percent_decrease_N,
                            V = Var,
                            W = replicate_facilities,
#                             mods = ~difference,
#                             mods = ~difference+field01, 
                            #dogg random = ~1|Sitecode,
                            data=metaplantsPotato)

summary(mr.metaplantsPotato)

Potatoout<-data.frame(
  "pred"=mr.metaplantsPotato$b,
  "se"=mr.metaplantsPotato$se,
  "p"=mr.metaplantsPotato$pval,
  "elevated"=mean(metaplantsPotato$elevated),
  "fieldpots"= mean(metaplantsPotato[,"field01"], na.rm=TRUE),
  "experiments"=length(metaplantsPotato$percent_decrease_N))


# 
# predict.mr.d.metaplantsPotato.lowN<-predict(mr.metaplantsPotato, newmods=c(178.22,1))
# predict.mr.d.metaplantsPotato.moderateN<-predict(mr.metaplantsPotato, newmods=c(178.22,1))
# predict.mr.d.metaplantsPotato.highN<-predict(mr.metaplantsPotato, newmods=c(178.22,1))
# predictPotato<-rbind(predict.mr.d.metaplantsPotato.lowN,predict.mr.d.metaplantsPotato.moderateN,predict.mr.d.metaplantsPotato.highN)
# View(predictPotato)




metaplantsSugarbeet<-subset(metaplantsc550,plantorder==11)

length(metaplantsSugarbeet$percent_decrease_N) #if it's >4, continue, otherwise... B
#3
range(metaplantsSugarbeet$difference) #if it has 2 numbers and they're sane, continue, otherwise...
#1
range(metaplantsSugarbeet$nitrogen) #if it has 2 numbers and they're sane, continue, otherwise...
#2
range(metaplantsSugarbeet$field01) #0 and 1 otherwise B
#0

# doesn't work once exclude data
mr.metaplantsSugarbeet<-rma.mv(yi = percent_decrease_N,
                               V = Var,
                               W = replicate_facilities,
#                                mods = ~nitrogen, 
                               # #dogg random = ~1|Sitecode,
                               data=metaplantsSugarbeet)

summary(mr.metaplantsSugarbeet)

Sugarbeetout<-data.frame(
  "pred"=mr.metaplantsSugarbeet$b,
  "se"=mr.metaplantsSugarbeet$se,
  "p"=mr.metaplantsSugarbeet$pval,
  "elevated"=mean(metaplantsSugarbeet$elevated),
  "fieldpots"= mean(metaplantsSugarbeet[,"field01"], na.rm=TRUE),
  "experiments"=length(metaplantsSugarbeet$percent_decrease_N))

# predict.mr.d.metaplantsSugarbeet.lowN<-predict(mr.metaplantsSugarbeet, newmods=1)
# predict.mr.d.metaplantsSugarbeet.moderateN<-predict(mr.metaplantsSugarbeet, newmods=2)
# predict.mr.d.metaplantsSugarbeet.highN<-predict(mr.metaplantsSugarbeet, newmods=2)
# View(predictSugarbeet)

# 
# predict.mr.d.metaplantsSugarbeet.lowN<-predict.mr.d.metaplantsRoot.lowN
#   predict.mr.d.metaplantsSugarbeet.moderateN<-predict.mr.d.metaplantsRoot.moderateN
#   predict.mr.d.metaplantsSugarbeet.highN<-predict.mr.d.metaplantsRoot.highN
# 
# predictSugarbeet<-rbind(predict.mr.d.metaplantsSugarbeet.lowN,predict.mr.d.metaplantsSugarbeet.moderateN,predict.mr.d.metaplantsSugarbeet.highN)


metaplantsBeans<-subset(metaplantsc550,plantorder==12)

length(metaplantsBeans$percent_decrease_N) #if it's >4, continue, otherwise... B
range(metaplantsBeans$difference) #if it has 2 numbers and they're sane, continue, otherwise...
range(metaplantsBeans$nitrogen) #if it has 2 numbers and they're sane, continue, otherwise...
range(metaplantsBeans$field01) #0 and 1 otherwise B

mr.metaplantsBeans<-rma.mv(yi = percent_decrease_N,
                           V = Var,
                           W = replicate_facilities,
                          # mods = ~difference+nitrogen, 
#                           mods = ~difference, 
                           #dogg random = ~1|Sitecode,
                           data=metaplantsBeans)

summary(mr.metaplantsBeans)

Beansout<-data.frame(
  "pred"=mr.metaplantsBeans$b,
  "se"=mr.metaplantsBeans$se,
  "p"=mr.metaplantsBeans$pval,
  "elevated"=mean(metaplantsBeans$elevated),
  "fieldpots"= mean(metaplantsBeans[,"field01"], na.rm=TRUE),
  "experiments"=length(metaplantsBeans$percent_decrease_N))

# 
# predict.mr.d.metaplantsBeans.lowN<-predict(mr.metaplantsBeans, newmods=c(178.22))
# predict.mr.d.metaplantsBeans.moderateN<-predict(mr.metaplantsBeans, newmods=c(178.22))
# predict.mr.d.metaplantsBeans.highN<-predict(mr.metaplantsBeans, newmods=c(178.22))

#doesn't work once data excluded
# predict.mr.d.metaplantsBeans.lowN<-predict(mr.metaplantsBeans, newmods=c(178.22,1))
# predict.mr.d.metaplantsBeans.moderateN<-predict(mr.metaplantsBeans, newmods=c(178.22,2))
# predict.mr.d.metaplantsBeans.highN<-predict(mr.metaplantsBeans, newmods=c(178.22,2))
# 
# 
# predictBeans<-rbind(predict.mr.d.metaplantsBeans.lowN,predict.mr.d.metaplantsBeans.moderateN,predict.mr.d.metaplantsBeans.highN)
# View(predictBeans)
# 

metaplantsPeas<-subset(metaplants,plantorder==13)

length(metaplantsPeas$percent_decrease_N) #if it's >4, continue, otherwise... B
range(metaplantsPeas$difference) #if it has 2 numbers and they're sane, continue, otherwise...
range(metaplantsPeas$nitrogen) #if it has 2 numbers and they're sane, continue, otherwise...
#not entirely sure why there are two numbers here. better go back to the data
range(metaplantsPeas$field01) #0 and 1 otherwise B

mr.metaplantsPeas<-rma.mv(yi = percent_decrease_N,
                          V = Var,
                          W = replicate_facilities,
                          mods = ~difference+field01, 
                          #dogg random = ~1|Sitecode,
                          data=metaplantsPeas)

summary(mr.metaplantsPeas)

predict.mr.d.metaplantsPeas.lowN<-predict(mr.metaplantsPeas, newmods=c(178.22,1))
predict.mr.d.metaplantsPeas.moderateN<-predict(mr.metaplantsPeas, newmods=c(178.22,1))
predict.mr.d.metaplantsPeas.highN<-predict(mr.metaplantsPeas, newmods=c(178.22,1))
predictPeas<-rbind(predict.mr.d.metaplantsPeas.lowN,predict.mr.d.metaplantsPeas.moderateN,predict.mr.d.metaplantsPeas.highN)
View(predictPeas)


Peasout<-data.frame(
  "pred"=predict.mr.d.metaplantsPeas.moderateN$pred,
  "se"=predict.mr.d.metaplantsPeas.moderateN$se,
  "p"="d and f <0.1",
  "elevated"=mean(metaplantsPeas$elevated),
  "fieldpots"= mean(metaplantsPeas[,"field01"], na.rm=TRUE),
  "experiments"=length(metaplantsPeas$percent_decrease_N))




metaplantsChickpea<-subset(metaplants,plantorder==14)
# doesn't work now we're excluding sites
# length(metaplantsChickpea$percent_decrease_N) #if it's >4, continue, otherwise... B
# range(metaplantsChickpea$difference) #if it has 2 numbers and they're sane, continue, otherwise...
# range(metaplantsChickpea$nitrogen) #if it has 2 numbers and they're sane, continue, otherwise...
# range(metaplantsChickpea$field01) #0 and 1 otherwise B
# 
mr.metaplantsChickpea<-rma.mv(yi = percent_decrease_N,
                              V = Var,
                              W = replicate_facilities,
#                               mods = ~difference+field01+nitrogen, 
# mods = ~difference, 
#                               #dogg random = ~1|Sitecode,
                              data=metaplantsChickpea)

summary(mr.metaplantsChickpea)


# predict.mr.d.metaplantsChickpea.lowN<-predict.mr.d.metaplantsPulse.lowN
# predict.mr.d.metaplantsChickpea.moderateN<-predict.mr.d.metaplantsPulse.moderateN
# predict.mr.d.metaplantsChickpea.highN<-predict.mr.d.metaplantsPulse.highN
# predict.mr.d.metaplantsChickpea.lowN<-predict(mr.metaplantsChickpea)
# predict.mr.d.metaplantsChickpea.moderateN<-predict(mr.metaplantsChickpea)
# predict.mr.d.metaplantsChickpea.highN<-predict(mr.metaplantsChickpea)
# predictChickpea<-rbind(predict.mr.d.metaplantsChickpea.lowN,predict.mr.d.metaplantsChickpea.moderateN,predict.mr.d.metaplantsChickpea.highN)
# View(predictChickpea)


Chickpeaout<-data.frame(
  "pred"=mr.metaplantsChickpea$b,
  "se"=mr.metaplantsChickpea$se,
  "p"=mr.metaplantsChickpea$pval,
  "elevated"=mean(metaplantsChickpea$elevated),
  "fieldpots"= mean(metaplantsChickpea[,"field01"], na.rm=TRUE),
  "experiments"=length(metaplantsChickpea$percent_decrease_N))


metaplantsSoy<-subset(metaplantsc550,plantorder==15)
length(metaplantsSoy$percent_decrease_N) #if it's >4, continue, otherwise... B
range(metaplantsSoy$difference) #if it has 2 numbers and they're sane, continue, otherwise...
range(metaplantsSoy$nitrogen) #if it has 2 numbers and they're sane, continue, otherwise...
range(metaplantsSoy$field01) #0 and 1 otherwise B

mr.metaplantsSoy<-rma.mv(yi = percent_decrease_N,
                         V = Var,
                         W = replicate_facilities,
#                          mods = ~difference+field01+nitrogen, 
#                          mods = ~difference+field01,
#                          mods = ~difference+field01+nitrogen, 
                         
                         #dogg random = ~1|Sitecode,
                         data=metaplantsSoy)

summary(mr.metaplantsSoy)

Soyout<-data.frame(
  "pred"=mr.metaplantsSoy$b,
  "se"=mr.metaplantsSoy$se,
  "p"=mr.metaplantsSoy$pval,
  "elevated"=mean(metaplantsSoy$elevated),
  "fieldpots"= mean(metaplantsSoy[,"field01"], na.rm=TRUE),
  "experiments"=length(metaplantsSoy$percent_decrease_N))



# predict.mr.d.metaplantsSoy.lowN<-predict(mr.metaplantsSoy, newmods=c(178.22,1,1))
# predict.mr.d.metaplantsSoy.moderateN<-predict(mr.metaplantsSoy, newmods=c(178.22,1,2))
# predict.mr.d.metaplantsSoy.highN<-predict(mr.metaplantsSoy, newmods=c(178.22,1,2))
# predictSoy<-rbind(predict.mr.d.metaplantsSoy.lowN,predict.mr.d.metaplantsSoy.moderateN,predict.mr.d.metaplantsSoy.highN)
# View(predictSoy)





# metaplantsPeanut<-subset(metaplantsc550,plantorder==16)
# 
# length(metaplantsPeanut$percent_decrease_N) #if it's >4, continue, otherwise... B
# range(metaplantsPeanut$difference) #if it has 2 numbers and they're sane, continue, otherwise...
# range(metaplantsPeanut$nitrogen) #if it has 2 numbers and they're sane, continue, otherwise...
# range(metaplantsPeanut$field01) #0 and 1 otherwise B
# 
# mr.metaplantsPeanut<-rma.mv(yi = percent_decrease_N,
#                             V = Var,
#                             W = replicate_facilities,
# #                             mods = ~difference, 
# #                             #dogg random = ~1|Sitecode,
#                             data=metaplantsPeanut)
# 
# summary(mr.metaplantsPeanut)
# 
# Peanutout<-data.frame(
#   "pred"=mr.metaplantsPeanut$b,
#   "se"=mr.metaplantsPeanut$se,
#   "p"=mr.metaplantsPeanut$pval,
#   "elevated"=mean(metaplantsPeanut$elevated),
#   "fieldpots"= mean(metaplantsPeanut[,"field01"], na.rm=TRUE),
#   "experiments"=length(metaplantsPeanut$percent_decrease_N))

# predict.mr.d.metaplantsPeanut.lowN<-predict(mr.metaplantsPeanut, newmods=178.22)
# predict.mr.d.metaplantsPeanut.moderateN<-predict(mr.metaplantsPeanut, newmods=178.22)
# predict.mr.d.metaplantsPeanut.highN<-predict(mr.metaplantsPeanut, newmods=178.22)
# predictPeanut<-rbind(predict.mr.d.metaplantsPeanut.lowN,predict.mr.d.metaplantsPeanut.moderateN,predict.mr.d.metaplantsPeanut.highN)
# View(predictPeanut)




# metaplants17<-sunflowers

metaplantsMustard<-subset(metaplants,plantorder==18)

length(metaplantsMustard$percent_decrease_N) #if it's >4, continue, otherwise... B
range(metaplantsMustard$difference) #if it has 2 numbers and they're sane, continue, otherwise...
range(metaplantsMustard$nitrogen) #if it has 2 numbers and they're sane, continue, otherwise...
range(metaplantsMustard$field01) #0 and 1 otherwise B

mr.metaplantsMustard<-rma.mv(yi = percent_decrease_N,
                             V = Var,
                             W = replicate_facilities,
                             mods = ~difference+nitrogen,
#                              mods = ~difference+field01+nitrogen, 
                             #dogg random = ~1|Sitecode,
                             data=metaplantsMustard)

summary(mr.metaplantsMustard)

predict.mr.d.metaplantsMustard.lowN<-predict(mr.metaplantsMustard, newmods=c(178.22,1))
predict.mr.d.metaplantsMustard.moderateN<-predict(mr.metaplantsMustard, newmods=c(178.22,2))
predict.mr.d.metaplantsMustard.highN<-predict(mr.metaplantsMustard, newmods=c(178.22,3))
predictMustard<-rbind(predict.mr.d.metaplantsMustard.lowN,predict.mr.d.metaplantsMustard.moderateN,predict.mr.d.metaplantsMustard.highN)
# View(predictMustard)

predictMustarddf<-data.frame(matrix(unlist(predictMustard), nrow=3, byrow=F))
names(predictMustarddf)[1:2]<-c("pred","se")



Mustardout<-data.frame(
  "pred"=predictMustarddf[2,1],
  "se"=predictMustarddf[2,2],
  "p"="fert p<0.1, difference p<0.001, intercept p <0.001",
  "elevated"=mean(metaplantsMustard$elevated),
  "fieldpots"= mean(metaplantsMustard[,"field01"], na.rm=TRUE),
  "experiments"=length(metaplantsMustard$percent_decrease_N))


#metaplants19<-???
#veges metaplants20<-subset(metaplants,plantorder==20)
#fruit metaplants21<-subset(metaplants,plantorder==21)




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
#                        Cassavaout, 
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
  
output_only_sig$plant= c("Wheat","Rice", "Barley","Maize","Sorghum","C3", "C4","Grain","Root","Potato",
#            "Cassava", 
           "Sugarbeet","Pulse","Peas","Beans","Chickpea", "Oil","Soy",
# "Peanut",
"Mustard","Veg")

# 
# 
# predictions.type.lowN<-data.frame("N_level"=rep(1),"Plant_Type"=c(
#   "C3_cereal",
#   "C4_cereal",
#   "all grains",
#   "root_vegetable",
#   "pulses",
#   "oilcrop",
#   "vegetable",
#   "wheat",
#   "rice",
#   "barley",
#   "maize", 
#   "sorghum", 
#   "cassava", 
#   "potato", 
#   "sugarbeet", 
#   "beans", 
#   "peas", 
#   "chickpea", 
#   "soybean", 
#   "peanut", 
#   "rapeseed"),
#                                   rbind(predictC3[1,],
#                                         predictC4[1,], 
#                                         predictGrain[1,],
#                                         predictRoot[1,],
#                                         predictPulse[1,],
#                                         predictOil[1,],
#                                         predictVeg[1,],
#                                         predictWheat[1,],
#                                         predictRice[1,],
#                                         predictBarley[1,],
#                                         predictMaize[1,],
#                                         predictSorghum[1,],
#                                         predictCassava[1,],
#                                         predictPotato[1,],
#                                         predictSugarbeet[1,],
#                                         predictBeans[1,],
#                                         predictPeas[1,],
#                                         predictChickpea[1,],
#                                         predictSoy[1,],
#                                         predictPeanut[1,],
#                                         predictMustard[1,]
#                                         ))
# 
# predictions.type.medN<-data.frame("N_level"=rep(2),"Plant_Type"=c(
#   "C3_cereal",
#   "C4_cereal",
#   "all grains",
#   "root_vegetable",
#   "pulses",
#   "oilcrop",
#   "vegetable",
#   "wheat",
#   "rice",
#   "barley",
#   "maize", 
#   "sorghum", 
#   "cassava", 
#   "potato", 
#   "sugarbeet", 
#   "beans", 
#   "peas", 
#   "chickpea", 
#   "soybean", 
#   "peanut", 
#   "rapeseed"),
#   rbind(predictC3[2,],
#         predictC4[2,], 
#         predictGrain[2,],
#         predictRoot[2,],
#         predictPulse[2,],
#         predictOil[2,],
#         predictVeg[2,],
#         predictWheat[2,],
#         predictRice[2,],
#         predictBarley[2,],
#         predictMaize[2,],
#         predictSorghum[2,],
#         predictCassava[2,],
#         predictPotato[2,],
#         predictSugarbeet[2,],
#         predictBeans[2,],
#         predictPeas[2,],
#         predictChickpea[2,],
#         predictSoy[2,],
#         predictPeanut[2,],
#         predictMustard[2,]
#   ))
# 
# predictions.type.HighN<-data.frame("N_level"=rep(3),"Plant_Type"=c(
#   "C3_cereal",
#   "C4_cereal",
#   "all grains",
#   "root_vegetable",
#   "pulses",
#   "oilcrop",
#   "vegetable",
#   "wheat",
#   "rice",
#   "barley",
#   "maize", 
#   "sorghum", 
#   "cassava", 
#   "potato", 
#   "sugarbeet", 
#   "beans", 
#   "peas", 
#   "chickpea", 
#   "soybean", 
#   "peanut", 
#   "rapeseed"),
#   rbind(predictC3[3,],
#         predictC4[3,], 
#         predictGrain[3,],
#         predictRoot[3,],
#         predictPulse[3,],
#         predictOil[3,],
#         predictVeg[3,],
#         predictWheat[3,],
#         predictRice[3,],
#         predictBarley[3,],
#         predictMaize[3,],
#         predictSorghum[3,],
#         predictCassava[3,],
#         predictPotato[3,],
#         predictSugarbeet[3,],
#         predictBeans[3,],
#         predictPeas[3,],
#         predictChickpea[3,],
#         predictSoy[3,],
#         predictPeanut[3,],
#         predictMustard[3,]
#   ))
# 
# save(predictions.type.lowN, file="1_CO2/outputs/predictions.type.lowN.Rdata")
# save(predictions.type.medN, file="1_CO2/outputs/predictions.type.medN.Rdata")
# save(predictions.type.HighN, file="1_CO2/outputs/predictions.type.HighN.Rdata")


# predictions.type<-data.frame(rbind(predictions.type.lowN,predictions.type.medN,predictions.type.HighN))
predictions.type<-output_only_sig
predictions.forexport<-data.frame(lapply(predictions.type, function(predictions) factor(unlist(predictions))))
write.csv(predictions.forexport,"1_CO2/outputs/predictions-noSitegrouping-fullrange.csv")

