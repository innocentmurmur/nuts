require("metafor")
metaplants<-read.csv("1_CO2/inputs/meta_plants.csv")


metaplants$difference<-metaplants$elevated-metaplants$ambient

# mr.metaplants<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants[,"elevated"]+~metaplants[,"field01"], ni = replicate_facilities,   data=metaplants,   measure="MN")
# summary(mr.metaplants)
# predict.mr.metaplantsGrain<-predict(mr.metaplantsGrain, newmods=c(550,1))#predict for field = 1
# predict.mr.metaplantsGrain
###by type####
# metaplantsGrain<-subset(metaplants,type=="C3_cereal" | type=="C4_cereal")
# View(metaplantsGrain)
# 
# mr.metaplantsGrain<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsGrain[,"elevated"]+~metaplantsGrain[,"field01"], ni = replicate_facilities,   data=metaplantsGrain,   measure="MN")
# # mr.metaplantsallGrain<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsGrain[,"elevated"], ni = replicate_facilities,   data=metaplantsGrain,   measure="MN")
# 
# summary(mr.metaplantsGrain)
# predict.mr.metaplantsGrain<-predict(mr.metaplantsGrain, newmods=c(550,1))#predict for field = 1
# 
#                                     
# predict.mr.metaplantsGrain
# #probably better remove the 200 ppm ambient expt if you're going with this - done
# 
# 
# metaplantsC3<-subset(metaplants,type=="C3_cereal")
# metaplantsC4<-subset(metaplants,type=="C4_cereal")
# metaplantsRoot<-subset(metaplants,type=="root_vegetable")
# metaplantsPulse<-subset(metaplants,type=="pulses")
# metaplantsOil<-subset(metaplants,type=="oilcrop")
# metaplantsVeg<-subset(metaplants,type=="vegetable")
# metaplantsFruit<-subset(metaplants,type=="fruit")
# 
# 
# mr.metaplantsC3<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsC3[,"elevated"]+~metaplantsC3[,"field01"], ni = replicate_facilities,   data=metaplantsC3,   measure="MN")
# mr.metaplantsC4<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsC4[,"elevated"]+~metaplantsC4[,"field01"], ni = replicate_facilities,   data=metaplantsC4,   measure="MN")
# mr.metaplantsRoot<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsRoot[,"elevated"]+~metaplantsRoot[,"field01"], ni = replicate_facilities,   data=metaplantsRoot,   measure="MN")
# mr.metaplantsPulse<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsPulse[,"elevated"]+~metaplantsPulse[,"field01"], ni = replicate_facilities,   data=metaplantsPulse,   measure="MN")
# mr.metaplantsOil<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsOil[,"elevated"]+~metaplantsOil[,"field01"], ni = replicate_facilities,   data=metaplantsOil,   measure="MN")
# mr.metaplantsVeg<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsVeg[,"elevated"]+~metaplantsVeg[,"field01"], ni = replicate_facilities,   data=metaplantsVeg,   measure="MN")
# mr.metaplantsFruit<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsFruit[,"elevated"]+~metaplantsFruit[,"field01"], ni = replicate_facilities,   data=metaplantsFruit,   measure="MN")
# 
# # mr.metaplantsallC3<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsC3[,"elevated"], ni = replicate_facilities,   data=metaplantsC3,   measure="MN")
# # mr.metaplantsallC4<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsC4[,"elevated"], ni = replicate_facilities,   data=metaplantsC4,   measure="MN")
# # mr.metaplantsallRoot<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsRoot[,"elevated"], ni = replicate_facilities,   data=metaplantsRoot,   measure="MN")
# # mr.metaplantsallPulse<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsPulse[,"elevated"], ni = replicate_facilities,   data=metaplantsPulse,   measure="MN")
# # mr.metaplantsallOil<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsOil[,"elevated"], ni = replicate_facilities,   data=metaplantsOil,   measure="MN")
# # mr.metaplantsallVeg<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsVeg[,"elevated"], ni = replicate_facilities,   data=metaplantsVeg,   measure="MN")
# # mr.metaplantsallFruit<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsFruit[,"elevated"], ni = replicate_facilities,   data=metaplantsFruit,   measure="MN")
# 
# predict.mr.metaplantsC3<-predict(mr.metaplantsC3, newmods=c(550,1))
# predict.mr.metaplantsC4<-predict(mr.metaplantsC4, newmods=c(550,1))
# predict.mr.metaplantsRoot<-predict(mr.metaplantsRoot, newmods=c(550,1))
# predict.mr.metaplantsPulse<-predict(mr.metaplantsPulse, newmods=c(550,1))
# predict.mr.metaplantsOil<-predict(mr.metaplantsOil, newmods=c(550,1))
# predict.mr.metaplantsVeg<-predict(mr.metaplantsVeg, newmods=c(550,1))
# predict.mr.metaplantsFruit<-predict(mr.metaplantsFruit, newmods=c(550,1))
# 
# # predict.mr.metaplantsC3<-predict(mr.metaplantsallC3, newmods=c(550,1))
# # predict.mr.metaplantsC4<-predict(mr.metaplantsallC4, newmods=c(550,1))
# # predict.mr.metaplantsRoot<-predict(mr.metaplantsallRoot, newmods=c(550,1))
# # predict.mr.metaplantsPulse<-predict(mr.metaplantsallPulse, newmods=c(550,1))
# # predict.mr.metaplantsOil<-predict(mr.metaplantsallOil, newmods=c(550,1))
# # predict.mr.metaplantsVeg<-predict(mr.metaplantsallVeg, newmods=c(550,1))
# # predict.mr.metaplantsFruit<-predict(mr.metaplantsallFruit, newmods=c(550,1))
# 
# plantmr.predictions.type<-rbind(
#   predict.mr.metaplantsC3,
#   predict.mr.metaplantsC4,
#   predict.mr.metaplantsRoot,
#   predict.mr.metaplantsPulse,
#   predict.mr.metaplantsOil,
#   predict.mr.metaplantsVeg
# )
# write.csv(plantmr.predictions.type,  file="~/Rdata/nutrients/predictmrmetatypesall.csv")
# 
###by plant####
# 
# 
# metaplants1<-subset(metaplants,plantorder==1)
# metaplants2<-subset(metaplants,plant=="rice")
# View(metaplants2)
# metaplants3<-subset(metaplants,plantorder==3)
# metaplants4<-subset(metaplants,plantorder==4)
# metaplants5<-subset(metaplants,plantorder==5)
# metaplants6<-subset(metaplants,plantorder==6)
# metaplants7<-subset(metaplants,plantorder==7)
# metaplants8<-subset(metaplants,plantorder==8)
# metaplants9<-subset(metaplants,plantorder==9)
# metaplants10<-subset(metaplants,plantorder==10)
# metaplants11<-subset(metaplants,plantorder==11)
# metaplants12<-subset(metaplants,plantorder==12)
# metaplants13<-subset(metaplants,plantorder==13)
# metaplants14<-subset(metaplants,plantorder==14)
# metaplants15<-subset(metaplants,plantorder==15)
# metaplants16<-subset(metaplants,plantorder==16)
# metaplants17<-subset(metaplants,plantorder==17)
# metaplants18<-subset(metaplants,plantorder==18)
# metaplants19<-subset(metaplants,plantorder==19)
# metaplants20<-subset(metaplants,plantorder==20)
# metaplants21<-subset(metaplants,plantorder==21)
# 
# 
# mr.metaplants1<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants1[,"elevated"]+~metaplants1[,"field01"], ni = replicate_facilities,   data=metaplants1,   measure="MN")
# mr.metaplants2<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants2[,"elevated"]+~metaplants2[,"field01"], ni = replicate_facilities,   data=metaplants2,   measure="MN")
# mr.metaplants3<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants3[,"elevated"]+~metaplants3[,"field01"], ni = replicate_facilities,   data=metaplants3,   measure="MN")
# mr.metaplants4<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants4[,"elevated"]+~metaplants4[,"field01"], ni = replicate_facilities,   data=metaplants4,   measure="MN")
# mr.metaplants5<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants5[,"elevated"]+~metaplants5[,"field01"], ni = replicate_facilities,   data=metaplants5,   measure="MN")
# mr.metaplants6<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants6[,"elevated"]+~metaplants6[,"field01"], ni = replicate_facilities,   data=metaplants6,   measure="MN")
# mr.metaplants7<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants7[,"elevated"]+~metaplants7[,"field01"], ni = replicate_facilities,   data=metaplants7,   measure="MN")
# mr.metaplants8<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants8[,"elevated"]+~metaplants8[,"field01"], ni = replicate_facilities,   data=metaplants8,   measure="MN")
# mr.metaplants9<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants9[,"elevated"]+~metaplants9[,"field01"], ni = replicate_facilities,   data=metaplants9,   measure="MN")
# mr.metaplants10<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants10[,"elevated"]+~metaplants10[,"field01"], ni = replicate_facilities,   data=metaplants10,   measure="MN")
# mr.metaplants11<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants11[,"elevated"]+~metaplants11[,"field01"], ni = replicate_facilities,   data=metaplants11,   measure="MN")
# mr.metaplants12<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants12[,"elevated"]+~metaplants12[,"field01"], ni = replicate_facilities,   data=metaplants12,   measure="MN")
# mr.metaplants13<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants13[,"elevated"]+~metaplants13[,"field01"], ni = replicate_facilities,   data=metaplants13,   measure="MN")
# mr.metaplants14<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants14[,"elevated"]+~metaplants14[,"field01"], ni = replicate_facilities,   data=metaplants14,   measure="MN")
# mr.metaplants15<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants15[,"elevated"]+~metaplants15[,"field01"], ni = replicate_facilities,   data=metaplants15,   measure="MN")
# mr.metaplants16<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants16[,"elevated"]+~metaplants16[,"field01"], ni = replicate_facilities,   data=metaplants16,   measure="MN")
# mr.metaplants17<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants17[,"elevated"]+~metaplants17[,"field01"], ni = replicate_facilities,   data=metaplants17,   measure="MN")
# mr.metaplants18<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants18[,"elevated"]+~metaplants18[,"field01"], ni = replicate_facilities,   data=metaplants18,   measure="MN")
# mr.metaplants19<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants19[,"elevated"]+~metaplants19[,"field01"], ni = replicate_facilities,   data=metaplants19,   measure="MN")
# mr.metaplants20<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants20[,"elevated"]+~metaplants20[,"field01"], ni = replicate_facilities,   data=metaplants20,   measure="MN")
# mr.metaplants21<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants21[,"elevated"]+~metaplants21[,"field01"], ni = replicate_facilities,   data=metaplants21,   measure="MN")
# 
# # mr.metaplantsall1<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants1[,"elevated"], ni = replicate_facilities,   data=metaplants1,   measure="MN")
# mr.metaplantsall2<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants2[,"elevated"], ni = replicate_facilities,   data=metaplants2,   measure="MN")
# # mr.metaplantsall3<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants3[,"elevated"], ni = replicate_facilities,   data=metaplants3,   measure="MN")
# mr.metaplantsall4<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants4[,"elevated"], ni = replicate_facilities,   data=metaplants4,   measure="MN")
# mr.metaplantsall5<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants5[,"elevated"], ni = replicate_facilities,   data=metaplants5,   measure="MN")
# mr.metaplantsall6<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants6[,"elevated"], ni = replicate_facilities,   data=metaplants6,   measure="MN")
# # mr.metaplantsall7<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants7[,"elevated"], ni = replicate_facilities,   data=metaplants7,   measure="MN")
# mr.metaplantsall8<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants8[,"elevated"], ni = replicate_facilities,   data=metaplants8,   measure="MN")
# mr.metaplantsall9<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants9[,"elevated"], ni = replicate_facilities,   data=metaplants9,   measure="MN")
# # mr.metaplantsall10<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants10[,"elevated"], ni = replicate_facilities,   data=metaplants10,   measure="MN")
# mr.metaplantsall11<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants11[,"elevated"], ni = replicate_facilities,   data=metaplants11,   measure="MN")
# mr.metaplantsall12<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants12[,"elevated"], ni = replicate_facilities,   data=metaplants12,   measure="MN")
# mr.metaplantsall13<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants13[,"elevated"], ni = replicate_facilities,   data=metaplants13,   measure="MN")
# mr.metaplantsall14<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants14[,"elevated"], ni = replicate_facilities,   data=metaplants14,   measure="MN")
# # mr.metaplantsall15<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants15[,"elevated"], ni = replicate_facilities,   data=metaplants15,   measure="MN")
# mr.metaplantsall16<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants16[,"elevated"], ni = replicate_facilities,   data=metaplants16,   measure="MN")
# mr.metaplantsall17<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants17[,"elevated"], ni = replicate_facilities,   data=metaplants17,   measure="MN")
# # mr.metaplantsall18<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants18[,"elevated"], ni = replicate_facilities,   data=metaplants18,   measure="MN")
# mr.metaplantsall19<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants19[,"elevated"], ni = replicate_facilities,   data=metaplants19,   measure="MN")
# # mr.metaplantsall20<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants20[,"elevated"], ni = replicate_facilities,   data=metaplants20,   measure="MN")
# mr.metaplantsall21<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants21[,"elevated"], ni = replicate_facilities,   data=metaplants21,   measure="MN")
#  
# 
# 
# predict.mr.metaplants1<-predict(mr.metaplants1, newmods=c(550,1))
#   predict.mr.metaplants2<-predict(mr.metaplants2, newmods=c(550,1))
#   predict.mr.metaplants3<-predict(mr.metaplants3, newmods=c(550,1))
#   NA
#   NA
#   NA
#   predict.mr.metaplants7<-predict(mr.metaplants7, newmods=c(550,1))
#   NA
#   predict.mr.metaplants9<-predict(mr.metaplants9, newmods=c(550,1))
#   predict.mr.metaplants10<-predict(mr.metaplants10, newmods=c(550,1))
#   NA,
#   predict.mr.metaplants12<-predict(mr.metaplants12, newmods=c(550,1))
#   predict.mr.metaplants13<-predict(mr.metaplants13, newmods=c(550,1))
#   NA,
#   predict.mr.metaplants15<-predict(mr.metaplants15, newmods=c(550,1))
#   predict.mr.metaplants16<-predict(mr.metaplants16, newmods=c(550,1))
#   NA,
#   predict.mr.metaplants18<-predict(mr.metaplants18, newmods=c(550,1))
#   NA,
#   predict.mr.metaplants20<-predict(mr.metaplants20, newmods=c(550,1))
#   NA
# 
# 
# predict.mr.metaplants<-rbind(
#   predict.mr.metaplants1,
#   predict.mr.metaplants2,
#   predict.mr.metaplants3,
#   NA,
#   NA,
#   NA,
#   predict.mr.metaplants7,
#   NA,
#   predict.mr.metaplants9,
#   predict.mr.metaplants10,
#   NA,
#   predict.mr.metaplants12,
#   predict.mr.metaplants13,
#   NA,
#   predict.mr.metaplants15,
#   predict.mr.metaplants16,
#   NA,
#   predict.mr.metaplants18,
#   NA,
#   predict.mr.metaplants20,
#   NA
# )
# 
# write.csv(predict.mr.metaplants, file="~/Rdata/nutrients/predictmrmetaplants.csv")
# 
# View(predict.mr.metaplants)


####same with regression on difference rather than elevated####
###by type####
#type<-c("Grain","C3","C4","Root","Pulse","Oil","vegetable",1:21)

plantmr.d.predictions.type=NULL

for(PlantType in levels(metaplants$type)[-3])
  {metaplants.sub<-subset(metaplants,type==PlantType)
   ifelse(
     max(metaplants.sub[,"field01"],na.rm=TRUE)==1 && min(metaplants.sub[,"field01"])==0,
     mr.d.metaplants.sub<-rma(mi = percent_decrease_N, sdi = SD, mods = ~metaplants.sub[,"difference"]+~metaplants.sub[,"field01"], ni = replicate_facilities, data=metaplants.sub,   measure="MN"),
     mr.d.metaplants.sub<-rma(mi = percent_decrease_N, sdi = SD, mods = ~metaplants.sub[,"difference"], ni = replicate_facilities, data=metaplants.sub,   measure="MN")
     )
   ifelse(
     max(metaplants.sub[,"field01"],na.rm=TRUE)==1 && min(metaplants.sub[,"field01"],na.rm=TRUE)==0,
     predict.mr.d.metaplants.sub<-predict(mr.d.metaplants.sub, newmods=c(178.22,1)),
     predict.mr.d.metaplants.sub<-predict(mr.d.metaplants.sub, newmods=178.22)
     )
   plantmr.d.predictions.type<-rbind(plantmr.d.predictions.type,predict.mr.d.metaplants.sub)
}



predictions.type<-data.frame(
  "Plant_Type"=levels(metaplants$type)[-3],
  plantmr.d.predictions.type
  )


# #for some reason, the C4 cereal estimates the intercepts or something even though I want a point at 178.22
# metaplantsC4<-subset(metaplants,type=="C4_cereal")
# mr.d.metaplantsC4<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsC4[,"difference"], ni = replicate_facilities,   data=metaplantsC4,   measure="MN")
# predict.mr.d.metaplantsC4<-predict(mr.d.metaplantsC4, newmods=178.22)

#but don't do this unless you're sure C4 is still in second position.
# predictions.type[2,2:11]<-predict.mr.d.metaplantsC4


#need a grain combo for cereals, other
metaplantsGrain<-subset(metaplants,type=="C3_cereal" | type=="C4_cereal")
mr.d.metaplantsGrain<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsGrain[,"difference"]+~metaplantsGrain[,"field01"], ni = replicate_facilities,   data=metaplantsGrain,   measure="MN")
predict.mr.d.metaplantsGrain<-predict(mr.d.metaplantsGrain, newmods=c(178.22,1))
predictions.type[7,2:11]<-predict.mr.d.metaplantsGrain
levels(predictions.type[,1])<-c( "C3_cereal","C4_cereal","oilcrop","pulses","root_vegetable","vegetable","all grains")
predictions.type[7,1]<-"all grains"

# 
metaplantsC3<-subset(metaplants,type=="C3_cereal")
# metaplantsC4<-subset(metaplants,type=="C4_cereal")
# metaplantsRoot<-subset(metaplants,type=="root_vegetable")
# metaplantsPulse<-subset(metaplants,type=="pulses")
# metaplantsOil<-subset(metaplants,type=="oilcrop")
# metaplantsVeg<-subset(metaplants,type=="vegetable")
# metaplantsFruit<-subset(metaplants,type=="fruit")





# 
mr.d.metaplantsC3<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsC3[,"difference"]+~metaplantsC3[,"field01"], ni = replicate_facilities,   data=metaplantsC3,   measure="MN")
# mr.d.metaplantsC4<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsC4[,"difference"], ni = replicate_facilities,   data=metaplantsC4,   measure="MN")
# mr.d.metaplantsRoot<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsRoot[,"difference"]+~metaplantsRoot[,"field01"], ni = replicate_facilities,   data=metaplantsRoot,   measure="MN")
# mr.d.metaplantsPulse<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsPulse[,"difference"]+~metaplantsPulse[,"field01"], ni = replicate_facilities,   data=metaplantsPulse,   measure="MN")
# mr.d.metaplantsOil<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsOil[,"difference"]+~metaplantsOil[,"field01"], ni = replicate_facilities,   data=metaplantsOil,   measure="MN")
# mr.d.metaplantsVeg<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsVeg[,"difference"]+~metaplantsVeg[,"field01"], ni = replicate_facilities,   data=metaplantsVeg,   measure="MN")
# mr.d.metaplantsFruit<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsFruit[,"difference"]+~metaplantsFruit[,"field01"], ni = replicate_facilities,   data=metaplantsFruit,   measure="MN")

## mr.d.metaplantsallC3<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsC3[,"difference"], ni = replicate_facilities,   data=metaplantsC3,   measure="MN")
## mr.d.metaplantsallC4<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsC4[,"difference"], ni = replicate_facilities,   data=metaplantsC4,   measure="MN")
## mr.d.metaplantsallRoot<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsRoot[,"difference"], ni = replicate_facilities,   data=metaplantsRoot,   measure="MN")
## mr.d.metaplantsallPulse<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsPulse[,"difference"], ni = replicate_facilities,   data=metaplantsPulse,   measure="MN")
## mr.d.metaplantsallOil<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsOil[,"difference"], ni = replicate_facilities,   data=metaplantsOil,   measure="MN")
## mr.d.metaplantsallVeg<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsVeg[,"difference"], ni = replicate_facilities,   data=metaplantsVeg,   measure="MN")
## mr.d.metaplantsallFruit<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsFruit[,"difference"], ni = replicate_facilities,   data=metaplantsFruit,   measure="MN")
# 
 predict.mr.d.metaplantsC3<-predict(mr.d.metaplantsC3, newmods=c(178.22,1))
# predict.mr.d.metaplantsC4<-predict(mr.d.metaplantsC4, newmods=178.22)
# predict.mr.d.metaplantsRoot<-predict(mr.d.metaplantsRoot, newmods=c(178.22,1))
# predict.mr.d.metaplantsPulse<-predict(mr.d.metaplantsPulse, newmods=c(178.22,1))
# predict.mr.d.metaplantsOil<-predict(mr.d.metaplantsOil, newmods=c(178.22,1))
# predict.mr.d.metaplantsVeg<-predict(mr.d.metaplantsVeg, newmods=c(178.22,1))
# predict.mr.d.metaplantsFruit<-predict(mr.d.metaplantsFruit, newmods=c(178.22,1))
# 
# plantmr.d.predictions.type<-rbind(
#   predict.mr.d.metaplantsC3,
#   predict.mr.d.metaplantsC4,
#   predict.mr.d.metaplantsRoot,
#   predict.mr.d.metaplantsPulse,
#   predict.mr.d.metaplantsOil,
#   predict.mr.d.metaplantsVeg
# )

# write.csv(plantmr.d.predictions.type,  file="~/Rdata/nutrients/predictmrmetatypesall-difference.csv")
write.csv(predictions.type,  file="~/Rdata/nutrients/predictmrmetatypesall-difference.csv")

###by plant####
plantmr.d.predictions.order=NULL

for(PlantOrder in 1:21)
  {metaplants.sub<-subset(metaplants,plantorder==PlantOrder)
   ifelse(mean(metaplants.sub$difference)==max(metaplants.sub$difference),
          predict.mr.d.metaplants.sub<-c(0,0,0,0,0,0),
          (ifelse(
            max(metaplants.sub[,"field01"],na.rm=TRUE)==0,
            mr.d.metaplants.sub<-rma(mi = percent_decrease_N, sdi = SD, mods = ~metaplants.sub[,"difference"], ni = replicate_facilities, data=metaplants.sub,   measure="MN"),
            mr.d.metaplants.sub<-rma(mi = percent_decrease_N, sdi = SD, mods = ~metaplants.sub[,"difference"]+~metaplants.sub[,"field01"], ni = replicate_facilities, data=metaplants.sub,   measure="MN") 
            )))
       
     ifelse(
       (max(metaplants.sub[,"field01"],na.rm=TRUE)==0),
       (predict.mr.d.metaplants.sub<-predict(mr.d.metaplants.sub, newmods=178.22)),
       (predict.mr.d.metaplants.sub<-predict(mr.d.metaplants.sub, newmods=c(178.22,1)))
       )
   plantmr.d.predictions.order<-rbind(plantmr.d.predictions.order,predict.mr.d.metaplants.sub)
}



predictions.order<-data.frame(
  plantmr.d.predictions.order
)

PlantNameList = NULL
for(PlantOrder in 1:21)
{plantName<-ifelse(
  length(subset(metaplants,plantorder==PlantOrder))>0,
  subset(metaplants,plantorder==PlantOrder)[1,"plant"],
  NA)
  
df.Name<-data.frame("order"=PlantOrder,"Name"=plantName)
PlantNameList=rbind(PlantNameList,df.Name)

}

PlantNameList$Name<-as.factor(PlantNameList$Name)
levels(PlantNameList$Name)<-levels(metaplants$plant)#interesting but wrong

levels(df.Name$Name)<-levels(metaplants$plant)

metaplants1<-subset(metaplants,plantorder==1)
metaplants2<-subset(metaplants,plantorder==2)
metaplants3<-subset(metaplants,plantorder==3)
metaplants4<-subset(metaplants,plantorder==4)
metaplants5<-subset(metaplants,plantorder==5)
metaplants6<-subset(metaplants,plantorder==6)
metaplants7<-subset(metaplants,plantorder==7)
metaplants8<-subset(metaplants,plantorder==8)
metaplants9<-subset(metaplants,plantorder==9)
metaplants10<-subset(metaplants,plantorder==10)
metaplants11<-subset(metaplants,plantorder==11)
metaplants12<-subset(metaplants,plantorder==12)
metaplants13<-subset(metaplants,plantorder==13)
metaplants14<-subset(metaplants,plantorder==14)
metaplants15<-subset(metaplants,plantorder==15)
metaplants16<-subset(metaplants,plantorder==16)
metaplants17<-subset(metaplants,plantorder==17)
metaplants18<-subset(metaplants,plantorder==18)
metaplants19<-subset(metaplants,plantorder==19)
metaplants20<-subset(metaplants,plantorder==20)
metaplants21<-subset(metaplants,plantorder==21)




mr.d.metaplants1<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants1[,"difference"]+~metaplants1[,"field01"], ni = replicate_facilities,   data=metaplants1,   measure="MN")
mr.d.metaplants2<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants2[,"difference"]+~metaplants2[,"field01"], ni = replicate_facilities,   data=metaplants2,   measure="MN")
mr.d.metaplants3<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants3[,"difference"]+~metaplants3[,"field01"], ni = replicate_facilities,   data=metaplants3,   measure="MN")
mr.d.metaplants4<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants4[,"difference"]+~metaplants4[,"field01"], ni = replicate_facilities,   data=metaplants4,   measure="MN")
mr.d.metaplants5<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants5[,"difference"]+~metaplants5[,"field01"], ni = replicate_facilities,   data=metaplants5,   measure="MN")
mr.d.metaplants6<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants6[,"difference"]+~metaplants6[,"field01"], ni = replicate_facilities,   data=metaplants6,   measure="MN")
mr.d.metaplants7<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants7[,"difference"]+~metaplants7[,"field01"], ni = replicate_facilities,   data=metaplants7,   measure="MN")
mr.d.metaplants8<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants8[,"difference"]+~metaplants8[,"field01"], ni = replicate_facilities,   data=metaplants8,   measure="MN")
mr.d.metaplants9<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants9[,"difference"]+~metaplants9[,"field01"], ni = replicate_facilities,   data=metaplants9,   measure="MN")
mr.d.metaplants10<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants10[,"difference"]+~metaplants10[,"field01"], ni = replicate_facilities,   data=metaplants10,   measure="MN")
mr.d.metaplants11<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants11[,"difference"]+~metaplants11[,"field01"], ni = replicate_facilities,   data=metaplants11,   measure="MN")
mr.d.metaplants12<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants12[,"difference"]+~metaplants12[,"field01"], ni = replicate_facilities,   data=metaplants12,   measure="MN")
mr.d.metaplants13<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants13[,"difference"]+~metaplants13[,"field01"], ni = replicate_facilities,   data=metaplants13,   measure="MN")
mr.d.metaplants14<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants14[,"difference"]+~metaplants14[,"field01"], ni = replicate_facilities,   data=metaplants14,   measure="MN")
mr.d.metaplants15<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants15[,"difference"]+~metaplants15[,"field01"], ni = replicate_facilities,   data=metaplants15,   measure="MN")
mr.d.metaplants16<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants16[,"difference"]+~metaplants16[,"field01"], ni = replicate_facilities,   data=metaplants16,   measure="MN")
mr.d.metaplants17<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants17[,"difference"]+~metaplants17[,"field01"], ni = replicate_facilities,   data=metaplants17,   measure="MN")
mr.d.metaplants18<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants18[,"difference"]+~metaplants18[,"field01"], ni = replicate_facilities,   data=metaplants18,   measure="MN")
mr.d.metaplants19<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants19[,"difference"]+~metaplants19[,"field01"], ni = replicate_facilities,   data=metaplants19,   measure="MN")
mr.d.metaplants20<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants20[,"difference"]+~metaplants20[,"field01"], ni = replicate_facilities,   data=metaplants20,   measure="MN")
mr.d.metaplants21<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants21[,"difference"]+~metaplants21[,"field01"], ni = replicate_facilities,   data=metaplants21,   measure="MN")

mr.d.metaplantsall1<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants1[,"difference"], ni = replicate_facilities,   data=metaplants1,   measure="MN")
mr.d.metaplantsall2<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants2[,"difference"], ni = replicate_facilities,   data=metaplants2,   measure="MN")
 mr.d.metaplantsall3<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants3[,"difference"], ni = replicate_facilities,   data=metaplants3,   measure="MN")
mr.d.metaplantsall4<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants4[,"difference"], ni = replicate_facilities,   data=metaplants4,   measure="MN")
mr.d.metaplantsall5<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants5[,"difference"], ni = replicate_facilities,   data=metaplants5,   measure="MN")
mr.d.metaplantsall6<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants6[,"difference"], ni = replicate_facilities,   data=metaplants6,   measure="MN")
mr.d.metaplantsall7<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants7[,"difference"], ni = replicate_facilities,   data=metaplants7,   measure="MN")
mr.d.metaplantsall8<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants8[,"difference"], ni = replicate_facilities,   data=metaplants8,   measure="MN")
mr.d.metaplantsall9<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants9[,"difference"], ni = replicate_facilities,   data=metaplants9,   measure="MN")
 mr.d.metaplantsall10<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants10[,"difference"], ni = replicate_facilities,   data=metaplants10,   measure="MN")
mr.d.metaplantsall11<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants11[,"difference"], ni = replicate_facilities,   data=metaplants11,   measure="MN")
mr.d.metaplantsall12<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants12[,"difference"], ni = replicate_facilities,   data=metaplants12,   measure="MN")
 mr.d.metaplantsall13<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants13[,"difference"], ni = replicate_facilities,   data=metaplants13,   measure="MN")
mr.d.metaplantsall14<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants14[,"difference"], ni = replicate_facilities,   data=metaplants14,   measure="MN")
 mr.d.metaplantsall15<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants15[,"difference"], ni = replicate_facilities,   data=metaplants15,   measure="MN")
mr.d.metaplantsall16<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants16[,"difference"], ni = replicate_facilities,   data=metaplants16,   measure="MN")
mr.d.metaplantsall17<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants17[,"difference"], ni = replicate_facilities,   data=metaplants17,   measure="MN")
 mr.d.metaplantsall18<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants18[,"difference"], ni = replicate_facilities,   data=metaplants18,   measure="MN")
mr.d.metaplantsall19<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants19[,"difference"], ni = replicate_facilities,   data=metaplants19,   measure="MN")
 mr.d.metaplantsall20<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants20[,"difference"], ni = replicate_facilities,   data=metaplants20,   measure="MN")
mr.d.metaplantsall21<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplants21[,"difference"], ni = replicate_facilities,   data=metaplants21,   measure="MN")



predict.mr.d.metaplants1<-predict(mr.d.metaplants1, newmods=c(178.22,1))
predict.mr.d.metaplants2<-predict(mr.d.metaplants2, newmods=c(178.22,1))
predict.mr.d.metaplants3<-predict(mr.d.metaplants3, newmods=c(178.22,1))
predict.mr.d.metaplants4<-  NA
#   predict(mr.d.metaplants4, newmods=c(178.22,1))
predict.mr.d.metaplants5<-  NA
  #   predict(mr.d.metaplants5, newmods=c(178.22,1))
predict.mr.d.metaplants6<-  NA
#   predict(mr.d.metaplants6, newmods=c(178.22,1))
predict.mr.d.metaplants7<-predict(mr.d.metaplantsall7, newmods=178.22)
predict.mr.d.metaplants8<-NA
#   predict(mr.d.metaplants8, newmods=c(178.22,1))
predict.mr.d.metaplants9<-predict(mr.d.metaplantsall9, newmods=178.22)
predict.mr.d.metaplants10<-predict(mr.d.metaplants10, newmods=c(178.22,1))
predict.mr.d.metaplants11<- NA
#   predict(mr.d.metaplants11, newmods=c(178.22,1))
predict.mr.d.metaplants12<-predict(mr.d.metaplantsall12, newmods=178.22)
predict.mr.d.metaplants13<-predict(mr.d.metaplants13, newmods=c(178.22,1))
predict.mr.d.metaplants14<-  NA
#   predict(mr.d.metaplants14, newmods=c(178.22,1))
predict.mr.d.metaplants15<-predict(mr.d.metaplants15, newmods=c(178.22,1))
predict.mr.d.metaplants16<-predict(mr.d.metaplantsall16, newmods=178.22)
predict.mr.d.metaplants17<-  NA
#   predict(mr.d.metaplants17, newmods=c(178.22,1))
predict.mr.d.metaplants18<-predict(mr.d.metaplants18, newmods=c(178.22,1))
predict.mr.d.metaplants19<-  NA
#   predict(mr.d.metaplants19, newmods=c(178.22,1))
predict.mr.d.metaplants20<-predict(mr.d.metaplants20, newmods=c(178.22,1))
predict.mr.d.metaplants21<-  NA
#   predict(mr.d.metaplants21, newmods=c(178.22,1))

predict.mr.d.metaplants1
predict.mr.d.metaplants2
predict.mr.d.metaplants3
predict.mr.d.metaplants4
predict.mr.d.metaplants5
predict.mr.d.metaplants6
predict.mr.d.metaplants7
predict.mr.d.metaplants8
predict.mr.d.metaplants9
predict.mr.d.metaplants10
predict.mr.d.metaplants11
predict.mr.d.metaplants12
predict.mr.d.metaplants13
predict.mr.d.metaplants14
predict.mr.d.metaplants15
predict.mr.d.metaplants16
predict.mr.d.metaplants17
predict.mr.d.metaplants18
predict.mr.d.metaplants19
predict.mr.d.metaplants20
predict.mr.d.metaplants21



predict.mr.d.metaplants<-rbind(
  predict.mr.d.metaplants1,
  predict.mr.d.metaplants2,
  predict.mr.d.metaplants3,
  predict.mr.d.metaplants4,
  predict.mr.d.metaplants5,
  predict.mr.d.metaplants6,
  predict.mr.d.metaplants7,
  predict.mr.d.metaplants8,
  predict.mr.d.metaplants9,
  predict.mr.d.metaplants10,
  predict.mr.d.metaplants11,
  predict.mr.d.metaplants12,
  predict.mr.d.metaplants13,
  predict.mr.d.metaplants14,
  predict.mr.d.metaplants15,
  predict.mr.d.metaplants16,
  predict.mr.d.metaplants17,
  predict.mr.d.metaplants18,
  predict.mr.d.metaplants19,
  predict.mr.d.metaplants20,
  predict.mr.d.metaplants21
)

View(predict.mr.d.metaplants)
write.csv(predict.mr.d.metaplants, file="~/Rdata/nutrients/predictmrmetaplants-difference.csv")


####just playing around####
# metaplants$plantorder<-as.factor(metaplants$plantorder)
# levels(metaplants$plantorder)
# 
# View(metaplants)
# 
# View(dat.bcg)
# result.mr<-rma(a)
# 
# rma(mi = percent_decrease_N, 
#     sdi = SD, 
#     mods = metaplants[,"elevated"], 
#     ni = replicate_facilities, 
#     data=metaplants, 
#     measure="MN")
# 
# wheat<-subset(metaplants,plantorder=1)
# wheatfield<-subset(wheat,fieldpots=="field")
# 
# mr.d.wheat<-rma(mi = percent_decrease_N, 
#                 sdi = SD, 
#                 mods = wheat[,"elevated"], 
#                 ni = replicate_facilities, 
#                 data=wheat, 
#                 measure="MN")
# 
# mr.d.wheatfield<-rma(mi = percent_decrease_N, 
#                      sdi = SD, 
#                      mods = wheatfield[,"elevated"], 
#                      ni = replicate_facilities, 
#                      data=wheatfield, 
#                      measure="MN")
# 
# soy<-subset(metaplants,plantnumber==54)
# soyfield<-subset(soy,fieldpots=="field")
# 
# mr.d.soy<-rma(mi = percent_decrease_N, 
#               sdi = SD, 
#               mods = ~soy[,"elevated"]+ ~soy[,"field01"],
#               ni = replicate_facilities, 
#               data=soy, 
#               measure="MN")
# 
# 
# mr.d.soyfield<-rma(mi = percent_decrease_N, 
#                    sdi = SD, 
#                    mods = ~soyfield[,"elevated"]
#                    ni = replicate_facilities, 
#                    data=soyfield, 
#                    measure="MN")
# 
# 
# predict(mr.d.metaplants1,newmods=c(178.22,1))
# predict(mr.d.soyfield,newmods=c(178.22,1))

###excluding pots####
###by type####
# metaplantsfield<-subset(metaplants,fieldpots=="field")
# View(metaplantsfield)
# 
# metaplantsfieldGrain<-subset(metaplantsfield,type=="C3_cereal" | type=="C4_cereal")
# View(metaplantsfieldGrain)
# 
# mr.d.metaplantsfieldGrain<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfieldGrain[,"difference"]+~metaplantsfieldGrain[,"field01"], ni = replicate_facilities,   data=metaplantsfieldGrain,   measure="MN")
# mr.d.metaplantsfieldallGrain<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfieldGrain[,"difference"], ni = replicate_facilities,   data=metaplantsfieldGrain,   measure="MN")
# predict.mr.d.metaplantsfieldGrain<-predict(mr.d.metaplantsfieldallGrain, newmods=c(178.22,1))
# 
# predict.mr.d.metaplantsfieldGrain
# 
# 
# metaplantsfieldC3<-subset(metaplantsfield,type=="C3_cereal")
# metaplantsfieldC4<-subset(metaplantsfield,type=="C4_cereal")
# metaplantsfieldRoot<-subset(metaplantsfield,type=="root_vegetable")
# metaplantsfieldPulse<-subset(metaplantsfield,type=="pulses")
# metaplantsfieldOil<-subset(metaplantsfield,type=="oilcrop")
# metaplantsfieldVeg<-subset(metaplantsfield,type=="vegetable")
# metaplantsfieldFruit<-subset(metaplantsfield,type=="fruit")
# 
# mr.d.metaplantsfieldC3<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfieldC3[,"difference"]+~metaplantsfieldC3[,"field01"], ni = replicate_facilities,   data=metaplantsfieldC3,   measure="MN")
# mr.d.metaplantsfieldC4<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfieldC4[,"difference"]+~metaplantsfieldC4[,"field01"], ni = replicate_facilities,   data=metaplantsfieldC4,   measure="MN")
# mr.d.metaplantsfieldRoot<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfieldRoot[,"difference"]+~metaplantsfieldRoot[,"field01"], ni = replicate_facilities,   data=metaplantsfieldRoot,   measure="MN")
# mr.d.metaplantsfieldPulse<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfieldPulse[,"difference"]+~metaplantsfieldPulse[,"field01"], ni = replicate_facilities,   data=metaplantsfieldPulse,   measure="MN")
# mr.d.metaplantsfieldOil<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfieldOil[,"difference"]+~metaplantsfieldOil[,"field01"], ni = replicate_facilities,   data=metaplantsfieldOil,   measure="MN")
# mr.d.metaplantsfieldVeg<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfieldVeg[,"difference"]+~metaplantsfieldVeg[,"field01"], ni = replicate_facilities,   data=metaplantsfieldVeg,   measure="MN")
# mr.d.metaplantsfieldFruit<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfieldFruit[,"difference"]+~metaplantsfieldFruit[,"field01"], ni = replicate_facilities,   data=metaplantsfieldFruit,   measure="MN")
# 
# mr.d.metaplantsfieldallC3<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfieldC3[,"difference"], ni = replicate_facilities,   data=metaplantsfieldC3,   measure="MN")
# mr.d.metaplantsfieldallC4<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfieldC4[,"difference"], ni = replicate_facilities,   data=metaplantsfieldC4,   measure="MN")
# mr.d.metaplantsfieldallRoot<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfieldRoot[,"difference"], ni = replicate_facilities,   data=metaplantsfieldRoot,   measure="MN")
# mr.d.metaplantsfieldallPulse<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfieldPulse[,"difference"], ni = replicate_facilities,   data=metaplantsfieldPulse,   measure="MN")
# mr.d.metaplantsfieldallOil<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfieldOil[,"difference"], ni = replicate_facilities,   data=metaplantsfieldOil,   measure="MN")
# mr.d.metaplantsfieldallVeg<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfieldVeg[,"difference"], ni = replicate_facilities,   data=metaplantsfieldVeg,   measure="MN")
# 
# predict.mr.d.metaplantsfieldC3<-predict(mr.d.metaplantsfieldallC3, newmods=c(178.22,1))
# predict.mr.d.metaplantsfieldC4<-predict(mr.d.metaplantsfieldallC4, newmods=c(178.22,1))
# predict.mr.d.metaplantsfieldRoot<-predict(mr.d.metaplantsfieldallRoot, newmods=c(178.22,1))
# predict.mr.d.metaplantsfieldOil<-predict(mr.d.metaplantsfieldallOil, newmods=c(178.22,1))
# predict.mr.d.metaplantsfieldVeg<-predict(mr.d.metaplantsfieldallVeg, newmods=c(178.22,1))
# 
# plantfieldmr.d.predictions.type<-rbind(
#   predict.mr.d.metaplantsfieldC3,
#   predict.mr.d.metaplantsfieldC4,
#   predict.mr.d.metaplantsfieldRoot,
#   predict.mr.d.metaplantsfieldOil,
#   predict.mr.d.metaplantsfieldVeg
# )
# write.csv(plantfieldmr.d.predictions.type,  file="~/Rdata/nutrients/predictmrmetatypesall-field-difference.csv")
# 
# ###by plant####
# 
# 
# metaplantsfield1<-subset(metaplantsfield,plantorder==1)
# metaplantsfield2<-subset(metaplantsfield,plantorder==2)
# metaplantsfield3<-subset(metaplantsfield,plantorder==3)
# metaplantsfield4<-subset(metaplantsfield,plantorder==4)
# metaplantsfield5<-subset(metaplantsfield,plantorder==5)
# metaplantsfield6<-subset(metaplantsfield,plantorder==6)
# metaplantsfield7<-subset(metaplantsfield,plantorder==7)
# metaplantsfield8<-subset(metaplantsfield,plantorder==8)
# metaplantsfield9<-subset(metaplantsfield,plantorder==9)
# metaplantsfield10<-subset(metaplantsfield,plantorder==10)
# metaplantsfield11<-subset(metaplantsfield,plantorder==11)
# metaplantsfield12<-subset(metaplantsfield,plantorder==12)
# metaplantsfield13<-subset(metaplantsfield,plantorder==13)
# metaplantsfield14<-subset(metaplantsfield,plantorder==14)
# metaplantsfield15<-subset(metaplantsfield,plantorder==15)
# metaplantsfield16<-subset(metaplantsfield,plantorder==16)
# metaplantsfield17<-subset(metaplantsfield,plantorder==17)
# metaplantsfield18<-subset(metaplantsfield,plantorder==18)
# metaplantsfield19<-subset(metaplantsfield,plantorder==19)
# metaplantsfield20<-subset(metaplantsfield,plantorder==20)
# metaplantsfield21<-subset(metaplantsfield,plantorder==21)
# 
# mr.d.metaplantsfield1<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfield1[,"difference"]+~metaplantsfield1[,"field01"], ni = replicate_facilities,   data=metaplantsfield1,   measure="MN")
# mr.d.metaplantsfield2<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfield2[,"difference"]+~metaplantsfield2[,"field01"], ni = replicate_facilities,   data=metaplantsfield2,   measure="MN")
# mr.d.metaplantsfield3<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfield3[,"difference"]+~metaplantsfield3[,"field01"], ni = replicate_facilities,   data=metaplantsfield3,   measure="MN")
# mr.d.metaplantsfield4<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfield4[,"difference"]+~metaplantsfield4[,"field01"], ni = replicate_facilities,   data=metaplantsfield4,   measure="MN")
# mr.d.metaplantsfield5<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfield5[,"difference"]+~metaplantsfield5[,"field01"], ni = replicate_facilities,   data=metaplantsfield5,   measure="MN")
# mr.d.metaplantsfield6<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfield6[,"difference"]+~metaplantsfield6[,"field01"], ni = replicate_facilities,   data=metaplantsfield6,   measure="MN")
# mr.d.metaplantsfield7<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfield7[,"difference"]+~metaplantsfield7[,"field01"], ni = replicate_facilities,   data=metaplantsfield7,   measure="MN")
# mr.d.metaplantsfield8<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfield8[,"difference"]+~metaplantsfield8[,"field01"], ni = replicate_facilities,   data=metaplantsfield8,   measure="MN")
# mr.d.metaplantsfield9<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfield9[,"difference"]+~metaplantsfield9[,"field01"], ni = replicate_facilities,   data=metaplantsfield9,   measure="MN")
# mr.d.metaplantsfield10<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfield10[,"difference"]+~metaplantsfield10[,"field01"], ni = replicate_facilities,   data=metaplantsfield10,   measure="MN")
# mr.d.metaplantsfield11<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfield11[,"difference"]+~metaplantsfield11[,"field01"], ni = replicate_facilities,   data=metaplantsfield11,   measure="MN")
# mr.d.metaplantsfield12<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfield12[,"difference"]+~metaplantsfield12[,"field01"], ni = replicate_facilities,   data=metaplantsfield12,   measure="MN")
# mr.d.metaplantsfield13<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfield13[,"difference"]+~metaplantsfield13[,"field01"], ni = replicate_facilities,   data=metaplantsfield13,   measure="MN")
# mr.d.metaplantsfield14<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfield14[,"difference"]+~metaplantsfield14[,"field01"], ni = replicate_facilities,   data=metaplantsfield14,   measure="MN")
# mr.d.metaplantsfield15<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfield15[,"difference"]+~metaplantsfield15[,"field01"], ni = replicate_facilities,   data=metaplantsfield15,   measure="MN")
# mr.d.metaplantsfield16<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfield16[,"difference"]+~metaplantsfield16[,"field01"], ni = replicate_facilities,   data=metaplantsfield16,   measure="MN")
# mr.d.metaplantsfield17<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfield17[,"difference"]+~metaplantsfield17[,"field01"], ni = replicate_facilities,   data=metaplantsfield17,   measure="MN")
# mr.d.metaplantsfield18<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfield18[,"difference"]+~metaplantsfield18[,"field01"], ni = replicate_facilities,   data=metaplantsfield18,   measure="MN")
# mr.d.metaplantsfield19<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfield19[,"difference"]+~metaplantsfield19[,"field01"], ni = replicate_facilities,   data=metaplantsfield19,   measure="MN")
# mr.d.metaplantsfield20<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfield20[,"difference"]+~metaplantsfield20[,"field01"], ni = replicate_facilities,   data=metaplantsfield20,   measure="MN")
# mr.d.metaplantsfield21<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfield21[,"difference"]+~metaplantsfield21[,"field01"], ni = replicate_facilities,   data=metaplantsfield21,   measure="MN")
# 
# mr.d.metaplantsfieldall1<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfield1[,"difference"], ni = replicate_facilities,   data=metaplantsfield1,   measure="MN")
# mr.d.metaplantsfieldall2<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfield2[,"difference"], ni = replicate_facilities,   data=metaplantsfield2,   measure="MN")
# mr.d.metaplantsfieldall3<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfield3[,"difference"], ni = replicate_facilities,   data=metaplantsfield3,   measure="MN")
# mr.d.metaplantsfieldall4<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfield4[,"difference"], ni = replicate_facilities,   data=metaplantsfield4,   measure="MN")
# mr.d.metaplantsfieldall5<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfield5[,"difference"], ni = replicate_facilities,   data=metaplantsfield5,   measure="MN")
# mr.d.metaplantsfieldall6<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfield6[,"difference"], ni = replicate_facilities,   data=metaplantsfield6,   measure="MN")
# mr.d.metaplantsfieldall7<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfield7[,"difference"], ni = replicate_facilities,   data=metaplantsfield7,   measure="MN")
# mr.d.metaplantsfieldall8<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfield8[,"difference"], ni = replicate_facilities,   data=metaplantsfield8,   measure="MN")
# mr.d.metaplantsfieldall9<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfield9[,"difference"], ni = replicate_facilities,   data=metaplantsfield9,   measure="MN")
# mr.d.metaplantsfieldall10<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfield10[,"difference"], ni = replicate_facilities,   data=metaplantsfield10,   measure="MN")
# mr.d.metaplantsfieldall11<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfield11[,"difference"], ni = replicate_facilities,   data=metaplantsfield11,   measure="MN")
# mr.d.metaplantsfieldall12<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfield12[,"difference"], ni = replicate_facilities,   data=metaplantsfield12,   measure="MN")
# mr.d.metaplantsfieldall13<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfield13[,"difference"], ni = replicate_facilities,   data=metaplantsfield13,   measure="MN")
# mr.d.metaplantsfieldall14<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfield14[,"difference"], ni = replicate_facilities,   data=metaplantsfield14,   measure="MN")
# mr.d.metaplantsfieldall15<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfield15[,"difference"], ni = replicate_facilities,   data=metaplantsfield15,   measure="MN")
# mr.d.metaplantsfieldall16<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfield16[,"difference"], ni = replicate_facilities,   data=metaplantsfield16,   measure="MN")
# mr.d.metaplantsfieldall17<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfield17[,"difference"], ni = replicate_facilities,   data=metaplantsfield17,   measure="MN")
# mr.d.metaplantsfieldall18<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfield18[,"difference"], ni = replicate_facilities,   data=metaplantsfield18,   measure="MN")
# mr.d.metaplantsfieldall19<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfield19[,"difference"], ni = replicate_facilities,   data=metaplantsfield19,   measure="MN")
# mr.d.metaplantsfieldall20<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfield20[,"difference"], ni = replicate_facilities,   data=metaplantsfield20,   measure="MN")
# mr.d.metaplantsfieldall21<-rma(mi = percent_decrease_N,     sdi = SD,     mods = ~metaplantsfield21[,"difference"], ni = replicate_facilities,   data=metaplantsfield21,   measure="MN")
# 
# 
# 
# predict.mr.d.metaplantsfield1<-predict(mr.d.metaplantsfieldall1, newmods=c(178.22,1))
# predict.mr.d.metaplantsfield2<-predict(mr.d.metaplantsfieldall2, newmods=c(178.22,1))
# predict.mr.d.metaplantsfield3<-predict(mr.d.metaplantsfieldall3, newmods=c(178.22,1))
# predict.mr.d.metaplantsfield7<-predict(mr.d.metaplantsfieldall7, newmods=c(178.22,1))
# predict.mr.d.metaplantsfield10<-predict(mr.d.metaplantsfieldall10, newmods=c(178.22,1))
# predict.mr.d.metaplantsfield15<-predict(mr.d.metaplantsfieldall15, newmods=c(178.22,1))
# predict.mr.d.metaplantsfield18<-predict(mr.d.metaplantsfieldall18, newmods=c(178.22,1))
# predict.mr.d.metaplantsfield20<-predict(mr.d.metaplantsfieldall20, newmods=c(178.22,1))
# 
# 
# predict.mr.d.metaplantsfieldall<-rbind(
#   predict.mr.d.metaplantsfield1,
#   predict.mr.d.metaplantsfield2,
#   predict.mr.d.metaplantsfield3,
#   predict.mr.d.metaplantsfield7,
#   predict.mr.d.metaplantsfield10,
#   predict.mr.d.metaplantsfield15,
#   predict.mr.d.metaplantsfield18,
#   predict.mr.d.metaplantsfield20)
# 
# View(predict.mr.d.metaplantsfieldall)
# write.csv(predict.mr.d.metaplantsfieldall, file="~/Rdata/nutrients/predictmrmetaplantsfieldall-difference.csv")

###getting authors####

authors<-cbind(
  data.frame(summary(metaplants1$Author)),
  data.frame(summary(metaplants2$Author)),
  data.frame(summary(metaplants3$Author)),
  data.frame(summary(metaplants7$Author)),
  data.frame(summary(metaplants9$Author)),
  data.frame(summary(metaplants10$Author)),
  data.frame(summary(metaplants12$Author)),
  data.frame(summary(metaplants13$Author)),
  data.frame(summary(metaplants15$Author)),
  data.frame(summary(metaplants18$Author)),
  data.frame(summary(metaplants20$Author)),
  data.frame(summary(metaplantsC3$Author)),
  data.frame(summary(metaplantsC4$Author)),
  data.frame(summary(metaplantsRoot$Author)),
  data.frame(summary(metaplantsOil$Author)),
  data.frame(summary(metaplantsVeg$Author)))

View(authors)
write.csv(authors,"~/Rdata/nutrients/authors24jul.csv")
  