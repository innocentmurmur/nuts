require("metafor")

metaplants<-read.csv("1_CO2/inputs/meta_plants.csv")

metaplants$difference<-metaplants$elevated-metaplants$ambient

names(metaplants)

plantmr.d.predictions.type=NULL

for(PlantType in levels(metaplants$type)[-3])
{metaplants.sub<-subset(metaplants,type==PlantType)
 ifelse(
   max(metaplants.sub[,"field01"],na.rm=TRUE)==1 && min(metaplants.sub[,"field01"])==0,
   mr.d.metaplants.sub<-rma(yi = percent_decrease_N, sei = SE, mods = ~metaplants.sub[,"difference"]+metaplants.sub[,"field01"], ni = replicate_facilities, data=metaplants.sub,   measure="MN"),
   mr.d.metaplants.sub<-rma(mi = percent_decrease_N, sei = SE, mods = ~metaplants.sub[,"difference"], ni = replicate_facilities, data=metaplants.sub,   measure="MN")
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


#need a grain combo for cereals, other
metaplantsGrain<-subset(metaplants,type=="C3_cereal" | type=="C4_cereal")
mr.d.metaplantsGrain<-rma.mv(yi = percent_decrease_N,
                             V = Var,
                             W = replicate_facilities,
                             mods = ~metaplantsGrain[,"difference"]+metaplantsGrain[,"field01"], 
                             random = ~1|Sitecode+1|plantorder,
                             data=metaplantsGrain)


predict.mr.d.metaplantsGrain<-predict(mr.d.metaplantsGrain, newmods=c(178.22,1))


levels(predictions.type[,1])<-c( "C3_cereal","C4_cereal","oilcrop","pulses","root_vegetable","vegetable","all grains")
# names(predictions.type)[3]<-"se"
predictions.type<-rbind(predictions.type[1:3], data.frame(
  "Plant_Type"="all grains", 
  "pred"=predict.mr.d.metaplantsGrain$pred,
  "se"=predict.mr.d.metaplantsGrain$se))

predictions.type$ppm<-550
predictions.type$potsfield<-1

# predictions.type[2:3]<-as.character(predictions.type[2:3])
predictions.type.forexport<-data.frame(lapply(predictions.type, function(predictions.type) factor(unlist(predictions.type))))
write.csv(predictions.type.forexport, "1_CO2/outputs/predictmrmetatypesall-difference.csv")

###by plant####

plantmr.d.predictions.order=NULL
maxPlantOrder<-max(metaplants$plantorder, na.rm=TRUE)



for(PlantOrder in 1:maxPlantOrder)
  {metaplants.sub<-subset(metaplants,plantorder==PlantOrder) #subsetting for each crop
   ifelse(length(metaplants.sub$difference)<4, # if there are only 3 values, why do a big regression. This is for peanuts primarily
          mr.d.metaplants.sub<-NA,
          ifelse(mean(metaplants.sub$difference)==max(metaplants.sub$difference), #if the mean = the max, that means there's only one CO2 level, so we can't do a regression
                 mr.d.metaplants.sub<-rma(mi = percent_decrease_N, 
                                          sdi = SD,ni = replicate_facilities, 
                                          data=metaplants.sub,   
                                          measure="MN"), #meta analysis without the regression
                 (ifelse(
                   max(metaplants.sub[,"field01"],na.rm=TRUE)==sqrt(max(metaplants.sub[,"field01"],na.rm=TRUE)), 
                   mr.d.metaplants.sub<-rma(mi = percent_decrease_N, 
                                            sdi = SD, 
                                            mods = ~metaplants.sub[,"difference"], 
                                            ni = replicate_facilities, 
                                            data=metaplants.sub,   
                                            measure="MN"),#if there are no field or no pot experiments on this crop we can't predict what will happen in the field
                   mr.d.metaplants.sub<-rma(mi = percent_decrease_N, 
                                            sdi = SD, 
                                            mods = ~metaplants.sub[,"difference"]+metaplants.sub[,"field01"], 
                                            ni = replicate_facilities, 
                                            data=metaplants.sub,   
                                            measure="MN")
            ))))
       
     ifelse(length(metaplants.sub$difference)<4, #reconsider this....
            predict.mr.d.metaplants.sub<-data.frame(
              "pred"=mean(metaplants.sub$percent_decrease_N),
              "se"=sqrt(sum((metaplants.sub$SD)^2)/length((metaplants.sub$SD)^2)),
              "ci.ub"=mean(metaplants.sub$elevated),
              "fieldpots"= mean(metaplants.sub[,"field01"], na.rm=TRUE)
              ),
            ifelse(
              mean(metaplants.sub$difference)==max(metaplants.sub$difference),
              predict.mr.d.metaplants.sub<-data.frame(
                "pred"=mr.d.metaplants.sub$b,
                "se"=mr.d.metaplants.sub$se,
                "ci.ub"=mean(metaplants.sub$elevated),
                "fieldpots"= mean(metaplants.sub[,"field01"], na.rm=TRUE)),  
              # if you don't do the rma at all... but the means won't be weighted

              
              ifelse(
                max(metaplants.sub[,"field01"],na.rm=TRUE)==sqrt(max(metaplants.sub[,"field01"],na.rm=TRUE)),
                (predict.mr.d.metaplants.sub<-predict(mr.d.metaplants.sub, newmods=178.22)),
                (predict.mr.d.metaplants.sub<-predict(mr.d.metaplants.sub, newmods=c(178.22,1))))
            )
     )

   mr.predictions<-data.frame("order"=PlantOrder,
     "pred"=predict.mr.d.metaplants.sub$pred,
     "se"=predict.mr.d.metaplants.sub$se,
     "ppm"=
       ifelse(
         predict.mr.d.metaplants.sub$ci.ub>300,
         predict.mr.d.metaplants.sub$ci.ub,
         550
         ),
     "fieldpots"= mean(metaplants.sub[,"field01"], na.rm=TRUE)
   )
   plantmr.d.predictions.order<-rbind(
     plantmr.d.predictions.order, 
     mr.predictions)
}


#probably not necessary, but at least it has a better name and is definitely a dataframe
predictions.order<-data.frame(
  plantmr.d.predictions.order
)
names(predictions.order)<-c("order", "pred","se","ppm","potsfield")


#this next bit is probably not necessary if I understood what the differences between cell arrays and doubles and lists were. 
#Basically, I ended up with numbers rather than plant names
numberDifferentPlants<-length(levels(metaplants$plant))
PlantNameList = NULL
for(PlantOrder in 1:numberDifferentPlants)
{plantName<-ifelse(
  length(subset(metaplants,plantorder==PlantOrder)$plantorder)>0,
  subset(metaplants,plantorder==PlantOrder)[1,"plant"],
  NA)
  
df.Name<-data.frame("order"=PlantOrder,"level_number"=plantName)
PlantNameList=data.frame(rbind(PlantNameList,df.Name))
}

levels_metaplants<-data.frame("Plant_Type"=levels(metaplants$plant),"level_number"=c(1:length(levels(metaplants$plant))))

PlantNameList<-merge(PlantNameList,levels_metaplants,by="level_number")

predictions.order<-merge(PlantNameList[2:3],predictions.order,by="order") #sticking the plant names onto the list because it just didn't understand that they had names.


predictions.order
# predictions.order.test<-predictions.order
# predictions.order.test$Plant_Type<-as.character(predictions.order.test$Plant_Type)
# predictions.type.test<-predictions.type
# predictions.type.test$Plant_Type<-as.character(predictions.type.test$Plant_Type)

predictions<-rbind(predictions.type,predictions.order[2:6])


####outputs####
predictions.order
predictions.type
predictions

write.csv(predictions.order, file="1_CO2/outputs/predictions-order-difference.csv")
write.csv(predictions.type, file="1_CO2/outputs/predictions-type-difference.csv")

predictions.forexport<-data.frame(lapply(predictions, function(predictions) factor(unlist(predictions))))
write.csv(predictions.forexport, file="1_CO2/outputs/predictions-difference.csv")

#remove the tildas!
###getting authors####

authors<- data.frame("authors"=levels(metaplants$Author)[2:length(levels(metaplants$Author))])
authors$grains<-summary(metaplantsGrain$Author)[2:length(summary(metaplantsGrain$Author))]
 for(PlantType in levels(metaplants$type)[-3])
 {metaplants.sub<-subset(metaplants,type==PlantType)
  authors<-cbind(authors, summary(metaplants.sub$Author)[2:length(summary(metaplants.sub$Author))])
 }
names(authors)[3:length(authors)]<-levels(metaplants$type)[-3]
row.names(authors)<-1:length(authors$authors)

for(PlantOrder in 1:maxPlantOrder)
{metaplants.sub<-subset(metaplants,plantorder==PlantOrder)
 metaplants.sub.authors<-data.frame(summary(metaplants.sub$Author)[2:length(summary(metaplants.sub$Author))])
 names(metaplants.sub.authors)<-PlantOrder
 authors<-cbind(authors, metaplants.sub.authors)
}

names(authors)[3:length(authors)]<-levels(metaplants$type)[-3]
row.names(authors)<-1:length(authors$authors)



write.csv(authors,"authors.csv")
   