FBSall<-read.csv("2_countries/inputs/FBS11.csv") #the raw FBS data for 2011
preCO2<-data.frame("commodity"=levels(FBSall$ItemName)[2:length(levels(FBSall$ItemName))])
nonveg<-c(1,NA)
preCO2$DerivedFrom<-"unch"


levels(subset(preCO2,DerivedFrom=="unch")$commodity)


for(rownumber in 1:length(preCO2$commodity))
{if(preCO2$commodity[rownumber]=="Wheat and products")
  preCO2$DerivedFrom[rownumber]<-"wheat"
 else if(preCO2$commodity[rownumber]=="Rice (Milled Equivalent)")
   preCO2$DerivedFrom[rownumber]<-"rice"
 else if(preCO2$commodity[rownumber]=="Barley and products")
   preCO2$DerivedFrom[rownumber]<-"barley"
 else if(preCO2$commodity[rownumber]=="Barley and products")
   preCO2$DerivedFrom[rownumber]<-"barley"
 else if(preCO2$commodity[rownumber] %in% c(
   "Maize and products"
 ))
   preCO2$DerivedFrom[rownumber]<-"C4_cereal"
 else if(preCO2$commodity[rownumber] %in% c(
   "Rye and products",
   "Oats"
 ))
   preCO2$DerivedFrom[rownumber]<-"C3_cereal"
 else if(preCO2$commodity[rownumber]%in% c(
   "Millet and products",
   "Sorghum and products"
 ))
   preCO2$DerivedFrom[rownumber]<-"sorghum"
 else if(preCO2$commodity[rownumber]=="Cereals, Other")
   preCO2$DerivedFrom[rownumber]<-"all grains"
 else if(preCO2$commodity[rownumber]=="Cassava and products")
   preCO2$DerivedFrom[rownumber]<-"cassava"
 else if(preCO2$commodity[rownumber]=="Potatoes and products")
   preCO2$DerivedFrom[rownumber]<-"potato"
 else if(preCO2$commodity[rownumber]%in% c(
   "Sweet potatoes",
   "Yams",
   "Roots, Other",
   "Sugar beet"
 ))
   preCO2$DerivedFrom[rownumber]<-"root_vegetable"
 else if(preCO2$commodity[rownumber]=="Beans")
   preCO2$DerivedFrom[rownumber]<-"beans"
 else if(preCO2$commodity[rownumber]=="Beans")
   preCO2$DerivedFrom[rownumber]<-"beans"   
 else if(preCO2$commodity[rownumber]=="Peas")
   preCO2$DerivedFrom[rownumber]<-"peas" 
 else if(preCO2$commodity[rownumber]=="Pulses, Other and products")
   preCO2$DerivedFrom[rownumber]<-"pulses"
 else if(preCO2$commodity[rownumber]%in% c(
   "Cocoa Beans and products",
   "Coconut Oil",
   "Coconuts - Incl Copra",
   "Cottonseed Oil",
   "Cottonseed",
   "Groundnut Oil",
   "Groundnuts (Shelled Eq)",
   "Maize Germ Oil",
   "Oilcrops Oil, Other",
   "Oilcrops, Other",
   "Olive Oil",
   "Olives (including preserved)",
   "Palm kernels",
   "Palm Oil",
   "Palmkernel Oil",
   "Rape and Mustard Oil",
   "Ricebran Oil",
   "Sesame seed",
   "Sesameseed Oil",
   "Soyabean Oil",
   "Soyabeans",
   "Sunflower seed",
   "Sunflowerseed Oil"
   
 ))
 preCO2$DerivedFrom[rownumber]<-"root_vegetable"
 
 else if(preCO2$commodity[rownumber] %in% c(
   "Apples and products",
   "Aquatic Plants",
   "Bananas",
   "Citrus, Other",
   "Cloves",
   "Coffee and products",
   "Dates",
   "Fruits, Other",
   "Grapefruit and products",
   "Grapes and products (excl wine)",
   "Lemons, Limes and products",
   "Onions",
   "Oranges, Mandarines",
   "Pepper",
   "Pimento",
   "Pineapples and products", 
   "Plantains",
   "Spices, Other",
   "Sugar (Raw Equivalent)",
   "Sugar cane",
   "Sugar non-centrifugal",
   "Sweeteners, Other",
   "Tomatoes and products",
   "Vegetables, Other"
   
 ))
 preCO2$DerivedFrom[rownumber]<-"vegetable"
 else if(preCO2$commodity[rownumber] %in% c(
   "Aquatic Animals, Others",
   "Beer",
   "Beverages, Alcoholic",
   "Beverages, Fermented",
   "Bovine Meat",
   "Butter, Ghee",
   "Cephalopods",
   "Cream",
   "Crustaceans",
   "Demersal Fish",
   "Fats, Animals, Raw",
   "Fish, Body Oil",
   "Fish, Liver Oil",
   "Freshwater Fish",
   "Honey",
   "Marine Fish, Other",
   "Meat, Aquatic Mammals",
   "Meat, Other",
   "Molluscs, Other",
   "Mutton & Goat Meat",
   "Pelagic Fish",
   "Pigmeat",
   "Poultry Meat",
   "Tea (including mate)",
   "Wine",
   "Alcohol, Non-Food"
   
   
 ))
 preCO2$DerivedFrom[rownumber]<-"nonveg"
 
 else if(preCO2$commodity[rownumber]=="Rape and Mustardseed")
   preCO2$DerivedFrom[rownumber]<-"rapeseed"
 else 
   preCO2$DerivedFrom[rownumber]<-"unch"
}



subset(preCO2,DerivedFrom=="unch")$commodity

preCO2$mean<-NA
preCO2$se<-NA
length.preCO2<-length(preCO2$DerivedFrom)
length.predictions<-length(predictions.type.medN$Plant_Type)

# predictions.order<-data.frame(as.character(predictions.order))
# predictions.type<-data.frame(as.character(predictions.type))
# predictions$potsfield
# predictions.type
predDF<-data.frame(predictions.type.medN)

for(rownumber_DerivedFrom in 1:length.preCO2)
  {if(preCO2$DerivedFrom[rownumber_DerivedFrom]=="nonveg")
    preCO2[rownumber_DerivedFrom,3:4]<-c(1,0)
   for(rownumber_pred in 1:length.predictions)
     {if(predictions.type.medN$Plant_Type[rownumber_pred]==preCO2$DerivedFrom[rownumber_DerivedFrom])
             preCO2[rownumber_DerivedFrom,3:4]<-predictions.type.medN[rownumber_pred,3:4]}
}
         
preCO2_med<-preCO2
preCO2_hi<-preCO2

for(rownumber_DerivedFrom in 1:length.preCO2)
{if(preCO2_hi$DerivedFrom[rownumber_DerivedFrom]=="nonveg")
  preCO2_hi[rownumber_DerivedFrom,3:4]<-c(1,0)
 for(rownumber_pred in 1:length.predictions)
 {if(predictions.type.HighN$Plant_Type[rownumber_pred]==preCO2_hi$DerivedFrom[rownumber_DerivedFrom])
   preCO2_hi[rownumber_DerivedFrom,3:4]<-predictions.type.HighN[rownumber_pred,3:4]}
}







preCO2_lo<-preCO2

for(rownumber_DerivedFrom in 1:length.preCO2)
{if(preCO2_lo$DerivedFrom[rownumber_DerivedFrom]=="nonveg")
  preCO2_lo[rownumber_DerivedFrom,3:4]<-c(1,0)
 for(rownumber_pred in 1:length.predictions)
 {if(predictions.type.lowN$Plant_Type[rownumber_pred]==preCO2_lo$DerivedFrom[rownumber_DerivedFrom])
   preCO2_lo[rownumber_DerivedFrom,3:4]<-predictions.type.lowN[rownumber_pred,3:4]}
}


#   for(rownumber_predO in 1:length.predictionsO)
#      {if(predictions.order$Plant_Type[rownumber_predO]==preCO2$DerivedFrom[rownumber_DerivedFrom])
#        preCO2[rownumber_DerivedFrom,3:4]<-predictions.order[rownumber_predO,3:4]
#   }

save(preCO2_lo, file="2_countries/outputs/preCO2_lo.Rdata")
save(preCO2_med, file="2_countries/outputs/preCO2_med.Rdata")
save(preCO2_hi, file="2_countries/outputs/preCO2_hi.Rdata")




      
      
      
      
      
      
      