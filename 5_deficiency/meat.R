load("3_intake/outputs/SOFIGINIFBS.Rdata")

meatslope<-read.csv("5_deficiency/inputs/meatslope.csv")
SOFIGINIFBSmeatslope<-merge(SOFIGINIFBS,meatslope,by="countrycode")
write.csv(SOFIGINIFBSmeatslope,"SOFIGINIFBSmeatslope.csv")


GINImeat<-read.csv("3_intake/inputs/GINI/world_bank_GINI.csv")

FBSprotein<-read.csv("2_countries/outputs/FBSprotein.csv")


FBSall<-read.csv("2_countries/inputs/FBS11.csv") #the raw FBS data for 2011
preCO2MEAT<-data.frame("commodity"=levels(FBSall$ItemName)[2:length(levels(FBSall$ItemName))])
nonveg<-c(1,"nonveg")
preCO2MEAT$DerivedFrom<-"unch"


levels(subset(preCO2MEAT,DerivedFrom=="unch")$commodity)

for(rownumber in 1:length(preCO2MEAT$commodity))
{if(preCO2MEAT$commodity[rownumber] %in% c("Wheat and products","Rice (Milled Equivalent)","Barley and products",
   "Maize and products",
   "Rye and products",
   "Oats",
   "Millet and products",
   "Sorghum and products","Cereals, Other","Cassava and products","Potatoes and products",
   "Sweet potatoes",
   "Yams",
   "Roots, Other",
   "Sugar beet","Beans","Peas","Pulses, Other and products",
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
   "Sunflowerseed Oil",
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
   "Vegetables, Other",
   "Rape and Mustardseed"
   
 ))
 preCO2MEAT$DerivedFrom[rownumber]<-"Veg"
 else if(preCO2MEAT$commodity[rownumber] %in% c(
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
 preCO2MEAT$DerivedFrom[rownumber]<-"nonveg"
 
 else 
   preCO2MEAT$DerivedFrom[rownumber]<-"unch"
}



subset(preCO2MEAT,DerivedFrom=="unch")$commodity

names(preCO2MEAT)[2]<-"VegOrMeat"
save(preCO2MEAT, file="2_countries/outputs/preCO2MEAT.Rdata")
load("2_countries/outputs/preCO2.Rdata")

preCO2<-merge(preCO2,preCO2MEAT, by="commodity")
