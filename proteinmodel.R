###The big picture on the protein project############################################################
setwd('C:/Users/HolbrookLab_3107/OneDrive/Documents/Rdata/nutrients/')
setwd('C:/Users/danielle/SkyDrive/Documents/Rdata/nutrients/')
setwd("C:/Users/danielle/Google Drive/not_med/research/Rdata/nutrients/")
setwd("/media/lili/WOOWOO/Rdata/nutrients/")
setwd("/home/lili/Documents/Rdata/nutrients/")
setwd("C:/Users/HolbrookLab_3107/Documents/nuts")
#working directory of holbrook desktop: C:/Users/HolbrookLab_3107/OneDrive/Documents/Rdata/nutrients

###1_CO2: ####
#getting plant changes under elevated CO2
1_CO2/metaregression.R
1_CO2/metaregression_withN.R
#at the moment I'm using metaregression_New, but all the modifications are in metaplants_N_doesnt_work


#inputs: meta_plants.csv, 

#outputs: 

# save(predictions, file = "1_CO2/outputs/predictions.Rdata")
save(predictions.type, file = "1_CO2/outputs/predictions.type.Rdata")
rm(list=setdiff(ls(), "predictions.type"))

###0_labels####
countrycoding.R
#I'm not sure it's needed now

#inputs
FBSall

#input files
missing_countries_FBS_2011.csv
FBS11.csv
FBS11totals.csv

#outputs
rm(list=setdiff(ls(), c("countriesFBS","FBSall","predictions")))
save(countriesFBS, file = "0_labels/countriesFBS.Rdata")

###2_countries:#### 
#calculating country-based estimates of protein intake from the FBS

##making a CO2 percent for each ####

CO2.R
#issue: for some reason I have two types of beans, no wheat, and no cassava

#inputs:
load("1_CO2/outputs/predictions.Rdata")
load("1_CO2/outputs/predictions.type.lowN.Rdata")
load("1_CO2/outputs/predictions.type.medN.Rdata")
load("1_CO2/outputs/predictions.type.HighN.Rdata")
#outputs
preCO2

# rm(list=setdiff(ls(), c("FBStot","preCO2","countriesFBS","FBSall")))
rm(list=setdiff(ls(), c("FBStot","fbspop")))
save(preCO2, file = "1_CO2/outputs/preCO2.Rdata")

####working wtih protein from the FBS#####
#putting CO2 percent change on there, 
#calculating SE, 
#sticking country codes onto it, 
#calculating mean protien and percent of calories under ambient and elevated CO2
#file

2_countries/fbs_N.R

#inputs
countriesFBS or
order_FBS.csv

load("1_CO2/outputs/preCO2.Rdata")

#outputs:
rm(list=setdiff(ls(), c("FBStot","countriesFBS","countriesHHS")))

save(FBStot, file="2_countries/outputs/FBStot.Rdata")


###3_intake#####
load("0_labels/countriesFBS.Rdata")
#source("3_intake/DistribProtein.R")

#inputs
countriesFBS
FBStot

save(SOFIGINIFBS, file="3_intake/outputs/SOFIGINIFBS.Rdata")

#outputs
rm(list=setdiff(ls(), c("SOFIGINIFBS", "FBS_Distribution_parameters")))


#well, the problem is, the skewness and the C of V I calculated is so much greater than that in the energy, I wonder if they're really the same thing.

#Jumping ahead of myself the skew normal distribution is in the package sn. 
# and if you want to get the P of a lognormal distribution, do plnorm, but I don't know whether to log all the parameters before entering them or not. And if so, do I also log the EAR?


####4 EAR####

pop.R

###5 Deficiency#####
load("4_EAR/outputs/EAR_weighted.Rdata")

