#grab the 2010 population data from C:\Users\HolbrookLab_3107\Google Drive\not_med\research\xls\other databases\UNWPP2012_DB04_POPULATION_BY_AGE_ANNUAL
# see pop.R for that
#downloaded from http://faostat3.fao.org/faostat-gateway/go/to/download/FB/FBS/E in two parts, from "standard download". firstly, items, then items aggregated.



###Inputs ####

# FBSall<-read.csv("~/FBS11.csv")
# FBStots<-read.csv("FBS11totals.csv")
# CO2<-read.csv("CO2.csv")
# countrycode<-read.csv("country_plus_code.csv")

FBSall<-read.csv("2_countries/inputs/FBS11.csv") #the raw FBS data for 2011
FBStots<-read.csv("2_countries/inputs/FBS11totals.csv") #another FBS output, for things like total treenuts and total pelagic fish
# CO2<-read.csv("2_countries/inputs/CO2.csv") #the CO2 effect and SE

countrycode<-read.csv("0_labels/order_FBS.csv")
names(countrycode)<-c("order","AreaName","countrycode")
####making the CO2 dataset####

   
CO2_lowN<- data.frame(lapply(preCO2_lo, as.character), stringsAsFactors=FALSE)
CO2_medN<- data.frame(lapply(preCO2_med, as.character), stringsAsFactors=FALSE)
CO2_hiN<- data.frame(lapply(preCO2_hi, as.character), stringsAsFactors=FALSE)

names(CO2_lowN)[1]<-"ItemName"
names(CO2_lowN)[3]<-"CO2RR_lo"
names(CO2_lowN)[4]<-"CO2SE_lo"
names(CO2_medN)[3]<-"CO2RR_med"
names(CO2_medN)[4]<-"CO2SE_med"
names(CO2_hiN)[3]<-"CO2RR_hi"
names(CO2_hiN)[4]<-"CO2SE_hi"

CO2<-cbind(CO2_lowN,CO2_medN[3:4],CO2_hiN[3:4])
View(CO2)
typeof(CO2$CO2RR_lo)

CO2$CO2RR_lo<-as.numeric(CO2$CO2RR_lo)
CO2$CO2SE_lo<-as.numeric(CO2$CO2SE_lo)
CO2$CO2RR_med<-as.numeric(CO2$CO2RR_med)
CO2$CO2SE_med<-as.numeric(CO2$CO2SE_med)
CO2$CO2RR_hi<-as.numeric(CO2$CO2RR_hi)
CO2$CO2SE_hi<-as.numeric(CO2$CO2SE_hi)


#combining the two FBS datasets, i mean I could've downloaded them together to begin with, but I didn't.
FBS<-rbind(FBSall,FBStots)

#removing extraneous columns
keeps<-c("AreaName","ElementName","ItemName","Value")
FBS<-FBS[keeps]


#adding the missing countries
missing_countries<-read.csv("2_countries/inputs/missing_countries_FBS_2011.csv")

FBS<-rbind(FBS,missing_countries)

#adding the percent change in protein onto the FBS data
FBSco2<-merge(FBS,CO2,by="ItemName")

#splitting up the energy and protein into dataframes
FBSNRG<-subset(FBSco2,ElementName=="Food supply (kcal/capita/day)")
names(FBSNRG)
FBSNRG<-FBSNRG[keeps]
FBSprotein<-subset(FBSco2,ElementName=="Protein supply quantity (g/capita/day)")

#creating a column where protein is expressed in calories/capita/day
FBSprotein.cal<-FBSprotein$Value*4.25 
FBSprotein["protein.amb.cal"]<-FBSprotein.cal

#creating columns with expected g/capita/day and proteincalories/capita/day under elevated CO2
FBSprotein$proteinCO2_lo<-FBSprotein$Value*FBSprotein$CO2RR_lo
FBSprotein$proteinCO2_med<-FBSprotein$Value*FBSprotein$CO2RR_med
FBSprotein$proteinCO2_hi<-FBSprotein$Value*FBSprotein$CO2RR_hi
FBSprotein["protein.cal.CO2_lo"]<-FBSprotein$protein.amb.cal*FBSprotein$CO2RR_lo
FBSprotein["protein.cal.CO2_med"]<-FBSprotein$protein.amb.cal*FBSprotein$CO2RR_med
FBSprotein["protein.cal.CO2_hi"]<-FBSprotein$protein.amb.cal*FBSprotein$CO2RR_hi


#calculating the variance, weighted grams per capita per day
FBSprotein["CO2.SE.weighted_lo"]<-((FBSprotein$Value)^2)*((FBSprotein$CO2SE_lo)^2)
FBSprotein["CO2.SE.weighted_med"]<-((FBSprotein$Value)^2)*((FBSprotein$CO2SE_med)^2)
FBSprotein["CO2.SE.weighted_hi"]<-((FBSprotein$Value)^2)*((FBSprotein$CO2SE_hi)^2)

###outputs long and wide####
#long
write.csv(FBSNRG,"2_countries/outputs/FBSNRG.csv")
write.csv(FBSprotein,"2_countries/outputs/FBSprotein.csv")

#wide
#values are total calories per day per commodity, each column is a country
library(reshape)
castNRG <-data.frame(cast(FBSNRG,ItemName~AreaName,value="Value")) 
castProtein<-data.frame(cast(FBSprotein,ItemName~AreaName,value="Value"))


######totals now#####
#summing across commodities to get mean protein intake for each country
FBSprotein.tot<-rowsum(FBSprotein[c(4,12,13,14,15,16,17,18,19,20,21)],FBSprotein$AreaName,na.rm=TRUE)
FBSprotein.tot["AreaName"]<-row.names(FBSprotein.tot) #making a column with country in it
FBSprotein.tot$CO2.SE.weighted_lo<-sqrt(FBSprotein.tot$CO2.SE.weighted_lo)
FBSprotein.tot$CO2.SE.weighted_med<-sqrt(FBSprotein.tot$CO2.SE.weighted_med)
FBSprotein.tot$CO2.SE.weighted_hi<-sqrt(FBSprotein.tot$CO2.SE.weighted_hi)

#getting it back to SD from variance

#same again for total calories
FBSNRG.tot<-data.frame(rowsum(FBSNRG["Value"],FBSNRG$AreaName))
FBSNRG.tot["AreaName"]<-row.names(FBSNRG.tot)

#pasting three letter codes onto the protein dataset
FBSprotein.tot.code<-data.frame(merge(FBSprotein.tot,countrycode,by="AreaName"))
FBSNRG.tot.code<-data.frame(merge(FBSNRG.tot,countrycode,by="AreaName"))



#Sticking the energy and protein datasets together
FBStot<-data.frame(cbind(FBSprotein.tot.code,FBSNRG.tot.code[,2]))
names(FBStot)<-c("country",
                 "protein.amb.g",
                 "protein.amb.cal",
                 "protein.co2.g_lo",
                 "protein.co2.g_med",
                 "protein.co2.g_hi",
                 "protein.co2.cal_lo",
                 "protein.co2.cal_med",
                 "protein.co2.cal_hi",
                 "protein.co2.g.SE_lo",
                 "protein.co2.g.SE_med",
                 "protein.co2.g.SE_hi",
                 "order","countrycode","energy")

####calculating percent change in protein by g#####
#the mean bit
FBStot["protein.percent.decrease_lo"]<-((FBStot$protein.co2.g_lo/FBStot$protein.amb.g)-1)*100
FBStot["protein.percent.decrease_med"]<-((FBStot$protein.co2.g_med/FBStot$protein.amb.g)-1)*100
FBStot["protein.percent.decrease_hi"]<-((FBStot$protein.co2.g_hi/FBStot$protein.amb.g)-1)*100
#the SE bit
FBStot["protein.percent.decrease.SE_lo"]<-sqrt((FBStot$protein.co2.g.SE_lo^2)*((1/FBStot$protein.amb.g*100)^2))
FBStot["protein.percent.decrease.SE_med"]<-sqrt((FBStot$protein.co2.g.SE_med^2)*((1/FBStot$protein.amb.g*100)^2))
FBStot["protein.percent.decrease.SE_hi"]<-sqrt((FBStot$protein.co2.g.SE_hi^2)*((1/FBStot$protein.amb.g*100)^2))

###calculating difference in dietary protein ratio by cal####
#the mean bit
FBStot$protein.co2.cal_lo<-FBStot$protein.co2.g_lo*4.25
FBStot$protein.co2.cal_med<-FBStot$protein.co2.g_med*4.25
FBStot$protein.co2.cal_hi<-FBStot$protein.co2.g_hi*4.25


FBStot["protein.calorie.ratio.difference_lo"]<-((FBStot$protein.co2.cal_lo/FBStot$energy)-(FBStot$protein.amb.cal/FBStot$energy))*100
FBStot["protein.calorie.ratio.difference_med"]<-((FBStot$protein.co2.cal_med/FBStot$energy)-(FBStot$protein.amb.cal/FBStot$energy))*100
FBStot["protein.calorie.ratio.difference_hi"]<-((FBStot$protein.co2.cal_hi/FBStot$energy)-(FBStot$protein.amb.cal/FBStot$energy))*100


#the SE bit
FBStot["protein.calorie.ratio.SE_lo"]<-sqrt((FBStot$protein.co2.g.SE_lo^2)*(4.25/FBStot$energy*100)^2)
FBStot["protein.calorie.ratio.SE_med"]<-sqrt((FBStot$protein.co2.g.SE_med^2)*(4.25/FBStot$energy*100)^2)
FBStot["protein.calorie.ratio.SE_hi"]<-sqrt((FBStot$protein.co2.g.SE_hi^2)*(4.25/FBStot$energy*100)^2)

###Visualising the spread across countries in an arguably odd way####
plot(protein.calorie.ratio.difference_lo~protein.percent.decrease_lo,data=FBStot,pch=".")
text(FBStot$protein.percent.decrease_lo,FBStot$protein.calorie.ratio.difference_lo,FBStot$countrycode)

plot(protein.calorie.ratio.difference_med~protein.percent.decrease_med,data=FBStot,pch=".")
text(FBStot$protein.percent.decrease_med,FBStot$protein.calorie.ratio.difference_med,FBStot$countrycode)

plot(protein.calorie.ratio.difference_hi~protein.percent.decrease_hi,data=FBStot,pch=".")
text(FBStot$protein.percent.decrease_hi,FBStot$protein.calorie.ratio.difference_hi,FBStot$countrycode)

###To find what each country's staple protein source is####
staplerow<-NULL

for (country in 2:length(castProtein))
   {row.staple<-
    ifelse(which.max(castProtein[,country])>0, 
           which.max(castProtein[,country]),
           6)
    staplerow[country]<-row.staple
}
#though this will only find the first max, so it will underestimate wheat and moreso milk as they're at the bottom of the list.
#which.is.max will make it random rather than look down the columns. The question is, can you be bothered, because you'd have to cbind those different runs and do it a number of times.

staple<-data.frame("country"=names(castProtein),"proteinsource"=castProtein$ItemName[staplerow]) 
staple<-staple[-1,] #removing the first row whic was left over from the null matrix



staples.protein<-data.frame(merge(staple,FBStot, by="country"))
names(staples.protein)
# levels(staples.protein$source.meatorveg)
# staples.protein.single<-subset(staples.protein,source.meatorveg !="b")

staples.protein$proteinsource <- factor(staples.protein$proteinsource)
levels(staples.protein$proteinsource)

par(oma=c(6,1,1,1))
plot(protein.percent.decrease_lo~proteinsource,
     data=staples.protein,
     cex.axis=0.7,cex.main=0.8,
     las=2,
     xlab="", 
     main="percent change in protein by main protein source")



par(oma=c(6,1,1,1))
plot(protein.percent.decrease_med~proteinsource,
     data=staples.protein,
     cex.axis=0.7,cex.main=0.8,
     las=2,
     xlab="", 
     main="percent change in protein by main protein source")

par(oma=c(6,1,1,1))
plot(protein.percent.decrease_hi~proteinsource,
     data=staples.protein,
     cex.axis=0.7,cex.main=0.8,
     las=2,
     xlab="", 
     main="percent change in protein by main protein source")

#now save it. Got it?
dev.off()
###mapping####

library(rworldmap)
sPDF <- joinCountryData2Map(FBStot
                            , joinCode = "ISO3"
                            , nameJoinColumn = "countrycode"
)
mapCountryData(sPDF,nameColumnToPlot="protein.percent.decrease_lo")
mapCountryData(sPDF,nameColumnToPlot="protein.percent.decrease_med")
mapCountryData(sPDF,nameColumnToPlot="protein.percent.decrease_hi")

#mapCountryData(sPDF,nameColumnToPlot="protein.percent.decrease", catMethod=c(-20,-16,-8,-4,0), colourPalette = "black2White")
mapCountryData(sPDF,nameColumnToPlot="protein.calorie.ratio.difference")
#,  catMethod=c(-0.7,-0.6,-0.5,-0.4,-0.3,-0.2,-0.1,0), colourPalette = "black2White")



####making sure I have all three letter codes for all countries#####
# country_code<-sPDF$ISO3
# country<-sPDF$NAME_SORT
# country_plus_code<-data.frame("country"=country, "countrycode"=country_code)

# country_code_comparison<-merge(country_plus_code,countrycode, by="countrycode",all=TRUE)

####the main output####
write.csv(FBStot,"2_countries/outputs/FBStot.csv")
save(FBStot, file="2_countries/outputs/FBStot.Rdata")
