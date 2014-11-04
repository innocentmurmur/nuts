library(rworldmap)
####FBS#####

# countrycode<-read.csv("0_labels/country_plus_code.csv") #a list of countries and three letter codes that I have previously used for the FBS data.
FBSall<-read.csv("2_countries/inputs/FBS11.csv") #the raw FBS data for 2011
countriesFBS<-data.frame("countriestomergewithFBS"=c(levels(FBSall$AreaName)),"countriestomergewithsPDF"=c(levels(FBSall$AreaName)))
countriesFBS$countriestomergewithsPDF<-as.character(countriesFBS$countriestomergewithsPDF)

for(rownum in 1:length(countriesFBS$countriestomergewithFBS))
{if(countriesFBS$countriestomergewithsPDF[rownum]=="Antigua and Barbuda") 
  countriesFBS$countriestomergewithsPDF[rownum]<-"Antigua and Barb."
 if(countriesFBS$countriestomergewithsPDF[rownum]=="Bolivia (Plurinational State of)") 
   countriesFBS$countriestomergewithsPDF[rownum]<-"Bolivia"
 if(countriesFBS$countriestomergewithsPDF[rownum]=="Viet Nam")
   countriesFBS$countriestomergewithsPDF[rownum]<-"Vietnam"
 if(countriesFBS$countriestomergewithsPDF[rownum]=="United States of America")
   countriesFBS$countriestomergewithsPDF[rownum]<-"United States"
 if(countriesFBS$countriestomergewithsPDF[rownum]=="United Republic of Tanzania")
   countriesFBS$countriestomergewithsPDF[rownum]<-"Tanzania"
 if(countriesFBS$countriestomergewithsPDF[rownum]=="Timor-Leste")
   countriesFBS$countriestomergewithsPDF[rownum]<-"East Timor"
 if(countriesFBS$countriestomergewithsPDF[rownum]=="The former Yugoslav Republic of Macedonia")
   countriesFBS$countriestomergewithsPDF[rownum]<-"Macedonia"
 if(countriesFBS$countriestomergewithsPDF[rownum]=="Syrian Arab Republic")
   countriesFBS$countriestomergewithsPDF[rownum]<-"Syria"
 if(countriesFBS$countriestomergewithsPDF[rownum]=="Solomon Islands")
   countriesFBS$countriestomergewithsPDF[rownum]<-"Solomon Is."
 if(countriesFBS$countriestomergewithsPDF[rownum]=="Serbia and Montenegro")
   countriesFBS$countriestomergewithsPDF[rownum]<-"Serbia"
 if(countriesFBS$countriestomergewithsPDF[rownum]=="Russian Federation")
   countriesFBS$countriestomergewithsPDF[rownum]<-"Russia"
 if(countriesFBS$countriestomergewithsPDF[rownum]=="Republic of Moldova")
   countriesFBS$countriestomergewithsPDF[rownum]<-"Moldova"
 if(countriesFBS$countriestomergewithsPDF[rownum]=="Republic of Korea")
   countriesFBS$countriestomergewithsPDF[rownum]<-"S. Korea"
 if(countriesFBS$countriestomergewithsPDF[rownum]=="Republic of Korea")
   countriesFBS$countriestomergewithsPDF[rownum]<-"S. Korea"
 if(countriesFBS$countriestomergewithsPDF[rownum]=="China, Macao SAR")
   countriesFBS$countriestomergewithsPDF[rownum]<-"Macau"
 if(countriesFBS$countriestomergewithsPDF[rownum]=="Lao People's Democratic Republic")
   countriesFBS$countriestomergewithsPDF[rownum]<-"Laos"
 if(countriesFBS$countriestomergewithsPDF[rownum]=="Iran (Islamic Republic of)")
   countriesFBS$countriestomergewithsPDF[rownum]<-"Iran"
 if(countriesFBS$countriestomergewithsPDF[rownum]=="China, Hong Kong SAR")
   countriesFBS$countriestomergewithsPDF[rownum]<-"Hong Kong"
 if(countriesFBS$countriestomergewithsPDF[rownum]=="Guinea-Bissau")
   countriesFBS$countriestomergewithsPDF[rownum]<-"Guinea Bissau"
 if(countriesFBS$countriestomergewithsPDF[rownum]=="Federated States of Micronesia")
   countriesFBS$countriestomergewithsPDF[rownum]<-"Micronesia"
 if(countriesFBS$countriestomergewithsPDF[rownum]=="Federated States of Micronesia")
   countriesFBS$countriestomergewithsPDF[rownum]<-"Micronesia"
 if(countriesFBS$countriestomergewithsPDF[rownum]=="Dominican Republic")
   countriesFBS$countriestomergewithsPDF[rownum]<-"Dominican Rep."
 if(countriesFBS$countriestomergewithsPDF[rownum]=="Democratic People's Republic of Korea")
   countriesFBS$countriestomergewithsPDF[rownum]<-"N. Korea"
 if(countriesFBS$countriestomergewithsPDF[rownum]=="Czech Republic")
   countriesFBS$countriestomergewithsPDF[rownum]<-"Czech Rep."
 if(countriesFBS$countriestomergewithsPDF[rownum]=="Congo")
   countriesFBS$countriestomergewithsPDF[rownum]<-"Congo (Brazzaville)"
 if(countriesFBS$countriestomergewithsPDF[rownum]=="China, mainland")
   countriesFBS$countriestomergewithsPDF[rownum]<-"China"
 if(countriesFBS$countriestomergewithsPDF[rownum]=="Central African Republic")
   countriesFBS$countriestomergewithsPDF[rownum]<-"Central African Rep."
 if(countriesFBS$countriestomergewithsPDF[rownum]=="Brunei Darussalam")
   countriesFBS$countriestomergewithsPDF[rownum]<-"Brunei"
 if(countriesFBS$countriestomergewithsPDF[rownum]=="Bosnia and Herzegovina")
   countriesFBS$countriestomergewithsPDF[rownum]<-"Bosnia and Herz."
 if(countriesFBS$countriestomergewithsPDF[rownum]=="CÃ´te d'Ivoire")
   countriesFBS$countriestomergewithsPDF[rownum]<-"Ivory Coast"
 if(countriesFBS$countriestomergewithsPDF[rownum]=="Cabo Verde")
   countriesFBS$countriestomergewithsPDF[rownum]<-"Cape Verde"
 if(countriesFBS$countriestomergewithsPDF[rownum]=="China, Taiwan Province of")
   countriesFBS$countriestomergewithsPDF[rownum]<-"Taiwan"
 if(countriesFBS$countriestomergewithsPDF[rownum]=="Occupied Palestinian Territory")
   countriesFBS$countriestomergewithsPDF[rownum]<-"Gaza"
 if(countriesFBS$countriestomergewithsPDF[rownum]=="Sudan (former)")
   countriesFBS$countriestomergewithsPDF[rownum]<-"Sudan"
 if(countriesFBS$countriestomergewithsPDF[rownum]=="Venezuela (Bolivarian Republic of)")
    countriesFBS$countriestomergewithsPDF[rownum]<-"Venezuela"
}



countriesFBS$countriestomergewithsPDF<-as.factor(countriesFBS$countriestomergewithsPDF)


####merge with sPDF####
sPDF <- joinCountryData2Map(countriesFBS
                            , joinCode = "NAME"
                            , nameJoinColumn = "countriestomergewithsPDF"
)

View(sPDF)
write.csv(sPDF,"sPDF.csv")
fromsPDF<-data.frame("countriestomergewithsPDF"=sPDF$NAME,"countrycode"=sPDF$ISO3)
countriesFBS<-merge(countriesFBS,fromsPDF,by="countriestomergewithsPDF",all=TRUE)
countriesFBS$countrycode<-as.factor(countriesFBS$countrycode)
regions_and_small_islands<-subset(countriesFBS, is.na(countriesFBS$countrycode))


###intake####
# levels(proteinMCI$country)
# countriesHHS<-data.frame("country"=countriesFBS$countriestomergewithFBS, "countrycode"=countriesFBS$countrycode)
# proteinMCI<-merge(proteinMCI,countriesHHS, by="country")

write.csv(countriesFBS,"0_labels/countriesFBS.CSV")
