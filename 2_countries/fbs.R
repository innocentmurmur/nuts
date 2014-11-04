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

#combining the two FBS datasets, i mean I could've downloaded them together to begin with, but I didn't.
FBS<-rbind(FBSall,FBStots)

#removing extraneous columns
keeps<-c("AreaName","ElementName","ItemName","Value")
FBS<-FBS[keeps]


#adding the missing countries
missing_countries<-read.csv("2_countries/inputs/missing_countries_FBS_2011.csv")

FBS<-rbind(FBS,missing_countries)
##############making Wessels groups##################
fbspop<-read.csv("2_countries/inputs/pop from fbs 2011.csv")
fbspop<-fbspop[,c("AreaName","pop","countrycode")]


###HIGHIN####
HIGHIN.fbspop<-subset(fbspop,
                      AreaName=="Andorra" | AreaName=="Australia" | AreaName=="Austria" | AreaName=="Belgium" | AreaName=="Brunei Darussalam" | AreaName=="Canada" | AreaName=="Cyprus" | AreaName=="Denmark" | AreaName=="Finland" | AreaName=="France" | AreaName=="Germany" | AreaName=="Greece" | AreaName=="Greenland" | AreaName=="Iceland" | AreaName=="Ireland" | AreaName=="Israel" | AreaName=="Italy" | AreaName=="Japan" | AreaName==" Luxembourg" | AreaName=="Malta" | AreaName=="Netherlands" | AreaName=="New Zealand" | AreaName=="Norway" | AreaName=="Portugal" | AreaName=="Republic of Korea" | AreaName=="Singapore" | AreaName=="Spain" | AreaName=="Sweden" | AreaName=="Switzerland" | AreaName=="United Kingdom" | AreaName=="United States of America")
HIGHIN.FBS<-subset(FBS,
                   AreaName=="Andorra" | AreaName=="Australia" | AreaName=="Austria" | AreaName=="Belgium" | AreaName=="Brunei Darussalam" | AreaName=="Canada" | AreaName=="Cyprus" | AreaName=="Denmark" | AreaName=="Finland" | AreaName=="France" | AreaName=="Germany" | AreaName=="Greece" | AreaName=="Greenland" | AreaName=="Iceland" | AreaName=="Ireland" | AreaName=="Israel" | AreaName=="Italy" | AreaName=="Japan" | AreaName==" Luxembourg" | AreaName=="Malta" | AreaName=="Netherlands" | AreaName=="New Zealand" | AreaName=="Norway" | AreaName=="Portugal" | AreaName=="Republic of Korea" | AreaName=="Singapore" | AreaName=="Spain" | AreaName=="Sweden" | AreaName=="Switzerland" | AreaName=="United Kingdom" | AreaName=="United States of America")
length(row.names(HIGHIN.fbspop))
length(row.names(subset(HIGHIN.FBS,ElementName=="Protein supply quantity (g/capita/day)"& ItemName=="Wheat and products")))


HIGHIN.FBS<-merge(HIGHIN.FBS,HIGHIN.fbspop,by="AreaName")

HIGHIN.totpop<-sum(HIGHIN.FBS$pop[HIGHIN.FBS$ItemName=="Wheat and products" & HIGHIN.FBS$ElementName=="Protein supply quantity (g/capita/day)"])
HIGHIN.FBS$fraction_totpop<-HIGHIN.FBS$pop/HIGHIN.totpop
HIGHIN.FBSprotein<-subset(HIGHIN.FBS, ElementName=="Protein supply quantity (g/capita/day)")
HIGHIN.FBSnrg<-subset(HIGHIN.FBS, ElementName=="Food supply (kcal/capita/day)")

HIGHIN.FBSprotein$weightedValue<-HIGHIN.FBSprotein$Value*HIGHIN.FBSprotein$fraction_totpop
HIGHIN.FBSnrg$weightedValue<-HIGHIN.FBSnrg$Value*HIGHIN.FBSnrg$fraction_totpop


HIGHINnrg<-data.frame("ItemName"=NA,"Value"=NA)
for(food in levels(HIGHIN.FBSnrg$ItemName))
{HIGHINnrg1<-subset(HIGHIN.FBSnrg,ItemName==food)
 HIGHINnrg2<-data.frame("ItemName"=food,"Value"=sum(HIGHINnrg1$weightedValue))
 HIGHINnrg<-rbind(HIGHINnrg,HIGHINnrg2)
}

HIGHINnrg["AreaName"]="HIGHIN"
HIGHINnrg["ElementName"]="Food supply (kcal/capita/day)"

HIGHINnrg<-HIGHINnrg[,c("AreaName","ElementName","ItemName","Value")]

HIGHINprotein<-data.frame("ItemName"=NA,"Value"=NA)
for(food in levels(HIGHIN.FBSprotein$ItemName))
{HIGHINprotein1<-subset(HIGHIN.FBSprotein,ItemName==food)
 HIGHINprotein2<-data.frame("ItemName"=food,"Value"=sum(HIGHINprotein1$weightedValue))
 HIGHINprotein<-rbind(HIGHINprotein,HIGHINprotein2)
}
HIGHINprotein["AreaName"]="HIGHIN"
HIGHINprotein["ElementName"]="Protein supply quantity (g/capita/day)"
HIGHINprotein<-HIGHINprotein[,c("AreaName","ElementName","ItemName","Value")]

##SOTRLA####

SOTRLA.fbspop<-subset(fbspop,
                      AreaName=="Argentina" | AreaName=="Brazil" | AreaName=="Chile" | AreaName=="Paraguay" | AreaName=="Uruguay")
SOTRLA.FBS<-subset(FBS,
                   AreaName=="Argentina" | AreaName=="Brazil" | AreaName=="Chile" | AreaName=="Paraguay" | AreaName=="Uruguay"
                   )
SOTRLA.FBS<-merge(SOTRLA.FBS,SOTRLA.fbspop,by="AreaName")

SOTRLA.totpop<-sum(SOTRLA.FBS$pop[SOTRLA.FBS$ItemName=="Wheat and products" & SOTRLA.FBS$ElementName=="Protein supply quantity (g/capita/day)"])
SOTRLA.FBS$fraction_totpop<-SOTRLA.FBS$pop/SOTRLA.totpop
SOTRLA.FBSprotein<-subset(SOTRLA.FBS, ElementName=="Protein supply quantity (g/capita/day)")
SOTRLA.FBSnrg<-subset(SOTRLA.FBS, ElementName=="Food supply (kcal/capita/day)")

SOTRLA.FBSprotein$weightedValue<-SOTRLA.FBSprotein$Value*SOTRLA.FBSprotein$fraction_totpop
SOTRLA.FBSnrg$weightedValue<-SOTRLA.FBSnrg$Value*SOTRLA.FBSnrg$fraction_totpop


SOTRLAnrg<-data.frame("ItemName"=NA,"Value"=NA)
for(food in levels(SOTRLA.FBSnrg$ItemName))
{SOTRLAnrg1<-subset(SOTRLA.FBSnrg,ItemName==food)
 SOTRLAnrg2<-data.frame("ItemName"=food,"Value"=sum(SOTRLAnrg1$weightedValue))
 SOTRLAnrg<-rbind(SOTRLAnrg,SOTRLAnrg2)
}

SOTRLAnrg["AreaName"]="SOTRLA"
SOTRLAnrg["ElementName"]="Food supply (kcal/capita/day)"

SOTRLAnrg<-SOTRLAnrg[,c("AreaName","ElementName","ItemName","Value")]

SOTRLAprotein<-data.frame("ItemName"=NA,"Value"=NA)
for(food in levels(SOTRLA.FBSprotein$ItemName))
{SOTRLAprotein1<-subset(SOTRLA.FBSprotein,ItemName==food)
 SOTRLAprotein2<-data.frame("ItemName"=food,"Value"=sum(SOTRLAprotein1$weightedValue))
 SOTRLAprotein<-rbind(SOTRLAprotein,SOTRLAprotein2)
}
SOTRLAprotein["AreaName"]="SOTRLA"
SOTRLAprotein["ElementName"]="Protein supply quantity (g/capita/day)"
SOTRLAprotein<-SOTRLAprotein[,c("AreaName","ElementName","ItemName","Value")]

###CEEAEU####
CEEAEU.fbspop<-subset(fbspop,
                   AreaName=="Albania" | AreaName=="Belarus" | AreaName=="Bosnia and Herzegovina" | AreaName=="Bulgaria" | AreaName=="Croatia" | AreaName=="Czech Republic" | AreaName=="Estonia" | AreaName=="Hungary" | AreaName=="Latvia" | AreaName=="Lithuania" | AreaName=="The former Yugoslav Republic of Macedonia" | AreaName=="Republic of Moldova" | AreaName=="Montenegro" | AreaName=="Poland" | AreaName=="Romania" | AreaName=="Russian Federation" | AreaName=="Serbia" | AreaName=="Slovakia" | AreaName=="Slovenia" | AreaName=="Ukraine")
CEEAEU.FBS<-subset(FBS,
                   AreaName=="Albania" | AreaName=="Belarus" | AreaName=="Bosnia and Herzegovina" | AreaName=="Bulgaria" | AreaName=="Croatia" | AreaName=="Czech Republic" | AreaName=="Estonia" | AreaName=="Hungary" | AreaName=="Latvia" | AreaName=="Lithuania" | AreaName=="The former Yugoslav Republic of Macedonia" | AreaName=="Republic of Moldova" | AreaName=="Montenegro" | AreaName=="Poland" | AreaName=="Romania" | AreaName=="Russian Federation" | AreaName=="Serbia" | AreaName=="Slovakia" | AreaName=="Slovenia" | AreaName=="Ukraine")

CEEAEU.FBS<-merge(CEEAEU.FBS,CEEAEU.fbspop,by="AreaName")

CEEAEU.totpop<-sum(CEEAEU.FBS$pop[CEEAEU.FBS$ItemName=="Wheat and products" & CEEAEU.FBS$ElementName=="Protein supply quantity (g/capita/day)"])
CEEAEU.FBS$fraction_totpop<-CEEAEU.FBS$pop/CEEAEU.totpop
CEEAEU.FBSprotein<-subset(CEEAEU.FBS, ElementName=="Protein supply quantity (g/capita/day)")
CEEAEU.FBSnrg<-subset(CEEAEU.FBS, ElementName=="Food supply (kcal/capita/day)")

CEEAEU.FBSprotein$weightedValue<-CEEAEU.FBSprotein$Value*CEEAEU.FBSprotein$fraction_totpop
CEEAEU.FBSnrg$weightedValue<-CEEAEU.FBSnrg$Value*CEEAEU.FBSnrg$fraction_totpop


CEEAEUnrg<-data.frame("ItemName"=NA,"Value"=NA)
for(food in levels(CEEAEU.FBSnrg$ItemName))
{CEEAEUnrg1<-subset(CEEAEU.FBSnrg,ItemName==food)
 CEEAEUnrg2<-data.frame("ItemName"=food,"Value"=sum(CEEAEUnrg1$weightedValue))
 CEEAEUnrg<-rbind(CEEAEUnrg,CEEAEUnrg2)
}

CEEAEUnrg["AreaName"]="CEEAEU"
CEEAEUnrg["ElementName"]="Food supply (kcal/capita/day)"

CEEAEUnrg<-CEEAEUnrg[,c("AreaName","ElementName","ItemName","Value")]

CEEAEUprotein<-data.frame("ItemName"=NA,"Value"=NA)
for(food in levels(CEEAEU.FBSprotein$ItemName))
{CEEAEUprotein1<-subset(CEEAEU.FBSprotein,ItemName==food)
 CEEAEUprotein2<-data.frame("ItemName"=food,"Value"=sum(CEEAEUprotein1$weightedValue))
 CEEAEUprotein<-rbind(CEEAEUprotein,CEEAEUprotein2)
}
CEEAEUprotein["AreaName"]="CEEAEU"
CEEAEUprotein["ElementName"]="Protein supply quantity (g/capita/day)"
CEEAEUprotein<-CEEAEUprotein[,c("AreaName","ElementName","ItemName","Value")]

##CALACA####
CALACA.fbspop<-subset(fbspop,
                      AreaName=="Antigua and Barbuda" | AreaName=="Bahamas" | AreaName=="Barbados" | AreaName=="Belize" | AreaName=="Bermuda" | AreaName=="Bolivia (Plurinational State of)" | AreaName=="British Virgin Islands" | AreaName=="Colombia" | AreaName=="Costa Rica" | AreaName=="Cuba" | AreaName=="Dominica" | AreaName=="Dominican Republic" | AreaName=="Ecuador" | AreaName=="El Salvador" | AreaName=="Grenada" | AreaName=="Guatemala" | AreaName=="Guyana" | AreaName=="Haiti" | AreaName=="Honduras" | AreaName=="Jamaica" | AreaName=="Mexico" | AreaName=="Nicaragua" | AreaName=="Netherlands Antilles" | AreaName=="Panama" | AreaName=="Peru" | AreaName=="Puerto Rico" | AreaName=="Saint Kitts and Nevis" | AreaName=="Saint Lucia" | AreaName=="Saint Vincent and the Grenadines" | AreaName=="Suriname" | AreaName=="Trinidad and Tobago" | AreaName=="Venezuela (Bolivarian Republic of)")
CALACA.FBS<-subset(FBS,
                   AreaName=="Antigua and Barbuda" | AreaName=="Bahamas" | AreaName=="Barbados" | AreaName=="Belize" | AreaName=="Bermuda" | AreaName=="Bolivia (Plurinational State of)" | AreaName=="British Virgin Islands" | AreaName=="Colombia" | AreaName=="Costa Rica" | AreaName=="Cuba" | AreaName=="Dominica" | AreaName=="Dominican Republic" | AreaName=="Ecuador" | AreaName=="El Salvador" | AreaName=="Grenada" | AreaName=="Guatemala" | AreaName=="Guyana" | AreaName=="Haiti" | AreaName=="Honduras" | AreaName=="Jamaica" | AreaName=="Mexico" | AreaName=="Nicaragua" | AreaName=="Netherlands Antilles" | AreaName=="Panama" | AreaName=="Peru" | AreaName=="Puerto Rico" | AreaName=="Saint Kitts and Nevis" | AreaName=="Saint Lucia" | AreaName=="Saint Vincent and the Grenadines" | AreaName=="Suriname" | AreaName=="Trinidad and Tobago" | AreaName=="Venezuela (Bolivarian Republic of)")

CALACA.FBS<-merge(CALACA.FBS,CALACA.fbspop,by="AreaName")

CALACA.totpop<-sum(CALACA.FBS$pop[CALACA.FBS$ItemName=="Wheat and products" & CALACA.FBS$ElementName=="Protein supply quantity (g/capita/day)"])
CALACA.FBS$fraction_totpop<-CALACA.FBS$pop/CALACA.totpop
CALACA.FBSprotein<-subset(CALACA.FBS, ElementName=="Protein supply quantity (g/capita/day)")
CALACA.FBSnrg<-subset(CALACA.FBS, ElementName=="Food supply (kcal/capita/day)")

CALACA.FBSprotein$weightedValue<-CALACA.FBSprotein$Value*CALACA.FBSprotein$fraction_totpop
CALACA.FBSnrg$weightedValue<-CALACA.FBSnrg$Value*CALACA.FBSnrg$fraction_totpop


CALACAnrg<-data.frame("ItemName"=NA,"Value"=NA)
for(food in levels(CALACA.FBSnrg$ItemName))
{CALACAnrg1<-subset(CALACA.FBSnrg,ItemName==food)
 CALACAnrg2<-data.frame("ItemName"=food,"Value"=sum(CALACAnrg1$weightedValue))
 CALACAnrg<-rbind(CALACAnrg,CALACAnrg2)
}

CALACAnrg["AreaName"]="CALACA"
CALACAnrg["ElementName"]="Food supply (kcal/capita/day)"

CALACAnrg<-CALACAnrg[,c("AreaName","ElementName","ItemName","Value")]

CALACAprotein<-data.frame("ItemName"=NA,"Value"=NA)
for(food in levels(CALACA.FBSprotein$ItemName))
{CALACAprotein1<-subset(CALACA.FBSprotein,ItemName==food)
 CALACAprotein2<-data.frame("ItemName"=food,"Value"=sum(CALACAprotein1$weightedValue))
 CALACAprotein<-rbind(CALACAprotein,CALACAprotein2)
}
CALACAprotein["AreaName"]="CALACA"
CALACAprotein["ElementName"]="Protein supply quantity (g/capita/day)"
CALACAprotein<-CALACAprotein[,c("AreaName","ElementName","ItemName","Value")]

###CANAME####
CANAME.fbspop<-subset(fbspop,
                   AreaName=="Algeria" | AreaName=="Armenia" | AreaName=="Azerbaijan" | AreaName=="Bahrain" | AreaName=="Egypt" | AreaName=="Georgia" | AreaName=="Iran (Islamic Republic of)" | AreaName=="Iraq" | AreaName=="Jordan" | AreaName=="Kazakhstan" | AreaName==" Kuwait" | AreaName=="Kyrgyzstan" | AreaName=="Lebanon" | AreaName=="Libya" | AreaName=="Mongolia" | AreaName=="Morocco" | AreaName=="Occupied Palestinian Territory" | AreaName=="Oman" | AreaName=="Qatar" | AreaName=="Saudi Arabia" | AreaName=="Syrian Arab Republic" | AreaName=="Tajikistan" | AreaName=="Tunisia" | AreaName=="Turkey" | AreaName=="Turkmenistan" | AreaName=="United Arab Emirates" | AreaName=="Uzbekistan" | AreaName=="Yemen")
CANAME.FBS<-subset(FBS,
                   AreaName=="Algeria" | AreaName=="Armenia" | AreaName=="Azerbaijan" | AreaName=="Bahrain" | AreaName=="Egypt" | AreaName=="Georgia" | AreaName=="Iran (Islamic Republic of)" | AreaName=="Iraq" | AreaName=="Jordan" | AreaName=="Kazakhstan" | AreaName==" Kuwait" | AreaName=="Kyrgyzstan" | AreaName=="Lebanon" | AreaName=="Libya" | AreaName=="Mongolia" | AreaName=="Morocco" | AreaName=="Occupied Palestinian Territory" | AreaName=="Oman" | AreaName=="Qatar" | AreaName=="Saudi Arabia" | AreaName=="Syrian Arab Republic" | AreaName=="Tajikistan" | AreaName=="Tunisia" | AreaName=="Turkey" | AreaName=="Turkmenistan" | AreaName=="United Arab Emirates" | AreaName=="Uzbekistan" | AreaName=="Yemen")

CANAME.FBS<-merge(CANAME.FBS,CANAME.fbspop,by="AreaName")

CANAME.totpop<-sum(CANAME.FBS$pop[CANAME.FBS$ItemName=="Wheat and products" & CANAME.FBS$ElementName=="Protein supply quantity (g/capita/day)"])
CANAME.FBS$fraction_totpop<-CANAME.FBS$pop/CANAME.totpop
CANAME.FBSprotein<-subset(CANAME.FBS, ElementName=="Protein supply quantity (g/capita/day)")
CANAME.FBSnrg<-subset(CANAME.FBS, ElementName=="Food supply (kcal/capita/day)")

CANAME.FBSprotein$weightedValue<-CANAME.FBSprotein$Value*CANAME.FBSprotein$fraction_totpop
CANAME.FBSnrg$weightedValue<-CANAME.FBSnrg$Value*CANAME.FBSnrg$fraction_totpop


CANAMEnrg<-data.frame("ItemName"=NA,"Value"=NA)
for(food in levels(CANAME.FBSnrg$ItemName))
{CANAMEnrg1<-subset(CANAME.FBSnrg,ItemName==food)
 CANAMEnrg2<-data.frame("ItemName"=food,"Value"=sum(CANAMEnrg1$weightedValue))
 CANAMEnrg<-rbind(CANAMEnrg,CANAMEnrg2)
}

CANAMEnrg["AreaName"]="CANAME"
CANAMEnrg["ElementName"]="Food supply (kcal/capita/day)"

CANAMEnrg<-CANAMEnrg[,c("AreaName","ElementName","ItemName","Value")]

CANAMEprotein<-data.frame("ItemName"=NA,"Value"=NA)
for(food in levels(CANAME.FBSprotein$ItemName))
{CANAMEprotein1<-subset(CANAME.FBSprotein,ItemName==food)
 CANAMEprotein2<-data.frame("ItemName"=food,"Value"=sum(CANAMEprotein1$weightedValue))
 CANAMEprotein<-rbind(CANAMEprotein,CANAMEprotein2)
}
CANAMEprotein["AreaName"]="CANAME"
CANAMEprotein["ElementName"]="Protein supply quantity (g/capita/day)"
CANAMEprotein<-CANAMEprotein[,c("AreaName","ElementName","ItemName","Value")]

###ESEASP####
ESEASP.fbspop<-subset(fbspop,
                   AreaName=="Cambodia" | AreaName=="Cook Islands" | AreaName=="Democratic People’s Republic of Korea" | AreaName=="Fiji" | AreaName=="French Polynesia" | AreaName=="Indonesia" | AreaName=="Kiribati" | AreaName==" Lao People's Democratic Republic" | AreaName=="Malaysia" | AreaName=="Maldives" | AreaName=="Marshall Islands" | AreaName=="Micronesia" | AreaName=="Myanmar" | AreaName=="Nauru" | AreaName=="Palau" | AreaName=="Papua New Guinea" | AreaName=="Philippines" | AreaName=="Samoa" | AreaName=="Solomon Islands" | AreaName=="Sri Lanka" | AreaName=="Thailand" | AreaName=="Timor-Leste" | AreaName=="Tonga" | AreaName=="Viet Nam" | AreaName=="Vanuatu")

ESEASP.FBS<-subset(FBS,
                   AreaName=="Cambodia" | AreaName=="Cook Islands" | AreaName=="Democratic People’s Republic of Korea" | AreaName=="Fiji" | AreaName=="French Polynesia" | AreaName=="Indonesia" | AreaName=="Kiribati" | AreaName==" Lao People's Democratic Republic" | AreaName=="Malaysia" | AreaName=="Maldives" | AreaName=="Marshall Islands" | AreaName=="Micronesia" | AreaName=="Myanmar" | AreaName=="Nauru" | AreaName=="Palau" | AreaName=="Papua New Guinea" | AreaName=="Philippines" | AreaName=="Samoa" | AreaName=="Solomon Islands" | AreaName=="Sri Lanka" | AreaName=="Thailand" | AreaName=="Timor-Leste" | AreaName=="Tonga" | AreaName=="Viet Nam" | AreaName=="Vanuatu")
ESEASP.FBS<-merge(ESEASP.FBS,ESEASP.fbspop,by="AreaName")

ESEASP.totpop<-sum(ESEASP.FBS$pop[ESEASP.FBS$ItemName=="Wheat and products" & ESEASP.FBS$ElementName=="Protein supply quantity (g/capita/day)"])
ESEASP.FBS$fraction_totpop<-ESEASP.FBS$pop/ESEASP.totpop
ESEASP.FBSprotein<-subset(ESEASP.FBS, ElementName=="Protein supply quantity (g/capita/day)")
ESEASP.FBSnrg<-subset(ESEASP.FBS, ElementName=="Food supply (kcal/capita/day)")

ESEASP.FBSprotein$weightedValue<-ESEASP.FBSprotein$Value*ESEASP.FBSprotein$fraction_totpop
ESEASP.FBSnrg$weightedValue<-ESEASP.FBSnrg$Value*ESEASP.FBSnrg$fraction_totpop


ESEASPnrg<-data.frame("ItemName"=NA,"Value"=NA)
for(food in levels(ESEASP.FBSnrg$ItemName))
{ESEASPnrg1<-subset(ESEASP.FBSnrg,ItemName==food)
 ESEASPnrg2<-data.frame("ItemName"=food,"Value"=sum(ESEASPnrg1$weightedValue))
 ESEASPnrg<-rbind(ESEASPnrg,ESEASPnrg2)
}

ESEASPnrg["AreaName"]="ESEASP"
ESEASPnrg["ElementName"]="Food supply (kcal/capita/day)"

ESEASPnrg<-ESEASPnrg[,c("AreaName","ElementName","ItemName","Value")]

ESEASPprotein<-data.frame("ItemName"=NA,"Value"=NA)
for(food in levels(ESEASP.FBSprotein$ItemName))
{ESEASPprotein1<-subset(ESEASP.FBSprotein,ItemName==food)
 ESEASPprotein2<-data.frame("ItemName"=food,"Value"=sum(ESEASPprotein1$weightedValue))
 ESEASPprotein<-rbind(ESEASPprotein,ESEASPprotein2)
}
ESEASPprotein["AreaName"]="ESEASP"
ESEASPprotein["ElementName"]="Protein supply quantity (g/capita/day)"
ESEASPprotein<-ESEASPprotein[,c("AreaName","ElementName","ItemName","Value")]

###SUSAAF####
SUSAAF.fbspop<-subset(fbspop,
                   AreaName=="Angola" | AreaName=="Botswana" | AreaName=="Burkina Faso" | AreaName=="Burundi" | AreaName=="Cameroon" | AreaName=="Cabo Verde" | AreaName=="Central African Republic" | AreaName=="Chad" | AreaName=="Comoros" | AreaName=="Congo" | AreaName=="CÃ´te d'Ivoire" | AreaName=="Democratic Republic of the Congo" | AreaName=="Djibouti" | AreaName=="Eritrea" | AreaName=="Ethiopia" | AreaName=="Equatorial Guinea" | AreaName=="Gabon" | AreaName=="Gambia" | AreaName=="Ghana" | AreaName=="Guinea" | AreaName=="Guinea-Bissau" | AreaName=="Lesotho" | AreaName=="Liberia" | AreaName=="Kenya" | AreaName=="Madagascar" | AreaName=="Malawi" | AreaName=="Mali" | AreaName=="Mauritania" | AreaName=="Mauritius" | AreaName=="Mozambique" | AreaName=="Namibia" | AreaName=="Niger" | AreaName=="Nigeria" | AreaName=="Rwanda" | AreaName=="Sao Tome and Principe" | AreaName=="Senegal" | AreaName=="Seychelles" | AreaName=="Sierra Leone" | AreaName=="Somalia" | AreaName=="South Africa" | AreaName=="Sudan (former)" | AreaName=="Swaziland" | AreaName=="Togo" | AreaName=="Uganda" | AreaName=="United Republic of Tanzania" | AreaName=="Zambia" | AreaName=="Zimbabwe")
SUSAAF.FBS<-subset(FBS,
                      AreaName=="Angola" | AreaName=="Botswana" | AreaName=="Burkina Faso" | AreaName=="Burundi" | AreaName=="Cameroon" | AreaName=="Cabo Verde" | AreaName=="Central African Republic" | AreaName=="Chad" | AreaName=="Comoros" | AreaName=="Congo" | AreaName=="CÃ´te d'Ivoire" | AreaName=="Democratic Republic of the Congo" | AreaName=="Djibouti" | AreaName=="Eritrea" | AreaName=="Ethiopia" | AreaName=="Equatorial Guinea" | AreaName=="Gabon" | AreaName=="Gambia" | AreaName=="Ghana" | AreaName=="Guinea" | AreaName=="Guinea-Bissau" | AreaName=="Lesotho" | AreaName=="Liberia" | AreaName=="Kenya" | AreaName=="Madagascar" | AreaName=="Malawi" | AreaName=="Mali" | AreaName=="Mauritania" | AreaName=="Mauritius" | AreaName=="Mozambique" | AreaName=="Namibia" | AreaName=="Niger" | AreaName=="Nigeria" | AreaName=="Rwanda" | AreaName=="Sao Tome and Principe" | AreaName=="Senegal" | AreaName=="Seychelles" | AreaName=="Sierra Leone" | AreaName=="Somalia" | AreaName=="South Africa" | AreaName=="Sudan (former)" | AreaName=="Swaziland" | AreaName=="Togo" | AreaName=="Uganda" | AreaName=="United Republic of Tanzania" | AreaName=="Zambia" | AreaName=="Zimbabwe")

SUSAAF.FBS<-merge(SUSAAF.FBS,SUSAAF.fbspop,by="AreaName")

SUSAAF.totpop<-sum(SUSAAF.FBS$pop[SUSAAF.FBS$ItemName=="Wheat and products" & SUSAAF.FBS$ElementName=="Protein supply quantity (g/capita/day)"])
SUSAAF.FBS$fraction_totpop<-SUSAAF.FBS$pop/SUSAAF.totpop
SUSAAF.FBSprotein<-subset(SUSAAF.FBS, ElementName=="Protein supply quantity (g/capita/day)")
SUSAAF.FBSnrg<-subset(SUSAAF.FBS, ElementName=="Food supply (kcal/capita/day)")

SUSAAF.FBSprotein$weightedValue<-SUSAAF.FBSprotein$Value*SUSAAF.FBSprotein$fraction_totpop
SUSAAF.FBSnrg$weightedValue<-SUSAAF.FBSnrg$Value*SUSAAF.FBSnrg$fraction_totpop


SUSAAFnrg<-data.frame("ItemName"=NA,"Value"=NA)
for(food in levels(SUSAAF.FBSnrg$ItemName))
{SUSAAFnrg1<-subset(SUSAAF.FBSnrg,ItemName==food)
 SUSAAFnrg2<-data.frame("ItemName"=food,"Value"=sum(SUSAAFnrg1$weightedValue))
 SUSAAFnrg<-rbind(SUSAAFnrg,SUSAAFnrg2)
}

SUSAAFnrg["AreaName"]="SUSAAF"
SUSAAFnrg["ElementName"]="Food supply (kcal/capita/day)"

SUSAAFnrg<-SUSAAFnrg[,c("AreaName","ElementName","ItemName","Value")]

SUSAAFprotein<-data.frame("ItemName"=NA,"Value"=NA)
for(food in levels(SUSAAF.FBSprotein$ItemName))
{SUSAAFprotein1<-subset(SUSAAF.FBSprotein,ItemName==food)
 SUSAAFprotein2<-data.frame("ItemName"=food,"Value"=sum(SUSAAFprotein1$weightedValue))
 SUSAAFprotein<-rbind(SUSAAFprotein,SUSAAFprotein2)
}
SUSAAFprotein["AreaName"]="SUSAAF"
SUSAAFprotein["ElementName"]="Protein supply quantity (g/capita/day)"
SUSAAFprotein<-SUSAAFprotein[,c("AreaName","ElementName","ItemName","Value")]


###SOASIA####
SOASIA.fbspop<-subset(fbspop,
                   AreaName=="Afghanistan" | AreaName=="Bangladesh" | AreaName=="Bhutan" | AreaName=="Nepal" | AreaName=="Pakistan")

SOASIA.FBS<-subset(FBS,
                   AreaName=="Afghanistan" | AreaName=="Bangladesh" | AreaName=="Bhutan" | AreaName=="Nepal" | AreaName=="Pakistan")


SOASIA.FBS<-merge(SOASIA.FBS,SOASIA.fbspop,by="AreaName")

SOASIA.totpop<-sum(SOASIA.FBS$pop[SOASIA.FBS$ItemName=="Wheat and products" & SOASIA.FBS$ElementName=="Protein supply quantity (g/capita/day)"])
SOASIA.FBS$fraction_totpop<-SOASIA.FBS$pop/SOASIA.totpop
SOASIA.FBSprotein<-subset(SOASIA.FBS, ElementName=="Protein supply quantity (g/capita/day)")
SOASIA.FBSnrg<-subset(SOASIA.FBS, ElementName=="Food supply (kcal/capita/day)")

SOASIA.FBSprotein$weightedValue<-SOASIA.FBSprotein$Value*SOASIA.FBSprotein$fraction_totpop
SOASIA.FBSnrg$weightedValue<-SOASIA.FBSnrg$Value*SOASIA.FBSnrg$fraction_totpop


SOASIAnrg<-data.frame("ItemName"=NA,"Value"=NA)
for(food in levels(SOASIA.FBSnrg$ItemName))
{SOASIAnrg1<-subset(SOASIA.FBSnrg,ItemName==food)
 SOASIAnrg2<-data.frame("ItemName"=food,"Value"=sum(SOASIAnrg1$weightedValue))
 SOASIAnrg<-rbind(SOASIAnrg,SOASIAnrg2)
}

SOASIAnrg["AreaName"]="SOASIA"
SOASIAnrg["ElementName"]="Food supply (kcal/capita/day)"

SOASIAnrg<-SOASIAnrg[,c("AreaName","ElementName","ItemName","Value")]

SOASIAprotein<-data.frame("ItemName"=NA,"Value"=NA)
for(food in levels(SOASIA.FBSprotein$ItemName))
{SOASIAprotein1<-subset(SOASIA.FBSprotein,ItemName==food)
 SOASIAprotein2<-data.frame("ItemName"=food,"Value"=sum(SOASIAprotein1$weightedValue))
 SOASIAprotein<-rbind(SOASIAprotein,SOASIAprotein2)
}
SOASIAprotein["AreaName"]="SOASIA"
SOASIAprotein["ElementName"]="Protein supply quantity (g/capita/day)"
SOASIAprotein<-SOASIAprotein[,c("AreaName","ElementName","ItemName","Value")]


FBSgroups<-rbind(
           HIGHINnrg,
           HIGHINprotein,
           SOTRLAnrg,
           SOTRLAprotein,
           CEEAEUnrg,
           CEEAEUprotein,
           CALACAnrg, 
           CALACAprotein,
           CANAMEnrg,
           CANAMEprotein,
           ESEASPnrg,
           ESEASPprotein,
           SUSAAFnrg,
           SUSAAFprotein,
           SOASIAnrg,
           SOASIAprotein
           )

FBS<-rbind(FBS,FBSgroups)

continentpops<-data.frame("pop"=c(CALACA.totpop,CANAME.totpop,CEEAEU.totpop,ESEASP.totpop,HIGHIN.totpop,SOTRLA.totpop,SUSAAF.totpop,CANAME.totpop),
                          "country"=c("CALACA","CANAME","CEEAEU","ESEASP","HIGHIN","SOTRLA","SUSAAF","SOASIA"),
                          "countrycode"=c("CALACA","CANAME","CEEAEU","ESEASP","HIGHIN","SOTRLA","SUSAAF","SOASIA")
)

save(continentpops,file="2_countries/outputs/continentpops.Rdata")
####making the CO2 dataset####

load("2_countries/outputs/preCO2.Rdata")   
CO2<- data.frame(lapply(preCO2, as.character), stringsAsFactors=FALSE)
names(CO2)[1]<-"ItemName"
names(CO2)[3]<-"CO2RR"
names(CO2)[4]<-"CO2SE"

CO2$CO2RR<-as.numeric(CO2$CO2RR)
CO2$CO2SE<-as.numeric(CO2$CO2SE)


#adding the percent change in protein onto the FBS data
FBSco2<-merge(FBS,CO2,by="ItemName")

#splitting up the energy and protein into dataframes
FBSNRG<-subset(FBSco2,ElementName=="Food supply (kcal/capita/day)")
names(FBSNRG)
FBSNRG<-FBSNRG[c(keeps,"VegOrMeat")]


#sum for total calories
FBSNRG.tot<-data.frame(rowsum(FBSNRG["Value"],FBSNRG$AreaName))
FBSNRG.tot["AreaName"]<-row.names(FBSNRG.tot)


#vego diets
FBSNRG.veg<-subset(FBSNRG,VegOrMeat=="Veg")
FBSNRG.vegtot<-data.frame(rowsum(FBSNRG.veg["Value"],FBSNRG.veg$AreaName))
FBSNRG.vegtot["AreaName"]<-row.names(FBSNRG.vegtot)


FBSNRG.RatioOmiToVego<-data.frame("AreaName"=FBSNRG.tot$AreaName,"NRGratio"=FBSNRG.tot$Value/FBSNRG.vegtot$Value)






FBSprotein<-subset(FBSco2,ElementName=="Protein supply quantity (g/capita/day)")
FBSprotein<-data.frame(merge(FBSprotein,countrycode,by="AreaName",all=TRUE))
FBSprotein$countrycode[FBSprotein$AreaName=="CÃ´te d'Ivoire"]<-"CIV"
#calculating the variance, weighted grams per capita per day
FBSprotein["CO2.SE.weighted"]<-((FBSprotein$Value)^2)*((FBSprotein$CO2SE)^2)
# FBSprotein.veg["CO2.SE.weighted"]<-((FBSprotein.veg$Value)^2)*((FBSprotein.veg$CO2SE)^2) moved this up.

###adjusting for vegetables, other and cereals, other####
#load the csv with the precent of cereals other and vegetables other that is C3, C4, legumes, cassava leaves
cerealvegadj<-read.csv("2_countries/inputs/vegetable_cereal_adjustment.csv")

#create a subset of FBSprotein that is cereals, other and another that's vegetables, other
FBSprotein.vegother<-subset(FBSprotein,ItemName=="Vegetables, Other")
FBSprotein.cerealother<-subset(FBSprotein,ItemName=="Cereals, Other")

#create a subset of FBSprotein that is !cereals and !veg
FBSprotein.notVCother<-subset(FBSprotein,!(ItemName %in% c("Vegetables, Other","Cereals, Other")))

#get the relative change under high CO2 for C3, C4, legumes
load("1_CO2/outputs/predictions.type.Rdata")
RRC3<-predictions.type$pred[predictions.type$plant=="C3"]
RRC4<-predictions.type$pred[predictions.type$plant=="C4"]

RRVeg<-predictions.type$pred[predictions.type$plant=="Veg"]
RRPulse<-predictions.type$pred[predictions.type$plant=="Pulse"]

#get the CO2 SEs
SEC3<-predictions.type$se[predictions.type$plant=="C3"]
SEC4<-predictions.type$se[predictions.type$plant=="C4"]

SEVeg<-predictions.type$se[predictions.type$plant=="Veg"]
SEPulse<-predictions.type$se[predictions.type$plant=="Pulse"]

VarC3<-SEC3^2
VarC4<-SEC4^2
VarVeg<-SEVeg^2
VarPulse<-SEPulse^2
# the response ratio will be RR C3* fraction C3 + RRC4 * fraction C4...+
cerealvegadj$cerealadj<-(RRC3*cerealvegadj$c3.grain.fraction)+(RRC4*cerealvegadj$c4.grain.fraction.or.none)
cerealvegadj$vegadj<-(RRVeg*cerealvegadj$fraction.C3.veg)+(RRC4*cerealvegadj$fraction.c4.veg)+(RRPulse*cerealvegadj$fraction.legumes)


# the SE will be sqrt( (SEC3^2*fraction C3^2) + ...      )
cerealvegadj$cerealVar<-VarC3*((cerealvegadj$c3.grain.fraction)^2)+VarC4*((cerealvegadj$c4.grain.fraction.or.none)^2)
cerealvegadj$vegVar<-VarVeg*((cerealvegadj$fraction.C3.veg)^2)+VarC4*((cerealvegadj$fraction.c4.veg)^2)+VarPulse*((cerealvegadj$fraction.legumes)^2)


FBSprotein.cerealotheradj<-merge(FBSprotein.cerealother[c("ItemName","AreaName","ElementName","Value","DerivedFrom", "countrycode")],
            cerealvegadj[c("countrycode","cerealadj","cerealVar")],
            by="countrycode",
            all.x=TRUE)

FBSprotein.vegotheradj<-merge(FBSprotein.vegother[c("ItemName","AreaName","ElementName","Value","DerivedFrom", "countrycode")],
                                 cerealvegadj[c("countrycode","vegadj","vegVar")],
                                 by="countrycode",
                                 all.x=TRUE)

#fixing the missing values that actually matter
FBSprotein.cerealotheradj$cerealadj[FBSprotein.cerealotheradj$AreaName %in% c("Land Locked Developing Countries","Least Developed Countries","Africa","Eastern Africa","Middle Africa","Net Food Importing Developing Countries")]<-rep(FBSprotein.cerealotheradj$cerealadj[FBSprotein.cerealotheradj$AreaName=="Mali"])
FBSprotein.cerealotheradj$cerealVar[FBSprotein.cerealotheradj$AreaName %in% c("Land Locked Developing Countries","Least Developed Countries","Africa","Eastern Africa","Middle Africa","Net Food Importing Developing Countries")]<-rep(FBSprotein.cerealotheradj$cerealVar[FBSprotein.cerealotheradj$AreaName=="Mali"])
FBSprotein.cerealotheradj$cerealadj[FBSprotein.cerealotheradj$AreaName %in% c("China, Hong Kong SAR","China, Macao SAR")]<-rep(FBSprotein.cerealotheradj$cerealadj[FBSprotein.cerealotheradj$AreaName=="China"])
FBSprotein.cerealotheradj$cerealVar[FBSprotein.cerealotheradj$AreaName %in% c("China, Hong Kong SAR","China, Macao SAR")]<-rep(FBSprotein.cerealotheradj$cerealVar[FBSprotein.cerealotheradj$AreaName=="China"])

FBSprotein.cerealotheradj<-subset(FBSprotein.cerealotheradj,FBSprotein.cerealotheradj$Value>0)


#missing countries in the Veg table: give them the values for the continent they belong to
FBSprotein.vegotheradj$AreaName[is.na(FBSprotein.vegotheradj$vegVar)]

#SOASIA
FBSprotein.vegotheradj$vegadj[FBSprotein.vegotheradj$AreaName %in%c("Southern Asia","Afghanistan")]<-rep(FBSprotein.vegotheradj$vegadj[FBSprotein.vegotheradj$AreaName=="SOASIA"])
#SUSAAF
FBSprotein.vegotheradj$vegadj[FBSprotein.vegotheradj$AreaName %in%c("World","Western Africa","Somalia","Net Food Importing Developing Countries","Land Locked Developing Countries","Southern Africa","Middle Africa","Africa","Eastern Africa","Least Developed Countries","Low Income Food Deficit Countries")]<-rep(FBSprotein.vegotheradj$vegadj[FBSprotein.vegotheradj$AreaName=="SUSAAF"])
#CALACA
FBSprotein.vegotheradj$vegadj[FBSprotein.vegotheradj$AreaName %in%c("Small Island Developing States","South America","Caribbean","Americas","Central America")]<-rep(FBSprotein.vegotheradj$vegadj[FBSprotein.vegotheradj$AreaName=="CALACA"])
#HIGHIN
FBSprotein.vegotheradj$vegadj[FBSprotein.vegotheradj$AreaName %in%c("Western Europe","Australia & New Zealand","Northern Europe","Northern America")]<-rep(FBSprotein.vegotheradj$vegadj[FBSprotein.vegotheradj$AreaName=="HIGHIN"])
#CANAME
FBSprotein.vegotheradj$vegadj[FBSprotein.vegotheradj$AreaName %in%c("Northern Africa","Western Asia","Asia", "Central Asia","Iraq")]<-rep(FBSprotein.vegotheradj$vegadj[FBSprotein.vegotheradj$AreaName=="CANAME"])
#ESEASP
FBSprotein.vegotheradj$vegadj[FBSprotein.vegotheradj$AreaName %in%c("South-Eastern Asia","Polynesia","Oceania","Eastern Asia", "Micronesia","Melanesia")]<-rep(FBSprotein.vegotheradj$vegadj[FBSprotein.vegotheradj$AreaName=="ESEASP"])
#CEEAEU
FBSprotein.vegotheradj$vegadj[FBSprotein.vegotheradj$AreaName %in%c("Eastern Europe","Europe","European Union","Southern Europe" )]<-rep(FBSprotein.vegotheradj$vegadj[FBSprotein.vegotheradj$AreaName=="CEEAEU"])
#china
FBSprotein.vegotheradj$vegadj[FBSprotein.vegotheradj$AreaName %in%c("China, Taiwan Province of","China, Hong Kong SAR" ,"China, Macao SAR")]<-rep(FBSprotein.vegotheradj$vegadj[FBSprotein.vegotheradj$AreaName=="China"])



#SOASIA
FBSprotein.vegotheradj$vegVar[FBSprotein.vegotheradj$AreaName %in%c("Southern Asia","Afghanistan")]<-rep(FBSprotein.vegotheradj$vegVar[FBSprotein.vegotheradj$AreaName=="SOASIA"])
#SUSAAF
FBSprotein.vegotheradj$vegVar[FBSprotein.vegotheradj$AreaName %in%c("World","Western Africa","Somalia","Net Food Importing Developing Countries","Land Locked Developing Countries","Southern Africa","Middle Africa","Africa","Eastern Africa","Least Developed Countries","Low Income Food Deficit Countries")]<-rep(FBSprotein.vegotheradj$vegVar[FBSprotein.vegotheradj$AreaName=="SUSAAF"])
#CALACA
FBSprotein.vegotheradj$vegVar[FBSprotein.vegotheradj$AreaName %in%c("Small Island Developing States","South America","Caribbean","Americas","Central America")]<-rep(FBSprotein.vegotheradj$vegVar[FBSprotein.vegotheradj$AreaName=="CALACA"])
#HIGHIN
FBSprotein.vegotheradj$vegVar[FBSprotein.vegotheradj$AreaName %in%c("Western Europe","Australia & New Zealand","Northern Europe","Northern America")]<-rep(FBSprotein.vegotheradj$vegVar[FBSprotein.vegotheradj$AreaName=="HIGHIN"])
#CANAME
FBSprotein.vegotheradj$vegVar[FBSprotein.vegotheradj$AreaName %in%c("Northern Africa","Western Asia","Asia", "Central Asia","Iraq")]<-rep(FBSprotein.vegotheradj$vegVar[FBSprotein.vegotheradj$AreaName=="CANAME"])
#ESEASP
FBSprotein.vegotheradj$vegVar[FBSprotein.vegotheradj$AreaName %in%c("South-Eastern Asia","Polynesia","Oceania","Eastern Asia", "Micronesia","Melanesia")]<-rep(FBSprotein.vegotheradj$vegVar[FBSprotein.vegotheradj$AreaName=="ESEASP"])
#CEEAEU
FBSprotein.vegotheradj$vegVar[FBSprotein.vegotheradj$AreaName %in%c("Eastern Europe","Europe","European Union","Southern Europe" )]<-rep(FBSprotein.vegotheradj$vegVar[FBSprotein.vegotheradj$AreaName=="CEEAEU"])
#china
FBSprotein.vegotheradj$vegVar[FBSprotein.vegotheradj$AreaName %in%c("China, Taiwan Province of","China, Hong Kong SAR" ,"China, Macao SAR")]<-rep(FBSprotein.vegotheradj$vegVar[FBSprotein.vegotheradj$AreaName=="China"])


#create a table to replace the vegetable rows and cereal other rows
ReplaceCereal<-data.frame(
                          "AreaName"=FBSprotein.cerealotheradj$AreaName,
                          "ItemName"="Cereals, Other",
                          "ElementName"="Protein supply quantity (g/capita/day)",
                          "Value"=FBSprotein.cerealotheradj$Value,
                          "DerivedFrom"="Grain",
                          "CO2RR"=FBSprotein.cerealotheradj$cerealadj,
                          "CO2SE"=FBSprotein.cerealotheradj$cerealVar,
                          "VegOrMeat"="Veg",
                          "order"=999,
                          "countrycode"=FBSprotein.cerealotheradj$countrycode,
                          "CO2.SE.weighted"=((FBSprotein.cerealotheradj$Value)^2)*((FBSprotein.cerealotheradj$cerealVar)^2)
                          )

ReplaceVeg<-data.frame(
  "AreaName"=FBSprotein.vegotheradj$AreaName,
  "ItemName"="Vegetables, Other",
  "ElementName"="Protein supply quantity (g/capita/day)",
  "Value"=FBSprotein.vegotheradj$Value,
  "DerivedFrom"="Veg",
  "CO2RR"=FBSprotein.vegotheradj$vegadj,
  "CO2SE"=FBSprotein.vegotheradj$vegVar,
  "VegOrMeat"="Veg",
  "order"=999,
  "countrycode"=FBSprotein.vegotheradj$countrycode,
  "CO2.SE.weighted"=((FBSprotein.vegotheradj$Value)^2)*((FBSprotein.vegotheradj$vegVar)^2)
)

adj<-rbind(FBSprotein.notVCother,ReplaceCereal,ReplaceVeg)
length(row.names(FBSprotein))
# check that the columns are still correct below
#check that the CIV row is still correct
#watch that you don't end up with two countrycode columns
#rerun the whole thing
adj$CO2RR[adj$countrycode=="SOASIA"]
FBSprotein<-adj


FBSprotein.veg<-subset(FBSprotein, VegOrMeat=="Veg")
FBSprotein.veg<-merge(FBSprotein.veg,FBSNRG.RatioOmiToVego,by="AreaName")
FBSprotein.veg$Value<-FBSprotein.veg$Value*FBSprotein.veg$NRGratio

#creating a column where protein is expressed in calories/capita/day
FBSprotein.cal<-FBSprotein$Value*4.25 
FBSprotein.cal.veg<-FBSprotein.veg$Value*4.25
FBSprotein["protein.amb.cal"]<-FBSprotein.cal
FBSprotein.veg["protein.amb.cal"]<-FBSprotein.cal.veg


#creating columns with expected g/capita/day and proteincalories/capita/day under elevated CO2
FBSprotein$proteinCO2<-FBSprotein$Value*FBSprotein$CO2RR
FBSprotein.veg$proteinCO2<-FBSprotein.veg$Value*FBSprotein.veg$CO2RR

FBSprotein["protein.cal.CO2"]<-FBSprotein$protein.amb.cal*FBSprotein$CO2RR
FBSprotein.veg["protein.cal.CO2"]<-FBSprotein.veg$protein.amb.cal*FBSprotein.veg$CO2RR




###outputs long and wide####
#long
write.csv(FBSNRG,"2_countries/outputs/FBSNRG.csv")
write.csv(FBSprotein,"2_countries/outputs/FBSprotein.csv")
write.csv(FBSNRG.veg,"2_countries/outputs/FBSNRG.veg.csv")
write.csv(FBSprotein.veg,"2_countries/outputs/FBSprotein.veg.csv")



#wide
#values are total calories per day per commodity, each column is a country
library(reshape)
castNRG <-data.frame(cast(FBSNRG,ItemName~AreaName,value="Value")) 
castNRG.veg <-data.frame(cast(FBSNRG.veg,ItemName~AreaName,value="Value")) 

castProtein<-data.frame(cast(FBSprotein,ItemName~AreaName,value="Value"))
castProtein.veg<-data.frame(cast(FBSprotein.veg,ItemName~AreaName,value="Value"))


######totals now#####
#summing across commodities to get mean protein intake for each country
#FBSprotein.tot<-rowsum(FBSprotein[c(4,8,9,10,11)],FBSprotein$AreaName,na.rm=TRUE)#without meatorveg
FBSprotein.tot<-rowsum(FBSprotein[c("Value","protein.amb.cal","proteinCO2","protein.cal.CO2","CO2.SE.weighted")],FBSprotein$AreaName,na.rm=TRUE)#with meatorveg
FBSprotein.tot["AreaName"]<-row.names(FBSprotein.tot) #making a column with country in it
FBSprotein.tot$CO2.SE.weighted<-sqrt(FBSprotein.tot$CO2.SE.weighted) #getting it back to SD from variance

FBSprotein.vegtot<-rowsum(FBSprotein.veg[c("Value","protein.amb.cal","proteinCO2","protein.cal.CO2","CO2.SE.weighted")],FBSprotein.veg$AreaName,na.rm=TRUE)
FBSprotein.vegtot["AreaName"]<-row.names(FBSprotein.vegtot) #making a column with country in it
FBSprotein.vegtot$CO2.SE.weighted<-sqrt(FBSprotein.vegtot$CO2.SE.weighted) #getting it back to SD from variance



#pasting three letter codes onto the protein dataset
FBSprotein.tot.code<-data.frame(merge(FBSprotein.tot,countrycode,by="AreaName",all=TRUE))
FBSNRG.tot.code<-data.frame(merge(FBSNRG.tot,countrycode,by="AreaName",all=TRUE))

FBSprotein.vegtot.code<-data.frame(merge(FBSprotein.vegtot,countrycode,by="AreaName",all=TRUE))
FBSNRG.vegtot.code<-data.frame(merge(FBSNRG.vegtot,countrycode,by="AreaName",all=TRUE))

#Sticking the energy and protein datasets together
FBStot<-merge(FBSprotein.tot.code,FBSNRG.tot.code[c("countrycode","Value")], by="countrycode")
FBStot<-FBStot[!is.na(FBStot$countrycode),]

names(FBStot)<-c("countrycode","country","protein.amb.g","protein.amb.cal","protein.co2.g","protein.co2.cal","protein.co2.g.SE","irrelevantcolumn","energy")

FBStotVeg<-merge(FBSprotein.vegtot.code,FBSNRG.vegtot.code[c("countrycode","Value")], by="countrycode")
FBStotVeg<-FBStotVeg[!FBStotVeg$countrycode=="",]
FBStotVeg<-FBStotVeg[!is.na(FBStotVeg$countrycode),]
names(FBStotVeg)<-c("countrycode","country","protein.amb.g","protein.amb.cal","protein.co2.g","protein.co2.cal","protein.co2.g.SE","irrelevantcolumn","energy")

####calculating percent change in protein by g#####
#the mean bit
FBStot["protein.percent.decrease"]<-((FBStot$protein.co2.g/FBStot$protein.amb.g)-1)*100

FBStotVeg["protein.percent.decrease"]<-((FBStotVeg$protein.co2.g/FBStotVeg$protein.amb.g)-1)*100

#the SE bit
FBStot["protein.percent.decrease.SE"]<-sqrt((FBStot$protein.co2.g.SE^2)*((1/FBStot$protein.amb.g*100)^2))
FBStotVeg["protein.percent.decrease.SE"]<-sqrt((FBStotVeg$protein.co2.g.SE^2)*((1/FBStotVeg$protein.amb.g*100)^2))

###calculating difference in dietary protein ratio by cal####
#the mean bit
FBStot$protein.co2.cal<-FBStot$protein.co2.g*4.25
FBStot["protein.calorie.ratio.difference"]<-((FBStot$protein.co2.cal/FBStot$energy)-(FBStot$protein.amb.cal/FBStot$energy))*100

FBStotVeg$protein.co2.cal<-FBStotVeg$protein.co2.g*4.25
FBStotVeg["protein.calorie.ratio.difference"]<-((FBStotVeg$protein.co2.cal/FBStotVeg$energy)-(FBStotVeg$protein.amb.cal/FBStotVeg$energy))*100

#the SE bit
FBStot["protein.calorie.ratio.SE"]<-sqrt((FBStot$protein.co2.g.SE^2)*(4.25/FBStot$energy*100)^2)
FBStotVeg["protein.calorie.ratio.SE"]<-sqrt((FBStotVeg$protein.co2.g.SE^2)*(4.25/FBStotVeg$energy*100)^2)

###Visualising the spread across countries in an arguably odd way####
plot(protein.calorie.ratio.difference~protein.percent.decrease,data=FBStot,pch=".")
text(FBStot$protein.percent.decrease,FBStot$protein.calorie.ratio.difference,FBStot$countrycode)

plot(protein.calorie.ratio.difference~protein.percent.decrease,data=FBStotVeg,pch=".")
text(FBStotVeg$protein.percent.decrease,FBStotVeg$protein.calorie.ratio.difference,FBStotVeg$countrycode)


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
plot(protein.percent.decrease~proteinsource,
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
#View(sPDF)
mapCountryData(sPDF,nameColumnToPlot="protein.percent.decrease")
op <- palette(c("red","orange","yellow","darkgreen"))
cutVector <- quantile(sPDF@data[["protein.percent.decrease"]],na.rm=TRUE)
sPDF@data[["BIOcategories"]] <- cut(sPDF@data[["protein.percent.decrease"]]
                                    , cutVector
#                                     , include.lowest=TRUE )
# levels(sPDF@data[["BIOcategories"]]) <- c("low","med","high","vhigh")
# range(sPDF$protein.percent.decrease[sPDF$BIOcategories=="low"],na.rm=TRUE)
# range(sPDF$protein.percent.decrease[sPDF$BIOcategories=="med"],na.rm=TRUE)
# range(sPDF$protein.percent.decrease[sPDF$BIOcategories=="high"],na.rm=TRUE)
# range(sPDF$protein.percent.decrease[sPDF$BIOcategories=="vhigh"],na.rm=TRUE)
# levels(sPDF@data[["BIOcategories"]]) <- c("7.6 - 5.3%","5.2 - 4.5%","4.4 - 3.8%","3.8 - 1.4%")

mapCountryData( sPDF
                , nameColumnToPlot="protein.percent.decrease"
                , catMethod="quantiles"              
#               , nameColumnToPlot="BIOcategories"
#               , catMethod="categorical"
                , mapTitle="Percent decrease in protein"
                , colourPalette="palette"
                , oceanCol="lightblue"
                , missingCountryCol="white"                 )

mapCountryData( sPDF
                , nameColumnToPlot="protein.calorie.ratio.difference"
                , catMethod="quantiles"              
                #               , nameColumnToPlot="BIOcategories"
                #               , catMethod="categorical"
                , mapTitle="Decrease in % Calories from protein"
                , colourPalette="palette"
                , oceanCol="lightblue"
                , missingCountryCol="white"   )

#mapCountryData(sPDF,nameColumnToPlot="protein.percent.decrease", catMethod=c(-20,-16,-8,-4,0), colourPalette = "black2White")
mapCountryData(sPDF,nameColumnToPlot="protein.calorie.ratio.difference")
#,  catMethod=c(-0.7,-0.6,-0.5,-0.4,-0.3,-0.2,-0.1,0), colourPalette = "black2White")

sPDF <- joinCountryData2Map(FBStotVeg
                            , joinCode = "ISO3"
                            , nameJoinColumn = "countrycode"
)
#View(sPDF)
mapCountryData(sPDF,nameColumnToPlot="protein.percent.decrease")


mapCountryData( sPDF
                , nameColumnToPlot="protein.percent.decrease"
                , catMethod="quantiles"              
                #               , nameColumnToPlot="BIOcategories"
                #               , catMethod="categorical"
                , mapTitle="Vegan diet: Percent decrease in protein"
                , colourPalette="palette"
                , oceanCol="lightblue"
                , missingCountryCol="white"                 )

#mapCountryData(sPDF,nameColumnToPlot="protein.percent.decrease", catMethod=c(-20,-16,-8,-4,0), colourPalette = "black2White")
mapCountryData(sPDF,nameColumnToPlot="protein.calorie.ratio.difference")
#,  catMethod=c(-0.7,-0.6,-0.5,-0.4,-0.3,-0.2,-0.1,0), colourPalette = "black2White")


####the main output####
FBStot<-FBStot[FBStot$protein.co2.g>0,]

write.csv(FBStot,"2_countries/outputs/FBStot.csv")
save(FBStot, file="2_countries/outputs/FBStot.Rdata")

save(FBStotVeg, file="2_countries/outputs/FBStotVeg.Rdata")
write.csv(FBStotVeg,"2_countries/outputs/FBStotVeg.csv")



####meat to veg ratio####

FBSprotein.meat<-subset(FBSprotein, VegOrMeat=="nonveg")
FBSprotein.meattot<-rowsum(FBSprotein.meat[c(4,9,10,11,12)],FBSprotein.meat$AreaName,na.rm=TRUE)

FBSprotein.meattot["AreaName"]<-row.names(FBSprotein.meattot) #making a column with country in it

FBSprotein.meattot$CO2.SE.weighted<-sqrt(FBSprotein.meattot$CO2.SE.weighted) #getting it back to SD from variance

#the veg values have now been corrected, so you need to comment that bit out for it to be realistic.
#FBSprotein.vegormeat<-data.frame("country" = FBSprotein.meattot$AreaName,"fractionmeat"=FBSprotein.meattot$Value/(FBSprotein.vegtot$Value+FBSprotein.meattot$Value))
#FBSprotein.vegormeat.change<-merge(FBStot,FBSprotein.vegormeat, by="country")
# plot(protein.percent.decrease~fractionmeat ,data=FBSprotein.vegormeat.change)

