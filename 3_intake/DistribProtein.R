proteinMCI<-read.csv("3_intake/inputs/protein/food security mean protein intake and CI.csv")
#downloaded from this: http://faostat3.fao.org/faostat-gateway/go/to/download/D/HS/E
#containing protein intake and % calories whole-country and by income terciles
#containing mean, SD and sample size.


####making a column of countries and the last year of the survey####
SurveyCountryYear<-as.character(proteinMCI$Survey)
country_year <- matrix(unlist(strsplit(SurveyCountryYear," - ")),ncol = 2, byrow = TRUE)
country_year<-data.frame(country_year)
names(country_year)<-c("country","year")
country_year$year<-as.character(country_year$year)

#woah checkit! you can write functions right into R like this
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


for(rownum in 1:length(country_year$year))
{
  if(nchar(country_year$year[rownum])>4)
    country_year$yearL[rownum] <-substrRight(country_year$year[rownum], 4)
  else  
    country_year$yearL[rownum] <-country_year$year[rownum]
}

#Sticking this back onto the original file
proteinMCI<-cbind(country_year,proteinMCI)












####NOW JUMP OVER TO THE countrycoding.R file and make sure you're ready for....##########################################

countriesHHS<-read.csv("0_labels/order_HHS.csv")
countrycode<-countriesHHS
# NArownum<-which(is.na(countrycode$countriestomergewithFBS))
# countrycode$countriestomergewithFBS<-as.character(countrycode$countriestomergewithFBS)
# for(rownum in 1:length(countrycode$countrycode))
# {for(NAnum in 1:length(NArownum))
# {if(rownum == NArownum[NAnum])
#   countrycode$countriestomergewithFBS[rownum]<-as.character(countrycode$countriestomergewithsPDF[rownum])
# }
# }
# countrycode$countriestomergewithFBS<-as.factor(countrycode$countriestomergewithFBS)
#View(countrycode)

names(countriesHHS)[2]<-"country"
proteinMCI<-merge(proteinMCI,countriesHHS, by="country")


####extracting the data I want#####
percent.protein<-subset(proteinMCI,Indicator=="Protein consumption")
 
mean.percent.protein<-subset(percent.protein,Measure=="Mean")
SD.percent.protein<-subset(percent.protein,Measure=="Standard Deviation")

mean.percent.protein$SD<-SD.percent.protein$Value

mean.percent.protein$CofV<-mean.percent.protein$SD/mean.percent.protein$Value

country.mean.percent.protein<-subset(mean.percent.protein,Breakdown.Variable=="Country-level")

write.csv(country.mean.percent.protein,"3_intake/outputs/countryDistribProtein.csv")


#GDP<-read.csv("~/Rdata/nutrients/GDP_2011.csv")
#FAO_protein<-read.csv("~/Rdata/nutrients/FAO_householdsurvey_protein_gperday.csv")
#GDP_vs_Protein<-data.frame(merge(FAO_protein,GDP,by="countrycode"))

GINI<-read.csv("3_intake/inputs/GINI/world_bank_GINI_allyears.csv")
#GINI_vs_Protein<-data.frame(merge(FAO_protein,GINI,by="countrycode"))

#View(GINI)
GINI_vs_Protein<-data.frame(merge(country.mean.percent.protein,GINI,by="countrycode"), all=TRUE)
GINI_vs_FBSVegnodoubles<-GINI_vs_FBSVeg[-c(58:60),]


plot(GINI_vs_FBSVegnodoubles$protein.percent.decrease~GINI_vs_FBSVegnodoubles$mostrecentGINI,
     xlab="GINI",
     ylab="percent decrease in protein, vegetarians (%)",
     pch="")

text(GINI_vs_FBSVegnodoubles$mostrecentGINI,
     GINI_vs_FBSVegnodoubles$protein.percent.decrease,
     GINI_vs_FBSVegnodoubles$countrycode,
     cex=0.5
     )

# row.names(GINI_vs_FBSVeg[GINI_vs_FBSVeg$countrycode=="CHN",])
# row.names(GINI_vs_FBSVegnodoubles[GINI_vs_FBSVegnodoubles$countrycode=="CHN",])

# plot(CofV_food_security~X2011,data=GDP_vs_Protein)
# plot(CofV_food_security~FAO_Food_security__numbers,data=GDP_vs_Protein)
# plot(CofV_food_security~income.share,data=GINI_vs_Protein)


save(GINI_vs_Protein, file = "3_intake/outputs/GINI_vs_Protein.Rdata")

# FBSorder<-read.csv("~/Rdata/nutrients/FAO_excel_order.csv")
# for_excel<-data.frame(merge(FBSorder,GINI_vs_Protein,by="countrycode"))
# write.csv(for_excel,"~/Rdata/nutrients/for_excel.csv")

#how long has it been since the surveys?
GINI_vs_Protein$time_since_survey<-2011-as.numeric(GINI_vs_Protein$yearL)
max(GINI_vs_Protein$time_since_survey)
GINI_vs_Protein$country[which.max(GINI_vs_Protein$time_since_survey)]
GINI_vs_Protein$yearL[which.max(GINI_vs_Protein$time_since_survey)]




plot(CofV~mostrecentGINI,data=GINI_vs_Protein)


###getting the GINI vs protein C of V relationship####
#removing the two outliers ONLY DO THIS ONCE
GINI_vs_Protein$CofV[which.max(GINI_vs_Protein$CofV)]<-NA
plot(CofV~mostrecentGINI,data=GINI_vs_Protein)
GINI_vs_Protein$CofV[which.max(GINI_vs_Protein$CofV)]<-NA
plot(CofV~mostrecentGINI,data=GINI_vs_Protein)





Time.since.survey<-GINI_vs_Protein[with(GINI_vs_Protein, order(-GINI_vs_Protein$time_since_survey)), ]

#really, I should be comparing GINI at the time of the survey to the protein intake. This might remove some scatter.


###choosing the best GINI####
#load a database of GINI for multiple years

##find the GINI that is in the year closest to the household survey (yearL)
column_GINI_at_survey<-48-Time.since.survey$time_since_survey
for(rownum in 1:41)
{Time.since.survey$GINI_at_survey[rownum]<-Time.since.survey[rownum,column_GINI_at_survey[rownum]]}

for(rownum in 1:41)
{if(!is.na(Time.since.survey[rownum,column_GINI_at_survey[rownum]]))
  Time.since.survey$GINI_at_survey[rownum]<-Time.since.survey[rownum,column_GINI_at_survey[rownum]]
 else if(!is.na(Time.since.survey[rownum,(column_GINI_at_survey[rownum])+1]))
   Time.since.survey$GINI_at_survey[rownum]<-Time.since.survey[rownum,(column_GINI_at_survey[rownum])+1]
 else if(!is.na(Time.since.survey[rownum,(column_GINI_at_survey[rownum])-1]))
   Time.since.survey$GINI_at_survey[rownum]<-Time.since.survey[rownum,(column_GINI_at_survey[rownum])-1]
 else if(!is.na(Time.since.survey[rownum,(column_GINI_at_survey[rownum])+2]))
   Time.since.survey$GINI_at_survey[rownum]<-Time.since.survey[rownum,(column_GINI_at_survey[rownum])+2]
 else if(!is.na(Time.since.survey[rownum,(column_GINI_at_survey[rownum])-2]))
   Time.since.survey$GINI_at_survey[rownum]<-Time.since.survey[rownum,(column_GINI_at_survey[rownum])-2]
 else if(!is.na(Time.since.survey[rownum,(column_GINI_at_survey[rownum])+3]))
   Time.since.survey$GINI_at_survey[rownum]<-Time.since.survey[rownum,(column_GINI_at_survey[rownum])+3]
 else if(!is.na(Time.since.survey[rownum,(column_GINI_at_survey[rownum])-3]))
   Time.since.survey$GINI_at_survey[rownum]<-Time.since.survey[rownum,(column_GINI_at_survey[rownum])-3]
 else if(!is.na(Time.since.survey[rownum,(column_GINI_at_survey[rownum])+4]))
   Time.since.survey$GINI_at_survey[rownum]<-Time.since.survey[rownum,(column_GINI_at_survey[rownum])+4]
 else
   Time.since.survey$GINI_at_survey[rownum]<-Time.since.survey[rownum,(column_GINI_at_survey[rownum])-4]
}


#but realistically, we're estimating current C of V from past GINI so we need the relationship to past GINI.
for(rownum in 1:41)
{if(!is.na(Time.since.survey[rownum,column_GINI_at_survey[rownum]]))
  Time.since.survey$GINI_before_survey[rownum]<-Time.since.survey[rownum,column_GINI_at_survey[rownum]]
 else if(!is.na(Time.since.survey[rownum,(column_GINI_at_survey[rownum])-1]))
   Time.since.survey$GINI_before_survey[rownum]<-Time.since.survey[rownum,(column_GINI_at_survey[rownum])-1]
 else if(!is.na(Time.since.survey[rownum,(column_GINI_at_survey[rownum])-2]))
   Time.since.survey$GINI_before_survey[rownum]<-Time.since.survey[rownum,(column_GINI_at_survey[rownum])-2]
 else if(!is.na(Time.since.survey[rownum,(column_GINI_at_survey[rownum])-3]))
   Time.since.survey$GINI_before_survey[rownum]<-Time.since.survey[rownum,(column_GINI_at_survey[rownum])-3]
 else
   Time.since.survey$GINI_before_survey[rownum]<-Time.since.survey[rownum,(column_GINI_at_survey[rownum])-4]
}

mean(as.numeric(Time.since.survey$mostrecentyear[-10], na.rm=TRUE))
#most surveys were undertaken in 2008. So I should really model how the GINI three years before the household surveys matches the C of V
for(rownum in 1:41)
{Time.since.survey$GINI_3yrsbefore_survey[rownum]<-Time.since.survey[rownum,(column_GINI_at_survey[rownum])-3]}

# library(mgcv)
# attach(Time.since.survey)
# blah2<-gam(as.numeric(CofV)~s(as.numeric(GINI_before_survey)),data=Time.since.survey)
# plot(blah2)
# summary(blah2)

plot(log(Time.since.survey$CofV)~as.numeric(Time.since.survey$GINI_at_survey))
plot(Time.since.survey$CofV~as.numeric(Time.since.survey$GINI_before_survey))
plot(Time.since.survey$CofV~as.numeric(Time.since.survey$GINI_3yrsbefore_survey))



summary(lm(CofV~mostrecentGINI,data=GINI_vs_Protein))
summary(lm(CofV~as.numeric(GINI_at_survey),data=Time.since.survey))
summary(lm(CofV~as.numeric(GINI_before_survey),data=Time.since.survey))
summary(lm(CofV~as.numeric(GINI_3yrsbefore_survey),data=Time.since.survey))

summary(lm(log(CofV)~mostrecentGINI,data=GINI_vs_Protein))
summary(lm(log(CofV)~as.numeric(GINI_at_survey),data=Time.since.survey))

summary(lm(log(CofV)~as.numeric(GINI_before_survey),data=Time.since.survey))
# plot(lm(log(CofV)~as.numeric(GINI_before_survey),data=Time.since.survey))

# plot(log(CofV)~as.numeric(GINI_before_survey),data=Time.since.survey)

#I think I'll go with this one. Logging it makes the residual plots look better. Logging GINI as well doesn't make it any better.

summary(lm(log(CofV)~as.numeric(GINI_3yrsbefore_survey),data=Time.since.survey))
# plot(lm(log(CofV)~as.numeric(GINI_3yrsbefore_survey),data=Time.since.survey))
#there really aren't enough GINIs three years before the surveys to make a good model.


lm.cv.gini<-lm(log(CofV)~as.numeric(GINI_before_survey),data=Time.since.survey)
intercept<-coef(lm.cv.gini)[1]
slope<-coef(lm.cv.gini)[2]

summary(lm.cv.gini)
save(lm.cv.gini,file="3_intake/outputs/linearmodel_CV_vs_GINI.Rdata")


#####For the continents#####
names(GINI)
GINI<-GINI[,c(1,2,39,40)]
fbspop<-read.csv("2_countries/inputs/pop from fbs 2011.csv")
names(fbspop)[4]<-"Country.Name"
#need to merge them by countrycode still...


HIGHIN.fbspop<-subset(fbspop,
                      countrycode=="AND" | countrycode=="AUS" | countrycode=="AUT" | countrycode=="BEL" | countrycode=="BRN" | countrycode=="CAN" | countrycode=="CYP" | countrycode=="DNK" | countrycode=="FIN" | countrycode=="FRA" | countrycode=="DEU" | countrycode=="GRC" | countrycode=="GRL" | countrycode=="ISL" | countrycode=="IRL" | countrycode=="ISR" | countrycode=="ITA" | countrycode=="JPN" | countrycode=="LUX" | countrycode=="MLT" | countrycode=="NLD" | countrycode=="NZL" | countrycode=="NOR" | countrycode=="PRT" | countrycode=="KOR" | countrycode=="SGP" | countrycode=="ESP" | countrycode=="SWE" | countrycode=="CHE" | countrycode=="GBR" | countrycode=="USA")
HIGHIN.GINI<-subset(GINI,
                    countrycode=="AND" | countrycode=="AUS" | countrycode=="AUT" | countrycode=="BEL" | countrycode=="BRN" | countrycode=="CAN" | countrycode=="CYP" | countrycode=="DNK" | countrycode=="FIN" | countrycode=="FRA" | countrycode=="DEU" | countrycode=="GRC" | countrycode=="GRL" | countrycode=="ISL" | countrycode=="IRL" | countrycode=="ISR" | countrycode=="ITA" | countrycode=="JPN" | countrycode=="LUX" | countrycode=="MLT" | countrycode=="NLD" | countrycode=="NZL" | countrycode=="NOR" | countrycode=="PRT" | countrycode=="KOR" | countrycode=="SGP" | countrycode=="ESP" | countrycode=="SWE" | countrycode=="CHE" | countrycode=="GBR" | countrycode=="USA")



SOTRLA.fbspop<-subset(fbspop,
                      countrycode=="ARG" | countrycode=="BRA" | countrycode=="CHL" | countrycode=="PRY" | countrycode=="URY")
SOTRLA.GINI<-subset(GINI,
                    countrycode=="ARG" | countrycode=="BRA" | countrycode=="CHL" | countrycode=="PRY" | countrycode=="URY")


CEEAEU.fbspop<-subset(fbspop,
                      countrycode=="ALB" | countrycode=="BLR" | countrycode=="BIH" | countrycode=="BGR" | countrycode=="HRV" | countrycode=="CZE" | countrycode=="EST" | countrycode=="HUN" | countrycode=="LVA" | countrycode=="LTU" | countrycode=="MKD" | countrycode=="MDA" | countrycode=="MNE" | countrycode=="POL" | countrycode=="ROU" | countrycode=="RUS" | countrycode=="SRB" | countrycode=="SVK" | countrycode=="SVN" | countrycode=="UKR")
CEEAEU.GINI<-subset(GINI,
                    countrycode=="ALB" | countrycode=="BLR" | countrycode=="BIH" | countrycode=="BGR" | countrycode=="HRV" | countrycode=="CZE" | countrycode=="EST" | countrycode=="HUN" | countrycode=="LVA" | countrycode=="LTU" | countrycode=="MKD" | countrycode=="MDA" | countrycode=="MNE" | countrycode=="POL" | countrycode=="ROU" | countrycode=="RUS" | countrycode=="SRB" | countrycode=="SVK" | countrycode=="SVN" | countrycode=="UKR")


CALACA.fbspop<-subset(fbspop,
                      countrycode=="ATG" | countrycode=="BHS" | countrycode=="BRB" | countrycode=="BLZ" | countrycode=="BMU" | countrycode=="BOL" | countrycode=="VGB" | countrycode=="COL" | countrycode=="CRI" | countrycode=="CUB" | countrycode=="DMA" | countrycode=="DOM" | countrycode=="ECU" | countrycode=="SLV" | countrycode=="GRD" | countrycode=="GTM" | countrycode=="GUY" | countrycode=="HTI" | countrycode=="HND" | countrycode=="JAM" | countrycode=="MEX" | countrycode=="NIC" | countrycode=="ANT" | countrycode=="PAN" | countrycode=="PER" | countrycode=="PRI" | countrycode=="KNA" | countrycode=="LCA" | countrycode=="VCT" | countrycode=="SUR" | countrycode=="TTO" | countrycode=="VEN")
CALACA.GINI<-subset(GINI,
                    countrycode=="ATG" | countrycode=="BHS" | countrycode=="BRB" | countrycode=="BLZ" | countrycode=="BMU" | countrycode=="BOL" | countrycode=="VGB" | countrycode=="COL" | countrycode=="CRI" | countrycode=="CUB" | countrycode=="DMA" | countrycode=="DOM" | countrycode=="ECU" | countrycode=="SLV" | countrycode=="GRD" | countrycode=="GTM" | countrycode=="GUY" | countrycode=="HTI" | countrycode=="HND" | countrycode=="JAM" | countrycode=="MEX" | countrycode=="NIC" | countrycode=="ANT" | countrycode=="PAN" | countrycode=="PER" | countrycode=="PRI" | countrycode=="KNA" | countrycode=="LCA" | countrycode=="VCT" | countrycode=="SUR" | countrycode=="TTO" | countrycode=="VEN")

CANAME.fbspop<-subset(fbspop,
                      countrycode=="DZA" | countrycode=="ARM" | countrycode=="AZE" | countrycode=="BHR" | countrycode=="EGY" | countrycode=="GEO" | countrycode=="IRN" | countrycode=="IRQ" | countrycode=="JOR" | countrycode=="KAZ" | countrycode=="KWT" | countrycode=="KGZ" | countrycode=="LBN" | countrycode=="LBY" | countrycode=="MNG" | countrycode=="MAR" | countrycode=="PSE" | countrycode=="OMN" | countrycode=="QAT" | countrycode=="SAU" | countrycode=="SYR" | countrycode=="TJK" | countrycode=="TUN" | countrycode=="TUR" | countrycode=="TKM" | countrycode=="ARE" | countrycode=="UZB" | countrycode=="YEM")
CANAME.GINI<-subset(GINI,
                    countrycode=="DZA" | countrycode=="ARM" | countrycode=="AZE" | countrycode=="BHR" | countrycode=="EGY" | countrycode=="GEO" | countrycode=="IRN" | countrycode=="IRQ" | countrycode=="JOR" | countrycode=="KAZ" | countrycode=="KWT" | countrycode=="KGZ" | countrycode=="LBN" | countrycode=="LBY" | countrycode=="MNG" | countrycode=="MAR" | countrycode=="PSE" | countrycode=="OMN" | countrycode=="QAT" | countrycode=="SAU" | countrycode=="SYR" | countrycode=="TJK" | countrycode=="TUN" | countrycode=="TUR" | countrycode=="TKM" | countrycode=="ARE" | countrycode=="UZB" | countrycode=="YEM")

ESEASP.fbspop<-subset(fbspop,
                      countrycode=="KHM" | countrycode=="COK" | countrycode=="PRK" | countrycode=="FJI" | countrycode=="PYF" | countrycode=="IDN" | countrycode=="KIR" | countrycode=="LAO" | countrycode=="MYS" | countrycode=="MDV" | countrycode=="MHL" | countrycode=="FSM" | countrycode=="MMR" | countrycode=="NRU" | countrycode=="PLW" | countrycode=="PNG" | countrycode=="PHL" | countrycode=="WSM" | countrycode=="SLB" | countrycode=="LKA" | countrycode=="THA" | countrycode=="TLS" | countrycode=="TON" | countrycode=="VNM" | countrycode=="VUT")
ESEASP.GINI<-subset(GINI,
                    countrycode=="KHM" | countrycode=="COK" | countrycode=="PRK" | countrycode=="FJI" | countrycode=="PYF" | countrycode=="IDN" | countrycode=="KIR" | countrycode=="LAO" | countrycode=="MYS" | countrycode=="MDV" | countrycode=="MHL" | countrycode=="FSM" | countrycode=="MMR" | countrycode=="NRU" | countrycode=="PLW" | countrycode=="PNG" | countrycode=="PHL" | countrycode=="WSM" | countrycode=="SLB" | countrycode=="LKA" | countrycode=="THA" | countrycode=="TLS" | countrycode=="TON" | countrycode=="VNM" | countrycode=="VUT")

SUSAAF.fbspop<-subset(fbspop,
                      countrycode=="AGO" | countrycode=="BWA" | countrycode=="BFA" | countrycode=="BUR" | countrycode=="CMR" | countrycode=="CPV" | countrycode=="CAF" | countrycode=="TCD" | countrycode=="COM" | countrycode=="COG" | countrycode=="CIV" | countrycode=="COD" | countrycode=="DJI" | countrycode=="ERI" | countrycode=="ETH" | countrycode=="GNQ" | countrycode=="GAB" | countrycode=="GMB" | countrycode=="GHA" | countrycode=="GIN" | countrycode=="GNB" | countrycode=="LSO" | countrycode=="LBR" | countrycode=="KEN" | countrycode=="MDG" | countrycode=="MWI" | countrycode=="MLI" | countrycode=="MRT" | countrycode=="MUS" | countrycode=="MOZ" | countrycode=="NAM" | countrycode=="NER" | countrycode=="NGA" | countrycode=="RWA" | countrycode=="STP" | countrycode=="SEN" | countrycode=="SYC" | countrycode=="SLE" | countrycode=="SOM" | countrycode=="ZAF" | countrycode=="SDN" | countrycode=="SWZ" | countrycode=="TGO" | countrycode=="UGA" | countrycode=="TZA" | countrycode=="ZMB" | countrycode=="ZWE")
SUSAAF.GINI<-subset(GINI,
                    countrycode=="AGO" | countrycode=="BWA" | countrycode=="BFA" | countrycode=="BUR" | countrycode=="CMR" | countrycode=="CPV" | countrycode=="CAF" | countrycode=="TCD" | countrycode=="COM" | countrycode=="COG" | countrycode=="CIV" | countrycode=="COD" | countrycode=="DJI" | countrycode=="ERI" | countrycode=="ETH" | countrycode=="GNQ" | countrycode=="GAB" | countrycode=="GMB" | countrycode=="GHA" | countrycode=="GIN" | countrycode=="GNB" | countrycode=="LSO" | countrycode=="LBR" | countrycode=="KEN" | countrycode=="MDG" | countrycode=="MWI" | countrycode=="MLI" | countrycode=="MRT" | countrycode=="MUS" | countrycode=="MOZ" | countrycode=="NAM" | countrycode=="NER" | countrycode=="NGA" | countrycode=="RWA" | countrycode=="STP" | countrycode=="SEN" | countrycode=="SYC" | countrycode=="SLE" | countrycode=="SOM" | countrycode=="ZAF" | countrycode=="SDN" | countrycode=="SWZ" | countrycode=="TGO" | countrycode=="UGA" | countrycode=="TZA" | countrycode=="ZMB" | countrycode=="ZWE")

SOASIA.fbspop<-subset(fbspop,
                      countrycode=="AFG" | countrycode=="BGD" | countrycode=="BTN" | countrycode=="NPL" | countrycode=="PAK")
SOASIA.GINI<-subset(GINI,
                    countrycode=="AFG" | countrycode=="BGD" | countrycode=="BTN" | countrycode=="NPL" | countrycode=="PAK")



HIGHIN.GINI<-merge(HIGHIN.GINI,HIGHIN.fbspop, by="countrycode")
HIGHIN.totpop<-sum(HIGHIN.GINI$pop)
HIGHIN.GINI$fraction_totpop<-HIGHIN.GINI$pop/HIGHIN.totpop
HIGHIN.GINI$weightedmostrecentGINI<-HIGHIN.GINI$mostrecentGINI*HIGHIN.GINI$fraction_totpop
HIGHIN<-data.frame("countrycode"="HIGHIN","mostrecentGINI"=sum(HIGHIN.GINI$weightedmostrecentGINI,na.rm=TRUE),"mostrecentyear"=2050)

SOTRLA.GINI<-merge(SOTRLA.GINI,SOTRLA.fbspop, by="countrycode")
SOTRLA.totpop<-sum(SOTRLA.GINI$pop)
SOTRLA.GINI$fraction_totpop<-SOTRLA.GINI$pop/SOTRLA.totpop
SOTRLA.GINI$weightedmostrecentGINI<-SOTRLA.GINI$mostrecentGINI*SOTRLA.GINI$fraction_totpop
SOTRLA<-data.frame("countrycode"="SOTRLA","mostrecentGINI"=sum(SOTRLA.GINI$weightedmostrecentGINI,na.rm=TRUE),"mostrecentyear"=2050)

CEEAEU.GINI<-merge(CEEAEU.GINI,CEEAEU.fbspop, by="countrycode")
CEEAEU.totpop<-sum(CEEAEU.GINI$pop)
CEEAEU.GINI$fraction_totpop<-CEEAEU.GINI$pop/CEEAEU.totpop
CEEAEU.GINI$weightedmostrecentGINI<-CEEAEU.GINI$mostrecentGINI*CEEAEU.GINI$fraction_totpop
CEEAEU<-data.frame("countrycode"="CEEAEU","mostrecentGINI"=sum(CEEAEU.GINI$weightedmostrecentGINI,na.rm=TRUE),"mostrecentyear"=2050)

CALACA.GINI<-merge(CALACA.GINI,CALACA.fbspop, by="countrycode")
CALACA.totpop<-sum(CALACA.GINI$pop)
CALACA.GINI$fraction_totpop<-CALACA.GINI$pop/CALACA.totpop
CALACA.GINI$weightedmostrecentGINI<-CALACA.GINI$mostrecentGINI*CALACA.GINI$fraction_totpop
CALACA<-data.frame("countrycode"="CALACA","mostrecentGINI"=sum(CALACA.GINI$weightedmostrecentGINI,na.rm=TRUE),"mostrecentyear"=2050)

CANAME.GINI<-merge(CANAME.GINI,CANAME.fbspop, by="countrycode")
CANAME.totpop<-sum(CANAME.GINI$pop)
CANAME.GINI$fraction_totpop<-CANAME.GINI$pop/CANAME.totpop
CANAME.GINI$weightedmostrecentGINI<-CANAME.GINI$mostrecentGINI*CANAME.GINI$fraction_totpop
CANAME<-data.frame("countrycode"="CANAME","mostrecentGINI"=sum(CANAME.GINI$weightedmostrecentGINI,na.rm=TRUE),"mostrecentyear"=2050)

ESEASP.GINI<-merge(ESEASP.GINI,ESEASP.fbspop, by="countrycode")
ESEASP.totpop<-sum(ESEASP.GINI$pop)
ESEASP.GINI$fraction_totpop<-ESEASP.GINI$pop/ESEASP.totpop
ESEASP.GINI$weightedmostrecentGINI<-ESEASP.GINI$mostrecentGINI*ESEASP.GINI$fraction_totpop
ESEASP<-data.frame("countrycode"="ESEASP","mostrecentGINI"=sum(ESEASP.GINI$weightedmostrecentGINI,na.rm=TRUE),"mostrecentyear"=2050)

SUSAAF.GINI<-merge(SUSAAF.GINI,SUSAAF.fbspop, by="countrycode")
SUSAAF.totpop<-sum(SUSAAF.GINI$pop)
SUSAAF.GINI$fraction_totpop<-SUSAAF.GINI$pop/SUSAAF.totpop
SUSAAF.GINI$weightedmostrecentGINI<-SUSAAF.GINI$mostrecentGINI*SUSAAF.GINI$fraction_totpop
SUSAAF<-data.frame("countrycode"="SUSAAF","mostrecentGINI"=sum(SUSAAF.GINI$weightedmostrecentGINI,na.rm=TRUE),"mostrecentyear"=2050)

SOASIA.GINI<-merge(SOASIA.GINI,SOASIA.fbspop, by="countrycode")
SOASIA.totpop<-sum(SOASIA.GINI$pop)
SOASIA.GINI$fraction_totpop<-SOASIA.GINI$pop/SOASIA.totpop
SOASIA.GINI$weightedmostrecentGINI<-SOASIA.GINI$mostrecentGINI*SOASIA.GINI$fraction_totpop
SOASIA<-data.frame("countrycode"="SOASIA","mostrecentGINI"=sum(SOASIA.GINI$weightedmostrecentGINI,na.rm=TRUE),"mostrecentyear"=2050)



GINIgroups<-data.frame("Country.Name"=
                         c(
                           "HIGHIN",
                           "SOTRLA",
                           "CEEAEU",
                           "CALACA", 
                           "CANAME",
                           "ESEASP",
                           "SUSAAF",
                           "SOASIA"                          
                           ),
                         rbind(
                           HIGHIN,
                           SOTRLA,
                           CEEAEU,
                           CALACA, 
                           CANAME,
                           ESEASP,
                           SUSAAF,
                           SOASIA
                           ))

#View(GINIgroups)
  GINI<-rbind(GINI,GINIgroups)

#####separately, calculate a C of V for each country based on GINI####
load("2_countries/outputs/FBStot.Rdata")
load("2_countries/outputs/FBStotVeg.Rdata")
GINI_vs_FBS<-data.frame(merge(FBStot,GINI,by="countrycode", all = TRUE))
GINI_vs_FBSVeg<-data.frame(merge(FBStotVeg,GINI,by="countrycode", all = TRUE))

GINI_vs_FBS$CofV_modelled<-exp(intercept+(slope*GINI_vs_FBS$mostrecentGINI))
GINI_vs_FBSVeg$CofV_modelled<-exp(intercept+(slope*GINI_vs_FBSVeg$mostrecentGINI))


###calculate a skewness based on C of V####
GINI_vs_FBS$skewness_modelled<-(GINI_vs_FBS$CofV_modelled^2+3)*GINI_vs_FBS$CofV_modelled
GINI_vs_FBSVeg$skewness_modelled<-(GINI_vs_FBSVeg$CofV_modelled^2+3)*GINI_vs_FBSVeg$CofV_modelled


GINI_vs_FBS$country[which.max(GINI_vs_FBS$skewness_modelled)]
# "Seychelles"
GINI_vs_FBS$country[which.min(GINI_vs_FBS$skewness_modelled)]
#"Denmark"

###compare this to the skewness from SOFI####
#load the 2011 SOFI data
SOFI11<-read.csv("3_intake/inputs/skewness/SOFI_2011.csv")

order_SOFI<-read.csv("0_labels/order_SOFI.csv")
names(order_SOFI)
names(order_SOFI)[2]<-"country"
SOFI11<-merge(order_SOFI,SOFI11, by="country")

#you have to stick the pop column onto the sofi column first before summing it up and taking the fraction, or else you're going to be underestimating the numbers.
###continents####
HIGHIN.SOFI11<-subset(SOFI11,
                      countrycode=="AND" | countrycode=="AUS" | countrycode=="AUT" | countrycode=="BEL" | countrycode=="BRN" | countrycode=="CAN" | countrycode=="CYP" | countrycode=="DNK" | countrycode=="FIN" | countrycode=="FRA" | countrycode=="DEU" | countrycode=="GRC" | countrycode=="GRL" | countrycode=="ISL" | countrycode=="IRL" | countrycode=="ISR" | countrycode=="ITA" | countrycode=="JPN" | countrycode=="LUX" | countrycode=="MLT" | countrycode=="NLD" | countrycode=="NZL" | countrycode=="NOR" | countrycode=="PRT" | countrycode=="KOR" | countrycode=="SGP" | countrycode=="ESP" | countrycode=="SWE" | countrycode=="CHE" | countrycode=="GBR" | countrycode=="USA")
SOTRLA.SOFI11<-subset(SOFI11,
                      countrycode=="ARG" | countrycode=="BRA" | countrycode=="CHL" | countrycode=="PRY" | countrycode=="URY")
CEEAEU.SOFI11<-subset(SOFI11,
                      countrycode=="ALB" | countrycode=="BLR" | countrycode=="BIH" | countrycode=="BGR" | countrycode=="HRV" | countrycode=="CZE" | countrycode=="EST" | countrycode=="HUN" | countrycode=="LVA" | countrycode=="LTU" | countrycode=="MKD" | countrycode=="MDA" | countrycode=="MNE" | countrycode=="POL" | countrycode=="ROU" | countrycode=="RUS" | countrycode=="SRB" | countrycode=="SVK" | countrycode=="SVN" | countrycode=="UKR")
CALACA.SOFI11<-subset(SOFI11,
                      countrycode=="ATG" | countrycode=="BHS" | countrycode=="BRB" | countrycode=="BLZ" | countrycode=="BMU" | countrycode=="BOL" | countrycode=="VGB" | countrycode=="COL" | countrycode=="CRI" | countrycode=="CUB" | countrycode=="DMA" | countrycode=="DOM" | countrycode=="ECU" | countrycode=="SLV" | countrycode=="GRD" | countrycode=="GTM" | countrycode=="GUY" | countrycode=="HTI" | countrycode=="HND" | countrycode=="JAM" | countrycode=="MEX" | countrycode=="NIC" | countrycode=="ANT" | countrycode=="PAN" | countrycode=="PER" | countrycode=="PRI" | countrycode=="KNA" | countrycode=="LCA" | countrycode=="VCT" | countrycode=="SUR" | countrycode=="TTO" | countrycode=="VEN")
CANAME.SOFI11<-subset(SOFI11,
                      countrycode=="DZA" | countrycode=="ARM" | countrycode=="AZE" | countrycode=="BHR" | countrycode=="EGY" | countrycode=="GEO" | countrycode=="IRN" | countrycode=="IRQ" | countrycode=="JOR" | countrycode=="KAZ" | countrycode=="KWT" | countrycode=="KGZ" | countrycode=="LBN" | countrycode=="LBY" | countrycode=="MNG" | countrycode=="MAR" | countrycode=="PSE" | countrycode=="OMN" | countrycode=="QAT" | countrycode=="SAU" | countrycode=="SYR" | countrycode=="TJK" | countrycode=="TUN" | countrycode=="TUR" | countrycode=="TKM" | countrycode=="ARE" | countrycode=="UZB" | countrycode=="YEM")
ESEASP.SOFI11<-subset(SOFI11,
                      countrycode=="KHM" | countrycode=="COK" | countrycode=="PRK" | countrycode=="FJI" | countrycode=="PYF" | countrycode=="IDN" | countrycode=="KIR" | countrycode=="LAO" | countrycode=="MYS" | countrycode=="MDV" | countrycode=="MHL" | countrycode=="FSM" | countrycode=="MMR" | countrycode=="NRU" | countrycode=="PLW" | countrycode=="PNG" | countrycode=="PHL" | countrycode=="WSM" | countrycode=="SLB" | countrycode=="LKA" | countrycode=="THA" | countrycode=="TLS" | countrycode=="TON" | countrycode=="VNM" | countrycode=="VUT")
SUSAAF.SOFI11<-subset(SOFI11,
                      countrycode=="AGO" | countrycode=="BWA" | countrycode=="BFA" | countrycode=="BUR" | countrycode=="CMR" | countrycode=="CPV" | countrycode=="CAF" | countrycode=="TCD" | countrycode=="COM" | countrycode=="COG" | countrycode=="CIV" | countrycode=="COD" | countrycode=="DJI" | countrycode=="ERI" | countrycode=="ETH" | countrycode=="GNQ" | countrycode=="GAB" | countrycode=="GMB" | countrycode=="GHA" | countrycode=="GIN" | countrycode=="GNB" | countrycode=="LSO" | countrycode=="LBR" | countrycode=="KEN" | countrycode=="MDG" | countrycode=="MWI" | countrycode=="MLI" | countrycode=="MRT" | countrycode=="MUS" | countrycode=="MOZ" | countrycode=="NAM" | countrycode=="NER" | countrycode=="NGA" | countrycode=="RWA" | countrycode=="STP" | countrycode=="SEN" | countrycode=="SYC" | countrycode=="SLE" | countrycode=="SOM" | countrycode=="ZAF" | countrycode=="SDN" | countrycode=="SWZ" | countrycode=="TGO" | countrycode=="UGA" | countrycode=="TZA" | countrycode=="ZMB" | countrycode=="ZWE")
SOASIA.SOFI11<-subset(SOFI11,
                      countrycode=="AFG" | countrycode=="BGD" | countrycode=="BTN" | countrycode=="NPL" | countrycode=="PAK")


HIGHIN.SOFI11<-merge(HIGHIN.SOFI11,HIGHIN.fbspop, by="countrycode")
HIGHIN.totpop<-sum(HIGHIN.SOFI11$pop)
HIGHIN.SOFI11$fraction_totpop<-HIGHIN.SOFI11$pop/HIGHIN.totpop
HIGHIN.SOFI11$weightedSOFI11cv<-HIGHIN.SOFI11$CV_2011_SOFI*HIGHIN.SOFI11$fraction_totpop
HIGHIN<-data.frame("countrycode"="HIGHIN","SOFI11"=sum(HIGHIN.SOFI11$weightedSOFI11cv,na.rm=TRUE))

SOTRLA.SOFI11<-merge(SOTRLA.SOFI11,SOTRLA.fbspop, by="countrycode")
SOTRLA.totpop<-sum(SOTRLA.SOFI11$pop)
SOTRLA.SOFI11$fraction_totpop<-SOTRLA.SOFI11$pop/SOTRLA.totpop
SOTRLA.SOFI11$weightedSOFI11cv<-SOTRLA.SOFI11$CV_2011_SOFI*SOTRLA.SOFI11$fraction_totpop
SOTRLA<-data.frame("countrycode"="SOTRLA","SOFI11"=sum(SOTRLA.SOFI11$weightedSOFI11cv,na.rm=TRUE))

CEEAEU.SOFI11<-merge(CEEAEU.SOFI11,CEEAEU.fbspop, by="countrycode")
CEEAEU.totpop<-sum(CEEAEU.SOFI11$pop)
CEEAEU.SOFI11$fraction_totpop<-CEEAEU.SOFI11$pop/CEEAEU.totpop
CEEAEU.SOFI11$weightedSOFI11cv<-CEEAEU.SOFI11$CV_2011_SOFI*CEEAEU.SOFI11$fraction_totpop
CEEAEU<-data.frame("countrycode"="CEEAEU","SOFI11"=sum(CEEAEU.SOFI11$weightedSOFI11cv,na.rm=TRUE))

CALACA.SOFI11<-merge(CALACA.SOFI11,CALACA.fbspop, by="countrycode")
CALACA.totpop<-sum(CALACA.SOFI11$pop)
CALACA.SOFI11$fraction_totpop<-CALACA.SOFI11$pop/CALACA.totpop
CALACA.SOFI11$weightedSOFI11cv<-CALACA.SOFI11$CV_2011_SOFI*CALACA.SOFI11$fraction_totpop
CALACA<-data.frame("countrycode"="CALACA","SOFI11"=sum(CALACA.SOFI11$weightedSOFI11cv,na.rm=TRUE))

CANAME.SOFI11<-merge(CANAME.SOFI11,CANAME.fbspop, by="countrycode")
CANAME.totpop<-sum(CANAME.SOFI11$pop)
CANAME.SOFI11$fraction_totpop<-CANAME.SOFI11$pop/CANAME.totpop
CANAME.SOFI11$weightedSOFI11cv<-CANAME.SOFI11$CV_2011_SOFI*CANAME.SOFI11$fraction_totpop
CANAME<-data.frame("countrycode"="CANAME","SOFI11"=sum(CANAME.SOFI11$weightedSOFI11cv,na.rm=TRUE))

ESEASP.SOFI11<-merge(ESEASP.SOFI11,ESEASP.fbspop, by="countrycode")
ESEASP.totpop<-sum(ESEASP.SOFI11$pop)
ESEASP.SOFI11$fraction_totpop<-ESEASP.SOFI11$pop/ESEASP.totpop
ESEASP.SOFI11$weightedSOFI11cv<-ESEASP.SOFI11$CV_2011_SOFI*ESEASP.SOFI11$fraction_totpop
ESEASP<-data.frame("countrycode"="ESEASP","SOFI11"=sum(ESEASP.SOFI11$weightedSOFI11cv,na.rm=TRUE))

SUSAAF.SOFI11<-merge(SUSAAF.SOFI11,SUSAAF.fbspop, by="countrycode")
SUSAAF.totpop<-sum(SUSAAF.SOFI11$pop)
SUSAAF.SOFI11$fraction_totpop<-SUSAAF.SOFI11$pop/SUSAAF.totpop
SUSAAF.SOFI11$weightedSOFI11cv<-SUSAAF.SOFI11$CV_2011_SOFI*SUSAAF.SOFI11$fraction_totpop
SUSAAF<-data.frame("countrycode"="SUSAAF","SOFI11"=sum(SUSAAF.SOFI11$weightedSOFI11cv,na.rm=TRUE))

SOASIA.SOFI11<-merge(SOASIA.SOFI11,SOASIA.fbspop, by="countrycode")
SOASIA.totpop<-sum(SOASIA.SOFI11$pop)
SOASIA.SOFI11$fraction_totpop<-SOASIA.SOFI11$pop/SOASIA.totpop
SOASIA.SOFI11$weightedSOFI11cv<-SOASIA.SOFI11$CV_2011_SOFI*SOASIA.SOFI11$fraction_totpop
SOASIA<-data.frame("countrycode"="SOASIA","SOFI11"=sum(SOASIA.SOFI11$weightedSOFI11cv,na.rm=TRUE))


SOFIgroups<-data.frame("Country.Name"=
                         c(
                           "HIGHIN",
                           "SOTRLA",
                           "CEEAEU",
                           "CALACA", 
                           "CANAME",
                           "ESEASP",
                           "SUSAAF",
                           "SOASIA"                          
                         ),
                       rbind(
                         HIGHIN,
                         SOTRLA,
                         CEEAEU,
                         CALACA, 
                         CANAME,
                         ESEASP,
                         SUSAAF,
                         SOASIA
                       ))
SOFIgroups<-data.frame("country"=SOFIgroups[,1],
                       "order"=0,
                       "countrycode"=SOFIgroups[,2],
                       "X"=NA,
                       "SK_2011_SOFI"=NA,
                       "CV_2011_SOFI"=SOFIgroups[,3],
                       "skewlog_applied_2011"=1234
                       )
#View(SOFIgroups)
names(SOFIgroups)
names(SOFI11)

SOFI11<-rbind(SOFI11,SOFIgroups)


###merging sofi with GINI and FBS####
SOFIGINIFBS<-data.frame(merge(GINI_vs_FBS,SOFI11,by="countrycode", all = TRUE))
SOFIGINIFBSVeg<-data.frame(merge(GINI_vs_FBSVeg,SOFI11,by="countrycode", all = TRUE))
plot(SOFIGINIFBS$CV_2011_SOFI~SOFIGINIFBS$CofV_modelled)
plot(SOFIGINIFBS$CV_2011_SOFI~SOFIGINIFBS$mostrecentGINI)
plot(SOFIGINIFBS$SK_2011_SOFI~SOFIGINIFBS$skewness_modelled)

##option 1: assume all distributions are lognormal.####
#option 2: get the distribution type from SOFI,
 #when the distribution type isn't lognormal, remove the skewness and replace it with the energy skewness

###getting the confidence intervals for CO2 effect####
names(SOFIGINIFBS)
SOFIGINIFBS$CO2CI<-SOFIGINIFBS$protein.co2.g.SE*1.96
SOFIGINIFBS$CO2CIu<-SOFIGINIFBS$CO2CI+SOFIGINIFBS$protein.co2.g
SOFIGINIFBS$CO2CIl<-SOFIGINIFBS$protein.co2.g-SOFIGINIFBS$CO2CI

SOFIGINIFBSVeg$CO2CI<-SOFIGINIFBSVeg$protein.co2.g.SE*1.96
SOFIGINIFBSVeg$CO2CIu<-SOFIGINIFBSVeg$CO2CI+SOFIGINIFBSVeg$protein.co2.g
SOFIGINIFBSVeg$CO2CIl<-SOFIGINIFBSVeg$protein.co2.g-SOFIGINIFBSVeg$CO2CI
###Calculate the geometric mean ####
#based on SOFIGINIFBS protein.amb.g, protein.co2.g_lo, protein.co2.g_med, protein.co2.g_hi for CV_2011_SOFI and CofV_modelled

FBS_Distribution_parameters<-data.frame("countrycode" = SOFIGINIFBS$countrycode,
                                        "country" = SOFIGINIFBS$country.x)
FBS_Distribution_parameters$SD_HHS<-sqrt(log(1+SOFIGINIFBS$CofV_modelled))
FBS_Distribution_parameters$SD_SOFI<-sqrt(log(1+SOFIGINIFBS$CV_2011_SOFI))

FBS_Distribution_parameters$Amb_mean_HHS <- log(SOFIGINIFBS$protein.amb.g)- (FBS_Distribution_parameters$SD_HHS^2)/2
FBS_Distribution_parameters$Amb_mean_SOFI <- log(SOFIGINIFBS$protein.amb.g)- (FBS_Distribution_parameters$SD_SOFI^2)/2
FBS_Distribution_parameters$CO2_mean_HHS <- log(SOFIGINIFBS$protein.co2.g)- (FBS_Distribution_parameters$SD_HHS^2)/2
FBS_Distribution_parameters$CO2_CIu_HHS<-log(SOFIGINIFBS$CO2CIu)- (FBS_Distribution_parameters$SD_HHS^2)/2
FBS_Distribution_parameters$CO2_CIl_HHS<-log(SOFIGINIFBS$CO2CIl)- (FBS_Distribution_parameters$SD_HHS^2)/2
FBS_Distribution_parameters$CO2_mean_SOFI <- log(SOFIGINIFBS$protein.co2.g)- (FBS_Distribution_parameters$SD_SOFI^2)/2
FBS_Distribution_parameters$CO2_CIu_SOFI<-log(SOFIGINIFBS$CO2CIu)- (FBS_Distribution_parameters$SD_SOFI^2)/2
FBS_Distribution_parameters$CO2_CIl_SOFI<-log(SOFIGINIFBS$CO2CIl)- (FBS_Distribution_parameters$SD_SOFI^2)/2
#View(FBS_Distribution_parameters)

write.csv(FBS_Distribution_parameters, "3_intake/outputs/FBS_Distribution_parameters.csv")



FBS_Distribution_parametersVeg<-data.frame("countrycode" = SOFIGINIFBSVeg$countrycode,
                                        "country" = SOFIGINIFBSVeg$country.x)
FBS_Distribution_parametersVeg$SD_HHS<-sqrt(log(1+SOFIGINIFBSVeg$CofV_modelled))
FBS_Distribution_parametersVeg$SD_SOFI<-sqrt(log(1+SOFIGINIFBSVeg$CV_2011_SOFI))

FBS_Distribution_parametersVeg$Amb_mean_HHS <- log(SOFIGINIFBSVeg$protein.amb.g)- (FBS_Distribution_parametersVeg$SD_HHS^2)/2
FBS_Distribution_parametersVeg$Amb_mean_SOFI <- log(SOFIGINIFBSVeg$protein.amb.g)- (FBS_Distribution_parametersVeg$SD_SOFI^2)/2
FBS_Distribution_parametersVeg$CO2_mean_HHS <- log(SOFIGINIFBSVeg$protein.co2.g)- (FBS_Distribution_parametersVeg$SD_HHS^2)/2
FBS_Distribution_parametersVeg$CO2_CIu_HHS<-log(SOFIGINIFBSVeg$CO2CIu)- (FBS_Distribution_parametersVeg$SD_HHS^2)/2
FBS_Distribution_parametersVeg$CO2_CIl_HHS<-log(SOFIGINIFBSVeg$CO2CIl)- (FBS_Distribution_parametersVeg$SD_HHS^2)/2
FBS_Distribution_parametersVeg$CO2_mean_SOFI <- log(SOFIGINIFBSVeg$protein.co2.g)- (FBS_Distribution_parametersVeg$SD_SOFI^2)/2
FBS_Distribution_parametersVeg$CO2_CIu_SOFI<-log(SOFIGINIFBSVeg$CO2CIu)- (FBS_Distribution_parametersVeg$SD_SOFI^2)/2
FBS_Distribution_parametersVeg$CO2_CIl_SOFI<-log(SOFIGINIFBSVeg$CO2CIl)- (FBS_Distribution_parametersVeg$SD_SOFI^2)/2
#View(FBS_Distribution_parametersVeg)

write.csv(FBS_Distribution_parametersVeg, "3_intake/outputs/FBS_Distribution_parametersVeg.csv")


forGlobalGrams<-merge(SOFIGINIFBS[!SOFIGINIFBS$countrycode=="",c(1,14,3,5,7)],totalpop11[,c(1,3)],by="countrycode")
write.csv(forGlobalGrams,"3_intake/outputs/forGlobalGrams.csv")