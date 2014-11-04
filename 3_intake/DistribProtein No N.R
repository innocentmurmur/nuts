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
View(countrycode)

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

View(GINI)
GINI_vs_Protein<-data.frame(merge(country.mean.percent.protein,GINI,by="countrycode"), all=TRUE)

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

library(mgcv)
attach(Time.since.survey)
blah2<-gam(as.numeric(CofV)~s(as.numeric(GINI_before_survey)),data=Time.since.survey)
plot(blah2)
summary(blah2)

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
plot(lm(log(CofV)~as.numeric(GINI_before_survey),data=Time.since.survey))
plot(log(CofV)~as.numeric(GINI_before_survey),data=Time.since.survey)

#I think I'll go with this one. Logging it makes the residual plots look better. Logging GINI as well doesn't make it any better.

summary(lm(log(CofV)~as.numeric(GINI_3yrsbefore_survey),data=Time.since.survey))
plot(lm(log(CofV)~as.numeric(GINI_3yrsbefore_survey),data=Time.since.survey))
#there really aren't enough GINIs three years before the surveys to make a good model.


lm.cv.gini<-lm(log(CofV)~as.numeric(GINI_before_survey),data=Time.since.survey)
intercept<-coef(lm.cv.gini)[1]
slope<-coef(lm.cv.gini)[2]

summary(lm.cv.gini)
save(lm.cv.gini,file="3_intake/outputs/linearmodel_CV_vs_GINI.Rdata")
#####separately, calculate a C of V for each country based on GINI####
load("2_countries/outputs/FBStot.Rdata")
GINI_vs_FBS<-data.frame(merge(FBStot,GINI[,c(2,39,40)],by="countrycode", all = TRUE))
GINI_vs_FBS$CofV_modelled<-exp(intercept+(slope*GINI_vs_FBS$mostrecentGINI))


###calculate a skewness based on C of V####
GINI_vs_FBS$skewness_modelled<-(GINI_vs_FBS$CofV_modelled^2+3)*GINI_vs_FBS$CofV_modelled

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
SOFIGINIFBS<-data.frame(merge(GINI_vs_FBS,SOFI11,by="countrycode", all = TRUE))
plot(SOFIGINIFBS$CV_2011_SOFI~SOFIGINIFBS$CofV_modelled)
plot(SOFIGINIFBS$CV_2011_SOFI~SOFIGINIFBS$mostrecentGINI)
plot(SOFIGINIFBS$SK_2011_SOFI~SOFIGINIFBS$skewness_modelled)

#option 1: assume all distributions are lognormal.
#option 2: get the distribution type from SOFI,
    #when the distribution type isn't lognormal, remove the skewness and replace it with the energy skewness
names(SOFIGINIFBS)
#Calculate the geometric mean based on SOFIGINIFBS protein.amb.g, protein.co2.g_lo, protein.co2.g_med, protein.co2.g_hi for CV_2011_SOFI and CofV_modelled

FBS_Distribution_parameters<-data.frame("countrycode" = SOFIGINIFBS$countrycode,
                                        "country" = SOFIGINIFBS$country.x)
FBS_Distribution_parameters$SD_HHS<-sqrt(log(1+SOFIGINIFBS$CofV_modelled))
FBS_Distribution_parameters$SD_SOFI<-sqrt(log(1+SOFIGINIFBS$CV_2011_SOFI))

FBS_Distribution_parameters$Amb_mean_HHS <- log(SOFIGINIFBS$protein.amb.g)- (FBS_Distribution_parameters$SD_HHS^2)/2
FBS_Distribution_parameters$Amb_mean_SOFI <- log(SOFIGINIFBS$protein.amb.g)- (FBS_Distribution_parameters$SD_SOFI^2)/2
FBS_Distribution_parameters$CO2_mean_HHS <- log(SOFIGINIFBS$protein.co2.g)- (FBS_Distribution_parameters$SD_HHS^2)/2
FBS_Distribution_parameters$CO2_mean_SOFI <- log(SOFIGINIFBS$protein.co2.g)- (FBS_Distribution_parameters$SD_SOFI^2)/2

View(FBS_Distribution_parameters)

write.csv(FBS_Distribution_parameters, "3_intake/outputs/FBS_Distribution_parameters.csv")

