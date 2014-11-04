library(reshape2)

####population demographics####
pop<-read.csv("4_EAR/inputs/population/POPULATION_BY_AGE.CSV")
# pop<-read.csv("POPULATION_BY_AGE.CSV")
pop$Time<-as.factor(pop$Time)
pop<-subset(pop,Time=="2010")

names(pop)
notin<-names(pop) %in% c("LocID", "VarID", "Variant","MidPeriod","SexID","Pop_80_100")


pop<-pop[!notin]

country.pop<-read.csv("0_labels/order_pop.csv")

pop<-merge(pop,country.pop, by="Location")


HIGHIN.pop<-subset(pop,
                      countrycode=="AND" | countrycode=="AUS" | countrycode=="AUT" | countrycode=="BEL" | countrycode=="BRN" | countrycode=="CAN" | countrycode=="CYP" | countrycode=="DNK" | countrycode=="FIN" | countrycode=="FRA" | countrycode=="DEU" | countrycode=="GRC" | countrycode=="GRL" | countrycode=="ISL" | countrycode=="IRL" | countrycode=="ISR" | countrycode=="ITA" | countrycode=="JPN" | countrycode=="LUX" | countrycode=="MLT" | countrycode=="NLD" | countrycode=="NZL" | countrycode=="NOR" | countrycode=="PRT" | countrycode=="KOR" | countrycode=="SGP" | countrycode=="ESP" | countrycode=="SWE" | countrycode=="CHE" | countrycode=="GBR" | countrycode=="USA")
HIGHIN.popsumf<-data.frame("Location"="HIGHIN", "Time"="2010", "Sex"="Female",colSums(subset(HIGHIN.pop,Sex=="Female")[4:24]))
HIGHIN.popsumm<-data.frame("Location"="HIGHIN", "Time"="2010", "Sex"="Male",colSums(subset(HIGHIN.pop,Sex=="Male")[4:24]))
HIGHIN.popsumb<-data.frame("Location"="HIGHIN", "Time"="2010", "Sex"="Both",colSums(subset(HIGHIN.pop,Sex=="Both")[4:24]))

names(HIGHIN.popsumf)[4]<-"pop"
names(HIGHIN.popsumm)[4]<-"pop"
names(HIGHIN.popsumb)[4]<-"pop"
HIGHIN.popsumf$agebracket<-row.names(HIGHIN.popsumf)
HIGHIN.popsumm$agebracket<-row.names(HIGHIN.popsumm)
HIGHIN.popsumb$agebracket<-row.names(HIGHIN.popsumb)


HIGHIN.popsum<-rbind(HIGHIN.popsumf,HIGHIN.popsumm,HIGHIN.popsumb)
HIGHIN.popsumw<-dcast(HIGHIN.popsum,Sex~agebracket,value.var="pop")

SOTRLA.pop<-subset(pop,
                      countrycode=="ARG" | countrycode=="BRA" | countrycode=="CHL" | countrycode=="PRY" | countrycode=="URY")
SOTRLA.popsumf<-data.frame("Location"="SOTRLA", "Time"="2010", "Sex"="Female",colSums(subset(SOTRLA.pop,Sex=="Female")[4:24]))
SOTRLA.popsumm<-data.frame("Location"="SOTRLA", "Time"="2010", "Sex"="Male",colSums(subset(SOTRLA.pop,Sex=="Male")[4:24]))
SOTRLA.popsumb<-data.frame("Location"="SOTRLA", "Time"="2010", "Sex"="Both",colSums(subset(SOTRLA.pop,Sex=="Both")[4:24]))

names(SOTRLA.popsumf)[4]<-"pop"
names(SOTRLA.popsumm)[4]<-"pop"
names(SOTRLA.popsumb)[4]<-"pop"
SOTRLA.popsumf$agebracket<-row.names(SOTRLA.popsumf)
SOTRLA.popsumm$agebracket<-row.names(SOTRLA.popsumm)
SOTRLA.popsumb$agebracket<-row.names(SOTRLA.popsumb)


SOTRLA.popsum<-rbind(SOTRLA.popsumf,SOTRLA.popsumm,SOTRLA.popsumb)
SOTRLA.popsumw<-dcast(SOTRLA.popsum,Sex~agebracket,value.var="pop")


CEEAEU.pop<-subset(pop,
                      countrycode=="ALB" | countrycode=="BLR" | countrycode=="BIH" | countrycode=="BGR" | countrycode=="HRV" | countrycode=="CZE" | countrycode=="EST" | countrycode=="HUN" | countrycode=="LVA" | countrycode=="LTU" | countrycode=="MKD" | countrycode=="MDA" | countrycode=="MNE" | countrycode=="POL" | countrycode=="ROU" | countrycode=="RUS" | countrycode=="SRB" | countrycode=="SVK" | countrycode=="SVN" | countrycode=="UKR")
CEEAEU.popsumf<-data.frame("Location"="CEEAEU", "Time"="2010", "Sex"="Female",colSums(subset(CEEAEU.pop,Sex=="Female")[4:24]))
CEEAEU.popsumm<-data.frame("Location"="CEEAEU", "Time"="2010", "Sex"="Male",colSums(subset(CEEAEU.pop,Sex=="Male")[4:24]))
CEEAEU.popsumb<-data.frame("Location"="CEEAEU", "Time"="2010", "Sex"="Both",colSums(subset(CEEAEU.pop,Sex=="Both")[4:24]))

names(CEEAEU.popsumf)[4]<-"pop"
names(CEEAEU.popsumm)[4]<-"pop"
names(CEEAEU.popsumb)[4]<-"pop"
CEEAEU.popsumf$agebracket<-row.names(CEEAEU.popsumf)
CEEAEU.popsumm$agebracket<-row.names(CEEAEU.popsumm)
CEEAEU.popsumb$agebracket<-row.names(CEEAEU.popsumb)


CEEAEU.popsum<-rbind(CEEAEU.popsumf,CEEAEU.popsumm,CEEAEU.popsumb)
CEEAEU.popsumw<-dcast(CEEAEU.popsum,Sex~agebracket,value.var="pop")


CALACA.pop<-subset(pop,
                      countrycode=="ATG" | countrycode=="BHS" | countrycode=="BRB" | countrycode=="BLZ" | countrycode=="BMU" | countrycode=="BOL" | countrycode=="VGB" | countrycode=="COL" | countrycode=="CRI" | countrycode=="CUB" | countrycode=="DMA" | countrycode=="DOM" | countrycode=="ECU" | countrycode=="SLV" | countrycode=="GRD" | countrycode=="GTM" | countrycode=="GUY" | countrycode=="HTI" | countrycode=="HND" | countrycode=="JAM" | countrycode=="MEX" | countrycode=="NIC" | countrycode=="ANT" | countrycode=="PAN" | countrycode=="PER" | countrycode=="PRI" | countrycode=="KNA" | countrycode=="LCA" | countrycode=="VCT" | countrycode=="SUR" | countrycode=="TTO" | countrycode=="VEN")
 
CALACA.popsumf<-data.frame("Location"="CALACA", "Time"="2010", "Sex"="Female",colSums(subset(CALACA.pop,Sex=="Female")[4:24]))
CALACA.popsumm<-data.frame("Location"="CALACA", "Time"="2010", "Sex"="Male",colSums(subset(CALACA.pop,Sex=="Male")[4:24]))
CALACA.popsumb<-data.frame("Location"="CALACA", "Time"="2010", "Sex"="Both",colSums(subset(CALACA.pop,Sex=="Both")[4:24]))

names(CALACA.popsumf)[4]<-"pop"
names(CALACA.popsumm)[4]<-"pop"
names(CALACA.popsumb)[4]<-"pop"
CALACA.popsumf$agebracket<-row.names(CALACA.popsumf)
CALACA.popsumm$agebracket<-row.names(CALACA.popsumm)
CALACA.popsumb$agebracket<-row.names(CALACA.popsumb)


CALACA.popsum<-rbind(CALACA.popsumf,CALACA.popsumm,CALACA.popsumb)
CALACA.popsumw<-dcast(CALACA.popsum,Sex~agebracket,value.var="pop")

CANAME.pop<-subset(pop,
                      countrycode=="DZA" | countrycode=="ARM" | countrycode=="AZE" | countrycode=="BHR" | countrycode=="EGY" | countrycode=="GEO" | countrycode=="IRN" | countrycode=="IRQ" | countrycode=="JOR" | countrycode=="KAZ" | countrycode=="KWT" | countrycode=="KGZ" | countrycode=="LBN" | countrycode=="LBY" | countrycode=="MNG" | countrycode=="MAR" | countrycode=="PSE" | countrycode=="OMN" | countrycode=="QAT" | countrycode=="SAU" | countrycode=="SYR" | countrycode=="TJK" | countrycode=="TUN" | countrycode=="TUR" | countrycode=="TKM" | countrycode=="ARE" | countrycode=="UZB" | countrycode=="YEM")
CANAME.popsumf<-data.frame("Location"="CANAME", "Time"="2010", "Sex"="Female",colSums(subset(CANAME.pop,Sex=="Female")[4:24]))
CANAME.popsumm<-data.frame("Location"="CANAME", "Time"="2010", "Sex"="Male",colSums(subset(CANAME.pop,Sex=="Male")[4:24]))
CANAME.popsumb<-data.frame("Location"="CANAME", "Time"="2010", "Sex"="Both",colSums(subset(CANAME.pop,Sex=="Both")[4:24]))

names(CANAME.popsumf)[4]<-"pop"
names(CANAME.popsumm)[4]<-"pop"
names(CANAME.popsumb)[4]<-"pop"
CANAME.popsumf$agebracket<-row.names(CANAME.popsumf)
CANAME.popsumm$agebracket<-row.names(CANAME.popsumm)
CANAME.popsumb$agebracket<-row.names(CANAME.popsumb)


CANAME.popsum<-rbind(CANAME.popsumf,CANAME.popsumm,CANAME.popsumb)
CANAME.popsumw<-dcast(CANAME.popsum,Sex~agebracket,value.var="pop")

ESEASP.pop<-subset(pop,
                      countrycode=="KHM" | countrycode=="COK" | countrycode=="PRK" | countrycode=="FJI" | countrycode=="PYF" | countrycode=="IDN" | countrycode=="KIR" | countrycode=="LAO" | countrycode=="MYS" | countrycode=="MDV" | countrycode=="MHL" | countrycode=="FSM" | countrycode=="MMR" | countrycode=="NRU" | countrycode=="PLW" | countrycode=="PNG" | countrycode=="PHL" | countrycode=="WSM" | countrycode=="SLB" | countrycode=="LKA" | countrycode=="THA" | countrycode=="TLS" | countrycode=="TON" | countrycode=="VNM" | countrycode=="VUT")
ESEASP.popsumf<-data.frame("Location"="ESEASP", "Time"="2010", "Sex"="Female",colSums(subset(ESEASP.pop,Sex=="Female")[4:24]))
ESEASP.popsumm<-data.frame("Location"="ESEASP", "Time"="2010", "Sex"="Male",colSums(subset(ESEASP.pop,Sex=="Male")[4:24]))
ESEASP.popsumb<-data.frame("Location"="ESEASP", "Time"="2010", "Sex"="Both",colSums(subset(ESEASP.pop,Sex=="Both")[4:24]))

names(ESEASP.popsumf)[4]<-"pop"
names(ESEASP.popsumm)[4]<-"pop"
names(ESEASP.popsumb)[4]<-"pop"
ESEASP.popsumf$agebracket<-row.names(ESEASP.popsumf)
ESEASP.popsumm$agebracket<-row.names(ESEASP.popsumm)
ESEASP.popsumb$agebracket<-row.names(ESEASP.popsumb)


ESEASP.popsum<-rbind(ESEASP.popsumf,ESEASP.popsumm,ESEASP.popsumb)
ESEASP.popsumw<-dcast(ESEASP.popsum,Sex~agebracket,value.var="pop")

SUSAAF.pop<-subset(pop,
                      countrycode=="AGO" | countrycode=="BWA" | countrycode=="BFA" | countrycode=="BUR" | countrycode=="CMR" | countrycode=="CPV" | countrycode=="CAF" | countrycode=="TCD" | countrycode=="COM" | countrycode=="COG" | countrycode=="CIV" | countrycode=="COD" | countrycode=="DJI" | countrycode=="ERI" | countrycode=="ETH" | countrycode=="GNQ" | countrycode=="GAB" | countrycode=="GMB" | countrycode=="GHA" | countrycode=="GIN" | countrycode=="GNB" | countrycode=="LSO" | countrycode=="LBR" | countrycode=="KEN" | countrycode=="MDG" | countrycode=="MWI" | countrycode=="MLI" | countrycode=="MRT" | countrycode=="MUS" | countrycode=="MOZ" | countrycode=="NAM" | countrycode=="NER" | countrycode=="NGA" | countrycode=="RWA" | countrycode=="STP" | countrycode=="SEN" | countrycode=="SYC" | countrycode=="SLE" | countrycode=="SOM" | countrycode=="ZAF" | countrycode=="SDN" | countrycode=="SWZ" | countrycode=="TGO" | countrycode=="UGA" | countrycode=="TZA" | countrycode=="ZMB" | countrycode=="ZWE")
SUSAAF.popsumf<-data.frame("Location"="SUSAAF", "Time"="2010", "Sex"="Female",colSums(subset(SUSAAF.pop,Sex=="Female")[4:24]))
SUSAAF.popsumm<-data.frame("Location"="SUSAAF", "Time"="2010", "Sex"="Male",colSums(subset(SUSAAF.pop,Sex=="Male")[4:24]))
SUSAAF.popsumb<-data.frame("Location"="SUSAAF", "Time"="2010", "Sex"="Both",colSums(subset(SUSAAF.pop,Sex=="Both")[4:24]))

names(SUSAAF.popsumf)[4]<-"pop"
names(SUSAAF.popsumm)[4]<-"pop"
names(SUSAAF.popsumb)[4]<-"pop"
SUSAAF.popsumf$agebracket<-row.names(SUSAAF.popsumf)
SUSAAF.popsumm$agebracket<-row.names(SUSAAF.popsumm)
SUSAAF.popsumb$agebracket<-row.names(SUSAAF.popsumb)


SUSAAF.popsum<-rbind(SUSAAF.popsumf,SUSAAF.popsumm,SUSAAF.popsumb)
SUSAAF.popsumw<-dcast(SUSAAF.popsum,Sex~agebracket,value.var="pop")

SOASIA.pop<-subset(pop,
                      countrycode=="AFG" | countrycode=="BGD" | countrycode=="BTN" | countrycode=="NPL" | countrycode=="PAK")
SOASIA.popsumf<-data.frame("Location"="SOASIA", "Time"="2010", "Sex"="Female",colSums(subset(SOASIA.pop,Sex=="Female")[4:24]))
SOASIA.popsumm<-data.frame("Location"="SOASIA", "Time"="2010", "Sex"="Male",colSums(subset(SOASIA.pop,Sex=="Male")[4:24]))
SOASIA.popsumb<-data.frame("Location"="SOASIA", "Time"="2010", "Sex"="Both",colSums(subset(SOASIA.pop,Sex=="Both")[4:24]))

names(SOASIA.popsumf)[4]<-"pop"
names(SOASIA.popsumm)[4]<-"pop"
names(SOASIA.popsumb)[4]<-"pop"
SOASIA.popsumf$agebracket<-row.names(SOASIA.popsumf)
SOASIA.popsumm$agebracket<-row.names(SOASIA.popsumm)
SOASIA.popsumb$agebracket<-row.names(SOASIA.popsumb)


SOASIA.popsum<-rbind(SOASIA.popsumf,SOASIA.popsumm,SOASIA.popsumb)
SOASIA.popsumw<-dcast(SOASIA.popsum,Sex~agebracket,value.var="pop")


popsum<-data.frame("countrycode"=
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
                     HIGHIN.popsumw,
                     SOTRLA.popsumw,
                     CEEAEU.popsumw,
                     CALACA.popsumw, 
                     CANAME.popsumw,
                     ESEASP.popsumw,
                     SUSAAF.popsumw,
                     SOASIA.popsumw
                   ))

View(popsum)





pop<-pop[,c(26,3:24)]
names(pop)
names(popsum)
pop<-rbind(pop,popsum)

View(pop)








bothpop<-subset(pop,Sex=="Both")
# names(bothpop)
# bothpop$sum<-sum(bothpop[1:236,1:21])
# 
# View(pop)
# View(bothpop)
long<-melt(bothpop)
names(long)
longpop<-melt(pop)


totalpop<-data.frame(rowsum(long$value,long$countrycode))

names(totalpop)[1]<-"total_population"
totalpop$countrycode<-row.names(totalpop)
View(totalpop)

#totalpop[4:25]<-totalpop[1]
#forgot i'd done that.... is it still important?

girlsLONG<-subset(longpop,Sex=="Female")
boysLONG<-subset(longpop,Sex=="Male")
girls<-subset(pop,Sex=="Female")

for(rownum in 1:length(girls[,23]))
{girls$adults[rownum]<-sum(girls[rownum,7:23])}

boys<-subset(pop,Sex=="Male")
for(rownum in 1:length(boys[,23]))
{boys$adults[rownum]<-sum(boys[rownum,7:23])}


#Fraction of total population in each category: might be useful for something, but not useful right now
boys.total<-data.frame("total"=with(boysLONG, tapply(value,countrycode, sum)))
girls.total<-data.frame("total"=with(girlsLONG, tapply(value,countrycode, sum)))


midpoint<-data.frame(2,7,12,17,22,27,32,37,42,47,52,57,62,67,72,77,82,87,92,97,102)

boys.fraction<-boys
for(rownum in 1:length(boys.fraction[,23]))
{boys.fraction[rownum,3:23]<-boys[rownum,3:23]/boys.total[rownum,1]*midpoint}

girls.fraction<-girls
for(rownum in 1:length(girls.fraction[,23]))
{girls.fraction[rownum,3:23]<-girls[rownum,3:23]/girls.total[rownum,1]*midpoint}

girls.fraction<-girls.fraction[1:23]

for(rownum in 1:length(girls.fraction[,23]))
{girls.fraction$adults[rownum]<-sum(girls.fraction[rownum,7:23])}
# 
# girls.fraction<-girls/totalpop
# boys.fraction<-boys/totalpop
# girls.fraction[1:3]<-girls[1:3]
# boys.fraction[1:3]<-boys[1:3]
# girls.weighted<-girls.fraction[1:3]
# girls.weighted["avgage"]<-  
#     girls.fraction[4]*2+
#     girls.fraction[5]*7+
#     girls.fraction[6]*12+
#     girls.fraction[7]*17+
#     girls.fraction[8]*22+
#     girls.fraction[9]*27+
#     girls.fraction[10]*32+
#     girls.fraction[11]*37+
#     girls.fraction[12]*42+
#     girls.fraction[13]*47+
#     girls.fraction[14]*52+
#     girls.fraction[15]*57+
#     girls.fraction[16]*62+
#     girls.fraction[17]*67+
#     girls.fraction[18]*72+
#     girls.fraction[19]*77+
#     girls.fraction[20]*82+
#     girls.fraction[21]*87+
#     girls.fraction[22]*92+
#     girls.fraction[23]*97+
#     girls.fraction[24]*102
# 
# 
# boys.weighted<-boys.fraction[1:3]
# boys.weighted["avgage"]<-  
#   boys.fraction[4]*2+
#   boys.fraction[5]*7+
#   boys.fraction[6]*12+
#   boys.fraction[7]*17+
#   boys.fraction[8]*22+
#   boys.fraction[9]*27+
#   boys.fraction[10]*32+
#   boys.fraction[11]*37+
#   boys.fraction[12]*42+
#   boys.fraction[13]*47+
#   boys.fraction[14]*52+
#   boys.fraction[15]*57+
#   boys.fraction[16]*62+
#   boys.fraction[17]*67+
#   boys.fraction[18]*72+
#   boys.fraction[19]*77+
#   boys.fraction[20]*82+
#   boys.fraction[21]*87+
#   boys.fraction[22]*92+
#   boys.fraction[23]*97+
#   boys.fraction[24]*102
# 
# av.age<-boys.weighted+girls.weighted
# av.age[1:3]<-boys.fraction[1:3]
# 
# View(av.age)
# #didn't quite work yet.
# write.csv(boys,"boys.csv")
# write.csv(girls,"girls.csv")
# write.csv(totalpop,"totalpop.csv")

#######weight####
#children under 5http://www.who.int/childgrowth/standards/en/e
#children over 5 http://www.who.int/growthref/en/
#62 kg is the average world adult weight calculated by the BMC "weight of nations" study. 
boys.idealweight<-data.frame("age_2"=12.083,"age_7"=22.9, "age_12"=38.9039175, "age_17_unless"=64.7665344, "adult"=62)
girls.idealweight<-data.frame("age_2"=11.474,"age_7"=22.4, "age_12"=41.150592, "age_17_unless"=55.726461, "adult"=62)

#option 1: assume all adults are 62 kg
#option 2: use the weight of nations per continent estimate that I've altered to be more realistic 
#so  i have a file with average weight per continent, but I had to call Yemen Europe and Tajikistan Asia

#option 3: just do it for females where you have BMI and height from DHS

#option 4: use the weight from wikipedia, not knowing how they came at it.

#option 5: special groups


#####special groups####
#from WHO

EAR_add_lact<-mean(c(16.2,15.6,14.8,14.3,14.4,15.5,10))

EAR_add_preg<-mean(c(0.5,7.7,24.9))


# these parameters = number of women in these categories as a proportion of total women * additional protein requirement in g


#Pregnant
birthrate_1000<-read.csv("4_EAR/inputs/breastfeeding_pregnant/HNP_birthrateper1000.csv")
birthrate_1000$extra_protein_Preg<-birthrate_1000$BR_2011*0.77*EAR_add_preg/1000

birthrate_1000<-birthrate_1000[,c("countrycode","extra_protein_Preg")]


###pregnant continents####
fbspop<-read.csv("2_countries/inputs/pop from fbs 2011.csv")

HIGHIN.fbspop<-subset(fbspop,
                      countrycode=="AND" | countrycode=="AUS" | countrycode=="AUT" | countrycode=="BEL" | countrycode=="BRN" | countrycode=="CAN" | countrycode=="CYP" | countrycode=="DNK" | countrycode=="FIN" | countrycode=="FRA" | countrycode=="DEU" | countrycode=="GRC" | countrycode=="GRL" | countrycode=="ISL" | countrycode=="IRL" | countrycode=="ISR" | countrycode=="ITA" | countrycode=="JPN" | countrycode=="LUX" | countrycode=="MLT" | countrycode=="NLD" | countrycode=="NZL" | countrycode=="NOR" | countrycode=="PRT" | countrycode=="KOR" | countrycode=="SGP" | countrycode=="ESP" | countrycode=="SWE" | countrycode=="CHE" | countrycode=="GBR" | countrycode=="USA")
HIGHIN.birthrate_1000<-subset(birthrate_1000,
                              countrycode=="AND" | countrycode=="AUS" | countrycode=="AUT" | countrycode=="BEL" | countrycode=="BRN" | countrycode=="CAN" | countrycode=="CYP" | countrycode=="DNK" | countrycode=="FIN" | countrycode=="FRA" | countrycode=="DEU" | countrycode=="GRC" | countrycode=="GRL" | countrycode=="ISL" | countrycode=="IRL" | countrycode=="ISR" | countrycode=="ITA" | countrycode=="JPN" | countrycode=="LUX" | countrycode=="MLT" | countrycode=="NLD" | countrycode=="NZL" | countrycode=="NOR" | countrycode=="PRT" | countrycode=="KOR" | countrycode=="SGP" | countrycode=="ESP" | countrycode=="SWE" | countrycode=="CHE" | countrycode=="GBR" | countrycode=="USA")



SOTRLA.fbspop<-subset(fbspop,
                      countrycode=="ARG" | countrycode=="BRA" | countrycode=="CHL" | countrycode=="PRY" | countrycode=="URY")
SOTRLA.birthrate_1000<-subset(birthrate_1000,
                              countrycode=="ARG" | countrycode=="BRA" | countrycode=="CHL" | countrycode=="PRY" | countrycode=="URY")


CEEAEU.fbspop<-subset(fbspop,
                      countrycode=="ALB" | countrycode=="BLR" | countrycode=="BIH" | countrycode=="BGR" | countrycode=="HRV" | countrycode=="CZE" | countrycode=="EST" | countrycode=="HUN" | countrycode=="LVA" | countrycode=="LTU" | countrycode=="MKD" | countrycode=="MDA" | countrycode=="MNE" | countrycode=="POL" | countrycode=="ROU" | countrycode=="RUS" | countrycode=="SRB" | countrycode=="SVK" | countrycode=="SVN" | countrycode=="UKR")
CEEAEU.birthrate_1000<-subset(birthrate_1000,
                              countrycode=="ALB" | countrycode=="BLR" | countrycode=="BIH" | countrycode=="BGR" | countrycode=="HRV" | countrycode=="CZE" | countrycode=="EST" | countrycode=="HUN" | countrycode=="LVA" | countrycode=="LTU" | countrycode=="MKD" | countrycode=="MDA" | countrycode=="MNE" | countrycode=="POL" | countrycode=="ROU" | countrycode=="RUS" | countrycode=="SRB" | countrycode=="SVK" | countrycode=="SVN" | countrycode=="UKR")


CALACA.fbspop<-subset(fbspop,
                      countrycode=="ATG" | countrycode=="BHS" | countrycode=="BRB" | countrycode=="BLZ" | countrycode=="BMU" | countrycode=="BOL" | countrycode=="VGB" | countrycode=="COL" | countrycode=="CRI" | countrycode=="CUB" | countrycode=="DMA" | countrycode=="DOM" | countrycode=="ECU" | countrycode=="SLV" | countrycode=="GRD" | countrycode=="GTM" | countrycode=="GUY" | countrycode=="HTI" | countrycode=="HND" | countrycode=="JAM" | countrycode=="MEX" | countrycode=="NIC" | countrycode=="ANT" | countrycode=="PAN" | countrycode=="PER" | countrycode=="PRI" | countrycode=="KNA" | countrycode=="LCA" | countrycode=="VCT" | countrycode=="SUR" | countrycode=="TTO" | countrycode=="VEN")
CALACA.birthrate_1000<-subset(birthrate_1000,
                              countrycode=="ATG" | countrycode=="BHS" | countrycode=="BRB" | countrycode=="BLZ" | countrycode=="BMU" | countrycode=="BOL" | countrycode=="VGB" | countrycode=="COL" | countrycode=="CRI" | countrycode=="CUB" | countrycode=="DMA" | countrycode=="DOM" | countrycode=="ECU" | countrycode=="SLV" | countrycode=="GRD" | countrycode=="GTM" | countrycode=="GUY" | countrycode=="HTI" | countrycode=="HND" | countrycode=="JAM" | countrycode=="MEX" | countrycode=="NIC" | countrycode=="ANT" | countrycode=="PAN" | countrycode=="PER" | countrycode=="PRI" | countrycode=="KNA" | countrycode=="LCA" | countrycode=="VCT" | countrycode=="SUR" | countrycode=="TTO" | countrycode=="VEN")

CANAME.fbspop<-subset(fbspop,
                      countrycode=="DZA" | countrycode=="ARM" | countrycode=="AZE" | countrycode=="BHR" | countrycode=="EGY" | countrycode=="GEO" | countrycode=="IRN" | countrycode=="IRQ" | countrycode=="JOR" | countrycode=="KAZ" | countrycode=="KWT" | countrycode=="KGZ" | countrycode=="LBN" | countrycode=="LBY" | countrycode=="MNG" | countrycode=="MAR" | countrycode=="PSE" | countrycode=="OMN" | countrycode=="QAT" | countrycode=="SAU" | countrycode=="SYR" | countrycode=="TJK" | countrycode=="TUN" | countrycode=="TUR" | countrycode=="TKM" | countrycode=="ARE" | countrycode=="UZB" | countrycode=="YEM")
CANAME.birthrate_1000<-subset(birthrate_1000,
                              countrycode=="DZA" | countrycode=="ARM" | countrycode=="AZE" | countrycode=="BHR" | countrycode=="EGY" | countrycode=="GEO" | countrycode=="IRN" | countrycode=="IRQ" | countrycode=="JOR" | countrycode=="KAZ" | countrycode=="KWT" | countrycode=="KGZ" | countrycode=="LBN" | countrycode=="LBY" | countrycode=="MNG" | countrycode=="MAR" | countrycode=="PSE" | countrycode=="OMN" | countrycode=="QAT" | countrycode=="SAU" | countrycode=="SYR" | countrycode=="TJK" | countrycode=="TUN" | countrycode=="TUR" | countrycode=="TKM" | countrycode=="ARE" | countrycode=="UZB" | countrycode=="YEM")

ESEASP.fbspop<-subset(fbspop,
                      countrycode=="KHM" | countrycode=="COK" | countrycode=="PRK" | countrycode=="FJI" | countrycode=="PYF" | countrycode=="IDN" | countrycode=="KIR" | countrycode=="LAO" | countrycode=="MYS" | countrycode=="MDV" | countrycode=="MHL" | countrycode=="FSM" | countrycode=="MMR" | countrycode=="NRU" | countrycode=="PLW" | countrycode=="PNG" | countrycode=="PHL" | countrycode=="WSM" | countrycode=="SLB" | countrycode=="LKA" | countrycode=="THA" | countrycode=="TLS" | countrycode=="TON" | countrycode=="VNM" | countrycode=="VUT")
ESEASP.birthrate_1000<-subset(birthrate_1000,
                              countrycode=="KHM" | countrycode=="COK" | countrycode=="PRK" | countrycode=="FJI" | countrycode=="PYF" | countrycode=="IDN" | countrycode=="KIR" | countrycode=="LAO" | countrycode=="MYS" | countrycode=="MDV" | countrycode=="MHL" | countrycode=="FSM" | countrycode=="MMR" | countrycode=="NRU" | countrycode=="PLW" | countrycode=="PNG" | countrycode=="PHL" | countrycode=="WSM" | countrycode=="SLB" | countrycode=="LKA" | countrycode=="THA" | countrycode=="TLS" | countrycode=="TON" | countrycode=="VNM" | countrycode=="VUT")

SUSAAF.fbspop<-subset(fbspop,
                      countrycode=="AGO" | countrycode=="BWA" | countrycode=="BFA" | countrycode=="BUR" | countrycode=="CMR" | countrycode=="CPV" | countrycode=="CAF" | countrycode=="TCD" | countrycode=="COM" | countrycode=="COG" | countrycode=="CIV" | countrycode=="COD" | countrycode=="DJI" | countrycode=="ERI" | countrycode=="ETH" | countrycode=="GNQ" | countrycode=="GAB" | countrycode=="GMB" | countrycode=="GHA" | countrycode=="GIN" | countrycode=="GNB" | countrycode=="LSO" | countrycode=="LBR" | countrycode=="KEN" | countrycode=="MDG" | countrycode=="MWI" | countrycode=="MLI" | countrycode=="MRT" | countrycode=="MUS" | countrycode=="MOZ" | countrycode=="NAM" | countrycode=="NER" | countrycode=="NGA" | countrycode=="RWA" | countrycode=="STP" | countrycode=="SEN" | countrycode=="SYC" | countrycode=="SLE" | countrycode=="SOM" | countrycode=="ZAF" | countrycode=="SDN" | countrycode=="SWZ" | countrycode=="TGO" | countrycode=="UGA" | countrycode=="TZA" | countrycode=="ZMB" | countrycode=="ZWE")
SUSAAF.birthrate_1000<-subset(birthrate_1000,
                              countrycode=="AGO" | countrycode=="BWA" | countrycode=="BFA" | countrycode=="BUR" | countrycode=="CMR" | countrycode=="CPV" | countrycode=="CAF" | countrycode=="TCD" | countrycode=="COM" | countrycode=="COG" | countrycode=="CIV" | countrycode=="COD" | countrycode=="DJI" | countrycode=="ERI" | countrycode=="ETH" | countrycode=="GNQ" | countrycode=="GAB" | countrycode=="GMB" | countrycode=="GHA" | countrycode=="GIN" | countrycode=="GNB" | countrycode=="LSO" | countrycode=="LBR" | countrycode=="KEN" | countrycode=="MDG" | countrycode=="MWI" | countrycode=="MLI" | countrycode=="MRT" | countrycode=="MUS" | countrycode=="MOZ" | countrycode=="NAM" | countrycode=="NER" | countrycode=="NGA" | countrycode=="RWA" | countrycode=="STP" | countrycode=="SEN" | countrycode=="SYC" | countrycode=="SLE" | countrycode=="SOM" | countrycode=="ZAF" | countrycode=="SDN" | countrycode=="SWZ" | countrycode=="TGO" | countrycode=="UGA" | countrycode=="TZA" | countrycode=="ZMB" | countrycode=="ZWE")

SOASIA.fbspop<-subset(fbspop,
                      countrycode=="AFG" | countrycode=="BGD" | countrycode=="BTN" | countrycode=="NPL" | countrycode=="PAK")
SOASIA.birthrate_1000<-subset(birthrate_1000,
                              countrycode=="AFG" | countrycode=="BGD" | countrycode=="BTN" | countrycode=="NPL" | countrycode=="PAK")



HIGHIN.birthrate_1000<-merge(HIGHIN.birthrate_1000,HIGHIN.fbspop, by="countrycode")
HIGHIN.totpop<-sum(HIGHIN.birthrate_1000$pop)
HIGHIN.birthrate_1000$fraction_totpop<-HIGHIN.birthrate_1000$pop/HIGHIN.totpop
HIGHIN.birthrate_1000$weightedbirthrate2011<-HIGHIN.birthrate_1000$extra_protein_Preg*HIGHIN.birthrate_1000$fraction_totpop
HIGHIN<-data.frame("countrycode"="HIGHIN","extra_protein_Preg"=sum(HIGHIN.birthrate_1000$weightedbirthrate2011,na.rm=TRUE))

SOTRLA.birthrate_1000<-merge(SOTRLA.birthrate_1000,SOTRLA.fbspop, by="countrycode")
SOTRLA.totpop<-sum(SOTRLA.birthrate_1000$pop)
SOTRLA.birthrate_1000$fraction_totpop<-SOTRLA.birthrate_1000$pop/SOTRLA.totpop
SOTRLA.birthrate_1000$weightedbirthrate2011<-SOTRLA.birthrate_1000$extra_protein_Preg*SOTRLA.birthrate_1000$fraction_totpop
SOTRLA<-data.frame("countrycode"="SOTRLA","extra_protein_Preg"=sum(SOTRLA.birthrate_1000$weightedbirthrate2011,na.rm=TRUE))

CEEAEU.birthrate_1000<-merge(CEEAEU.birthrate_1000,CEEAEU.fbspop, by="countrycode")
CEEAEU.totpop<-sum(CEEAEU.birthrate_1000$pop)
CEEAEU.birthrate_1000$fraction_totpop<-CEEAEU.birthrate_1000$pop/CEEAEU.totpop
CEEAEU.birthrate_1000$weightedbirthrate2011<-CEEAEU.birthrate_1000$extra_protein_Preg*CEEAEU.birthrate_1000$fraction_totpop
CEEAEU<-data.frame("countrycode"="CEEAEU","extra_protein_Preg"=sum(CEEAEU.birthrate_1000$weightedbirthrate2011,na.rm=TRUE))

CALACA.birthrate_1000<-merge(CALACA.birthrate_1000,CALACA.fbspop, by="countrycode")
CALACA.totpop<-sum(CALACA.birthrate_1000$pop)
CALACA.birthrate_1000$fraction_totpop<-CALACA.birthrate_1000$pop/CALACA.totpop
CALACA.birthrate_1000$weightedbirthrate2011<-CALACA.birthrate_1000$extra_protein_Preg*CALACA.birthrate_1000$fraction_totpop
CALACA<-data.frame("countrycode"="CALACA","extra_protein_Preg"=sum(CALACA.birthrate_1000$weightedbirthrate2011,na.rm=TRUE))

CANAME.birthrate_1000<-merge(CANAME.birthrate_1000,CANAME.fbspop, by="countrycode")
CANAME.totpop<-sum(CANAME.birthrate_1000$pop)
CANAME.birthrate_1000$fraction_totpop<-CANAME.birthrate_1000$pop/CANAME.totpop
CANAME.birthrate_1000$weightedbirthrate2011<-CANAME.birthrate_1000$extra_protein_Preg*CANAME.birthrate_1000$fraction_totpop
CANAME<-data.frame("countrycode"="CANAME","extra_protein_Preg"=sum(CANAME.birthrate_1000$weightedbirthrate2011,na.rm=TRUE))

ESEASP.birthrate_1000<-merge(ESEASP.birthrate_1000,ESEASP.fbspop, by="countrycode")
ESEASP.totpop<-sum(ESEASP.birthrate_1000$pop)
ESEASP.birthrate_1000$fraction_totpop<-ESEASP.birthrate_1000$pop/ESEASP.totpop
ESEASP.birthrate_1000$weightedbirthrate2011<-ESEASP.birthrate_1000$extra_protein_Preg*ESEASP.birthrate_1000$fraction_totpop
ESEASP<-data.frame("countrycode"="ESEASP","extra_protein_Preg"=sum(ESEASP.birthrate_1000$weightedbirthrate2011,na.rm=TRUE))

SUSAAF.birthrate_1000<-merge(SUSAAF.birthrate_1000,SUSAAF.fbspop, by="countrycode")
SUSAAF.totpop<-sum(SUSAAF.birthrate_1000$pop)
SUSAAF.birthrate_1000$fraction_totpop<-SUSAAF.birthrate_1000$pop/SUSAAF.totpop
SUSAAF.birthrate_1000$weightedbirthrate2011<-SUSAAF.birthrate_1000$extra_protein_Preg*SUSAAF.birthrate_1000$fraction_totpop
SUSAAF<-data.frame("countrycode"="SUSAAF","extra_protein_Preg"=sum(SUSAAF.birthrate_1000$weightedbirthrate2011,na.rm=TRUE))

SOASIA.birthrate_1000<-merge(SOASIA.birthrate_1000,SOASIA.fbspop, by="countrycode")
SOASIA.totpop<-sum(SOASIA.birthrate_1000$pop)
SOASIA.birthrate_1000$fraction_totpop<-SOASIA.birthrate_1000$pop/SOASIA.totpop
SOASIA.birthrate_1000$weightedbirthrate2011<-SOASIA.birthrate_1000$extra_protein_Preg*SOASIA.birthrate_1000$fraction_totpop
SOASIA<-data.frame("countrycode"="SOASIA","extra_protein_Preg"=sum(SOASIA.birthrate_1000$weightedbirthrate2011,na.rm=TRUE))



birthrate_1000groups<-data.frame(
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


birthrate_1000<-rbind(birthrate_1000,birthrate_1000groups)

#Lactating
#option 1: number of breastfeeding women = number of children who are being breastfed
#number of children breastfed = number of children aged 0 to median breast feeding duration * % ever breastfed

#median duration and percent ever breastfed
Breast_DHS<-read.csv("4_EAR/inputs/breastfeeding_pregnant/BF_DHS.csv")
order_DHS<-read.csv("0_labels/order_DHS.csv")
names(order_DHS)[3]<-"country"
names(Breast_DHS)[1]<-"country"
Breast_DHS<-merge(Breast_DHS,order_DHS, by="country")


HIGHIN.Breast<-subset(Breast_DHS,
                              countrycode=="AND" | countrycode=="AUS" | countrycode=="AUT" | countrycode=="BEL" | countrycode=="BRN" | countrycode=="CAN" | countrycode=="CYP" | countrycode=="DNK" | countrycode=="FIN" | countrycode=="FRA" | countrycode=="DEU" | countrycode=="GRC" | countrycode=="GRL" | countrycode=="ISL" | countrycode=="IRL" | countrycode=="ISR" | countrycode=="ITA" | countrycode=="JPN" | countrycode=="LUX" | countrycode=="MLT" | countrycode=="NLD" | countrycode=="NZL" | countrycode=="NOR" | countrycode=="PRT" | countrycode=="KOR" | countrycode=="SGP" | countrycode=="ESP" | countrycode=="SWE" | countrycode=="CHE" | countrycode=="GBR" | countrycode=="USA")
SOTRLA.Breast<-subset(Breast_DHS,
                              countrycode=="ARG" | countrycode=="BRA" | countrycode=="CHL" | countrycode=="PRY" | countrycode=="URY")
CEEAEU.Breast<-subset(Breast_DHS,
                              countrycode=="ALB" | countrycode=="BLR" | countrycode=="BIH" | countrycode=="BGR" | countrycode=="HRV" | countrycode=="CZE" | countrycode=="EST" | countrycode=="HUN" | countrycode=="LVA" | countrycode=="LTU" | countrycode=="MKD" | countrycode=="MDA" | countrycode=="MNE" | countrycode=="POL" | countrycode=="ROU" | countrycode=="RUS" | countrycode=="SRB" | countrycode=="SVK" | countrycode=="SVN" | countrycode=="UKR")
CALACA.Breast<-subset(Breast_DHS,
                              countrycode=="ATG" | countrycode=="BHS" | countrycode=="BRB" | countrycode=="BLZ" | countrycode=="BMU" | countrycode=="BOL" | countrycode=="VGB" | countrycode=="COL" | countrycode=="CRI" | countrycode=="CUB" | countrycode=="DMA" | countrycode=="DOM" | countrycode=="ECU" | countrycode=="SLV" | countrycode=="GRD" | countrycode=="GTM" | countrycode=="GUY" | countrycode=="HTI" | countrycode=="HND" | countrycode=="JAM" | countrycode=="MEX" | countrycode=="NIC" | countrycode=="ANT" | countrycode=="PAN" | countrycode=="PER" | countrycode=="PRI" | countrycode=="KNA" | countrycode=="LCA" | countrycode=="VCT" | countrycode=="SUR" | countrycode=="TTO" | countrycode=="VEN")
CANAME.Breast<-subset(Breast_DHS,
                              countrycode=="DZA" | countrycode=="ARM" | countrycode=="AZE" | countrycode=="BHR" | countrycode=="EGY" | countrycode=="GEO" | countrycode=="IRN" | countrycode=="IRQ" | countrycode=="JOR" | countrycode=="KAZ" | countrycode=="KWT" | countrycode=="KGZ" | countrycode=="LBN" | countrycode=="LBY" | countrycode=="MNG" | countrycode=="MAR" | countrycode=="PSE" | countrycode=="OMN" | countrycode=="QAT" | countrycode=="SAU" | countrycode=="SYR" | countrycode=="TJK" | countrycode=="TUN" | countrycode=="TUR" | countrycode=="TKM" | countrycode=="ARE" | countrycode=="UZB" | countrycode=="YEM")
ESEASP.Breast<-subset(Breast_DHS,
                              countrycode=="KHM" | countrycode=="COK" | countrycode=="PRK" | countrycode=="FJI" | countrycode=="PYF" | countrycode=="IDN" | countrycode=="KIR" | countrycode=="LAO" | countrycode=="MYS" | countrycode=="MDV" | countrycode=="MHL" | countrycode=="FSM" | countrycode=="MMR" | countrycode=="NRU" | countrycode=="PLW" | countrycode=="PNG" | countrycode=="PHL" | countrycode=="WSM" | countrycode=="SLB" | countrycode=="LKA" | countrycode=="THA" | countrycode=="TLS" | countrycode=="TON" | countrycode=="VNM" | countrycode=="VUT")
SUSAAF.Breast<-subset(Breast_DHS,
                              countrycode=="AGO" | countrycode=="BWA" | countrycode=="BFA" | countrycode=="BUR" | countrycode=="CMR" | countrycode=="CPV" | countrycode=="CAF" | countrycode=="TCD" | countrycode=="COM" | countrycode=="COG" | countrycode=="CIV" | countrycode=="COD" | countrycode=="DJI" | countrycode=="ERI" | countrycode=="ETH" | countrycode=="GNQ" | countrycode=="GAB" | countrycode=="GMB" | countrycode=="GHA" | countrycode=="GIN" | countrycode=="GNB" | countrycode=="LSO" | countrycode=="LBR" | countrycode=="KEN" | countrycode=="MDG" | countrycode=="MWI" | countrycode=="MLI" | countrycode=="MRT" | countrycode=="MUS" | countrycode=="MOZ" | countrycode=="NAM" | countrycode=="NER" | countrycode=="NGA" | countrycode=="RWA" | countrycode=="STP" | countrycode=="SEN" | countrycode=="SYC" | countrycode=="SLE" | countrycode=="SOM" | countrycode=="ZAF" | countrycode=="SDN" | countrycode=="SWZ" | countrycode=="TGO" | countrycode=="UGA" | countrycode=="TZA" | countrycode=="ZMB" | countrycode=="ZWE")
SOASIA.Breast<-subset(Breast_DHS,
                              countrycode=="AFG" | countrycode=="BGD" | countrycode=="BTN" | countrycode=="NPL" | countrycode=="PAK")

HIGHIN.Breast<-merge(HIGHIN.Breast,HIGHIN.fbspop, by="countrycode")
HIGHIN.Breast.ever<-subset(HIGHIN.Breast,Indicator=="Ever breastfed")

#number of children aged 0, 1 and 2
babies11<-read.csv("4_EAR/inputs/population/HNP_pop_aged_012_2011.csv")
totalpop11HNP<-read.csv("4_EAR/inputs/population/HNP_pop_total.csv")
names(babies11)<-c("country","demographic","population")
names(totalpop11HNP)<-c("country","demographic","population")

babies11$population1000<-babies11$population/1000
totalpop11HNP$population1000tot<-totalpop11HNP$population/1000

babies11_boys<-subset(babies11, demographic=="Age population, age 0, male, interpolated"|demographic=="Age population, age 01, male, interpolated"|demographic=="Age population, age 02, male, interpolated")
babies11_girls<-subset(babies11, demographic=="Age population, age 0, female, interpolated"|demographic=="Age population, age 01, female, interpolated"|demographic=="Age population, age 02, female, interpolated")
babies11_total<-babies11_boys[c(1,2,4)]
babies11_total[3]<-  babies11_boys[4]+babies11_girls[4]

#Note that the demographics levels in the totals are not actually male, i just can't be bothered changing them
babies0<-subset(babies11_total, demographic=="Age population, age 0, male, interpolated")
names(babies0)[3]<-"population1000_0"

babies0<-merge(babies0, totalpop11HNP, by="country")
names(babies0)
babies1<-subset(babies11_total, demographic=="Age population, age 01, male, interpolated")
names(babies1)[3]<-"population1000_1"
babies2<-subset(babies11_total, demographic=="Age population, age 02, male, interpolated")
names(babies2)[3]<-"population1000_2"

babies_pop<-cbind(babies0[c(1,6,3)],babies1[3],babies2[3])

order_HNP<-read.csv("0_labels/order_HNP.csv")[c(1,2)]
names(order_HNP)[1]<-"country"
babies_pop<-merge(babies_pop,order_HNP, by="country")

View(babies_pop)

median_breastfeeding_duration_yrs<-subset(Breast_DHS, Breast_DHS$Indicator=="median years breastfeeding duration")

percent_ever_breastfed<-subset(Breast_DHS, Indicator=="Ever breastfed")

breastfed<-merge(median_breastfeeding_duration_yrs[c(4,5)],percent_ever_breastfed[c(4,5)], by="countrycode", all=TRUE)
names(breastfed)[2]<-"median_years_breastfed"
names(breastfed)[3]<-"percent_ever_breastfed"


babies_breastfed<-merge(babies_pop, breastfed, by="countrycode", all=TRUE)

#if you don't have a percent, assume it's the mean
babies_breastfed$percent_ever_breastfed[is.na(babies_breastfed$percent_ever_breastfed)] <- mean(babies_breastfed$percent_ever_breastfed, na.rm=TRUE)
babies_breastfed$median_years_breastfed[is.na(babies_breastfed$median_years_breastfed)] <- median(babies_breastfed$median_years_breastfed, na.rm=TRUE)

for(rownum in 1:length(babies_breastfed$countrycode))
{if(babies_breastfed$median_years_breastfed[rownum] ==0)
  babies_breastfed$breastfed_babies[rownum] <-0
 if(babies_breastfed$median_years_breastfed[rownum] ==1)
   babies_breastfed$breastfed_babies[rownum] <-babies_breastfed$population1000_0[rownum]*babies_breastfed$percent_ever_breastfed[rownum]/100
 if(babies_breastfed$median_years_breastfed[rownum] ==2)
   babies_breastfed$breastfed_babies[rownum] <-(babies_breastfed$population1000_0[rownum]+babies_breastfed$population1000_1[rownum])*babies_breastfed$percent_ever_breastfed[rownum]/100
 if(babies_breastfed$median_years_breastfed[rownum] ==3)
   babies_breastfed$breastfed_babies[rownum] <-(babies_breastfed$population1000_0[rownum]+babies_breastfed$population1000_1[rownum]+babies_breastfed$population1000_2[rownum])*babies_breastfed$percent_ever_breastfed[rownum]/100
}
View(babies_breastfed)
babies_breastfed$ExtraEARLact<-babies_breastfed$breastfed_babies/babies_breastfed$population1000tot*EAR_add_lact
names(babies_breastfed)
BFP<-data.frame(merge(babies_breastfed[c(1,2,10)], birthrate_1000[c(5,6)],by="countrycode",all=TRUE))
BFP$BFP<-BFP$ExtraEARLact+BFP$extra_protein_Preg
BFP[57,1]<-NA #removing mainland China because we have China in its entirety and the program might get confused
BFP[315,1]<-NA #removing one of the St Vincent and the Grenadines

#issues: don't have data on quite a few countries. Should we estimate it from birth rates?


#######EAR####

#option 1
EAR_By_Age.boysL<-data.frame(0.8,0.72,0.75,0.68,0.66,0.66,0.66,0.66,0.66,0.66,0.66,0.66,0.66,0.66,0.66,0.66,0.66,0.66,0.66,0.66,0.66)

EAR_By_Age_perkg.boys<-data.frame("age_2"=0.8,"age_7"=0.72, "age_12"=0.75, "age_17_unless"=0.68, "adult"=0.66)
EAR_By_Age_perkg.girls<-data.frame("age_2"=0.8,"age_7"=0.72, "age_12"=0.73, "age_17_unless"=0.73, "adult"=0.66)

EAR_By_Age.boys<-boys.idealweight*EAR_By_Age_perkg.boys
EAR_By_Age.girls<-girls.idealweight*EAR_By_Age_perkg.girls
EAR_By_Age.girls$adult<-(EAR_By_Age_perkg.girls*girls.idealweight$adult)
#breast feeding and pregnant goes here

# total<-boys$adults+girls$adults #huh? why would I have done this?

names.boys.short<-names(boys)[c(26,4:7,27)]
boys.short<-boys[names.boys.short]
girls.short<-girls[names.boys.short]

EAR_weighted<-data.frame("countrycode"=boys$countrycode,"EAR_1"=0)
fractional.EAR.boys<-NULL
fractional.EAR.girls<-NULL

EAR_weighted<-merge(EAR_weighted, BFP, by="countrycode")


for(rownum in 1:length(totalpop$total_population))
{fractional.EAR.boys[rownum]<-sum(EAR_By_Age.boys*boys.short[rownum,2:6]/totalpop$total_population[rownum])
 fractional.EAR.girls[rownum]<-sum(EAR_By_Age.girls*girls.short[rownum,2:6]/totalpop$total_population[rownum])
 EAR_weighted$EAR_1[rownum]<-fractional.EAR.boys[rownum]+fractional.EAR.girls[rownum]+EAR_weighted$BFP[rownum]
}



#option 2
weight.continents<-read.csv("4_EAR/inputs/weight/weight_by_region.csv")[1:4]
length(weight.continents$weight_region)


merge.boys.kg.pop<-merge(boys.short,weight.continents, by="countrycode")
merge.boys.kg.pop<-merge(merge.boys.kg.pop,totalpop, by="countrycode")

names(merge.boys.kg.pop)
merge.boys.kg.pop$EAR_0_4<-rep(EAR_By_Age_perkg.boys[1]*boys.idealweight[1])
merge.boys.kg.pop$EAR_0_4wt<-as.numeric(merge.boys.kg.pop$EAR_0_4)*merge.boys.kg.pop$Pop_0_4/merge.boys.kg.pop$total_population

merge.boys.kg.pop$EAR_5_9<-rep(EAR_By_Age_perkg.boys[2]*boys.idealweight[2])
merge.boys.kg.pop$EAR_5_9wt<-as.numeric(merge.boys.kg.pop$EAR_5_9)*merge.boys.kg.pop$Pop_5_9/merge.boys.kg.pop$total_population

merge.boys.kg.pop$EAR_10_14<-rep(EAR_By_Age_perkg.boys[3]*boys.idealweight[3])
merge.boys.kg.pop$EAR_10_14wt<-as.numeric(merge.boys.kg.pop$EAR_10_14)*merge.boys.kg.pop$Pop_10_14/merge.boys.kg.pop$total_population

merge.boys.kg.pop$EAR_15_19<-rep(EAR_By_Age_perkg.boys[4]*boys.idealweight[4])
merge.boys.kg.pop$EAR_15_19wt<-as.numeric(merge.boys.kg.pop$EAR_15_19)*merge.boys.kg.pop$Pop_15_19/merge.boys.kg.pop$total_population

merge.boys.kg.pop$EAR_adults<-rep(EAR_By_Age_perkg.boys[5]*merge.boys.kg.pop$Weight)
merge.boys.kg.pop$EAR_adultswt<-as.numeric(merge.boys.kg.pop$EAR_adults)*merge.boys.kg.pop$adults/merge.boys.kg.pop$total_population

merge.boys.kg.pop$EARtot_wt<-merge.boys.kg.pop$EAR_adultswt+merge.boys.kg.pop$EAR_15_19wt+merge.boys.kg.pop$EAR_10_14wt+merge.boys.kg.pop$EAR_5_9wt+merge.boys.kg.pop$EAR_0_4wt

#girls

merge.girls.kg.pop<-merge(girls.short,weight.continents, by="countrycode")
merge.girls.kg.pop<-merge(merge.girls.kg.pop,totalpop, by="countrycode")

names(merge.girls.kg.pop)
merge.girls.kg.pop$EAR_0_4<-rep(EAR_By_Age_perkg.girls[1]*girls.idealweight[1])
merge.girls.kg.pop$EAR_0_4wt<-as.numeric(merge.girls.kg.pop$EAR_0_4)*merge.girls.kg.pop$Pop_0_4/merge.girls.kg.pop$total_population

merge.girls.kg.pop$EAR_5_9<-rep(EAR_By_Age_perkg.girls[2]*girls.idealweight[2])
merge.girls.kg.pop$EAR_5_9wt<-as.numeric(merge.girls.kg.pop$EAR_5_9)*merge.girls.kg.pop$Pop_5_9/merge.girls.kg.pop$total_population

merge.girls.kg.pop$EAR_10_14<-rep(EAR_By_Age_perkg.girls[3]*girls.idealweight[3])
merge.girls.kg.pop$EAR_10_14wt<-as.numeric(merge.girls.kg.pop$EAR_10_14)*merge.girls.kg.pop$Pop_10_14/merge.girls.kg.pop$total_population

merge.girls.kg.pop$EAR_15_19<-rep(EAR_By_Age_perkg.girls[4]*girls.idealweight[4])
merge.girls.kg.pop$EAR_15_19wt<-as.numeric(merge.girls.kg.pop$EAR_15_19)*merge.girls.kg.pop$Pop_15_19/merge.girls.kg.pop$total_population

merge.girls.kg.pop$EAR_adults<-rep((EAR_By_Age_perkg.girls[5]*merge.boys.kg.pop$Weight))
merge.girls.kg.pop$EAR_adultswt<-as.numeric(merge.girls.kg.pop$EAR_adults)*merge.girls.kg.pop$adults/merge.girls.kg.pop$total_population

merge.girls.kg.pop$EARtot_wt<-merge.girls.kg.pop$EAR_adultswt+merge.girls.kg.pop$EAR_15_19wt+merge.girls.kg.pop$EAR_10_14wt+merge.girls.kg.pop$EAR_5_9wt+merge.girls.kg.pop$EAR_0_4wt


EAR_2<-data.frame("countrycode"=merge.girls.kg.pop$countrycode, "EAR_2"=merge.girls.kg.pop$EARtot_wt+merge.boys.kg.pop$EARtot_wt)

EAR_weighted<-merge(EAR_weighted,EAR_2, by="countrycode",all=TRUE)


View(EAR_weighted)
EAR_weighted$EAR_2_BFP<-EAR_weighted$EAR_2+EAR_weighted$BFP

save(EAR_weighted, file="4_EAR/outputs/EAR_weighted.Rdata")




#option 3: use female weight, and assume men weigh the same
# DHSfemale<-read.csv("4_EAR/inputs/weight/STATcompiler - August 27 2014 12 09 - Raw Data - Multiple Indicators- mostrecent.csv")
# femaleBMI<-subset(DHSfemale,Indicator=="BMI mean")
# femaleheight<-subset(DHSfemale,Indicator=="Height mean")
# femaleweight<-femaleBMI$Value*((femaleheight$Value/100)^2)
# femaleweight<-data.frame("country"=as.factor(femaleBMI$Country),"weight"=femaleweight)
# save(femaleweight,file="4_EAR/femaleweight.Rdata")
# 
# write.csv(femaleheight,"femaleheight.csv")
# write.csv(femaleweight,"femaleweight.csv")
# write.csv(femaleBMI, "femaleBMI.csv")
# 



