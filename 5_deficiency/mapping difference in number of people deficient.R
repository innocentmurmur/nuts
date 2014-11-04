###mapping####
#people.at.risk<-read.csv("~/Rdata/nutrients/people_made_deficient.csv")
# people.at.risk<-read.csv("people_made_deficient.csv")
# people.at.risk$difference.in.percent.of.population<-abs(people.at.risk$difference.in.percent.of.population)
# sum(people.at.risk$difference.in.number.of.people, na.rm=TRUE)
load("5_deficiency/DIS_Par_EAR.Rdata")
load("5_deficiency/DIS_Par_EAR50.Rdata")
load("5_deficiency/DIS_Par_EAR75.Rdata")
load("5_deficiency/Dis_ParVeg_EAR.Rdata")


library(rworldmap)
sPDF <- joinCountryData2Map(DIS_Par_EAR
                            , joinCode = "ISO3"
                            , nameJoinColumn = "countrycode"
)

op <- palette(c("darkgreen","yellow","orange","red"))
mapCountryData( sPDF
                , nameColumnToPlot="Def_amb_HHS_EAR2"
                , catMethod="quantiles"              
                , mapTitle="Fraction of population already deficient"
                , colourPalette="palette"
                , oceanCol="lightblue"
                , missingCountryCol="white"                 )


# View(sPDF)
names(DIS_Par_EAR)
# mapCountryData(sPDF,nameColumnToPlot="People_made_deficient_SOFI_lo2")
# mapCountryData(sPDF,nameColumnToPlot="People_made_deficient_SOFI_med2")
# mapCountryData(sPDF,nameColumnToPlot="People_made_deficient_SOFI_hi2")
mapCountryData(sPDF,nameColumnToPlot="People_made_deficient_SOFI2")
mapCountryData(sPDF,nameColumnToPlot="People_made_deficientCIu_SOFI2")
mapCountryData(sPDF,nameColumnToPlot="People_made_deficientCIl_SOFI2")
mapCountryData(sPDF,nameColumnToPlot="Def_amb_HHS_EAR2",mapTitle="Fraction of the population already deficient",catMethod=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))
mapCountryData(sPDF,nameColumnToPlot="People_already_deficient_HHS2",mapTitle="Number of people already deficient")


mapCountryData(sPDF,nameColumnToPlot="People_made_deficient_HHS2")
mapCountryData(sPDF,nameColumnToPlot="People_made_deficientCIu_HHS2")
mapCountryData(sPDF,nameColumnToPlot="People_made_deficientCIl_HHS2")

mapCountryData(sPDF,nameColumnToPlot="Difference_SOFI2")
mapCountryData(sPDF,nameColumnToPlot="DifferenceCIu_SOFI2"
               #,catMethod=c(0,0.02,0.04,0.06,0.08,0.1)
               )
mapCountryData(sPDF,nameColumnToPlot="DifferenceCIl_SOFI2"
#                ,catMethod=c(0,0.02,0.04,0.06,0.08,0.1)
)

mapCountryData(sPDF,nameColumnToPlot="Difference_HHS2"
#                ,catMethod=c(0,0.02,0.04,0.06,0.08,0.1)
)
mapCountryData(sPDF,nameColumnToPlot="DifferenceCIu_HHS2"
               #,catMethod=c(0,0.02,0.04,0.06,0.08,0.1)
)
mapCountryData(sPDF,nameColumnToPlot="DifferenceCIl_HHS2"
               #                ,catMethod=c(0,0.02,0.04,0.06,0.08,0.1)
)


# mapCountryData(sPDF,nameColumnToPlot="Difference_SOFI_med2",catMethod=c(0,0.02,0.04,0.06,0.08,0.1))
# mapCountryData(sPDF,nameColumnToPlot="Difference_SOFI_hi2",catMethod=c(0,0.02,0.04,0.06,0.08,0.1))

# 
# sPDF <- joinCountryData2Map(fert
#                             , joinCode = "ISO3"
#                             , nameJoinColumn = "countrycode"
# )
# View(sPDF)
# names(fert)
# mapCountryData(sPDF,nameColumnToPlot="Fertilizer_consumption_kilograms_per_hectare_of_arable_land")
# 
# sPDF <- joinCountryData2Map(DIS_Par_EAR_fert
#                             , joinCode = "ISO3"
#                             , nameJoinColumn = "countrycode"
# )
# View(sPDF)
# names(DIS_Par_EAR_fert)
# mapCountryData(sPDF,nameColumnToPlot="People_deficient_Nfert_SOFI")
# #put in the N or take it out as appropriate
# 
# #mapCountryData(sPDF,nameColumnToPlot="protein.percent.decrease", catMethod=c(-20,-16,-8,-4,0), colourPalette = "black2White")
# mapCountryData(sPDF,nameColumnToPlot="difference.in.percent.of.population",mapTitle="difference in percent of population deficient")
# #,  catMethod=c(-0.7,-0.6,-0.5,-0.4,-0.3,-0.2,-0.1,0), colourPalette = "black2White")
sPDF <- joinCountryData2Map(DIS_Par_EAR50
                            , joinCode = "ISO3"
                            , nameJoinColumn = "countrycode"
)
mapCountryData(sPDF,nameColumnToPlot="People_made_deficient_HHS2",mapTitle="People newly below 50% of EAR")
mapCountryData(sPDF,nameColumnToPlot="Difference_HHS2",mapTitle="Fraction of population newly falling below 50% of EAR")
mapCountryData(sPDF,nameColumnToPlot="Def_amb_HHS_EAR2",mapTitle="Fraction of the population already below 50% of EAR",catMethod=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))

sPDF <- joinCountryData2Map(DIS_Par_EAR75
                            , joinCode = "ISO3"
                            , nameJoinColumn = "countrycode"
)
mapCountryData(sPDF,nameColumnToPlot="People_made_deficient_HHS2",mapTitle="People newly below 75% of EAR")
mapCountryData(sPDF,nameColumnToPlot="Difference_HHS2",mapTitle="Fraction of population newly falling below 75% of EAR")
mapCountryData(sPDF,nameColumnToPlot="Def_amb_HHS_EAR2",mapTitle="Fraction of the population already below 75% of EAR",catMethod=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))

sPDF <- joinCountryData2Map(Dis_ParVeg_EAR
                            , joinCode = "ISO3"
                            , nameJoinColumn = "countrycode"
)
write.csv(FBStot,"2_countries/outputs/FBStot.csv")
write.csv(FBStotVeg,"2_countries/outputs/FBStotVeg.csv")
