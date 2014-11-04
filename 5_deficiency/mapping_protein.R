proteinchange<-read.csv("~/Rdata/nutrients/PROTEINCHANGEBYCOUNTRY.csv")
library(rworldmap)
View(proteinchange)
sPDF <- joinCountryData2Map(proteinchange
                            , joinCode = "ISO3"
                            , nameJoinColumn = "countrycode"
                            )
mapCountryData(sPDF,nameColumnToPlot="PERCENT_OF_PROTEIN")
#mapCountryData(sPDF,nameColumnToPlot="PERCENT_OF_PROTEIN", catMethod=c(-20,-16,-8,-4,0), colourPalette = "black2White")
mapCountryData(sPDF,nameColumnToPlot="PERCENT_OF_DIET")
               #,  catMethod=c(-0.7,-0.6,-0.5,-0.4,-0.3,-0.2,-0.1,0), colourPalette = "black2White")
