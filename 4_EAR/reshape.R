library(reshape)
###reshaping the data from long to wide
worldbankdata<-read.csv("wbd12.csv")
View(worldbankdata)

wbd <-data.frame(cast(worldbankdata,CountryCode~SeriesName,value="value",fun.aggregate=max))
names(wbd)

write.csv(wbd,"wbd_cast.csv")

BMI_WHO<-read.csv("average_BMI_sml.csv") #average BMI for adults > 20
BMI<-data.frame(cast(BMI_WHO,COUNTRY+SEX~YEAR,value="Numeric"))
names(wbd)

write.csv(BMI,"BMI_cast.csv")
