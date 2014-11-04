##BPm over time####
BP<-read.csv("~/Rdata/nutrients/crude avg systolic BP.csv")

View(BP)

BPm<-subset(BP,Sex=="Male")

#You have to make the bottom male and then female as two separate LMs#
###linear model predictions####
Albania.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Albania"))
Albania.predict.BPm<-data.frame(country="Albania",fraction=predict(Albania.lm.BPm, newdata=data.frame(Year=2050)))

Algeria.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Algeria"))
Algeria.predict.BPm<-data.frame(country="Algeria",fraction=predict(Algeria.lm.BPm, newdata=data.frame(Year=2050)))

Angola.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Angola"))
Angola.predict.BPm<-data.frame(country="Angola",fraction=predict(Angola.lm.BPm, newdata=data.frame(Year=2050)))



Antigua_and_Barbuda.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Antigua and Barbuda"))
Antigua_and_Barbuda.predict.BPm<-data.frame(country="Antigua and Barbuda",fraction=predict(Antigua_and_Barbuda.lm.BPm, newdata=data.frame(Year=2050)))



Argentina.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Argentina"))
Argentina.predict.BPm<-data.frame(country="Argentina",fraction=predict(Argentina.lm.BPm, newdata=data.frame(Year=2050)))



Armenia.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Armenia"))
Armenia.predict.BPm<-data.frame(country="Armenia",fraction=predict(Armenia.lm.BPm, newdata=data.frame(Year=2050)))



Australia.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Australia"))
Australia.predict.BPm<-data.frame(country="Australia",fraction=predict(Australia.lm.BPm, newdata=data.frame(Year=2050)))



Austria.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Austria"))
Austria.predict.BPm<-data.frame(country="Austria",fraction=predict(Austria.lm.BPm, newdata=data.frame(Year=2050)))



Azerbaijan.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Azerbaijan"))
Azerbaijan.predict.BPm<-data.frame(country="Azerbaijan",fraction=predict(Azerbaijan.lm.BPm, newdata=data.frame(Year=2050)))



Bahamas.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Bahamas"))
Bahamas.predict.BPm<-data.frame(country="Bahamas",fraction=predict(Bahamas.lm.BPm, newdata=data.frame(Year=2050)))



Bangladesh.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Bangladesh"))
Bangladesh.predict.BPm<-data.frame(country="Bangladesh",fraction=predict(Bangladesh.lm.BPm, newdata=data.frame(Year=2050)))



Barbados.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Barbados"))
Barbados.predict.BPm<-data.frame(country="Barbados",fraction=predict(Barbados.lm.BPm, newdata=data.frame(Year=2050)))



Belarus.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Belarus"))
Belarus.predict.BPm<-data.frame(country="Belarus",fraction=predict(Belarus.lm.BPm, newdata=data.frame(Year=2050)))



Belgium.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Belgium"))
Belgium.predict.BPm<-data.frame(country="Belgium",fraction=predict(Belgium.lm.BPm, newdata=data.frame(Year=2050)))



Belize.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Belize"))
Belize.predict.BPm<-data.frame(country="Belize",fraction=predict(Belize.lm.BPm, newdata=data.frame(Year=2050)))



Benin.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Benin"))
Benin.predict.BPm<-data.frame(country="Benin",fraction=predict(Benin.lm.BPm, newdata=data.frame(Year=2050)))



Bolivia_Plurinational_State_of.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Bolivia (Plurinational State of)"))
Bolivia_Plurinational_State_of.predict.BPm<-data.frame(country="Bolivia (Plurinational State of)",fraction=predict(Bolivia_Plurinational_State_of.lm.BPm, newdata=data.frame(Year=2050)))



Bosnia_and_Herzegovina.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Bosnia and Herzegovina"))
Bosnia_and_Herzegovina.predict.BPm<-data.frame(country="Bosnia and Herzegovina",fraction=predict(Bosnia_and_Herzegovina.lm.BPm, newdata=data.frame(Year=2050)))



Botswana.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Botswana"))
Botswana.predict.BPm<-data.frame(country="Botswana",fraction=predict(Botswana.lm.BPm, newdata=data.frame(Year=2050)))



Brazil.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Brazil"))
Brazil.predict.BPm<-data.frame(country="Brazil",fraction=predict(Brazil.lm.BPm, newdata=data.frame(Year=2050)))



Brunei_Darussalam.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Brunei Darussalam"))
Brunei_Darussalam.predict.BPm<-data.frame(country="Brunei Darussalam",fraction=predict(Brunei_Darussalam.lm.BPm, newdata=data.frame(Year=2050)))



Bulgaria.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Bulgaria"))
Bulgaria.predict.BPm<-data.frame(country="Bulgaria",fraction=predict(Bulgaria.lm.BPm, newdata=data.frame(Year=2050)))



Burkina_Faso.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Burkina Faso"))
Burkina_Faso.predict.BPm<-data.frame(country="Burkina Faso",fraction=predict(Burkina_Faso.lm.BPm, newdata=data.frame(Year=2050)))



Burundi.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Burundi"))
Burundi.predict.BPm<-data.frame(country="Burundi",fraction=predict(Burundi.lm.BPm, newdata=data.frame(Year=2050)))



Cote_dIvoire.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="C?te d'Ivoire"))
Cote_dIvoire.predict.BPm<-data.frame(country="C?te d'Ivoire",fraction=predict(Cote_dIvoire.lm.BPm, newdata=data.frame(Year=2050)))



Cabo_Verde.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Cabo Verde"))
Cabo_Verde.predict.BPm<-data.frame(country="Cabo Verde",fraction=predict(Cabo_Verde.lm.BPm, newdata=data.frame(Year=2050)))



Cambodia.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Cambodia"))
Cambodia.predict.BPm<-data.frame(country="Cambodia",fraction=predict(Cambodia.lm.BPm, newdata=data.frame(Year=2050)))



Cameroon.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Cameroon"))
Cameroon.predict.BPm<-data.frame(country="Cameroon",fraction=predict(Cameroon.lm.BPm, newdata=data.frame(Year=2050)))



Canada.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Canada"))
Canada.predict.BPm<-data.frame(country="Canada",fraction=predict(Canada.lm.BPm, newdata=data.frame(Year=2050)))



Central_African_Republic.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Central African Republic"))
Central_African_Republic.predict.BPm<-data.frame(country="Central African Republic",fraction=predict(Central_African_Republic.lm.BPm, newdata=data.frame(Year=2050)))



Chad.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Chad"))
Chad.predict.BPm<-data.frame(country="Chad",fraction=predict(Chad.lm.BPm, newdata=data.frame(Year=2050)))



Chile.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Chile"))
Chile.predict.BPm<-data.frame(country="Chile",fraction=predict(Chile.lm.BPm, newdata=data.frame(Year=2050)))



China.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="China"))
China.predict.BPm<-data.frame(country="China",fraction=predict(China.lm.BPm, newdata=data.frame(Year=2050)))



Colombia.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Colombia"))
Colombia.predict.BPm<-data.frame(country="Colombia",fraction=predict(Colombia.lm.BPm, newdata=data.frame(Year=2050)))



Comoros.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Comoros"))
Comoros.predict.BPm<-data.frame(country="Comoros",fraction=predict(Comoros.lm.BPm, newdata=data.frame(Year=2050)))



Congo.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Congo"))
Congo.predict.BPm<-data.frame(country="Congo",fraction=predict(Congo.lm.BPm, newdata=data.frame(Year=2050)))



Costa_Rica.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Costa Rica"))
Costa_Rica.predict.BPm<-data.frame(country="Costa Rica",fraction=predict(Costa_Rica.lm.BPm, newdata=data.frame(Year=2050)))



Croatia.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Croatia"))
Croatia.predict.BPm<-data.frame(country="Croatia",fraction=predict(Croatia.lm.BPm, newdata=data.frame(Year=2050)))



Cuba.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Cuba"))
Cuba.predict.BPm<-data.frame(country="Cuba",fraction=predict(Cuba.lm.BPm, newdata=data.frame(Year=2050)))



Cyprus.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Cyprus"))
Cyprus.predict.BPm<-data.frame(country="Cyprus",fraction=predict(Cyprus.lm.BPm, newdata=data.frame(Year=2050)))



Czech_Republic.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Czech Republic"))
Czech_Republic.predict.BPm<-data.frame(country="Czech Republic",fraction=predict(Czech_Republic.lm.BPm, newdata=data.frame(Year=2050)))



Democratic_Peoples_Republic_of_Korea.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Democratic People's Republic of Korea"))
Democratic_Peoples_Republic_of_Korea.predict.BPm<-data.frame(country="Democratic People's Republic of Korea",fraction=predict(Democratic_Peoples_Republic_of_Korea.lm.BPm, newdata=data.frame(Year=2050)))



Democratic_Republic_of_the_Congo.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Democratic Republic of the Congo"))
Democratic_Republic_of_the_Congo.predict.BPm<-data.frame(country="Democratic Republic of the Congo",fraction=predict(Democratic_Republic_of_the_Congo.lm.BPm, newdata=data.frame(Year=2050)))



Denmark.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Denmark"))
Denmark.predict.BPm<-data.frame(country="Denmark",fraction=predict(Denmark.lm.BPm, newdata=data.frame(Year=2050)))



Djibouti.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Djibouti"))
Djibouti.predict.BPm<-data.frame(country="Djibouti",fraction=predict(Djibouti.lm.BPm, newdata=data.frame(Year=2050)))



Dominica.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Dominica"))
Dominica.predict.BPm<-data.frame(country="Dominica",fraction=predict(Dominica.lm.BPm, newdata=data.frame(Year=2050)))



Dominican_Republic.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Dominican Republic"))
Dominican_Republic.predict.BPm<-data.frame(country="Dominican Republic",fraction=predict(Dominican_Republic.lm.BPm, newdata=data.frame(Year=2050)))



Ecuador.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Ecuador"))
Ecuador.predict.BPm<-data.frame(country="Ecuador",fraction=predict(Ecuador.lm.BPm, newdata=data.frame(Year=2050)))



Egypt.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Egypt"))
Egypt.predict.BPm<-data.frame(country="Egypt",fraction=predict(Egypt.lm.BPm, newdata=data.frame(Year=2050)))



El_Salvador.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="El Salvador"))
El_Salvador.predict.BPm<-data.frame(country="El Salvador",fraction=predict(El_Salvador.lm.BPm, newdata=data.frame(Year=2050)))



Eritrea.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Eritrea"))
Eritrea.predict.BPm<-data.frame(country="Eritrea",fraction=predict(Eritrea.lm.BPm, newdata=data.frame(Year=2050)))



Estonia.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Estonia"))
Estonia.predict.BPm<-data.frame(country="Estonia",fraction=predict(Estonia.lm.BPm, newdata=data.frame(Year=2050)))



Ethiopia.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Ethiopia"))
Ethiopia.predict.BPm<-data.frame(country="Ethiopia",fraction=predict(Ethiopia.lm.BPm, newdata=data.frame(Year=2050)))



Fiji.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Fiji"))
Fiji.predict.BPm<-data.frame(country="Fiji",fraction=predict(Fiji.lm.BPm, newdata=data.frame(Year=2050)))



Finland.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Finland"))
Finland.predict.BPm<-data.frame(country="Finland",fraction=predict(Finland.lm.BPm, newdata=data.frame(Year=2050)))



France.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="France"))
France.predict.BPm<-data.frame(country="France",fraction=predict(France.lm.BPm, newdata=data.frame(Year=2050)))



Gabon.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Gabon"))
Gabon.predict.BPm<-data.frame(country="Gabon",fraction=predict(Gabon.lm.BPm, newdata=data.frame(Year=2050)))



Gambia.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Gambia"))
Gambia.predict.BPm<-data.frame(country="Gambia",fraction=predict(Gambia.lm.BPm, newdata=data.frame(Year=2050)))



Georgia.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Georgia"))
Georgia.predict.BPm<-data.frame(country="Georgia",fraction=predict(Georgia.lm.BPm, newdata=data.frame(Year=2050)))



Germany.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Germany"))
Germany.predict.BPm<-data.frame(country="Germany",fraction=predict(Germany.lm.BPm, newdata=data.frame(Year=2050)))



Ghana.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Ghana"))
Ghana.predict.BPm<-data.frame(country="Ghana",fraction=predict(Ghana.lm.BPm, newdata=data.frame(Year=2050)))



Greece.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Greece"))
Greece.predict.BPm<-data.frame(country="Greece",fraction=predict(Greece.lm.BPm, newdata=data.frame(Year=2050)))



Grenada.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Grenada"))
Grenada.predict.BPm<-data.frame(country="Grenada",fraction=predict(Grenada.lm.BPm, newdata=data.frame(Year=2050)))



Guatemala.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Guatemala"))
Guatemala.predict.BPm<-data.frame(country="Guatemala",fraction=predict(Guatemala.lm.BPm, newdata=data.frame(Year=2050)))



Guinea.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Guinea"))
Guinea.predict.BPm<-data.frame(country="Guinea",fraction=predict(Guinea.lm.BPm, newdata=data.frame(Year=2050)))



GuineaBissau.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Guinea-Bissau"))
GuineaBissau.predict.BPm<-data.frame(country="Guinea-Bissau",fraction=predict(GuineaBissau.lm.BPm, newdata=data.frame(Year=2050)))



Guyana.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Guyana"))
Guyana.predict.BPm<-data.frame(country="Guyana",fraction=predict(Guyana.lm.BPm, newdata=data.frame(Year=2050)))



Haiti.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Haiti"))
Haiti.predict.BPm<-data.frame(country="Haiti",fraction=predict(Haiti.lm.BPm, newdata=data.frame(Year=2050)))



Honduras.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Honduras"))
Honduras.predict.BPm<-data.frame(country="Honduras",fraction=predict(Honduras.lm.BPm, newdata=data.frame(Year=2050)))



Hungary.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Hungary"))
Hungary.predict.BPm<-data.frame(country="Hungary",fraction=predict(Hungary.lm.BPm, newdata=data.frame(Year=2050)))



Iceland.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Iceland"))
Iceland.predict.BPm<-data.frame(country="Iceland",fraction=predict(Iceland.lm.BPm, newdata=data.frame(Year=2050)))



India.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="India"))
India.predict.BPm<-data.frame(country="India",fraction=predict(India.lm.BPm, newdata=data.frame(Year=2050)))



Indonesia.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Indonesia"))
Indonesia.predict.BPm<-data.frame(country="Indonesia",fraction=predict(Indonesia.lm.BPm, newdata=data.frame(Year=2050)))



Iran_Islamic_Republic_of.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Iran (Islamic Republic of)"))
Iran_Islamic_Republic_of.predict.BPm<-data.frame(country="Iran (Islamic Republic of)",fraction=predict(Iran_Islamic_Republic_of.lm.BPm, newdata=data.frame(Year=2050)))



Ireland.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Ireland"))
Ireland.predict.BPm<-data.frame(country="Ireland",fraction=predict(Ireland.lm.BPm, newdata=data.frame(Year=2050)))



Israel.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Israel"))
Israel.predict.BPm<-data.frame(country="Israel",fraction=predict(Israel.lm.BPm, newdata=data.frame(Year=2050)))



Italy.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Italy"))
Italy.predict.BPm<-data.frame(country="Italy",fraction=predict(Italy.lm.BPm, newdata=data.frame(Year=2050)))



Jamaica.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Jamaica"))
Jamaica.predict.BPm<-data.frame(country="Jamaica",fraction=predict(Jamaica.lm.BPm, newdata=data.frame(Year=2050)))



Japan.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Japan"))
Japan.predict.BPm<-data.frame(country="Japan",fraction=predict(Japan.lm.BPm, newdata=data.frame(Year=2050)))



Jordan.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Jordan"))
Jordan.predict.BPm<-data.frame(country="Jordan",fraction=predict(Jordan.lm.BPm, newdata=data.frame(Year=2050)))



Kazakhstan.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Kazakhstan"))
Kazakhstan.predict.BPm<-data.frame(country="Kazakhstan",fraction=predict(Kazakhstan.lm.BPm, newdata=data.frame(Year=2050)))



Kenya.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Kenya"))
Kenya.predict.BPm<-data.frame(country="Kenya",fraction=predict(Kenya.lm.BPm, newdata=data.frame(Year=2050)))



Kiribati.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Kiribati"))
Kiribati.predict.BPm<-data.frame(country="Kiribati",fraction=predict(Kiribati.lm.BPm, newdata=data.frame(Year=2050)))



Kuwait.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Kuwait"))
Kuwait.predict.BPm<-data.frame(country="Kuwait",fraction=predict(Kuwait.lm.BPm, newdata=data.frame(Year=2050)))



Kyrgyzstan.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Kyrgyzstan"))
Kyrgyzstan.predict.BPm<-data.frame(country="Kyrgyzstan",fraction=predict(Kyrgyzstan.lm.BPm, newdata=data.frame(Year=2050)))



Lao_Peoples_Democratic_Republic.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Lao People's Democratic Republic"))
Lao_Peoples_Democratic_Republic.predict.BPm<-data.frame(country="Lao People's Democratic Republic",fraction=predict(Lao_Peoples_Democratic_Republic.lm.BPm, newdata=data.frame(Year=2050)))



Latvia.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Latvia"))
Latvia.predict.BPm<-data.frame(country="Latvia",fraction=predict(Latvia.lm.BPm, newdata=data.frame(Year=2050)))



Lebanon.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Lebanon"))
Lebanon.predict.BPm<-data.frame(country="Lebanon",fraction=predict(Lebanon.lm.BPm, newdata=data.frame(Year=2050)))



Lesotho.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Lesotho"))
Lesotho.predict.BPm<-data.frame(country="Lesotho",fraction=predict(Lesotho.lm.BPm, newdata=data.frame(Year=2050)))



Liberia.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Liberia"))
Liberia.predict.BPm<-data.frame(country="Liberia",fraction=predict(Liberia.lm.BPm, newdata=data.frame(Year=2050)))



Libya.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Libya"))
Libya.predict.BPm<-data.frame(country="Libya",fraction=predict(Libya.lm.BPm, newdata=data.frame(Year=2050)))



Lithuania.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Lithuania"))
Lithuania.predict.BPm<-data.frame(country="Lithuania",fraction=predict(Lithuania.lm.BPm, newdata=data.frame(Year=2050)))



Luxembourg.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Luxembourg"))
Luxembourg.predict.BPm<-data.frame(country="Luxembourg",fraction=predict(Luxembourg.lm.BPm, newdata=data.frame(Year=2050)))



Madagascar.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Madagascar"))
Madagascar.predict.BPm<-data.frame(country="Madagascar",fraction=predict(Madagascar.lm.BPm, newdata=data.frame(Year=2050)))



Malawi.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Malawi"))
Malawi.predict.BPm<-data.frame(country="Malawi",fraction=predict(Malawi.lm.BPm, newdata=data.frame(Year=2050)))



Malaysia.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Malaysia"))
Malaysia.predict.BPm<-data.frame(country="Malaysia",fraction=predict(Malaysia.lm.BPm, newdata=data.frame(Year=2050)))



Maldives.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Maldives"))
Maldives.predict.BPm<-data.frame(country="Maldives",fraction=predict(Maldives.lm.BPm, newdata=data.frame(Year=2050)))



Mali.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Mali"))
Mali.predict.BPm<-data.frame(country="Mali",fraction=predict(Mali.lm.BPm, newdata=data.frame(Year=2050)))



Malta.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Malta"))
Malta.predict.BPm<-data.frame(country="Malta",fraction=predict(Malta.lm.BPm, newdata=data.frame(Year=2050)))



Mauritania.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Mauritania"))
Mauritania.predict.BPm<-data.frame(country="Mauritania",fraction=predict(Mauritania.lm.BPm, newdata=data.frame(Year=2050)))



Mauritius.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Mauritius"))
Mauritius.predict.BPm<-data.frame(country="Mauritius",fraction=predict(Mauritius.lm.BPm, newdata=data.frame(Year=2050)))



Mexico.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Mexico"))
Mexico.predict.BPm<-data.frame(country="Mexico",fraction=predict(Mexico.lm.BPm, newdata=data.frame(Year=2050)))



Mongolia.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Mongolia"))
Mongolia.predict.BPm<-data.frame(country="Mongolia",fraction=predict(Mongolia.lm.BPm, newdata=data.frame(Year=2050)))



Montenegro.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Montenegro"))
Montenegro.predict.BPm<-data.frame(country="Montenegro",fraction=predict(Montenegro.lm.BPm, newdata=data.frame(Year=2050)))



Morocco.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Morocco"))
Morocco.predict.BPm<-data.frame(country="Morocco",fraction=predict(Morocco.lm.BPm, newdata=data.frame(Year=2050)))



Mozambique.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Mozambique"))
Mozambique.predict.BPm<-data.frame(country="Mozambique",fraction=predict(Mozambique.lm.BPm, newdata=data.frame(Year=2050)))



Myanmar.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Myanmar"))
Myanmar.predict.BPm<-data.frame(country="Myanmar",fraction=predict(Myanmar.lm.BPm, newdata=data.frame(Year=2050)))



Namibia.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Namibia"))
Namibia.predict.BPm<-data.frame(country="Namibia",fraction=predict(Namibia.lm.BPm, newdata=data.frame(Year=2050)))



Nepal.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Nepal"))
Nepal.predict.BPm<-data.frame(country="Nepal",fraction=predict(Nepal.lm.BPm, newdata=data.frame(Year=2050)))



Netherlands.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Netherlands"))
Netherlands.predict.BPm<-data.frame(country="Netherlands",fraction=predict(Netherlands.lm.BPm, newdata=data.frame(Year=2050)))



New_Zealand.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="New Zealand"))
New_Zealand.predict.BPm<-data.frame(country="New Zealand",fraction=predict(New_Zealand.lm.BPm, newdata=data.frame(Year=2050)))



Nicaragua.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Nicaragua"))
Nicaragua.predict.BPm<-data.frame(country="Nicaragua",fraction=predict(Nicaragua.lm.BPm, newdata=data.frame(Year=2050)))



Niger.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Niger"))
Niger.predict.BPm<-data.frame(country="Niger",fraction=predict(Niger.lm.BPm, newdata=data.frame(Year=2050)))



Nigeria.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Nigeria"))
Nigeria.predict.BPm<-data.frame(country="Nigeria",fraction=predict(Nigeria.lm.BPm, newdata=data.frame(Year=2050)))



Norway.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Norway"))
Norway.predict.BPm<-data.frame(country="Norway",fraction=predict(Norway.lm.BPm, newdata=data.frame(Year=2050)))



Pakistan.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Pakistan"))
Pakistan.predict.BPm<-data.frame(country="Pakistan",fraction=predict(Pakistan.lm.BPm, newdata=data.frame(Year=2050)))



Panama.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Panama"))
Panama.predict.BPm<-data.frame(country="Panama",fraction=predict(Panama.lm.BPm, newdata=data.frame(Year=2050)))



Paraguay.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Paraguay"))
Paraguay.predict.BPm<-data.frame(country="Paraguay",fraction=predict(Paraguay.lm.BPm, newdata=data.frame(Year=2050)))



Peru.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Peru"))
Peru.predict.BPm<-data.frame(country="Peru",fraction=predict(Peru.lm.BPm, newdata=data.frame(Year=2050)))



Philippines.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Philippines"))
Philippines.predict.BPm<-data.frame(country="Philippines",fraction=predict(Philippines.lm.BPm, newdata=data.frame(Year=2050)))



Poland.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Poland"))
Poland.predict.BPm<-data.frame(country="Poland",fraction=predict(Poland.lm.BPm, newdata=data.frame(Year=2050)))



Portugal.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Portugal"))
Portugal.predict.BPm<-data.frame(country="Portugal",fraction=predict(Portugal.lm.BPm, newdata=data.frame(Year=2050)))



Republic_of_Korea.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Republic of Korea"))
Republic_of_Korea.predict.BPm<-data.frame(country="Republic of Korea",fraction=predict(Republic_of_Korea.lm.BPm, newdata=data.frame(Year=2050)))



Republic_of_Moldova.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Republic of Moldova"))
Republic_of_Moldova.predict.BPm<-data.frame(country="Republic of Moldova",fraction=predict(Republic_of_Moldova.lm.BPm, newdata=data.frame(Year=2050)))



Romania.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Romania"))
Romania.predict.BPm<-data.frame(country="Romania",fraction=predict(Romania.lm.BPm, newdata=data.frame(Year=2050)))



Russian_Federation.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Russian Federation"))
Russian_Federation.predict.BPm<-data.frame(country="Russian Federation",fraction=predict(Russian_Federation.lm.BPm, newdata=data.frame(Year=2050)))



Rwanda.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Rwanda"))
Rwanda.predict.BPm<-data.frame(country="Rwanda",fraction=predict(Rwanda.lm.BPm, newdata=data.frame(Year=2050)))



Saint_Kitts_and_Nevis.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Saint Kitts and Nevis"))
Saint_Kitts_and_Nevis.predict.BPm<-data.frame(country="Saint Kitts and Nevis",fraction=predict(Saint_Kitts_and_Nevis.lm.BPm, newdata=data.frame(Year=2050)))



Saint_Lucia.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Saint Lucia"))
Saint_Lucia.predict.BPm<-data.frame(country="Saint Lucia",fraction=predict(Saint_Lucia.lm.BPm, newdata=data.frame(Year=2050)))



Saint_Vincent_and_the_Grenadines.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Saint Vincent and the Grenadines"))
Saint_Vincent_and_the_Grenadines.predict.BPm<-data.frame(country="Saint Vincent and the Grenadines",fraction=predict(Saint_Vincent_and_the_Grenadines.lm.BPm, newdata=data.frame(Year=2050)))



Samoa.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Samoa"))
Samoa.predict.BPm<-data.frame(country="Samoa",fraction=predict(Samoa.lm.BPm, newdata=data.frame(Year=2050)))



Sao_Tome_and_Principe.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Sao Tome and Principe"))
Sao_Tome_and_Principe.predict.BPm<-data.frame(country="Sao Tome and Principe",fraction=predict(Sao_Tome_and_Principe.lm.BPm, newdata=data.frame(Year=2050)))



Saudi_Arabia.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Saudi Arabia"))
Saudi_Arabia.predict.BPm<-data.frame(country="Saudi Arabia",fraction=predict(Saudi_Arabia.lm.BPm, newdata=data.frame(Year=2050)))



Senegal.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Senegal"))
Senegal.predict.BPm<-data.frame(country="Senegal",fraction=predict(Senegal.lm.BPm, newdata=data.frame(Year=2050)))



Serbia.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Serbia"))
Serbia.predict.BPm<-data.frame(country="Serbia",fraction=predict(Serbia.lm.BPm, newdata=data.frame(Year=2050)))



Seychelles.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Seychelles"))
Seychelles.predict.BPm<-data.frame(country="Seychelles",fraction=predict(Seychelles.lm.BPm, newdata=data.frame(Year=2050)))



Sierra_Leone.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Sierra Leone"))
Sierra_Leone.predict.BPm<-data.frame(country="Sierra Leone",fraction=predict(Sierra_Leone.lm.BPm, newdata=data.frame(Year=2050)))



Slovakia.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Slovakia"))
Slovakia.predict.BPm<-data.frame(country="Slovakia",fraction=predict(Slovakia.lm.BPm, newdata=data.frame(Year=2050)))



Slovenia.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Slovenia"))
Slovenia.predict.BPm<-data.frame(country="Slovenia",fraction=predict(Slovenia.lm.BPm, newdata=data.frame(Year=2050)))



Solomon_Islands.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Solomon Islands"))
Solomon_Islands.predict.BPm<-data.frame(country="Solomon Islands",fraction=predict(Solomon_Islands.lm.BPm, newdata=data.frame(Year=2050)))



South_Africa.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="South Africa"))
South_Africa.predict.BPm<-data.frame(country="South Africa",fraction=predict(South_Africa.lm.BPm, newdata=data.frame(Year=2050)))



Spain.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Spain"))
Spain.predict.BPm<-data.frame(country="Spain",fraction=predict(Spain.lm.BPm, newdata=data.frame(Year=2050)))



Sri_Lanka.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Sri Lanka"))
Sri_Lanka.predict.BPm<-data.frame(country="Sri Lanka",fraction=predict(Sri_Lanka.lm.BPm, newdata=data.frame(Year=2050)))



Sudan_former.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Sudan (former)"))
Sudan_former.predict.BPm<-data.frame(country="Sudan (former)",fraction=predict(Sudan_former.lm.BPm, newdata=data.frame(Year=2050)))



Suriname.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Suriname"))
Suriname.predict.BPm<-data.frame(country="Suriname",fraction=predict(Suriname.lm.BPm, newdata=data.frame(Year=2050)))



Swaziland.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Swaziland"))
Swaziland.predict.BPm<-data.frame(country="Swaziland",fraction=predict(Swaziland.lm.BPm, newdata=data.frame(Year=2050)))



Sweden.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Sweden"))
Sweden.predict.BPm<-data.frame(country="Sweden",fraction=predict(Sweden.lm.BPm, newdata=data.frame(Year=2050)))



Switzerland.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Switzerland"))
Switzerland.predict.BPm<-data.frame(country="Switzerland",fraction=predict(Switzerland.lm.BPm, newdata=data.frame(Year=2050)))



Syrian_Arab_Republic.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Syrian Arab Republic"))
Syrian_Arab_Republic.predict.BPm<-data.frame(country="Syrian Arab Republic",fraction=predict(Syrian_Arab_Republic.lm.BPm, newdata=data.frame(Year=2050)))



Tajikistan.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Tajikistan"))
Tajikistan.predict.BPm<-data.frame(country="Tajikistan",fraction=predict(Tajikistan.lm.BPm, newdata=data.frame(Year=2050)))



Thailand.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Thailand"))
Thailand.predict.BPm<-data.frame(country="Thailand",fraction=predict(Thailand.lm.BPm, newdata=data.frame(Year=2050)))



The_former_Yugoslav_Republic_of_Macedonia.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="The former Yugoslav Republic of Macedonia"))
The_former_Yugoslav_Republic_of_Macedonia.predict.BPm<-data.frame(country="The former Yugoslav Republic of Macedonia",fraction=predict(The_former_Yugoslav_Republic_of_Macedonia.lm.BPm, newdata=data.frame(Year=2050)))



TimorLeste.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Timor-Leste"))
TimorLeste.predict.BPm<-data.frame(country="Timor-Leste",fraction=predict(TimorLeste.lm.BPm, newdata=data.frame(Year=2050)))



Togo.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Togo"))
Togo.predict.BPm<-data.frame(country="Togo",fraction=predict(Togo.lm.BPm, newdata=data.frame(Year=2050)))



Trinidad_and_Tobago.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Trinidad and Tobago"))
Trinidad_and_Tobago.predict.BPm<-data.frame(country="Trinidad and Tobago",fraction=predict(Trinidad_and_Tobago.lm.BPm, newdata=data.frame(Year=2050)))



Tunisia.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Tunisia"))
Tunisia.predict.BPm<-data.frame(country="Tunisia",fraction=predict(Tunisia.lm.BPm, newdata=data.frame(Year=2050)))



Turkey.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Turkey"))
Turkey.predict.BPm<-data.frame(country="Turkey",fraction=predict(Turkey.lm.BPm, newdata=data.frame(Year=2050)))



Turkmenistan.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Turkmenistan"))
Turkmenistan.predict.BPm<-data.frame(country="Turkmenistan",fraction=predict(Turkmenistan.lm.BPm, newdata=data.frame(Year=2050)))



Uganda.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Uganda"))
Uganda.predict.BPm<-data.frame(country="Uganda",fraction=predict(Uganda.lm.BPm, newdata=data.frame(Year=2050)))



Ukraine.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Ukraine"))
Ukraine.predict.BPm<-data.frame(country="Ukraine",fraction=predict(Ukraine.lm.BPm, newdata=data.frame(Year=2050)))



United_Arab_Emirates.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="United Arab Emirates"))
United_Arab_Emirates.predict.BPm<-data.frame(country="United Arab Emirates",fraction=predict(United_Arab_Emirates.lm.BPm, newdata=data.frame(Year=2050)))



United_Kingdom.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="United Kingdom"))
United_Kingdom.predict.BPm<-data.frame(country="United Kingdom",fraction=predict(United_Kingdom.lm.BPm, newdata=data.frame(Year=2050)))



United_Republic_of_Tanzania.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="United Republic of Tanzania"))
United_Republic_of_Tanzania.predict.BPm<-data.frame(country="United Republic of Tanzania",fraction=predict(United_Republic_of_Tanzania.lm.BPm, newdata=data.frame(Year=2050)))



United_States_of_America.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="United States of America"))
United_States_of_America.predict.BPm<-data.frame(country="United States of America",fraction=predict(United_States_of_America.lm.BPm, newdata=data.frame(Year=2050)))



Uruguay.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Uruguay"))
Uruguay.predict.BPm<-data.frame(country="Uruguay",fraction=predict(Uruguay.lm.BPm, newdata=data.frame(Year=2050)))



Uzbekistan.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Uzbekistan"))
Uzbekistan.predict.BPm<-data.frame(country="Uzbekistan",fraction=predict(Uzbekistan.lm.BPm, newdata=data.frame(Year=2050)))



Vanuatu.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Vanuatu"))
Vanuatu.predict.BPm<-data.frame(country="Vanuatu",fraction=predict(Vanuatu.lm.BPm, newdata=data.frame(Year=2050)))



Venezuela_Bolivarian_Republic_of.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Venezuela (Bolivarian Republic of)"))
Venezuela_Bolivarian_Republic_of.predict.BPm<-data.frame(country="Venezuela (Bolivarian Republic of)",fraction=predict(Venezuela_Bolivarian_Republic_of.lm.BPm, newdata=data.frame(Year=2050)))



Viet_Nam.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Viet Nam"))
Viet_Nam.predict.BPm<-data.frame(country="Viet Nam",fraction=predict(Viet_Nam.lm.BPm, newdata=data.frame(Year=2050)))



Yemen.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Yemen"))
Yemen.predict.BPm<-data.frame(country="Yemen",fraction=predict(Yemen.lm.BPm, newdata=data.frame(Year=2050)))



Zambia.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Zambia"))
Zambia.predict.BPm<-data.frame(country="Zambia",fraction=predict(Zambia.lm.BPm, newdata=data.frame(Year=2050)))



Zimbabwe.lm.BPm <- lm(Numeric~Year,data=subset(BPm, Country=="Zimbabwe"))
Zimbabwe.predict.BPm<-data.frame(country="Zimbabwe",fraction=predict(Zimbabwe.lm.BPm, newdata=data.frame(Year=2050)))

###Getting coefficients for male BP###
Albania.BPmCoef=data.frame(Country='Albania', BPmCoef= coef(Albania.lm.BPm)[2])
Algeria.BPmCoef=data.frame(Country='Algeria', BPmCoef= coef(Algeria.lm.BPm)[2])
Angola.BPmCoef=data.frame(Country='Angola', BPmCoef= coef(Angola.lm.BPm)[2])
Antigua_and_Barbuda.BPmCoef=data.frame(Country='Antigua and Barbuda', BPmCoef= coef(Antigua_and_Barbuda.lm.BPm)[2])
Argentina.BPmCoef=data.frame(Country='Argentina', BPmCoef= coef(Argentina.lm.BPm)[2])
Armenia.BPmCoef=data.frame(Country='Armenia', BPmCoef= coef(Armenia.lm.BPm)[2])
Australia.BPmCoef=data.frame(Country='Australia', BPmCoef= coef(Australia.lm.BPm)[2])
Austria.BPmCoef=data.frame(Country='Austria', BPmCoef= coef(Austria.lm.BPm)[2])
Azerbaijan.BPmCoef=data.frame(Country='Azerbaijan', BPmCoef= coef(Azerbaijan.lm.BPm)[2])
Bahamas.BPmCoef=data.frame(Country='Bahamas', BPmCoef= coef(Bahamas.lm.BPm)[2])
Bangladesh.BPmCoef=data.frame(Country='Bangladesh', BPmCoef= coef(Bangladesh.lm.BPm)[2])
Barbados.BPmCoef=data.frame(Country='Barbados', BPmCoef= coef(Barbados.lm.BPm)[2])
Belarus.BPmCoef=data.frame(Country='Belarus', BPmCoef= coef(Belarus.lm.BPm)[2])
Belgium.BPmCoef=data.frame(Country='Belgium', BPmCoef= coef(Belgium.lm.BPm)[2])
Belize.BPmCoef=data.frame(Country='Belize', BPmCoef= coef(Belize.lm.BPm)[2])
Benin.BPmCoef=data.frame(Country='Benin', BPmCoef= coef(Benin.lm.BPm)[2])
Bolivia_Plurinational_State_of.BPmCoef=data.frame(Country='Bolivia (Plurinational State of)', BPmCoef= coef(Bolivia_Plurinational_State_of.lm.BPm)[2])
Bosnia_and_Herzegovina.BPmCoef=data.frame(Country='Bosnia and Herzegovina', BPmCoef= coef(Bosnia_and_Herzegovina.lm.BPm)[2])
Botswana.BPmCoef=data.frame(Country='Botswana', BPmCoef= coef(Botswana.lm.BPm)[2])
Brazil.BPmCoef=data.frame(Country='Brazil', BPmCoef= coef(Brazil.lm.BPm)[2])
Brunei_Darussalam.BPmCoef=data.frame(Country='Brunei Darussalam', BPmCoef= coef(Brunei_Darussalam.lm.BPm)[2])
Bulgaria.BPmCoef=data.frame(Country='Bulgaria', BPmCoef= coef(Bulgaria.lm.BPm)[2])
Burkina_Faso.BPmCoef=data.frame(Country='Burkina Faso', BPmCoef= coef(Burkina_Faso.lm.BPm)[2])
Burundi.BPmCoef=data.frame(Country='Burundi', BPmCoef= coef(Burundi.lm.BPm)[2])
Cote_dIvoire.BPmCoef=data.frame(Country='Cote_dIvoire', BPmCoef= coef(Cote_dIvoire.lm.BPm)[2])
Cabo_Verde.BPmCoef=data.frame(Country='Cabo Verde', BPmCoef= coef(Cabo_Verde.lm.BPm)[2])
Cambodia.BPmCoef=data.frame(Country='Cambodia', BPmCoef= coef(Cambodia.lm.BPm)[2])
Cameroon.BPmCoef=data.frame(Country='Cameroon', BPmCoef= coef(Cameroon.lm.BPm)[2])
Canada.BPmCoef=data.frame(Country='Canada', BPmCoef= coef(Canada.lm.BPm)[2])
Central_African_Republic.BPmCoef=data.frame(Country='Central African Republic', BPmCoef= coef(Central_African_Republic.lm.BPm)[2])
Chad.BPmCoef=data.frame(Country='Chad', BPmCoef= coef(Chad.lm.BPm)[2])
Chile.BPmCoef=data.frame(Country='Chile', BPmCoef= coef(Chile.lm.BPm)[2])
China.BPmCoef=data.frame(Country='China', BPmCoef= coef(China.lm.BPm)[2])
Colombia.BPmCoef=data.frame(Country='Colombia', BPmCoef= coef(Colombia.lm.BPm)[2])
Comoros.BPmCoef=data.frame(Country='Comoros', BPmCoef= coef(Comoros.lm.BPm)[2])
Congo.BPmCoef=data.frame(Country='Congo', BPmCoef= coef(Congo.lm.BPm)[2])
Costa_Rica.BPmCoef=data.frame(Country='Costa Rica', BPmCoef= coef(Costa_Rica.lm.BPm)[2])
Croatia.BPmCoef=data.frame(Country='Croatia', BPmCoef= coef(Croatia.lm.BPm)[2])
Cuba.BPmCoef=data.frame(Country='Cuba', BPmCoef= coef(Cuba.lm.BPm)[2])
Cyprus.BPmCoef=data.frame(Country='Cyprus', BPmCoef= coef(Cyprus.lm.BPm)[2])
Czech_Republic.BPmCoef=data.frame(Country='Czech Republic', BPmCoef= coef(Czech_Republic.lm.BPm)[2])
Democratic_Peoples_Republic_of_Korea.BPmCoef=data.frame(Country='Democratic_Peoples_Republic_of_Korea', BPmCoef= coef(Democratic_Peoples_Republic_of_Korea.lm.BPm)[2])
Democratic_Republic_of_the_Congo.BPmCoef=data.frame(Country='Democratic Republic of the Congo', BPmCoef= coef(Democratic_Republic_of_the_Congo.lm.BPm)[2])
Denmark.BPmCoef=data.frame(Country='Denmark', BPmCoef= coef(Denmark.lm.BPm)[2])
Djibouti.BPmCoef=data.frame(Country='Djibouti', BPmCoef= coef(Djibouti.lm.BPm)[2])
Dominica.BPmCoef=data.frame(Country='Dominica', BPmCoef= coef(Dominica.lm.BPm)[2])
Dominican_Republic.BPmCoef=data.frame(Country='Dominican Republic', BPmCoef= coef(Dominican_Republic.lm.BPm)[2])
Ecuador.BPmCoef=data.frame(Country='Ecuador', BPmCoef= coef(Ecuador.lm.BPm)[2])
Egypt.BPmCoef=data.frame(Country='Egypt', BPmCoef= coef(Egypt.lm.BPm)[2])
El_Salvador.BPmCoef=data.frame(Country='El Salvador', BPmCoef= coef(El_Salvador.lm.BPm)[2])
Eritrea.BPmCoef=data.frame(Country='Eritrea', BPmCoef= coef(Eritrea.lm.BPm)[2])
Estonia.BPmCoef=data.frame(Country='Estonia', BPmCoef= coef(Estonia.lm.BPm)[2])
Ethiopia.BPmCoef=data.frame(Country='Ethiopia', BPmCoef= coef(Ethiopia.lm.BPm)[2])
Fiji.BPmCoef=data.frame(Country='Fiji', BPmCoef= coef(Fiji.lm.BPm)[2])
Finland.BPmCoef=data.frame(Country='Finland', BPmCoef= coef(Finland.lm.BPm)[2])
France.BPmCoef=data.frame(Country='France', BPmCoef= coef(France.lm.BPm)[2])
Gabon.BPmCoef=data.frame(Country='Gabon', BPmCoef= coef(Gabon.lm.BPm)[2])
Gambia.BPmCoef=data.frame(Country='Gambia', BPmCoef= coef(Gambia.lm.BPm)[2])
Georgia.BPmCoef=data.frame(Country='Georgia', BPmCoef= coef(Georgia.lm.BPm)[2])
Germany.BPmCoef=data.frame(Country='Germany', BPmCoef= coef(Germany.lm.BPm)[2])
Ghana.BPmCoef=data.frame(Country='Ghana', BPmCoef= coef(Ghana.lm.BPm)[2])
Greece.BPmCoef=data.frame(Country='Greece', BPmCoef= coef(Greece.lm.BPm)[2])
Grenada.BPmCoef=data.frame(Country='Grenada', BPmCoef= coef(Grenada.lm.BPm)[2])
Guatemala.BPmCoef=data.frame(Country='Guatemala', BPmCoef= coef(Guatemala.lm.BPm)[2])
Guinea.BPmCoef=data.frame(Country='Guinea', BPmCoef= coef(Guinea.lm.BPm)[2])
GuineaBissau.BPmCoef=data.frame(Country='Guinea-Bissau', BPmCoef= coef(GuineaBissau.lm.BPm)[2])
Guyana.BPmCoef=data.frame(Country='Guyana', BPmCoef= coef(Guyana.lm.BPm)[2])
Haiti.BPmCoef=data.frame(Country='Haiti', BPmCoef= coef(Haiti.lm.BPm)[2])
Honduras.BPmCoef=data.frame(Country='Honduras', BPmCoef= coef(Honduras.lm.BPm)[2])
Hungary.BPmCoef=data.frame(Country='Hungary', BPmCoef= coef(Hungary.lm.BPm)[2])
Iceland.BPmCoef=data.frame(Country='Iceland', BPmCoef= coef(Iceland.lm.BPm)[2])
India.BPmCoef=data.frame(Country='India', BPmCoef= coef(India.lm.BPm)[2])
Indonesia.BPmCoef=data.frame(Country='Indonesia', BPmCoef= coef(Indonesia.lm.BPm)[2])
Iran_Islamic_Republic_of.BPmCoef=data.frame(Country='Iran (Islamic Republic of)', BPmCoef= coef(Iran_Islamic_Republic_of.lm.BPm)[2])
Ireland.BPmCoef=data.frame(Country='Ireland', BPmCoef= coef(Ireland.lm.BPm)[2])
Israel.BPmCoef=data.frame(Country='Israel', BPmCoef= coef(Israel.lm.BPm)[2])
Italy.BPmCoef=data.frame(Country='Italy', BPmCoef= coef(Italy.lm.BPm)[2])
Jamaica.BPmCoef=data.frame(Country='Jamaica', BPmCoef= coef(Jamaica.lm.BPm)[2])
Japan.BPmCoef=data.frame(Country='Japan', BPmCoef= coef(Japan.lm.BPm)[2])
Jordan.BPmCoef=data.frame(Country='Jordan', BPmCoef= coef(Jordan.lm.BPm)[2])
Kazakhstan.BPmCoef=data.frame(Country='Kazakhstan', BPmCoef= coef(Kazakhstan.lm.BPm)[2])
Kenya.BPmCoef=data.frame(Country='Kenya', BPmCoef= coef(Kenya.lm.BPm)[2])
Kiribati.BPmCoef=data.frame(Country='Kiribati', BPmCoef= coef(Kiribati.lm.BPm)[2])
Kuwait.BPmCoef=data.frame(Country='Kuwait', BPmCoef= coef(Kuwait.lm.BPm)[2])
Kyrgyzstan.BPmCoef=data.frame(Country='Kyrgyzstan', BPmCoef= coef(Kyrgyzstan.lm.BPm)[2])
Lao_Peoples_Democratic_Republic.BPmCoef=data.frame(Country='Lao_Peoples_Democratic_Republic', BPmCoef= coef(Lao_Peoples_Democratic_Republic.lm.BPm)[2])
Latvia.BPmCoef=data.frame(Country='Latvia', BPmCoef= coef(Latvia.lm.BPm)[2])
Lebanon.BPmCoef=data.frame(Country='Lebanon', BPmCoef= coef(Lebanon.lm.BPm)[2])
Lesotho.BPmCoef=data.frame(Country='Lesotho', BPmCoef= coef(Lesotho.lm.BPm)[2])
Liberia.BPmCoef=data.frame(Country='Liberia', BPmCoef= coef(Liberia.lm.BPm)[2])
Libya.BPmCoef=data.frame(Country='Libya', BPmCoef= coef(Libya.lm.BPm)[2])
Lithuania.BPmCoef=data.frame(Country='Lithuania', BPmCoef= coef(Lithuania.lm.BPm)[2])
Luxembourg.BPmCoef=data.frame(Country='Luxembourg', BPmCoef= coef(Luxembourg.lm.BPm)[2])
Madagascar.BPmCoef=data.frame(Country='Madagascar', BPmCoef= coef(Madagascar.lm.BPm)[2])
Malawi.BPmCoef=data.frame(Country='Malawi', BPmCoef= coef(Malawi.lm.BPm)[2])
Malaysia.BPmCoef=data.frame(Country='Malaysia', BPmCoef= coef(Malaysia.lm.BPm)[2])
Maldives.BPmCoef=data.frame(Country='Maldives', BPmCoef= coef(Maldives.lm.BPm)[2])
Mali.BPmCoef=data.frame(Country='Mali', BPmCoef= coef(Mali.lm.BPm)[2])
Malta.BPmCoef=data.frame(Country='Malta', BPmCoef= coef(Malta.lm.BPm)[2])
Mauritania.BPmCoef=data.frame(Country='Mauritania', BPmCoef= coef(Mauritania.lm.BPm)[2])
Mauritius.BPmCoef=data.frame(Country='Mauritius', BPmCoef= coef(Mauritius.lm.BPm)[2])
Mexico.BPmCoef=data.frame(Country='Mexico', BPmCoef= coef(Mexico.lm.BPm)[2])
Mongolia.BPmCoef=data.frame(Country='Mongolia', BPmCoef= coef(Mongolia.lm.BPm)[2])
Montenegro.BPmCoef=data.frame(Country='Montenegro', BPmCoef= coef(Montenegro.lm.BPm)[2])
Morocco.BPmCoef=data.frame(Country='Morocco', BPmCoef= coef(Morocco.lm.BPm)[2])
Mozambique.BPmCoef=data.frame(Country='Mozambique', BPmCoef= coef(Mozambique.lm.BPm)[2])
Myanmar.BPmCoef=data.frame(Country='Myanmar', BPmCoef= coef(Myanmar.lm.BPm)[2])
Namibia.BPmCoef=data.frame(Country='Namibia', BPmCoef= coef(Namibia.lm.BPm)[2])
Nepal.BPmCoef=data.frame(Country='Nepal', BPmCoef= coef(Nepal.lm.BPm)[2])
Netherlands.BPmCoef=data.frame(Country='Netherlands', BPmCoef= coef(Netherlands.lm.BPm)[2])
New_Zealand.BPmCoef=data.frame(Country='New Zealand', BPmCoef= coef(New_Zealand.lm.BPm)[2])
Nicaragua.BPmCoef=data.frame(Country='Nicaragua', BPmCoef= coef(Nicaragua.lm.BPm)[2])
Niger.BPmCoef=data.frame(Country='Niger', BPmCoef= coef(Niger.lm.BPm)[2])
Nigeria.BPmCoef=data.frame(Country='Nigeria', BPmCoef= coef(Nigeria.lm.BPm)[2])
Norway.BPmCoef=data.frame(Country='Norway', BPmCoef= coef(Norway.lm.BPm)[2])
Pakistan.BPmCoef=data.frame(Country='Pakistan', BPmCoef= coef(Pakistan.lm.BPm)[2])
Panama.BPmCoef=data.frame(Country='Panama', BPmCoef= coef(Panama.lm.BPm)[2])
Paraguay.BPmCoef=data.frame(Country='Paraguay', BPmCoef= coef(Paraguay.lm.BPm)[2])
Peru.BPmCoef=data.frame(Country='Peru', BPmCoef= coef(Peru.lm.BPm)[2])
Philippines.BPmCoef=data.frame(Country='Philippines', BPmCoef= coef(Philippines.lm.BPm)[2])
Poland.BPmCoef=data.frame(Country='Poland', BPmCoef= coef(Poland.lm.BPm)[2])
Portugal.BPmCoef=data.frame(Country='Portugal', BPmCoef= coef(Portugal.lm.BPm)[2])
Republic_of_Korea.BPmCoef=data.frame(Country='Republic of Korea', BPmCoef= coef(Republic_of_Korea.lm.BPm)[2])
Republic_of_Moldova.BPmCoef=data.frame(Country='Republic of Moldova', BPmCoef= coef(Republic_of_Moldova.lm.BPm)[2])
Romania.BPmCoef=data.frame(Country='Romania', BPmCoef= coef(Romania.lm.BPm)[2])
Russian_Federation.BPmCoef=data.frame(Country='Russian Federation', BPmCoef= coef(Russian_Federation.lm.BPm)[2])
Rwanda.BPmCoef=data.frame(Country='Rwanda', BPmCoef= coef(Rwanda.lm.BPm)[2])
Saint_Kitts_and_Nevis.BPmCoef=data.frame(Country='Saint Kitts and Nevis', BPmCoef= coef(Saint_Kitts_and_Nevis.lm.BPm)[2])
Saint_Lucia.BPmCoef=data.frame(Country='Saint Lucia', BPmCoef= coef(Saint_Lucia.lm.BPm)[2])
Saint_Vincent_and_the_Grenadines.BPmCoef=data.frame(Country='Saint Vincent and the Grenadines', BPmCoef= coef(Saint_Vincent_and_the_Grenadines.lm.BPm)[2])
Samoa.BPmCoef=data.frame(Country='Samoa', BPmCoef= coef(Samoa.lm.BPm)[2])
Sao_Tome_and_Principe.BPmCoef=data.frame(Country='Sao Tome and Principe', BPmCoef= coef(Sao_Tome_and_Principe.lm.BPm)[2])
Saudi_Arabia.BPmCoef=data.frame(Country='Saudi Arabia', BPmCoef= coef(Saudi_Arabia.lm.BPm)[2])
Senegal.BPmCoef=data.frame(Country='Senegal', BPmCoef= coef(Senegal.lm.BPm)[2])
Serbia.BPmCoef=data.frame(Country='Serbia', BPmCoef= coef(Serbia.lm.BPm)[2])
Seychelles.BPmCoef=data.frame(Country='Seychelles', BPmCoef= coef(Seychelles.lm.BPm)[2])
Sierra_Leone.BPmCoef=data.frame(Country='Sierra Leone', BPmCoef= coef(Sierra_Leone.lm.BPm)[2])
Slovakia.BPmCoef=data.frame(Country='Slovakia', BPmCoef= coef(Slovakia.lm.BPm)[2])
Slovenia.BPmCoef=data.frame(Country='Slovenia', BPmCoef= coef(Slovenia.lm.BPm)[2])
Solomon_Islands.BPmCoef=data.frame(Country='Solomon Islands', BPmCoef= coef(Solomon_Islands.lm.BPm)[2])
South_Africa.BPmCoef=data.frame(Country='South Africa', BPmCoef= coef(South_Africa.lm.BPm)[2])
Spain.BPmCoef=data.frame(Country='Spain', BPmCoef= coef(Spain.lm.BPm)[2])
Sri_Lanka.BPmCoef=data.frame(Country='Sri Lanka', BPmCoef= coef(Sri_Lanka.lm.BPm)[2])
Sudan_former.BPmCoef=data.frame(Country='Sudan (former)', BPmCoef= coef(Sudan_former.lm.BPm)[2])
Suriname.BPmCoef=data.frame(Country='Suriname', BPmCoef= coef(Suriname.lm.BPm)[2])
Swaziland.BPmCoef=data.frame(Country='Swaziland', BPmCoef= coef(Swaziland.lm.BPm)[2])
Sweden.BPmCoef=data.frame(Country='Sweden', BPmCoef= coef(Sweden.lm.BPm)[2])
Switzerland.BPmCoef=data.frame(Country='Switzerland', BPmCoef= coef(Switzerland.lm.BPm)[2])
Syrian_Arab_Republic.BPmCoef=data.frame(Country='Syrian Arab Republic', BPmCoef= coef(Syrian_Arab_Republic.lm.BPm)[2])
Tajikistan.BPmCoef=data.frame(Country='Tajikistan', BPmCoef= coef(Tajikistan.lm.BPm)[2])
Thailand.BPmCoef=data.frame(Country='Thailand', BPmCoef= coef(Thailand.lm.BPm)[2])
The_former_Yugoslav_Republic_of_Macedonia.BPmCoef=data.frame(Country='The former Yugoslav Republic of Macedonia', BPmCoef= coef(The_former_Yugoslav_Republic_of_Macedonia.lm.BPm)[2])
TimorLeste.BPmCoef=data.frame(Country='Timor-Leste', BPmCoef= coef(TimorLeste.lm.BPm)[2])
Togo.BPmCoef=data.frame(Country='Togo', BPmCoef= coef(Togo.lm.BPm)[2])
Trinidad_and_Tobago.BPmCoef=data.frame(Country='Trinidad and Tobago', BPmCoef= coef(Trinidad_and_Tobago.lm.BPm)[2])
Tunisia.BPmCoef=data.frame(Country='Tunisia', BPmCoef= coef(Tunisia.lm.BPm)[2])
Turkey.BPmCoef=data.frame(Country='Turkey', BPmCoef= coef(Turkey.lm.BPm)[2])
Turkmenistan.BPmCoef=data.frame(Country='Turkmenistan', BPmCoef= coef(Turkmenistan.lm.BPm)[2])
Uganda.BPmCoef=data.frame(Country='Uganda', BPmCoef= coef(Uganda.lm.BPm)[2])
Ukraine.BPmCoef=data.frame(Country='Ukraine', BPmCoef= coef(Ukraine.lm.BPm)[2])
United_Arab_Emirates.BPmCoef=data.frame(Country='United Arab Emirates', BPmCoef= coef(United_Arab_Emirates.lm.BPm)[2])
United_Kingdom.BPmCoef=data.frame(Country='United Kingdom', BPmCoef= coef(United_Kingdom.lm.BPm)[2])
United_Republic_of_Tanzania.BPmCoef=data.frame(Country='United Republic of Tanzania', BPmCoef= coef(United_Republic_of_Tanzania.lm.BPm)[2])
United_States_of_America.BPmCoef=data.frame(Country='United States of America', BPmCoef= coef(United_States_of_America.lm.BPm)[2])
Uruguay.BPmCoef=data.frame(Country='Uruguay', BPmCoef= coef(Uruguay.lm.BPm)[2])
Uzbekistan.BPmCoef=data.frame(Country='Uzbekistan', BPmCoef= coef(Uzbekistan.lm.BPm)[2])
Vanuatu.BPmCoef=data.frame(Country='Vanuatu', BPmCoef= coef(Vanuatu.lm.BPm)[2])
Venezuela_Bolivarian_Republic_of.BPmCoef=data.frame(Country='Venezuela (Bolivarian Republic of)', BPmCoef= coef(Venezuela_Bolivarian_Republic_of.lm.BPm)[2])
Viet_Nam.BPmCoef=data.frame(Country='Viet Nam', BPmCoef= coef(Viet_Nam.lm.BPm)[2])
Yemen.BPmCoef=data.frame(Country='Yemen', BPmCoef= coef(Yemen.lm.BPm)[2])
Zambia.BPmCoef=data.frame(Country='Zambia', BPmCoef= coef(Zambia.lm.BPm)[2])
Zimbabwe.BPmCoef=data.frame(Country='Zimbabwe', BPmCoef= coef(Zimbabwe.lm.BPm)[2])

predicted.BPm.coef<-rbind(
  Albania.BPmCoef,
  Algeria.BPmCoef,
  Angola.BPmCoef,
 # Antigua_and_Barbuda.BPmCoef,
  Argentina.BPmCoef,
  Armenia.BPmCoef,
  Australia.BPmCoef,
  Austria.BPmCoef,
  Azerbaijan.BPmCoef,
  Bahamas.BPmCoef,
  Bangladesh.BPmCoef,
  Barbados.BPmCoef,
  Belarus.BPmCoef,
  Belgium.BPmCoef,
  Belize.BPmCoef,
  Benin.BPmCoef,
  Bolivia_Plurinational_State_of.BPmCoef,
  Bosnia_and_Herzegovina.BPmCoef,
  Botswana.BPmCoef,
  Brazil.BPmCoef,
  Brunei_Darussalam.BPmCoef,
  Bulgaria.BPmCoef,
  Burkina_Faso.BPmCoef,
  Burundi.BPmCoef,
  Cabo_Verde.BPmCoef,
  Cambodia.BPmCoef,
  Cameroon.BPmCoef,
  Canada.BPmCoef,
  Central_African_Republic.BPmCoef,
  Chad.BPmCoef,
  Chile.BPmCoef,
  China.BPmCoef,
  Colombia.BPmCoef,
  Comoros.BPmCoef,
  Congo.BPmCoef,
  Costa_Rica.BPmCoef,
  Cote_dIvoire.BPmCoef,
  Croatia.BPmCoef,
  Cuba.BPmCoef,
  Cyprus.BPmCoef,
  Czech_Republic.BPmCoef,
  Democratic_Peoples_Republic_of_Korea.BPmCoef,
  
  Denmark.BPmCoef,
  Djibouti.BPmCoef,
# Dominica.BPmCoef,
  Dominican_Republic.BPmCoef,
  Ecuador.BPmCoef,
  Egypt.BPmCoef,
  El_Salvador.BPmCoef,
  Eritrea.BPmCoef,
  Estonia.BPmCoef,
  Ethiopia.BPmCoef,
  Fiji.BPmCoef,
  Finland.BPmCoef,
  France.BPmCoef,
  Gabon.BPmCoef,
  Gambia.BPmCoef,
  Georgia.BPmCoef,
  Germany.BPmCoef,
  Ghana.BPmCoef,
  Greece.BPmCoef,
  Grenada.BPmCoef,
  Guatemala.BPmCoef,
  Guinea.BPmCoef,
  GuineaBissau.BPmCoef,
  Guyana.BPmCoef,
  Haiti.BPmCoef,
  Honduras.BPmCoef,
  Hungary.BPmCoef,
  Iceland.BPmCoef,
  India.BPmCoef,
  Indonesia.BPmCoef,
  Iran_Islamic_Republic_of.BPmCoef,
  Ireland.BPmCoef,
  Israel.BPmCoef,
  Italy.BPmCoef,
  Jamaica.BPmCoef,
  Japan.BPmCoef,
  Jordan.BPmCoef,
  Kazakhstan.BPmCoef,
  Kenya.BPmCoef,
 # Kiribati.BPmCoef,
  Kuwait.BPmCoef,
  Kyrgyzstan.BPmCoef,
  Lao_Peoples_Democratic_Republic.BPmCoef,
  Latvia.BPmCoef,
  Lebanon.BPmCoef,
  Lesotho.BPmCoef,
  Liberia.BPmCoef,
  Libya.BPmCoef,
  Lithuania.BPmCoef,
  Luxembourg.BPmCoef,
  Madagascar.BPmCoef,
  Malawi.BPmCoef,
  Malaysia.BPmCoef,
  Maldives.BPmCoef,
  Mali.BPmCoef,
  Malta.BPmCoef,
  Mauritania.BPmCoef,
  Mauritius.BPmCoef,
  Mexico.BPmCoef,
  Mongolia.BPmCoef,
  Montenegro.BPmCoef,
  Morocco.BPmCoef,
  Mozambique.BPmCoef,
  Myanmar.BPmCoef,
  Namibia.BPmCoef,
  Nepal.BPmCoef,
  Netherlands.BPmCoef,
  New_Zealand.BPmCoef,
  Nicaragua.BPmCoef,
  Niger.BPmCoef,
  Nigeria.BPmCoef,
  Norway.BPmCoef,
  Pakistan.BPmCoef,
  Panama.BPmCoef,
  Paraguay.BPmCoef,
  Peru.BPmCoef,
  Philippines.BPmCoef,
  Poland.BPmCoef,
  Portugal.BPmCoef,
  Republic_of_Korea.BPmCoef,
  Republic_of_Moldova.BPmCoef,
  Romania.BPmCoef,
  Russian_Federation.BPmCoef,
  Rwanda.BPmCoef,
#  Saint_Kitts_and_Nevis.BPmCoef,
  Saint_Lucia.BPmCoef,
  Saint_Vincent_and_the_Grenadines.BPmCoef,
  Samoa.BPmCoef,
  Sao_Tome_and_Principe.BPmCoef,
  Saudi_Arabia.BPmCoef,
  Senegal.BPmCoef,
  Serbia.BPmCoef,
  Seychelles.BPmCoef,
  Sierra_Leone.BPmCoef,
  Slovakia.BPmCoef,
  Slovenia.BPmCoef,
  Solomon_Islands.BPmCoef,
  South_Africa.BPmCoef,
  Spain.BPmCoef,
  Sri_Lanka.BPmCoef,
  #Sudan_former.BPmCoef,
  Suriname.BPmCoef,
  Swaziland.BPmCoef,
  Sweden.BPmCoef,
  Switzerland.BPmCoef,
  Syrian_Arab_Republic.BPmCoef,
  Tajikistan.BPmCoef,
  Thailand.BPmCoef,
  The_former_Yugoslav_Republic_of_Macedonia.BPmCoef,
  TimorLeste.BPmCoef,
  Togo.BPmCoef,
  Trinidad_and_Tobago.BPmCoef,
  Tunisia.BPmCoef,
  Turkey.BPmCoef,
  Turkmenistan.BPmCoef,
  Uganda.BPmCoef,
  Ukraine.BPmCoef,
  United_Arab_Emirates.BPmCoef,
  United_Kingdom.BPmCoef,
  United_Republic_of_Tanzania.BPmCoef,
  United_States_of_America.BPmCoef,
  Uruguay.BPmCoef,
  Uzbekistan.BPmCoef,
  Vanuatu.BPmCoef,
  Venezuela_Bolivarian_Republic_of.BPmCoef,
  Viet_Nam.BPmCoef,
  Yemen.BPmCoef,
  Zambia.BPmCoef,
  Zimbabwe.BPmCoef
)

BPvTotProtein<-merge(predicted.BPm.coef,predicted.proteinfract.coef80,by="Country")
View(BPvTotProtein)
require("lattice")
plot(BPmCoef~fraccoef80,data=BPvTotProtein, main="trend in BP since 1980 vs trend in percent protein since 1980")
summary(lm(BPmCoef~fraccoef80,data=BPvTotProtein)) #NS

xyplot(Numeric~Year, subset=Country=="Greece", data=BP,groups = BP$Sex, main="Greece BP",ylab="Systolic BP", auto.key =  list(corner = c(1,1),  border = TRUE, lines = FALSE)) 


BPvProtein<-merge(BPm,tot.protein, By="CountryYear")
View(BPvProtein)

levels(BP$Region)
xyplot(Numeric~ProteinCalFraction, data=BPvProtein,groups=Region, auto.key =  list(corner = c(1,1),  border = TRUE, lines = FALSE)) 

xyplot(Numeric~ProteinCalFraction, subset=Region=="Africa",data=BPvProtein,groups=Country, auto.key =  list(corner = c(1,1),  border = TRUE, lines = FALSE)) 
xyplot(Numeric~ProteinCalFraction, subset=Region=="Europe",data=BPvProtein,groups=Country)
xyplot(Numeric~ProteinCalFraction, subset=Region=="Americas",data=BPvProtein,groups=Country)
xyplot(Numeric~ProteinCalFraction, subset=Region=="South-East Asia",data=BPvProtein,groups=Country)
xyplot(Numeric~ProteinCalFraction, subset=Region=="Eastern Mediterranean",data=BPvProtein,groups=Country)
xyplot(Numeric~ProteinCalFraction, subset=Region=="Western Pacific",data=BPvProtein,groups=Country)

require(lme4)
summary(lmer(Numeric~ProteinCalFraction +(1|Country),data=BPvProtein))
