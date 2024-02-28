rm(list=ls())


library(tidyverse)
library(janitor)
library(dplyr)
library(xlsx)
library(readxl)
library(tools)
library(Hmisc)
library(corrplot)
library(psych)



country_list=c( "Albania","Algeria",
                "Angola", "Argentina","Armenia",
                "Australia","Austria","Azerbaijan",
                "Bahrain","Bangladesh","Belarus",
                "Belgium","Benin","Bhutan",
                "Bolivia","Botswana","Brazil",
                "Bulgaria","Burkina Faso","Burundi",
                "Cambodia","Cameroon","Canada",
                "Cape Verde","Central African Republic","Chad",
                "Chile","China","Colombia",
                "Comoros","Congo, Dem. Rep.", "Congo, Rep.",
                "Costa Rica","Cote d'Ivoire","Croatia",
                "Cuba","Cyprus","Czech Republic",
                "Denmark","Djibouti","Dominican Republic",
                "Ecuador","Egypt","El Salvador",
                "Equatorial Guinea","Estonia",
                "Ethiopia","Fiji","Finland", 
                "France","Gabon","Gambia, The",
                "Georgia","Germany","Ghana",
                "Greece","Guatemala","Guinea",
                "Guinea-Bissau","Guyana","Haiti",
                "Honduras","Hungary","India",
                "Indonesia","Iran","Iraq",
                "Ireland","Israel","Italy",
                "Jamaica","Japan","Jordan",
                "Kazakhstan","Kenya","Korea",
                "Kuwait","Kyrgyzstan","Latvia",
                "Lebanon","Lesotho","Liberia",
                "Libya","Lithuania",
                "Madagascar","Malawi","Malaysia",
                "Mali","Mauritania","Mauritius",
                "Mexico","Moldova",
                "Mongolia","Morocco","Mozambique",
                "Myanmar","Namibia","Nepal",
                "Netherlands","New Zealand","Nicaragua",
                "Niger","Nigeria","Norway",
                "Oman","Pakistan","Panama",
                "Paraguay","Peru",
                "Philippines", "Poland","Portugal",
                "Qatar","Romania","Russia",
                "Rwanda","Saudi Arabia","Senegal",
                "Serbia","Sierra Leone","Singapore",
                "Slovak Republic","Slovenia","Solomon Islands",
                "Somalia","South Africa","South Korea","Spain",
                "Sri Lanka","Sudan","Suriname",
                "Sweden","Switzerland",
                "Syria","Tajikistan","Tanzania",
                "Thailand","Togo","Trinidad",
                "Tunisia","Turkey","Turkmenistan",
                "Uganda","Ukraine","United Arab Emirates",
                "United Kingdom","United States","Uruguay",
                "Uzbekistan","Venezuela, RB","Vietnam",
                "Yemen","Zambia","Zimbabwe")

#1997-2011 data set 

NTD <- read_excel("NTD constructed dataset.xlsx", sheet = "panelvaryentry")
unique(NTD$country)
#Subset years
NTD=subset(NTD,NTD$year>="1998" & NTD$year<="2011")

#stability data
##########
#Stability 
##########
s <- read_excel("C:/Users/catis/Box/Catalina and Natasha/PS and Emigration/Datasets/Raw Data/Data_Extract_From_Worldwide_Governance_Indicators.xlsx")

#clean names
s <- janitor::clean_names(s)
colnames(s)

#Select columns
s = s %>% select(time,country_name,political_stability_and_absence_of_violence_terrorism_estimate_pv_est)
#Change Names
names(s)[1] = "year"
names(s)[2] = "country"
names(s)[3] = "stability"

#Order alphabetically
s[order(rownames(s)), ]
s=s[order(s[,'country']), ]

#round numbers
#s$stability=as.numeric(s$stability)
#s=s[-c(4709:4713),]

#Subset years
s=subset(s,s$year>="1998" & s$year<="2011")


#merge

d=merge(NTD,s, by=c("country","year"))


#income

dd$income=NA
dd$income[dd$gdp > 12.995]=1
dd$income[dd$gdp >= "1.045" & dd$gdp <= "12.995"]=2
dd$income[dd$gdp >= "0" & dd$gdp <="1.045"]=3

#dd[dd$gdp < 1.045 & !is.na(dd$gdp),28]<- 3

table(dd$income, exclude = NULL) 

des.mat <- describeBy(dd, dd$AFRICA,mat=TRUE)

#save Model 1 data
write.xlsx(d,"C:/Users/catis/Box/Catalina and Natasha/PS and Emigration/Datasets/Clean Data//1997-2011.xlsx")

#summary statistics 

d=as.data.frame(d)
d=as.matrix(d)

d$gdp=as.numeric(d$gdp)

table1=describe(d)
table1
#save excel 

write.xlsx(table1, file="C:/Users/catis/Box/Catalina and Natasha/PS and Emigration/Analysis//model1.xlsx", sheetName = "summary statistics", 
           col.names = TRUE, row.names = TRUE, append = TRUE)


##correlation

dm=data.matrix(d)


m1 = rcorr(as.matrix(dm)) #correlation matrix
m1

m1.coeff = m1$r
m1.p = m1$P

## Graphical Correlation Matrix:
# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

#flaten correlation matrix
cor_matrix=flattenCorrMatrix(m1$r, m1$P) 

cor_matrix

#save cor matrix
write.xlsx(cor_matrix, file="C:/Users/catis/Box/Catalina and Natasha/PS and Emigration/Analysis//model1.xlsx", sheetName = "correlation matrix", 
           col.names = TRUE, row.names = TRUE, append = TRUE)



#emigration correlations

# Function to extract correlation coefficient and p-values
corrFunc <- function(var1, var2, data) {
  result = cor.test(data[,var1], data[,var2])
  data.frame(var1, var2, result[c("estimate","p.value","statistic","method")], 
             stringsAsFactors=FALSE)
}

## Pairs of variables for which we want correlations
vars = data.frame(v1=names(d)[9],v2=names(d)[-1])

# Apply corrFunc to all rows of vars
corrs1 = do.call(rbind, mapply(corrFunc, vars[,1], vars[,2], MoreArgs=list(data=d), 
                              SIMPLIFY=FALSE))

write.xlsx(corrs1, file="C:/Users/catis/Box/Catalina and Natasha/PS and Emigration/Analysis//model1.xlsx", sheetName = "emigration correlation", 
           col.names = TRUE, row.names = TRUE, append = TRUE)

#low skil and high skill corrlation 

d <- read_excel("C:/Users/catis/Box/Catalina and Natasha/PS and Emigration/Datasets/Clean Data/1997-2011.xlsx")

d$stability=as.numeric(d$stability)

low_skill_stability <- cor.test(d$lwsklflw, d$stability, 
                method = "pearson")
low_skill_stability

low_skill_polity <- cor.test(d$lwsklflw, d$polity, 
                      method = "pearson")
low_skill_polity

low_skill_gdp <- cor.test(d$lwsklflw, d$gdp, 
                             method = "pearson")
low_skill_gdp

low_skill_gdp <- cor.test(d$lwsklflw, d$density, 
                          method = "pearson")
low_skill_gdp

low_skill_gdp <- cor.test(d$lwsklflw, d$Trade, 
                          method = "pearson")
low_skill_gdp

low_skill_gdp <- cor.test(d$lwsklflw, d$popgrwt, 
                          method = "pearson")
low_skill_gdp

low_skill_gdp <- cor.test(d$lwsklflw, d$civilw, 
                          method = "pearson")
low_skill_gdp
high_skill_stability <- cor.test(d$hsklflw, d$stability, 
                      method = "pearson")
high_skill_stability

high_skill_polity <- cor.test(d$hsklflw, d$polity, 
                       method = "pearson")
high_skill_polity

high_skill_gdp <- cor.test(d$hsklflw, d$gdp, 
                              method = "pearson")
high_skill_gdp

high_skill_gdp <- cor.test(d$hsklflw, d$density, 
                           method = "pearson")
high_skill_gdp

high_skill_gdp <- cor.test(d$hsklflw, d$Trade, 
                           method = "pearson")
high_skill_gdp

high_skill_gdp <- cor.test(d$hsklflw, d$popgrwt, 
                           method = "pearson")
high_skill_gdp

high_skill_gdp <- cor.test(d$hsklflw, d$civilw, 
                           method = "pearson")
high_skill_gdp

#2 MODEL
###############
#FSI data
#############

fsi_2006 <- read_excel("C:/Users/catis/Box/Catalina and Natasha/PS and Emigration/Datasets/Raw Data/fsi-2006.xlsx")
fsi_2007 <- read_excel("C:/Users/catis/Box/Catalina and Natasha/PS and Emigration/Datasets/Raw Data/fsi-2007.xlsx")
fsi_2008 <- read_excel("C:/Users/catis/Box/Catalina and Natasha/PS and Emigration/Datasets/Raw Data/fsi-2008.xlsx")
fsi_2009 <- read_excel("C:/Users/catis/Box/Catalina and Natasha/PS and Emigration/Datasets/Raw Data/fsi-2009.xlsx")
fsi_2010 <- read_excel("C:/Users/catis/Box/Catalina and Natasha/PS and Emigration/Datasets/Raw Data/fsi-2010.xlsx")
fsi_2011 <- read_excel("C:/Users/catis/Box/Catalina and Natasha/PS and Emigration/Datasets/Raw Data/fsi-2011.xlsx")
fsi_2012 <- read_excel("C:/Users/catis/Box/Catalina and Natasha/PS and Emigration/Datasets/Raw Data/fsi-2012.xlsx")
fsi_2013 <- read_excel("C:/Users/catis/Box/Catalina and Natasha/PS and Emigration/Datasets/Raw Data/fsi-2013.xlsx")
fsi_2014 <- read_excel("C:/Users/catis/Box/Catalina and Natasha/PS and Emigration/Datasets/Raw Data/fsi-2014.xlsx")
fsi_2015 <- read_excel("C:/Users/catis/Box/Catalina and Natasha/PS and Emigration/Datasets/Raw Data/fsi-2015.xlsx")
fsi_2016 <- read_excel("C:/Users/catis/Box/Catalina and Natasha/PS and Emigration/Datasets/Raw Data/fsi-2016.xlsx")
fsi_2017 <- read_excel("C:/Users/catis/Box/Catalina and Natasha/PS and Emigration/Datasets/Raw Data/fsi-2017.xlsx")
fsi_2018 <- read_excel("C:/Users/catis/Box/Catalina and Natasha/PS and Emigration/Datasets/Raw Data/fsi-2018.xlsx")
fsi_2019 <- read_excel("C:/Users/catis/Box/Catalina and Natasha/PS and Emigration/Datasets/Raw Data/fsi-2019.xlsx")
fsi_2020 <- read_excel("C:/Users/catis/Box/Catalina and Natasha/PS and Emigration/Datasets/Raw Data/fsi-2020.xlsx")
fsi_2021 <- read_excel("C:/Users/catis/Box/Catalina and Natasha/PS and Emigration/Datasets/Raw Data/fsi-2021.xlsx")



##############
# Clean Names
############
fsi_2006 <- janitor::clean_names(fsi_2006)
fsi_2007 <- janitor::clean_names(fsi_2007)
fsi_2008 <- janitor::clean_names(fsi_2008)
fsi_2009 <- janitor::clean_names(fsi_2009)
fsi_2010 <- janitor::clean_names(fsi_2010)
fsi_2011 <- janitor::clean_names(fsi_2011)
fsi_2012 <- janitor::clean_names(fsi_2012)
fsi_2013 <- janitor::clean_names(fsi_2013)
fsi_2014 <- janitor::clean_names(fsi_2014)
fsi_2015 <- janitor::clean_names(fsi_2015)
fsi_2016 <- janitor::clean_names(fsi_2016)
fsi_2017 <- janitor::clean_names(fsi_2017)
fsi_2018 <- janitor::clean_names(fsi_2018)
#fsi_2019 <- janitor::clean_names(fsi_2019)
#fsi_2020 <- janitor::clean_names(fsi_2020)
#fsi_2021 <- janitor::clean_names(fsi_2021)

###########
##Clean year
##########
fsi_2006$year = 2006
fsi_2007$year = 2007
fsi_2008$year = 2008
fsi_2009$year = 2009
fsi_2010$year = 2010
fsi_2011$year = 2011
fsi_2012$year = 2012
fsi_2013$year = 2013
fsi_2014$year = 2014
fsi_2015$year = 2015
fsi_2016$year = 2016
fsi_2017$year = 2017
fsi_2018$year = 2018
#fsi_2019$year = 2019
#fsi_2020$year = 2020
#fsi_2021$year = 2021

# subset

#fsi_2006b=subset(fsi_2006, select = c(country,year,e3_human_flight_and_brain_drain,c3_group_grievance))
fsi_2007b=subset(fsi_2007, select = c(country,year,e3_human_flight_and_brain_drain,c3_group_grievance))
fsi_2008b=subset(fsi_2008, select = c(country,year,e3_human_flight_and_brain_drain,c3_group_grievance))
fsi_2009b=subset(fsi_2009, select = c(country,year,e3_human_flight_and_brain_drain,c3_group_grievance))
fsi_2010b=subset(fsi_2010, select = c(country,year,e3_human_flight_and_brain_drain,c3_group_grievance))
fsi_2011b=subset(fsi_2011, select = c(country,year,e3_human_flight_and_brain_drain,c3_group_grievance))
fsi_2012b=subset(fsi_2012, select = c(country,year,e3_human_flight_and_brain_drain,c3_group_grievance))
fsi_2013b=subset(fsi_2013, select = c(country,year,e3_human_flight_and_brain_drain,c3_group_grievance))
fsi_2014b=subset(fsi_2014, select = c(country,year,e3_human_flight_and_brain_drain,c3_group_grievance))
fsi_2015b=subset(fsi_2015, select = c(country,year,e3_human_flight_and_brain_drain,c3_group_grievance))
fsi_2016b=subset(fsi_2016, select = c(country,year,e3_human_flight_and_brain_drain,c3_group_grievance))
fsi_2017b=subset(fsi_2017, select = c(country,year,e3_human_flight_and_brain_drain,c3_group_grievance))
fsi_2018b=subset(fsi_2018, select = c(country,year,e3_human_flight_and_brain_drain,c3_group_grievance))
#fsi_2019b=subset(fsi_2019, select = c(country,year,e3_human_flight_and_brain_drain,c3_group_grievance))
#fsi_2020b=subset(fsi_2020, select = c(country,year,e3_human_flight_and_brain_drain,c3_group_grievance))
#fsi_2021b=subset(fsi_2021, select = c(country,year,e3_human_flight_and_brain_drain,c3_group_grievance))

###Merge 

fsi = rbind(fsi_2007b,fsi_2008b,fsi_2009b,fsi_2010b,fsi_2011b,fsi_2012b,fsi_2013b,fsi_2014b,fsi_2015b,fsi_2016b,fsi_2017b,fsi_2018b)####
#Check Data
#####
unique(fsi$country)

#sort countries alphabetically

fsi[order(rownames(fsi)), ]

fsi=fsi[order(fsi[,"country"]), ]

sort(unique(fsi$country))

                
#change names 

#clean mismatch country names
fsi$country[fsi$country=="Bahamas"] = "Bahamas, The"
fsi$country[fsi$country=="Congo Democratic Republic"] = "Congo, Dem. Rep."
fsi$country[fsi$country=="Congo Republic"] = "Congo, Rep."
fsi$country[fsi$country=="Gambia"] = "Gambia, The"
fsi$country[fsi$country=="Guinea Bissau"] = "Guinea-Bissau"
fsi$country[fsi$country=="Israel and West Bank"] = "Israel"
fsi$country[fsi$country=="Venezuela"] = "Venezuela, RB"
fsi$country[fsi$country=="Kyrgyz Republic"] = "Kyrgyzstan"
fsi$country[fsi$country=="Trinidad and Tobago"] = "Trinidad"
#subset countries 
fsi<- subset(fsi, fsi$country  %in% country_list)

#check
sort(unique(fsi$country))


###############################
#World development indicators data
##################################


wdi <- read_excel("C:/Users/catis/Box/Catalina and Natasha/PS and Emigration/Datasets/Raw Data/Data_Extract_From_World_Development_Indicators.xlsx")
wdi

#Clean Names
#####
wdi <- janitor::clean_names(wdi)

#Subset the columns 
wdi= wdi %>% select( time,
                     country_name,
                     population_density_people_per_sq_km_of_land_area_en_pop_dnst,
                     population_ages_0_14_total_sp_pop_0014_to,
                     trade_percent_of_gdp_ne_trd_gnfs_zs,
                     population_growth_annual_percent_sp_pop_grow)
##remane colums
names(wdi)[1] = "year"
names(wdi)[2] = "country"
names(wdi)[3] = "density"
names(wdi)[4] = "pop014"
names(wdi)[5] = "trade"
names(wdi)[6] = "popgrwt"

#subset year
wdi=subset(wdi,wdi$year>="2007" & wdi$year<="2018")

#round 
#wdi$density=as.numeric(wdi$density)


#subset countries 

unique(wdi$country)
wdi$country[wdi$country=="Yemen, Rep."] = "Yemen"
wdi$country[wdi$country=="Cabo Verde"] = "Cape Verde"
wdi$country[wdi$country=="Korea, Dem. People's Rep."] = "North Korea"
wdi$country[wdi$country=="Korea, Rep."] = "South Korea"
wdi$country[wdi$country=="Russian Federation"] = "Russia"
wdi$country[wdi$country=="Egypt, Arab Rep."] = "Egypt"
wdi$country[wdi$country=="Iran, Islamic Rep.."] = "Iran"
wdi$country[wdi$country=="Kyrgyz Republic"] = "Kyrgyzstan"
wdi$country[wdi$country=="Trinidad and Tobago"] = "Trinidad"
wdi$country[wdi$country=="Iran, Islamic Rep."] = "Iran"
wdi$country[wdi$country=="Syrian Arab Republic"] = "Syria"
#change country names

wdi<- subset(wdi, wdi$country  %in% country_list)
unique(wdi$country)

#wdi$density=round(wdi$density,6)
wdi$density=as.numeric(wdi$density)
wdi$pop014=as.numeric(wdi$pop014)
wdi$trade=as.numeric(wdi$trade)
#wdi$trade=round(wdi$trade,6)
wdi$popgrwt=as.numeric(wdi$popgrwt)
#wdi$popgrwt=round(wdi$popgrwt,6)



############################
#Polity Data
###############################

####
##polity5 data set
#######

load("C:/Users/catis/Downloads/polityIV.rda")

p5=polityIV

#check data
colnames(p5)

unique(p5$polityIV_country)
unique(p5$polity)
unique(p5$polity2)
unique(p5$year)


#subset columns we need

p5 = p5 %>% select(polityIV_country, year,polity, polity2)


##remane colums
names(p5)[1] = "country"
names(p5)[2] = "year"
names(p5)[3] = "polity4"
names(p5)[4] = "polity5"
#sort countries alphabetically

p5[order(rownames(p5)), ]

p5=p5[order(p5[,'country']), ]


#subset years 2006-2018

p5=subset(p5,p5$year>="2007" & p5$year<="2018")

#subset countries 
unique(p5$country)
#169

#change country name
unique(p5$country)
p5$country[p5$country=="Myanmar (Burma)"] = "Myanmar"
p5$country[p5$country=="Congo Kinshasa"] = "Congo, Rep."
p5$country[p5$country=="Korea North"] = "North Korea"
p5$country[p5$country=="Korea South"] = "South Korea"
p5$country[p5$country=="Venezuela"] = "Venezuela, RB"
p5$country[p5$country=="Congo Brazzaville"] = "Congo, Dem. Rep."
p5$country[p5$country=="Cote D'Ivoire"] = "Cote d'Ivoire"
p5$country[p5$country=="Gambia"] = "Gambia, The"
p5$country[p5$country=="Trinidad and Tobago"] = "Trinidad"
p5<- subset(p5, p5$country  %in% country_list)

#############
#Fraccionalization Data
##############

f <- read.csv("C:/Users/catis/Box/Catalina and Natasha/PS and Emigration/Datasets/Raw Data/HIEF_data.csv")
f
#clean names
f <- janitor::clean_names(f)


#Subset years
f=subset(f,f$year>="2007" & f$year<="2018")

#Sort countries alphabetically
f[order(rownames(f)), ]
f=f[order(f[,'country']), ]

#check data
#155
unique(f$country)
unique(f$year)
#change contry names
f$country[f$country=="Congo"] = "Congo, Rep."
f$country[f$country=="Democratic People's Republic of Korea"] = "North Korea"
f$country[f$country=="Republic of Korea"] = "South Korea"
f$country[f$country=="Democratic Republic of Congo"] = "Congo, Dem. Rep."
f$country[f$country=="Democratic Republic of Vietnam"] = "Vietnam"
f$country[f$country=="German Federal Republic" ] = "Germany"
f$country[f$country=="Gambia"] = "Gambia, The"
f$country[f$country=="Trinidad and Tobago"] = "Trinidad"
f$country[f$country=="United States of America"] = "United States"
f$country[f$country=="Venezuela"] = "Venezuela, RB"
f$country[f$country=="Yemen Arab Republic"] = "Yemen"
#subset countries 
f<- subset(f, f$country  %in% country_list)

##########
#Stability 
##########
s <- read_excel("C:/Users/catis/Box/Catalina and Natasha/PS and Emigration/Datasets/Raw Data/Data_Extract_From_Worldwide_Governance_Indicators.xlsx")

#clean names
s <- janitor::clean_names(s)
colnames(s)

#Select columns
s = s %>% select(time,country_name,political_stability_and_absence_of_violence_terrorism_estimate_pv_est)
#Change Names
names(s)[1] = "year"
names(s)[2] = "country"
names(s)[3] = "stability"

#Order alphabetically
s[order(rownames(s)), ]
s=s[order(s[,'country']), ]

#round numbers
s$stability=as.numeric(s$stability)
#s=s[-c(4709:4713),]

#154 change country names
unique(s$country)
s$country[s$country=="Yemen, Rep."] = "Yemen"
s$country[s$country=="Cabo Verde"] = "Cape Verde"
s$country[s$country=="Trinidad and Tobago"] = "Trinidad"
s$country[s$country=="Russian Federation"] = "Russia"
s$country[s$country=="Korea, Dem. People's Rep."] = "North Korea"
s$country[s$country=="Korea, Rep."] = "South Korea"
s$country[s$country=="Iran, Islamic Rep."] = "Iran"
s$country[s$country=="Egypt, Arab Rep."] = "Egypt"
s$country[s$country=="Slovakia"] = "Slovak Republic"
s$country[s$country=="Kyrgyz Republic"] = "Kyrgyzstan"
s$country[s$country=="Syrian Arab Republic"] = "Syria"
#subset years
s=subset(s,s$year>="2007" & s$year<="2018")

#subset countries 
s<- subset(s, s$country  %in% country_list)

###########################
#Freedom House
#########################
remotes::install_github("xmarquez/democracyData")
library(democracyData)

fh <- download_fh(verbose = FALSE)


#subset
fh <- fh %>% select(fh_country,year,fh_total)

#select years
fh=subset(fh,fh$year>="2007" & fh$year<="2018")

names(fh)[1]="country"

#countries
unique(fh$country)

fh$country[fh$country=="Cabo Verde"] = "Cape Verde"
fh$country[fh$country=="Congo (Brazzaville)"] = "Congo, Rep."
fh$country[fh$country=="Congo (Kinshasa)"] = "Congo, Dem. Rep."
fh$country[fh$country=="The Gambia"] = "Gambia, The"
fh$country[fh$country=="Slovakia"] = "Slovak Republic"
fh$country[fh$country=="Venezuela"] = "Venezuela, RB"
fh<- subset(fh, fh$country  %in% country_list)
#GDP 
#########

pwt100 <- read_excel("C:/Users/catis/Downloads/pwt100.xlsx", sheet = "Data")

##subset columns
colnames(pwt100)
gdp= pwt100 %>% select(country, year,rgdpna)

#subset by year 
gdp=subset(gdp,gdp$year>="2007" & gdp$year<="2018")


#check data
sort(unique(gdp$country))
gdp$country[gdp$country=="Slovakia"] = "Slovak Republic"
gdp$country[gdp$country=="Bolivia (Plurinational State of)"] = "Bolivia"
gdp$country[gdp$country=="D.R. of the Congo"] = "Congo, Dem. Rep."
gdp$country[gdp$country=="Congo"] = "Congo, Rep."
gdp$country[gdp$country=="Gambia"] = "Gambia, The"
gdp$country[gdp$country=="China, Hong Kong SAR"] = "China"
gdp$country[gdp$country=="Iran (Islamic Republic of)"] = "Iran"
gdp$country[gdp$country=="Republic of Korea"] = "South Korea"
gdp$country[gdp$country=="Russian Federation"] = "Russia"
gdp$country[gdp$country=="Venezuela (Bolivarian Republic of)"] = "Venezuela, RB"
gdp$country[gdp$country=="Viet Nam"] = "Vietnam"
gdp$country[gdp$country=="Trinidad and Tobago"] = "Trinidad"
gdp$country[gdp$country=="Cabo Verde"] = "Cape Verde"
gdp$country[gdp$country=="U.R. of Tanzania: Mainland"] = "Tanzania"
gdp$country[gdp$country=="Côte d'Ivoire"] = "Cote d'Ivoire"
gdp$country[gdp$country=="Republic of Moldova"] = "Moldova"
gdp$country[gdp$country=="Kyrgyzstan"] = "Kyrgyzstan"
gdp$country[gdp$country=="Syrian Arab Republic"] = "Syria"
#subset countries 
gdp<- subset(gdp, gdp$country  %in% country_list)
###
#Correlates of war
###

cw= NTD %>% select(civilw, country)

#subset countries 
cw<- subset(cw, cw$country  %in% country_list)

cw=distinct(cw, country, civilw)

#unique
unique(cw$country)
cw$country[cw$country=="Russian"] = "Russia"
cw$country[cw$country=="Venezuela"] = "Venezuela, RB"
#MERGE
#######

d <- merge(gdp, wdi, all = TRUE)
sort(unique(d$country))

d <- merge(d, s, all = TRUE)
sort(unique(d$country))

#d <- merge(d, f, all = TRUE)
#sort(unique(d$country))

d <- merge(d, fsi, all = TRUE)
sort(unique(d$country))

d <- merge(d, p5, all = TRUE)
sort(unique(d$country))

d <- merge(d, fh, all = TRUE)
sort(unique(d$country))
d <- merge(d, cw, by="country", all = TRUE)



d=d[-c(913), ]

d=[order(rownames(d)), ]
d=d[order(d[,'country']), ]
#income

#d$income=NA
#d$income[d$rgdpna > 12.995]=1
#d$income[d$rgdpna >= 1.045 && dd$rgdpna <= 12.995]=2
#d$income[d$rgdpna >= 0 & dd$rgdpna <=1.045]=3

table(d$income, exclude = NULL) 
#save dataset
write.xlsx(d,"C:/Users/catis/Box/Catalina and Natasha/PS and Emigration/Datasets/Clean Data//2007-2018.xlsx")


#summary statistics 
table2=describe(d)
table2

#save excel 

write.xlsx(table2, file="C:/Users/catis/Box/Catalina and Natasha/PS and Emigration/Analysis//model2_2007_2008.xlsx", sheetName = "summary statistics", 
           col.names = TRUE, row.names = TRUE, append = TRUE)


##correlation

dm2=data.matrix(d)


m2 = rcorr(as.matrix(dm2)) #correlation matrix
m2

m2.coeff = m2$r
m2.p = m2$P

## Graphical Correlation Matrix:
# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

#flaten correlation matrix
cor_matrix2=flattenCorrMatrix(m2$r, m2$P) 

cor_matrix2

#save cor matrix

writexl::write_xlsx(cor_matrix2, "C:/Users/catis/Box/Catalina and Natasha/PS and Emigration/Analysis//model2.xlsx", )
write.xlsx(cor_matrix2, file="C:/Users/catis/Box/Catalina and Natasha/PS and Emigration/Analysis//model2_2007_2008.xlsx", sheetName = "correlation matrix", 
           col.names = TRUE, row.names = TRUE, append = TRUE)



#emigration correlations

# Function to extract correlation coefficient and p-values
corrFunc <- function(var1, var2, data) {
  result = cor.test(data[,var1], data[,var2])
  data.frame(var1, var2, result[c("estimate","p.value","statistic","method")], 
             stringsAsFactors=FALSE)
}

## Pairs of variables for which we want correlations
vars = data.frame(v1=names(d)[9], v2=names(d)[-1])


# Apply corrFunc to all rows of vars
corrs2 = do.call(rbind, mapply(corrFunc, vars[,1], vars[,2], MoreArgs=list(data=d), 
                               SIMPLIFY=FALSE))

write.xlsx(corrs2, file="C:/Users/catis/Box/Catalina and Natasha/PS and Emigration/Analysis//model2_2007_2008.xlsx", sheetName = "emigration correlation", 
           col.names = TRUE, row.names = TRUE, append = TRUE)


#group griveance correlations

# Function to extract correlation coefficient and p-values


## Pairs of variables for which we want correlations
vars2= data.frame(v1=names(d)[10], v2=names(d)[-1])


# Apply corrFunc to all rows of vars
corrs3 = do.call(rbind, mapply(corrFunc, vars2[,1], vars2[,2], MoreArgs=list(data=d), 
                               SIMPLIFY=FALSE))


write.xlsx(corrs3, file="C:/Users/catis/Box/Catalina and Natasha/PS and Emigration/Analysis//model2_2007_2008.xlsx", sheetName = "group griveances correlation", 
           col.names = TRUE, row.names = TRUE, append = TRUE)
