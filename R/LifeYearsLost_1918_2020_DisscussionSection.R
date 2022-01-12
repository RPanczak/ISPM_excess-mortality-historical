# comment : this is the "back of the envelope" calculation on life years lost



getwd()
require(demography)
require(HMDHFDplus)



# get life expectancy tables from mortality.org : the table is called "bltper_1x1"
# the age-conditional life expectancy is in column ex
# add country variable

# text file with password or modify password in calls below
password <- readr::read_file("MZsecrets.txt")
password




Switzerland <- readHMDweb(CNTRY = "CHE", item ="bltper_1x1" , username="zwahlen@ispm.unibe.ch", password=password,
           fixup=FALSE)
Switzerland$country <- "CHE"


Spain <- readHMDweb(CNTRY ="ESP", item ="bltper_1x1" , username="zwahlen@ispm.unibe.ch", password=password,
                          fixup=FALSE)

Spain$country  <- "SPAIN"

Sweden <- readHMDweb(CNTRY = "SWE", item ="bltper_1x1" , username="zwahlen@ispm.unibe.ch", password=password,
                         fixup=FALSE)
Sweden$country <-"Schwe"

countries <- rbind(Switzerland,Spain, Sweden)
table(countries$country)


countr <- subset(countries, Year==1917 | Year==2018)
table(countr$Age)
countr <- subset(countr, Age!="110+")
countr$Age <- as.numeric(countr$Age)
summary(countr)
table(countr$Year)

# replace 1917 with 1918 and 2018 with 2020 - for later merge on 
# excess deaths by age

countr$Year[countr$Year == 1917] <- 1918
table(countr$Year)
countr$Year[countr$Year == 2018] <- 2020
table(countr$Year)
# create age groups


countr$Age_cat=cut(countr$Age, breaks=c(seq(from=0, to=90, by=10), 120), right = FALSE)
table(countr$Age_cat)

# check Age by Age_cat
aggregate(Age  ~ Age_cat, data = countr, FUN = summary)
table(countr$Year)


# now aggregate by Age_cat and get mean life expectancy for that age group

countryshort <- subset(countr, select =c(Year, Age_cat, ex, country))
table(countryshort$Year)

df <- aggregate(cbind(Age,ex) ~ Year+Age_cat+ country, data = countr, FUN = mean)



# Bring in age results from github - could be improved

resage <- readRDS("data/results_age.Rds") 
table(resage$Age_cat)
summary(resage)

resage2 <-subset(resage,select = c(Year, Country, Age_cat, Population, excess_grouped_deaths, yearly_excess_grouped_deaths))
resage3 <- subset(resage2, Year==1918 | Year==2020)

summary(resage3)
table(resage3$Country)
str(resage3$Country)

resage3$country <- ""
resage3$country[resage3$Country=="Switzerland"] <- "CHE"
resage3$country[resage3$Country=="Spain"] <- "SPAIN"
resage3$country[resage3$Country=="Sweden"] <- "Schwe"
table(resage3$country)
resage3 <- subset(resage3, select = -Country)
# check numeric values of Age_cat before merging with mean life exp file
table(resage3$country)
table(as.numeric(resage3$Age_cat))
table(df$country)
table(as.numeric(df$Age_cat))
names(resage3)
names(df)
?merge

dataprep <- merge(resage3, df, by=c("Age_cat","country", "Year"))


summary(dataprep$ex)
dataprep$lifeYL <- dataprep$excess_grouped_deaths*dataprep$ex 


## now collapse /aggregate by summing over Age_cat lifeYL and Population over year and country and then divide by population size


lifeyLostYear<- aggregate(cbind(Population,lifeYL) ~ Year+country, data = dataprep, FUN = sum, na.rm = TRUE)


names(lifeyLostYear)
summary(lifeyLostYear)

# now divide Lost years by Population size per 10^6 

lifeyLostYear$LYlostperMillion <- 10^6*lifeyLostYear$lifeYL/lifeyLostYear$Population 


#  now the ratio of 1918 lYlost compared to 2020 by country


ratioCH=lifeyLostYear$LYlostperMillion[lifeyLostYear$country=="CHE" & lifeyLostYear$Year==1918]/lifeyLostYear$LYlostperMillion[lifeyLostYear$country=="CHE" & lifeyLostYear$Year==2020]
ratioCH

ratioSchwe=lifeyLostYear$LYlostperMillion[lifeyLostYear$country=="Schwe" & lifeyLostYear$Year==1918]/lifeyLostYear$LYlostperMillion[lifeyLostYear$country=="Schwe" & lifeyLostYear$Year==2020]
ratioSchwe

ratioSpain=lifeyLostYear$LYlostperMillion[lifeyLostYear$country=="SPAIN" & lifeyLostYear$Year==1918]/lifeyLostYear$LYlostperMillion[lifeyLostYear$country=="SPAIN" & lifeyLostYear$Year==2020]
ratioSpain










