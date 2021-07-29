# library(openxlsx)
library(tidyverse)
library(dplyr)
library(readxl)
library(janitor)
library(lubridate)


death_spain <-  suppressWarnings(read_excel("data-raw/INE/Spain_2020.xlsx",sheet=1,range="A8:AK119") %>%
  select(starts_with("...1" ) | starts_with("Total")) %>%
  rename(Age="...1",
         January=Total...4,
         February=Total...7,
         March=Total...10,
         April=Total...13,
         May=Total...16,
         June=Total...19,
         July=Total...22,
         August=Total...25,
         September=Total...28,
         October=Total...31,
         November=Total...34,
         December=Total...37) %>%
  mutate(January=as.integer(January),
         February=as.integer(February),
         March =as.integer(March ),
         April =as.integer(April ),
         May =as.integer(May ),
         June =as.integer(June ),
         July =as.integer(July ),
         August =as.integer(August),
         September =as.integer(September),
         October =as.integer(October),
         November =as.integer(November),
         December =as.integer(December),
         Age=str_remove_all(Age, " years old"),
         Age=str_remove_all(Age, " year old"),
         Age=str_remove_all(Age, " and over"))%>%
  mutate_if(is.integer, replace_na, 0))

death_spain_tmp <- death_spain %>%
  mutate(Age=as.numeric(Age)) %>%
  filter(Age >99)%>%
  mutate(Age=as.factor(Age))%>%
  adorn_totals("row")%>%
  filter(Age=="Total")%>%
  mutate(Age=replace(Age, Age=="Total","100"))

death_spain <- death_spain %>%
  mutate(Age=as.numeric(Age)) %>%
  filter(Age <100)%>%
  mutate(Age=as.factor(Age)) %>%
  rbind(death_spain_tmp)
  
  
pop_spain <-  read_excel("data-raw/INE/Spain_Population_age-2019.xlsx",sheet=1,range="A8:E212") %>%
  slice(., -(1:2)) %>%
  select(1,5) %>%
  rename(Population=`Both sexes...5`) %>%
  filter(!is.na(Population)) %>%
  mutate(Age=0:(nrow(.)-1)) %>%
  select(Age, Population)
  
pop_spain_prev <- read_rds("data/INE/es_pop_year.Rds")
death_spain_prev <- read_rds("data/INE/es_deaths_month.Rds")


pop_spain_total <- pop_spain %>%
  dplyr::summarize(Population=sum(Population,na.rm=TRUE))%>%
  mutate(Year=2020,
         Population=as.integer(Population))

death_spain_monthly <- death_spain %>%
  gather(., Month, Deaths, January:December, factor_key=TRUE) %>%
  group_by(Month) %>%
  dplyr::summarize(Deaths=sum(Deaths,na.rm=TRUE))%>%
  mutate(Year=2020,
         Deaths=as.integer(Deaths)) %>%
  left_join(pop_spain_total)%>%
  mutate(Date=ymd(paste0("2020-",Month,"-01")))%>%
  select(Year, Month, Date, Deaths, Population)


death_spain_monthly_new <- death_spain_prev %>%
  mutate(Month = as.factor(Month),
         Deaths=as.integer(Deaths),
         Population=as.integer(Population))%>%
  filter(!Year==2020)%>%
  rbind(death_spain_monthly)

write_rds(death_spain_monthly_new ,paste0("data/INE/es_deaths_month_new.Rds"))

death_spain_age <- death_spain %>%
  gather(., Month, Deaths, January:December, factor_key=TRUE) %>%
  mutate(Age=as.factor(Age))%>%
  group_by(Age) %>%
  dplyr::summarize(Deaths=sum(Deaths,na.rm=TRUE))%>%
  mutate(Year=as.integer(2020),
         Total=as.integer(Deaths),
         Country="Spain") %>%
  select(Country, Year,Age,Total)


write_rds(death_spain_age,paste0("data/INE/es_deaths_age_sex_2020_new.Rds"))


pop_spain_new <- pop_spain_prev %>%
  filter(!Year==2020)%>%
  rbind(pop_spain_total)


write_rds(pop_spain_new,paste0("data/INE/es_pop_year_new.Rds"))


