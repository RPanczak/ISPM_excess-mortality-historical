library(openxlsx)
library(tidyverse)
library(dplyr)

death_spain <-  read.xlsx("data-raw/INE/Spain_2020_death.xlsx",sheet=1)
pop_spain <-  read.xlsx("data-raw/INE/Spain_Population_age_2020.xlsx",sheet=1)
pop_spain_prev <- read_rds("data/INE/es_pop_year.Rds")
death_spain_prev <- read_rds("data/INE/es_deaths_month.Rds")


pop_spain_total <- pop_spain %>%
  dplyr::summarize(Population=sum(Population,na.rm=TRUE))%>%
  mutate(Year=2020,
         Population=as.integer(Population))

death_spain_monthly <- death_spain %>%
  gather(., Month, Deaths, 2:13, factor_key=TRUE) %>%
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
  gather(., Month, Deaths, 2:13, factor_key=TRUE) %>%
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


