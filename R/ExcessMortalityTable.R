library(dplyr)
library(openxlsx)


data_mort <- readRDS("data/expected_deaths_monthly.RdS")
head(data_mort)

pandemic_year <- c("1890","1918","1957","2020")
  
table_mort <- as.data.frame(data_mort) %>%
  select(Country,Year, deaths, pop, fit_flu_hc) %>%
  filter(Year %in% pandemic_year)%>%
 group_by(Year, Country) %>%
 summarise(Expected_Mortality=round(sum(fit_flu_hc),0),
           Deaths_num=sum(deaths),
           Cum_excess_death=round(sum(deaths)-sum(fit_flu_hc),0),
           Percent_excess_death=round((Cum_excess_death/Expected_Mortality)*100,1))%>%
  ungroup()

write.xlsx(table_mort,"res/Excess_Mortalty_table.xlsx", row.names=FALSE)

# 
# table_mort_month <- as.data.frame(data_mort) %>%
#   select(Country,Year, deaths, pop, fit_flu_hc,Month) %>%
#   filter(Year=="2020") %>%
#   group_by(Month, Country) %>%
#   summarise(Expected_Mortality=round(sum(fit_flu_hc),0),
#             Cum_excess_death=sum(deaths)-Expected_Mortality,
#             Percent_excess_death=round((Cum_excess_death/Expected_Mortality)*100,1))%>%
#   ungroup()
# 
# write.xlsx(table_mort_month,"C:/Users/kmatth/Dropbox/SpanischeGrippe/ExcessMortality/Excess_Mortalty_Covid19_Month.xlsx", row.names = 
#              FALSE)
# 
# 
