library(openxlsx)
library(dplyr)


data.year <- read.xlsx("data-raw/Kathrin/S1File.xlsx",sheet=1) %>%
  filter(Location=="Switzerland") %>%
  gather(.,Diagnosis,Number_death,Nr..death.TB.Lung:Nr..death.influenza)%>%
  mutate(Diagnosis=replace(Diagnosis, Diagnosis=="Nr..death.TB.Lung","TB"),
         Diagnosis=replace(Diagnosis, Diagnosis=="Nr..death.acute.lung.diseases","acut"),
         Diagnosis=replace(Diagnosis,Diagnosis=="Nr..death.influenza","influenza"))
saveRDS(data.year, "data/Kathrin/Kathrin_death_years.Rds")

data.month <- read.xlsx("data-raw/Kathrin/S1File.xlsx",sheet=3) %>%
  filter(Location=="Switzerland") %>%
  select(1:7)%>%
  gather(.,Diagnosis,Number_death,Nr..death.TB.Lung:Nr..death.influenza)%>%
  mutate(Diagnosis=replace(Diagnosis, Diagnosis=="Nr..death.TB.Lung","TB"),
         Diagnosis=replace(Diagnosis, Diagnosis=="Nr..death.acute.lung.diseases","acut"),
         Diagnosis=replace(Diagnosis, Diagnosis=="Nr..death.influenza","influenza"))
saveRDS(data.month, "data/Kathrin/Kathrin_death_month.Rds")

data.age <- read.xlsx("data-raw/Kathrin/S1File.xlsx",sheet=4) %>%
  filter(Location=="switzerland") %>%
  gather(.,Age,Number_death,5:14)%>%
  gather(.,Population2,Population,5:14)%>%
  select(-Population2)%>%
  mutate(Location=replace(Location, Location=="switzerland","Switzerland"),
         Age = gsub("Age:.","",as.character(Age)),
         Number_death=as.integer(Number_death))

saveRDS(data.age, "data/Kathrin/Kathrin_death_age.Rds")