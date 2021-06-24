library(openxlsx)
library(tidyr)


Altersklassen_delete <- c("Total der Sterbefälle unter dem 20. Lebensjahr","Total der Sterbefälle über dem 20. Lebensjahr","Total der sämtlichen Sterbefälle",
                          "Entsprechende Sterblichkeitsziffer per Jahr und 20 Einwohner")

data.schmid <- read.xlsx("data-raw/Schmid/Schmid_Tabelle_XX.xlsx") %>%
  fill(Altersklassen) %>%
  filter(!(Altersklassen %in% Altersklassen_delete))%>%
  select(-Total)%>%
  gather(.,Month,Number_death,3:14)%>%
  mutate(Age=Altersklassen,
         Age=replace(Age, Age=="Unter 1 Jahr","<1"),
         Age=replace(Age, Age=="1-4 Jahre","1-4"),
         Age=replace(Age, Age=="5-19 Jahre","5-9"),
         Age=replace(Age, Age=="20-39 Jahre","20-39"),
         Age=replace(Age, Age=="40-59 Jahre","40-59"),
         Age=replace(Age, Age=="60-79 Jahre ","60-79"),
         Age=replace(Age, Age=="80 Jahre und darüber",">79"),
         Month=replace(Month, Month=="Januar","January"),
         Month=replace(Month, Month=="Februar","February"),
         Month=replace(Month, Month=="März","March"),
         Month=replace(Month, Month=="Mai","May"),
         Month=replace(Month, Month=="Juni","June"),
         Month=replace(Month, Month=="Juli","July"),
         Month=replace(Month, Month=="Sept.","September"),
         Month=replace(Month, Month=="Oktober","October"),
         Month=replace(Month, Month=="Nov.","November"),
         Month=replace(Month, Month=="Dez.","December"))%>%
  select(Year=Jahre,Month,Number_death,Age)


saveRDS(data.schmid, "data/Schmid/Schmid_death.Rds")

