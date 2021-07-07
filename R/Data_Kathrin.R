library(openxlsx)
library(dplyr)
library(tidyr)
library(readr)


data.year <- read.xlsx("data-raw/Kathrin/S1File.xlsx",sheet=1) %>%
  filter(Location=="Switzerland") %>%
  gather(.,Diagnosis,Number_death,Nr..death.TB.Lung:Nr..death.influenza)%>%
  mutate(Diagnosis=replace(Diagnosis, Diagnosis=="Nr..death.TB.Lung","TB"),
         Diagnosis=replace(Diagnosis, Diagnosis=="Nr..death.acute.lung.diseases","acut"),
         Diagnosis=replace(Diagnosis,Diagnosis=="Nr..death.influenza","influenza")) %>% 
  as_tibble()

write_rds(data.year, "data/Kathrin/Kathrin_death_years.Rds")

data.month <- read.xlsx("data-raw/Kathrin/S1File.xlsx",sheet=3) %>%
  filter(Location=="Switzerland") %>%
  select(1:7)%>%
  gather(.,Diagnosis,Number_death,Nr..death.TB.Lung:Nr..death.influenza)%>%
  mutate(Diagnosis=replace(Diagnosis, Diagnosis=="Nr..death.TB.Lung","TB"),
         Diagnosis=replace(Diagnosis, Diagnosis=="Nr..death.acute.lung.diseases","acut"),
         Diagnosis=replace(Diagnosis, Diagnosis=="Nr..death.influenza","influenza")) %>%
  mutate(Month_num = as.integer(factor(Month, levels = month.name))) %>% 
  mutate(Date = lubridate::ymd(paste0(Year, "-", Month_num, "-01"))) %>% 
  as_tibble()

write_rds(data.month, "data/Kathrin/Kathrin_death_month.Rds")

data.age <- read.xlsx("data-raw/Kathrin/S1File.xlsx",sheet=4) %>%
  filter(Location=="switzerland") %>%
  gather(.,Age,Number_death,5:14)%>%
  gather(.,Population2,Population,5:14)%>%
  select(-Population2)%>%
  mutate(Location=replace(Location, Location=="switzerland","Switzerland"),
         Age = gsub("Age:.","",as.character(Age)),
         Number_death=as.integer(Number_death)) %>% 
  as_tibble()

write_rds(data.age, "data/Kathrin/Kathrin_death_age.Rds")

# library(ggplot2)
# 
# ggplot(data.year, aes(x = Year, y = Number_death, fill = Sex)) +
#   geom_col() +
#   facet_grid(vars(Diagnosis)) +
#   theme_minimal()
# 
# ggplot(data.month, aes(x = Date, y = Number_death)) +
#   geom_col() +
#   facet_grid(vars(Diagnosis)) +
#   theme_minimal()
