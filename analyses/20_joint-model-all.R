# install.packages(c(“StanHeaders”, “rstan”), type = “source”)

library(pacman)
p_load(tidyverse, lubridate, scales, magrittr, 
       doParallel, foreach, 
       rstan)

source("R/fn_global_serfling.R")
source("R/fn_global_serfling_stan.R")
source("R/fn_global_serfling_nb_stan.R")
source("R/fn_age_serfling_nb_stan.R")

set.seed(12345)
options(scipen = 999)
theme_set(theme_minimal())
registerDoParallel(cores = parallel::detectCores())
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Data 

## Monthly deaths with yearly pops

deaths_monthly <- bind_rows(
  read_rds("data/BfS/ch_deaths_month.Rds") %>% mutate(Country = "Switzerland"),
  read_rds("data/INE/es_deaths_month.Rds") %>% mutate(Country = "Sweden"),
  read_rds("data/SCB/se_deaths_month.Rds") %>% mutate(Country = "Spain") %>% 
    filter(Year >= 1908)
) %>% 
  arrange(Country, Year) %>% 
  mutate(si_one = sin(2*pi*Month/12),
         si_two = sin(4*pi*Month/12),
         co_one = cos(2*pi*Month/12),
         co_two = cos(4*pi*Month/12))

## Yearly deaths by sex and age group with pops

pop_yearly_age_sex <- bind_rows(
  read_rds("data/mortality_org/hmd_pop_age_sex.Rds"),
  
  read_rds("data/mortality_org/hmd_pop_age_sex.Rds") %>% 
    filter(Country == "Spain" & Year == 2019) %>% 
    mutate(Year = 2020),
  
  read_rds("data/mortality_org/hmd_pop_age_sex.Rds") %>% 
    filter(Country == "Switzerland" & Year == 2019) %>% 
    mutate(Year = 2020)) %>% 
  
  select(-Female, -Male) %>% 
  mutate(Age = ifelse(Age == "110+", "111", Age),
         Age = as.numeric(Age), 
         # Age_cat = cut(Age, 
         # breaks = c(0, 1, 5, 20, 40, 60, 80, 112),
         # # labels = c("<1", "1-4", "5-19", "20-39", "40-59", "60-79","80+"), 
         # right=FALSE), 
         Age_cat = cut(Age, 
                       breaks = c(seq(0, 90, 5), 113), 
                       # labels = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+")), 
                       right=FALSE)
  ) %>%
  group_by(Country, Year, Age_cat) %>% 
  summarise(Population = sum(Total)) %>% 
  ungroup() 

deaths_yearly_age_sex <- bind_rows(
  read_rds("data/mortality_org/hmd_deaths_age_sex.Rds"),
  
  read_rds("data/SCB/se_deaths_age_sex_2019.Rds") %>% 
    filter(Year == 2020),
  
  read_rds("data/INE/es_deaths_age_sex_2019.Rds"),
  
  read_rds("data/BfS/ch_deaths_age_sex_2019.Rds")
) %>% 
  select(-Female, -Male) %>% 
  mutate(Age = ifelse(Age == "110+", "111", Age),
         Age = as.numeric(Age), 
         # Age_cat = cut(Age, 
         # breaks = c(0, 1, 5, 20, 40, 60, 80, 112),
         # # labels = c("<1", "1-4", "5-19", "20-39", "40-59", "60-79","80+"), 
         # right=FALSE), 
         Age_cat = cut(Age, 
                       breaks = c(seq(0, 90, 5), 113), 
                       # labels = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+")), 
                       right=FALSE)
  ) %>%
  group_by(Country, Year, Age_cat) %>% 
  summarise(Deaths = sum(Total)) %>% 
  ungroup() %>% 
  left_join(pop_yearly_age_sex)

rm(pop_yearly_age_sex)

## Set up

year_smooth <- 5

pandemic <- c(1890, 1918, 2020)

pandemic_affected <- c(seq(1890 + 1, 1890 + year_smooth),
                       seq(1918 + 1, 1918 + year_smooth))


# Modelling

results_month <- tibble(Country = character(), 
                        Year = double(), Month = double(), 
                        Deaths = double(),
                        pred = double(), 
                        lower = double(), 
                        upper = double(),
                        excess_month = double(), 
                        excess_month_lower = double(), 
                        excess_month_upper = double(),
                        Model = character())

results_year <- tibble(Country = character(), 
                       Year = double(), 
                       pred = double(), 
                       lower = double(), 
                       upper = double(),
                       excess_year = double(), 
                       excess_year_lower = double(), 
                       excess_year_upper = double(),
                       Model = character())

results_age <- tibble(Country = character(), 
                      Year = double(), 
                      Age_cat = factor(), 
                      pred = double(), 
                      lower = double(), 
                      upper = double(),
                      excess_year = double(), 
                      excess_year_lower = double(), 
                      excess_year_upper = double(),
                      Model = character())

for (COUNTRY in unique(deaths_monthly$Country)) {
  # for (COUNTRY in c("Sweden")) {
  
  # #############################################
  # Data params preps
  
  YEARS <- deaths_monthly %>% 
    filter(Country == COUNTRY) %>% 
    summarize(MIN = min(Year))
  
  REG_DATA <- deaths_monthly %>% 
    filter(Country == COUNTRY)
  
  AGE_DATA <- deaths_yearly_age_sex %>% 
     filter(Country == COUNTRY)
  
  print(paste("Analysing", COUNTRY))
  
  for (YEAR in YEARS$MIN+5:2020) {
    # for (YEAR in 1918:1919) {
    
    print(paste("     Analysing year", YEAR))
    
    # #############################################
    # Model 1
    print("          Global Serfling")
    
    global_serfling <- fn_global_serfling(YEAR, REG_DATA)
    
    extract_month <- global_serfling$pred_total_deaths %>% 
      select(Country, Year, Month, Deaths, pred, lower, upper) %>% 
      mutate(excess_month = Deaths - pred,
             excess_month_upper = Deaths - upper,
             excess_month_lower = Deaths - lower) %>% 
      mutate(Model = "Global Serfling")
    
    results_month <- bind_rows(results_month, extract_month)
    
    # #############################################
    # Model 2
    print("          Global Serfling (Stan)")
    
    global_serfling_stan <- fn_global_serfling_stan(YEAR, REG_DATA)
    
    extract_month <- global_serfling_stan$pred_total_deaths %>% 
      select(Country, Year, Month, Deaths,
             pred, lower, upper,
             excess_month, excess_month_lower, excess_month_upper) %>% 
      mutate(Model = "Global Serfling (Stan)")
    
    results_month <- bind_rows(results_month, extract_month)
    
    extract_year <- global_serfling_stan$pred_total_deaths %>% 
      filter(row_number() == 1) %>% 
      select(Country, Year, 
             pred, lower, upper,
             excess_year, excess_year_lower, excess_year_upper) %>% 
      mutate(Model = "Global Serfling (Stan)")
    
    results_year <- bind_rows(results_year, extract_year)
    
    # #############################################
    # Model 3
    print("          Global Serfling (Stan, NB)")
    
    global_serfling_nb_stan <- fn_global_serfling_nb_stan(YEAR, REG_DATA)
    
    extract_month <- global_serfling_nb_stan$pred_total_deaths %>% 
      select(Country, Year, Month, Deaths,
             pred, lower, upper,
             excess_month, excess_month_lower, excess_month_upper) %>% 
      mutate(Model = "Global Serfling (Stan, NB)")
    
    results_month <- bind_rows(results_month, extract_month)
    
    extract_year <- global_serfling_nb_stan$pred_total_deaths %>% 
      filter(row_number() == 1) %>% 
      select(Country, Year, 
             pred, lower, upper,
             excess_year, excess_year_lower, excess_year_upper) %>% 
      mutate(Model = "Global Serfling (Stan, NB)")
    
    results_year <- bind_rows(results_year, extract_year)
    
    # #############################################
    # Model 4
    print("          Age Serfling (Stan, NB)")
    
    age_serfling_nb_stan <- fn_age_serfling_nb_stan(YEAR, REG_DATA, AGE_DATA)
    
    extract_month <- age_serfling_nb_stan$pred_total_deaths %>% 
      select(Country, Year, Month, Deaths,
             pred, lower, upper,
             excess_month, excess_month_lower, excess_month_upper) %>% 
      mutate(Model = "Age Serfling (Stan, NB)")
    
    results_month <- bind_rows(results_month, extract_month)
    
    extract_year <- age_serfling_nb_stan$pred_total_deaths %>% 
      filter(row_number() == 1) %>% 
      select(Country, Year, 
             pred, lower, upper,
             excess_year, excess_year_lower, excess_year_upper) %>% 
      mutate(Model = "Age Serfling (Stan, NB)")
    
    results_year <- bind_rows(results_year, extract_year)
    
    extract_age <- age_serfling_nb_stan$pred_grouped_deaths %>% 
      select(Country, Year, Age_cat, Deaths,
             pred, lower, upper,
             excess_year, excess_year_lower, excess_year_upper) %>% 
      mutate(Model = "Age Serfling (Stan, NB)")
    
    results_age <- bind_rows(results_age, extract_age)
    
  }
} 

write_rds(results_month, "data/results_month.Rds")
write_rds(results_year, "data/results_year.Rds")
write_rds(results_age, "data/results_age.Rds")
