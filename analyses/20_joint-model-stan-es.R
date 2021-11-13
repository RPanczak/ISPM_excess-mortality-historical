

# 0 - Set up ----

# Libs
library(pacman)
p_load(tidyverse, magrittr, rstan)
source("R/fn_global_serfling_nb_stan.R")
source("R/fn_age_serfling_nb_stan.R")
source("R/fn_global_serfling_nb_stan_21.R")
source("R/fn_age_serfling_nb_stan_21.R")
source("R/fn_global_serfling_nb_stan_sst1.R")
source("R/fn_age_serfling_nb_stan_sst1.R")
source("R/fn_global_serfling_nb_stan_21_sst1.R")
source("R/fn_age_serfling_nb_stan_21_sst1.R")

## Paths
# setwd("~/projects/ISPM_excess-mortality/")
# setwd("C:/projects/ISPM_excess-mortality/")
path0 = paste0("data/outputs_",Sys.Date(),"/")
dir.create(path0,showWarnings = FALSE)

# Options
set.seed(12345)
options(scipen = 999)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = FALSE)

## Data 
deaths_monthly <- read_rds("data/deaths_monthly.Rds") %>% 
  filter(Country == "Spain")
deaths_yearly_age_sex <- read_rds("data/deaths_yearly_age_sex.Rds") %>% 
  filter(Country == "Spain") 

## Years
year_smooth <- 5
pandemic <- c(1890, 1918, 1957, 2020)
pandemic_affected <- c(seq(1890 + 1, 1890 + year_smooth),
                       seq(1918 + 1, 1918 + year_smooth),
                       seq(1957 + 1, 1957 + year_smooth))


# 1 - Excluding pandemic years, observed population data, until 2020 ----

## Empty outputs
results_month <- tibble()
results_year <- tibble()
results_age <- tibble()

## Select years
YEARS <- deaths_monthly %>% 
  summarize(MIN = min(Year))

## Loop
for (YEAR in (YEARS$MIN+5):2020) {
  print(paste("     Analysing year", YEAR))
  
  ### Global model
  
  print("          Global Serfling (Stan, NB)")
  
  global_serfling_stan <- fn_global_serfling_nb_stan(YEAR, deaths_monthly, pandemic_years = pandemic, pop="obs")
  
  extract_month <- global_serfling_stan$pred_total_deaths %>% 
    select(Country, Year, Month, Deaths,
           pred, lower, upper, n_eff, Rhat,
           excess_month, excess_month_lower, excess_month_upper) %>% 
    mutate(Model = "Global Serfling (Stan, NB)",
           mutate(across(pred:excess_month_upper, round)))
  
  results_month <- bind_rows(results_month, extract_month)
  
  extract_year <- global_serfling_stan$pred_total_deaths %>% 
    filter(row_number() == 1) %>% 
    select(Country, Year, 
           excess_year, excess_year_lower, excess_year_upper) %>% 
    mutate(Model = "Global Serfling (Stan, NB)",
           mutate(across(excess_year:excess_year_upper, round)))
  
  results_year <- bind_rows(results_year, extract_year)
  
  ### Age model
  
  print("          Age Serfling (Stan, NB)")
  
  age_serfling_nb_stan <- fn_age_serfling_nb_stan(YEAR, deaths_monthly, deaths_yearly_age_sex, pandemic_years = pandemic, pop="obs")
  
  extract_month <- age_serfling_nb_stan$pred_total_deaths %>% 
    select(Country, Year, Month, Deaths,
           pred, lower, upper, n_eff, Rhat,
           excess_month, excess_month_lower, excess_month_upper) %>% 
    mutate(Model = "Age Serfling (Stan, NB)",
           mutate(across(pred:excess_month_upper, round)))
  
  results_month <- bind_rows(results_month, extract_month)
  
  extract_year <- age_serfling_nb_stan$pred_total_deaths %>% 
    filter(row_number() == 1) %>% 
    select(Country, Year, 
           excess_year, excess_year_lower, excess_year_upper) %>% 
    mutate(Model = "Age Serfling (Stan, NB)",
           mutate(across(excess_year:excess_year_upper, round)))
  
  results_year <- bind_rows(results_year, extract_year)
  
  extract_age <- age_serfling_nb_stan$pred_grouped_deaths %>% 
    select(Country, Year, Age_cat, Deaths,
           pred, lower, upper,
           excess_year, excess_year_lower, excess_year_upper) %>% 
    mutate(Model = "Age Serfling (Stan, NB)",
           mutate(across(pred:excess_year_upper, round)))
  
  results_age <- bind_rows(results_age, extract_age)
  
  ### Save
  rm(extract_month, extract_year, extract_age)
  write_rds(results_month, paste0(path0,"Spain_results_month.Rds"))
  write_rds(results_year, paste0(path0,"Spain_results_year.Rds"))
  write_rds(results_age, paste0(path0,"Spain_results_age.Rds"))
}






# 2 - Excluding pandemic years, observed population data, 2021 ----


## Global model

print("Global Serfling (Stan, NB)")

global_serfling_stan <- fn_global_serfling_nb_stan_21(2021, deaths_monthly, pandemic_years = pandemic)

extract_month <- global_serfling_stan$pred_total_deaths %>% 
  select(Country, Year, Month, Deaths,
         pred, lower, upper, n_eff, Rhat,
         excess_month, excess_month_lower, excess_month_upper) %>% 
  mutate(Model = "Global Serfling (Stan, NB)",
         mutate(across(pred:excess_month_upper, round)))

results_month <- bind_rows(results_month, extract_month)

extract_year <- global_serfling_stan$pred_total_deaths %>% 
  filter(row_number() == 1) %>% 
  select(Country, Year, 
         excess_year, excess_year_lower, excess_year_upper) %>% 
  mutate(Model = "Global Serfling (Stan, NB)",
         mutate(across(excess_year:excess_year_upper, round)))

results_year <- bind_rows(results_year, extract_year)

## Age model

print("Age Serfling (Stan, NB)")

age_serfling_nb_stan <- fn_age_serfling_nb_stan_21(2021, deaths_monthly, deaths_yearly_age_sex, pandemic_years = pandemic)

extract_month <- age_serfling_nb_stan$pred_total_deaths %>% 
  select(Country, Year, Month, Deaths,
         pred, lower, upper, n_eff, Rhat,
         excess_month, excess_month_lower, excess_month_upper) %>% 
  mutate(Model = "Age Serfling (Stan, NB)",
         mutate(across(pred:excess_month_upper, round)))

results_month <- bind_rows(results_month, extract_month)

extract_year <- age_serfling_nb_stan$pred_total_deaths %>% 
  filter(row_number() == 1) %>% 
  select(Country, Year, 
         excess_year, excess_year_lower, excess_year_upper) %>% 
  mutate(Model = "Age Serfling (Stan, NB)",
         mutate(across(excess_year:excess_year_upper, round)))

results_year <- bind_rows(results_year, extract_year)

extract_age <- age_serfling_nb_stan$pred_grouped_deaths %>% 
  select(Country, Year, Age_cat, Deaths,
         pred, lower, upper,
         excess_year, excess_year_lower, excess_year_upper) %>% 
  mutate(Model = "Age Serfling (Stan, NB)",
         mutate(across(pred:excess_year_upper, round)))

results_age <- bind_rows(results_age, extract_age)

rm(extract_month, extract_year, extract_age)

write_rds(results_month, paste0(path0,"Spain_results_month.Rds"))
write_rds(results_year, paste0(path0,"Spain_results_year.Rds"))
write_rds(results_age, paste0(path0,"Spain_results_age.Rds"))



# 3 - Excluding pandemic years, expected population data, until 2020 ----

## Empty outputs
results_month <- tibble()
results_year <- tibble()
results_age <- tibble()

## Select years
YEARS <- deaths_monthly %>% 
  summarize(MIN = min(Year))

## Loop
for (YEAR in (YEARS$MIN+5):2020) {
  print(paste("     Analysing year", YEAR))
  
  ### Global model
  
  print("          Global Serfling (Stan, NB)")
  
  global_serfling_stan <- fn_global_serfling_nb_stan(YEAR, deaths_monthly, pandemic_years = pandemic, pop="exp")
  
  extract_month <- global_serfling_stan$pred_total_deaths %>% 
    select(Country, Year, Month, Deaths,
           pred, lower, upper, n_eff, Rhat,
           excess_month, excess_month_lower, excess_month_upper) %>% 
    mutate(Model = "Global Serfling (Stan, NB)",
           mutate(across(pred:excess_month_upper, round)))
  
  results_month <- bind_rows(results_month, extract_month)
  
  extract_year <- global_serfling_stan$pred_total_deaths %>% 
    filter(row_number() == 1) %>% 
    select(Country, Year, 
           excess_year, excess_year_lower, excess_year_upper) %>% 
    mutate(Model = "Global Serfling (Stan, NB)",
           mutate(across(excess_year:excess_year_upper, round)))
  
  results_year <- bind_rows(results_year, extract_year)
  
  ### Age model
  
  print("          Age Serfling (Stan, NB)")
  
  age_serfling_nb_stan <- fn_age_serfling_nb_stan(YEAR, deaths_monthly, deaths_yearly_age_sex, pandemic_years = pandemic, pop="exp")
  
  extract_month <- age_serfling_nb_stan$pred_total_deaths %>% 
    select(Country, Year, Month, Deaths,
           pred, lower, upper, n_eff, Rhat,
           excess_month, excess_month_lower, excess_month_upper) %>% 
    mutate(Model = "Age Serfling (Stan, NB)",
           mutate(across(pred:excess_month_upper, round)))
  
  results_month <- bind_rows(results_month, extract_month)
  
  extract_year <- age_serfling_nb_stan$pred_total_deaths %>% 
    filter(row_number() == 1) %>% 
    select(Country, Year, 
           excess_year, excess_year_lower, excess_year_upper) %>% 
    mutate(Model = "Age Serfling (Stan, NB)",
           mutate(across(excess_year:excess_year_upper, round)))
  
  results_year <- bind_rows(results_year, extract_year)
  
  extract_age <- age_serfling_nb_stan$pred_grouped_deaths %>% 
    select(Country, Year, Age_cat, Deaths,
           pred, lower, upper,
           excess_year, excess_year_lower, excess_year_upper) %>% 
    mutate(Model = "Age Serfling (Stan, NB)",
           mutate(across(pred:excess_year_upper, round)))
  
  results_age <- bind_rows(results_age, extract_age)
  
  ### Save
  rm(extract_month, extract_year, extract_age)
  write_rds(results_month, paste0(path0,"Spain_results_month_exp.Rds"))
  write_rds(results_year, paste0(path0,"Spain_results_year_exp.Rds"))
  write_rds(results_age, paste0(path0,"Spain_results_age_exp.Rds"))
}






# 4 - Excluding pandemic years, observed population data, 2021 ----


## Global model

print("Global Serfling (Stan, NB)")

global_serfling_stan <- fn_global_serfling_nb_stan_21(2021, deaths_monthly, pandemic_years = pandemic, pop="exp")

extract_month <- global_serfling_stan$pred_total_deaths %>% 
  select(Country, Year, Month, Deaths,
         pred, lower, upper, n_eff, Rhat,
         excess_month, excess_month_lower, excess_month_upper) %>% 
  mutate(Model = "Global Serfling (Stan, NB)",
         mutate(across(pred:excess_month_upper, round)))

results_month <- bind_rows(results_month, extract_month)

extract_year <- global_serfling_stan$pred_total_deaths %>% 
  filter(row_number() == 1) %>% 
  select(Country, Year, 
         excess_year, excess_year_lower, excess_year_upper) %>% 
  mutate(Model = "Global Serfling (Stan, NB)",
         mutate(across(excess_year:excess_year_upper, round)))

results_year <- bind_rows(results_year, extract_year)


## Age model

print("Age Serfling (Stan, NB)")

age_serfling_nb_stan <- fn_age_serfling_nb_stan_21(2021, deaths_monthly, deaths_yearly_age_sex, pandemic_years = pandemic, pop="exp")

extract_month <- age_serfling_nb_stan$pred_total_deaths %>% 
  select(Country, Year, Month, Deaths,
         pred, lower, upper, n_eff, Rhat,
         excess_month, excess_month_lower, excess_month_upper) %>% 
  mutate(Model = "Age Serfling (Stan, NB)",
         mutate(across(pred:excess_month_upper, round)))

results_month <- bind_rows(results_month, extract_month)

extract_year <- age_serfling_nb_stan$pred_total_deaths %>% 
  filter(row_number() == 1) %>% 
  select(Country, Year, 
         excess_year, excess_year_lower, excess_year_upper) %>% 
  mutate(Model = "Age Serfling (Stan, NB)",
         mutate(across(excess_year:excess_year_upper, round)))

results_year <- bind_rows(results_year, extract_year)

extract_age <- age_serfling_nb_stan$pred_grouped_deaths %>% 
  select(Country, Year, Age_cat, Deaths,
         pred, lower, upper,
         excess_year, excess_year_lower, excess_year_upper) %>% 
  mutate(Model = "Age Serfling (Stan, NB)",
         mutate(across(pred:excess_year_upper, round)))

results_age <- bind_rows(results_age, extract_age)

rm(extract_month, extract_year, extract_age)
write_rds(results_month, paste0(path0,"Spain_results_month_exp.Rds"))
write_rds(results_year, paste0(path0,"Spain_results_year_exp.Rds"))
write_rds(results_age, paste0(path0,"Spain_results_age_exp.Rds"))





# 5 - Including pandemic years, observed population data, until 2020 ----

## Empty outputs
results_month <- tibble()
results_year <- tibble()
results_age <- tibble()

## Select years
YEARS <- deaths_monthly %>% 
  summarize(MIN = min(Year))

## Loop
for (YEAR in (YEARS$MIN+5):2020) {
  print(paste("     Analysing year", YEAR))
  
  ### Global model
  
  print("          Global Serfling (Stan, NB)")
  
  global_serfling_stan <- fn_global_serfling_nb_stan(YEAR, deaths_monthly, pandemic_years = NULL, pop="obs")
  
  extract_month <- global_serfling_stan$pred_total_deaths %>% 
    select(Country, Year, Month, Deaths,
           pred, lower, upper, n_eff, Rhat,
           excess_month, excess_month_lower, excess_month_upper) %>% 
    mutate(Model = "Global Serfling (Stan, NB)",
           mutate(across(pred:excess_month_upper, round)))
  
  results_month <- bind_rows(results_month, extract_month)
  
  extract_year <- global_serfling_stan$pred_total_deaths %>% 
    filter(row_number() == 1) %>% 
    select(Country, Year, 
           excess_year, excess_year_lower, excess_year_upper) %>% 
    mutate(Model = "Global Serfling (Stan, NB)",
           mutate(across(excess_year:excess_year_upper, round)))
  
  results_year <- bind_rows(results_year, extract_year)
  
  ### Age model
  
  print("          Age Serfling (Stan, NB)")
  
  age_serfling_nb_stan <- fn_age_serfling_nb_stan(YEAR, deaths_monthly, deaths_yearly_age_sex, pandemic_years = NULL, pop="obs")
  
  extract_month <- age_serfling_nb_stan$pred_total_deaths %>% 
    select(Country, Year, Month, Deaths,
           pred, lower, upper, n_eff, Rhat,
           excess_month, excess_month_lower, excess_month_upper) %>% 
    mutate(Model = "Age Serfling (Stan, NB)",
           mutate(across(pred:excess_month_upper, round)))
  
  results_month <- bind_rows(results_month, extract_month)
  
  extract_year <- age_serfling_nb_stan$pred_total_deaths %>% 
    filter(row_number() == 1) %>% 
    select(Country, Year, 
           excess_year, excess_year_lower, excess_year_upper) %>% 
    mutate(Model = "Age Serfling (Stan, NB)",
           mutate(across(excess_year:excess_year_upper, round)))
  
  results_year <- bind_rows(results_year, extract_year)
  
  extract_age <- age_serfling_nb_stan$pred_grouped_deaths %>% 
    select(Country, Year, Age_cat, Deaths,
           pred, lower, upper,
           excess_year, excess_year_lower, excess_year_upper) %>% 
    mutate(Model = "Age Serfling (Stan, NB)",
           mutate(across(pred:excess_year_upper, round)))
  
  results_age <- bind_rows(results_age, extract_age)
  
  ### Save
  rm(extract_month, extract_year, extract_age)
  write_rds(results_month, paste0(path0,"Spain_results_month_pand.Rds"))
  write_rds(results_year, paste0(path0,"Spain_results_year_pand.Rds"))
  write_rds(results_age, paste0(path0,"Spain_results_age_pand.Rds"))
}






# 6 - Including pandemic years, observed population data, 2021 ----


## Global model

print("Global Serfling (Stan, NB)")

global_serfling_stan <- fn_global_serfling_nb_stan_21(2021, deaths_monthly, pandemic_years = NULL)

extract_month <- global_serfling_stan$pred_total_deaths %>% 
  select(Country, Year, Month, Deaths,
         pred, lower, upper, n_eff, Rhat,
         excess_month, excess_month_lower, excess_month_upper) %>% 
  mutate(Model = "Global Serfling (Stan, NB)",
         mutate(across(pred:excess_month_upper, round)))

results_month <- bind_rows(results_month, extract_month)

extract_year <- global_serfling_stan$pred_total_deaths %>% 
  filter(row_number() == 1) %>% 
  select(Country, Year, 
         excess_year, excess_year_lower, excess_year_upper) %>% 
  mutate(Model = "Global Serfling (Stan, NB)",
         mutate(across(excess_year:excess_year_upper, round)))

results_year <- bind_rows(results_year, extract_year)

## Age model

print("Age Serfling (Stan, NB)")

age_serfling_nb_stan <- fn_age_serfling_nb_stan_21(2021, deaths_monthly, deaths_yearly_age_sex, pandemic_years = NULL)

extract_month <- age_serfling_nb_stan$pred_total_deaths %>% 
  select(Country, Year, Month, Deaths,
         pred, lower, upper, n_eff, Rhat,
         excess_month, excess_month_lower, excess_month_upper) %>% 
  mutate(Model = "Age Serfling (Stan, NB)",
         mutate(across(pred:excess_month_upper, round)))

results_month <- bind_rows(results_month, extract_month)

extract_year <- age_serfling_nb_stan$pred_total_deaths %>% 
  filter(row_number() == 1) %>% 
  select(Country, Year, 
         excess_year, excess_year_lower, excess_year_upper) %>% 
  mutate(Model = "Age Serfling (Stan, NB)",
         mutate(across(excess_year:excess_year_upper, round)))

results_year <- bind_rows(results_year, extract_year)

extract_age <- age_serfling_nb_stan$pred_grouped_deaths %>% 
  select(Country, Year, Age_cat, Deaths,
         pred, lower, upper,
         excess_year, excess_year_lower, excess_year_upper) %>% 
  mutate(Model = "Age Serfling (Stan, NB)",
         mutate(across(pred:excess_year_upper, round)))

results_age <- bind_rows(results_age, extract_age)

rm(extract_month, extract_year, extract_age)

write_rds(results_month, paste0(path0,"Spain_results_month_pand.Rds"))
write_rds(results_year, paste0(path0,"Spain_results_year_pand.Rds"))
write_rds(results_age, paste0(path0,"Spain_results_age_pand.Rds"))









# 7 - Excluding pandemic years, observed population data, until 2020, exclude highest and lowest of last 7 years ----

## Years
year_smooth <- 7
pandemic <- c(1890, 1918, 1957, 2020)
pandemic_affected <- c(seq(1890 + 1, 1890 + year_smooth),
                       seq(1918 + 1, 1918 + year_smooth),
                       seq(1957 + 1, 1957 + year_smooth))


## Empty outputs
results_month <- tibble()
results_year <- tibble()
results_age <- tibble()

## Select years
YEARS <- deaths_monthly %>% 
  summarize(MIN = min(Year))

## Loop
for (YEAR in (YEARS$MIN+7):2020) {
  print(paste("     Analysing year", YEAR))
  
  ### Global model
  
  print("          Global Serfling (Stan, NB)")
  
  global_serfling_stan <- fn_global_serfling_nb_stan_sst1(YEAR, deaths_monthly, pandemic_years = pandemic, pop="obs")
  
  extract_month <- global_serfling_stan$pred_total_deaths %>% 
    select(Country, Year, Month, Deaths,
           pred, lower, upper, n_eff, Rhat,
           excess_month, excess_month_lower, excess_month_upper) %>% 
    mutate(Model = "Global Serfling (Stan, NB)",
           mutate(across(pred:excess_month_upper, round)))
  
  results_month <- bind_rows(results_month, extract_month)
  
  extract_year <- global_serfling_stan$pred_total_deaths %>% 
    filter(row_number() == 1) %>% 
    select(Country, Year, 
           excess_year, excess_year_lower, excess_year_upper) %>% 
    mutate(Model = "Global Serfling (Stan, NB)",
           mutate(across(excess_year:excess_year_upper, round)))
  
  results_year <- bind_rows(results_year, extract_year)
  
  ### Age model
  
  print("          Age Serfling (Stan, NB)")
  
  age_serfling_nb_stan <- fn_age_serfling_nb_stan_sst1(YEAR, deaths_monthly, deaths_yearly_age_sex, pandemic_years = pandemic, pop="obs")
  
  extract_month <- age_serfling_nb_stan$pred_total_deaths %>% 
    select(Country, Year, Month, Deaths,
           pred, lower, upper, n_eff, Rhat,
           excess_month, excess_month_lower, excess_month_upper) %>% 
    mutate(Model = "Age Serfling (Stan, NB)",
           mutate(across(pred:excess_month_upper, round)))
  
  results_month <- bind_rows(results_month, extract_month)
  
  extract_year <- age_serfling_nb_stan$pred_total_deaths %>% 
    filter(row_number() == 1) %>% 
    select(Country, Year, 
           excess_year, excess_year_lower, excess_year_upper) %>% 
    mutate(Model = "Age Serfling (Stan, NB)",
           mutate(across(excess_year:excess_year_upper, round)))
  
  results_year <- bind_rows(results_year, extract_year)
  
  extract_age <- age_serfling_nb_stan$pred_grouped_deaths %>% 
    select(Country, Year, Age_cat, Deaths,
           pred, lower, upper,
           excess_year, excess_year_lower, excess_year_upper) %>% 
    mutate(Model = "Age Serfling (Stan, NB)",
           mutate(across(pred:excess_year_upper, round)))
  
  results_age <- bind_rows(results_age, extract_age)
  
  ### Save
  rm(extract_month, extract_year, extract_age)
  write_rds(results_month, paste0(path0,"Spain_results_month_sst1.Rds"))
  write_rds(results_year, paste0(path0,"Spain_results_year_sst1.Rds"))
  write_rds(results_age, paste0(path0,"Spain_results_age_sst1.Rds"))
}






# 8 - Excluding pandemic years, observed population data, 2021, exclude highest and lowest of last 7 years ----


## Global model

print("Global Serfling (Stan, NB)")

global_serfling_stan <- fn_global_serfling_nb_stan_21_sst1(2021, deaths_monthly, pandemic_years = pandemic)

extract_month <- global_serfling_stan$pred_total_deaths %>% 
  select(Country, Year, Month, Deaths,
         pred, lower, upper, n_eff, Rhat,
         excess_month, excess_month_lower, excess_month_upper) %>% 
  mutate(Model = "Global Serfling (Stan, NB)",
         mutate(across(pred:excess_month_upper, round)))

results_month <- bind_rows(results_month, extract_month)

extract_year <- global_serfling_stan$pred_total_deaths %>% 
  filter(row_number() == 1) %>% 
  select(Country, Year, 
         excess_year, excess_year_lower, excess_year_upper) %>% 
  mutate(Model = "Global Serfling (Stan, NB)",
         mutate(across(excess_year:excess_year_upper, round)))

results_year <- bind_rows(results_year, extract_year)

## Age model

print("Age Serfling (Stan, NB)")

age_serfling_nb_stan <- fn_age_serfling_nb_stan_21_sst1(2021, deaths_monthly, deaths_yearly_age_sex, pandemic_years = pandemic)

extract_month <- age_serfling_nb_stan$pred_total_deaths %>% 
  select(Country, Year, Month, Deaths,
         pred, lower, upper, n_eff, Rhat,
         excess_month, excess_month_lower, excess_month_upper) %>% 
  mutate(Model = "Age Serfling (Stan, NB)",
         mutate(across(pred:excess_month_upper, round)))

results_month <- bind_rows(results_month, extract_month)

extract_year <- age_serfling_nb_stan$pred_total_deaths %>% 
  filter(row_number() == 1) %>% 
  select(Country, Year, 
         excess_year, excess_year_lower, excess_year_upper) %>% 
  mutate(Model = "Age Serfling (Stan, NB)",
         mutate(across(excess_year:excess_year_upper, round)))

results_year <- bind_rows(results_year, extract_year)

extract_age <- age_serfling_nb_stan$pred_grouped_deaths %>% 
  select(Country, Year, Age_cat, Deaths,
         pred, lower, upper,
         excess_year, excess_year_lower, excess_year_upper) %>% 
  mutate(Model = "Age Serfling (Stan, NB)",
         mutate(across(pred:excess_year_upper, round)))

results_age <- bind_rows(results_age, extract_age)

rm(extract_month, extract_year, extract_age)

write_rds(results_month, paste0(path0,"Spain_results_month_sst1.Rds"))
write_rds(results_year, paste0(path0,"Spain_results_year_sst1.Rds"))
write_rds(results_age, paste0(path0,"Spain_results_age_sst1.Rds"))

