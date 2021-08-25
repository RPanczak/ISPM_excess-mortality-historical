# install.packages(c("StanHeaders", "rstan"), repos = "https://cloud.r-project.org/", dependencies = TRUE, type = "source")

# setwd("~/projects/ISPM_excess-mortality/")
setwd("C:/projects/ISPM_excess-mortality/")

library(pacman)
p_load(tidyverse, magrittr, rstan)

source("R/fn_global_serfling_nb_stan_21.R")
source("R/fn_age_serfling_nb_stan_21.R")

path0 = paste0("data/outputs_",Sys.Date(),"/")
dir.create(path0, showWarnings = FALSE)

set.seed(12345)
options(scipen = 999)
theme_set(theme_minimal())
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = FALSE)

# Set up

## Data 

deaths_monthly <- read_rds("data/deaths_monthly.Rds") %>% 
  filter(Country == "Sweden", Year >= 2016)

deaths_yearly_age_sex <- read_rds("data/deaths_yearly_age_sex.Rds") %>% 
  filter(Country == "Sweden", Year >= 2016) 

## Years

pandemic <- c(1890, 1918, 1957, 2020, 2021)

# Modelling 

# #############################################
## Excluding pandemic years

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

# Model 2
print("Global Serfling (Stan, NB)")

global_serfling_stan <- fn_global_serfling_nb_stan(2021, deaths_monthly, pandemic_years = pandemic)

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

# Model 3
print("Age Serfling (Stan, NB)")

age_serfling_nb_stan <- fn_age_serfling_nb_stan(2021, deaths_monthly, deaths_yearly_age_sex, pandemic_years = pandemic)

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

write_rds(results_month, paste0(path0,"Sweden_results_month.Rds"))
write_rds(results_year, paste0(path0,"Sweden_results_year.Rds"))
write_rds(results_age, paste0(path0,"Sweden_results_age.Rds"))

# #############################################
## Including pandemic years

results_month_pand <- tibble(Country = character(), 
                             Year = double(), Month = double(), 
                             Deaths = double(),
                             pred = double(), 
                             lower = double(), 
                             upper = double(),
                             excess_month = double(), 
                             excess_month_lower = double(), 
                             excess_month_upper = double(),
                             Model = character())

results_year_pand <- tibble(Country = character(), 
                            Year = double(), 
                            excess_year = double(), 
                            excess_year_lower = double(), 
                            excess_year_upper = double(),
                            Model = character())

results_age_pand <- tibble(Country = character(), 
                           Year = double(), 
                           Age_cat = factor(), 
                           pred = double(), 
                           lower = double(), 
                           upper = double(),
                           excess_year = double(), 
                           excess_year_lower = double(), 
                           excess_year_upper = double(),
                           Model = character())

# Model 2
print("Global Serfling (Stan, NB, pandemic)")

global_serfling_stan <- fn_global_serfling_nb_stan(2021, deaths_monthly, pandemic_years = NULL)

extract_month <- global_serfling_stan$pred_total_deaths %>% 
  select(Country, Year, Month, Deaths,
         pred, lower, upper, n_eff, Rhat,
         excess_month, excess_month_lower, excess_month_upper) %>% 
  mutate(Model = "Global Serfling (Stan, NB, pandemic)",
         mutate(across(pred:excess_month_upper, round)))

results_month_pand <- bind_rows(results_month_pand, extract_month)

extract_year <- global_serfling_stan$pred_total_deaths %>% 
  filter(row_number() == 1) %>% 
  select(Country, Year, 
         excess_year, excess_year_lower, excess_year_upper) %>% 
  mutate(Model = "Global Serfling (Stan, NB, pandemic)",
         mutate(across(excess_year:excess_year_upper, round)))

results_year_pand <- bind_rows(results_year_pand, extract_year)

# Model 3
print("Age Serfling (Stan, NB, pandemic)")

age_serfling_nb_stan <- fn_age_serfling_nb_stan(2021, deaths_monthly, deaths_yearly_age_sex, pandemic_years = NULL)

extract_month <- age_serfling_nb_stan$pred_total_deaths %>% 
  select(Country, Year, Month, Deaths,
         pred, lower, upper, n_eff, Rhat,
         excess_month, excess_month_lower, excess_month_upper) %>% 
  mutate(Model = "Age Serfling (Stan, NB, pandemic)",
         mutate(across(pred:excess_month_upper, round)))

results_month_pand <- bind_rows(results_month_pand, extract_month)

extract_year <- age_serfling_nb_stan$pred_total_deaths %>% 
  filter(row_number() == 1) %>% 
  select(Country, Year, 
         excess_year, excess_year_lower, excess_year_upper) %>% 
  mutate(Model = "Age Serfling (Stan, NB, pandemic)",
         mutate(across(excess_year:excess_year_upper, round)))

results_year_pand <- bind_rows(results_year_pand, extract_year)

extract_age <- age_serfling_nb_stan$pred_grouped_deaths %>% 
  select(Country, Year, Age_cat, Deaths,
         pred, lower, upper,
         excess_year, excess_year_lower, excess_year_upper) %>% 
  mutate(Model = "Age Serfling (Stan, NB, pandemic)",
         mutate(across(pred:excess_year_upper, round)))

results_age_pand <- bind_rows(results_age_pand, extract_age)

rm(extract_month, extract_year, extract_age)

write_rds(results_month_pand, paste0(path0,"Sweden_results_month_pand.Rds"))
write_rds(results_year_pand, paste0(path0,"Sweden_results_year_pand.Rds"))
write_rds(results_age_pand, paste0(path0,"Sweden_results_age_pand.Rds"))