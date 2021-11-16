# 0 - Set up ----

## Libs - stan et al.
# install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

# check_cmdstan_toolchain()
# install_cmdstan(cores = 4)

## Libs - other
library(pacman)
p_load(tidyverse, magrittr,
       cmdstanr, posterior, bayesplot)

cmdstan_version()

## Counrtry 
COUNTRY = "Switzerland"

## Paths
# ubelix
# setwd("~/projects/ISPM_excess-mortality/")
# local runs outside of RStudio
# setwd("C:/projects/ISPM_excess-mortality/")
path0 = paste0("data/outputs_", Sys.Date(), "/")
dir.create(path0, showWarnings = FALSE)

## Funcs
source("R/fn_global_serfling_nb_cmdstan.R")
source("R/fn_age_serfling_nb_cmdstan.R")

## Options
set.seed(12345)
options(scipen = 999)
options(mc.cores = parallel::detectCores())

## Data 
deaths_monthly <- read_rds("data/deaths_monthly.Rds") %>% 
  filter(Country == COUNTRY)
deaths_yearly_age_sex <- read_rds("data/deaths_yearly_age_sex.Rds") %>% 
  filter(Country == COUNTRY) 

## Smoothing window
year_smooth <- 5

## Pandemics & subsequent post-pandemic years to exclude
if (COUNTRY == "Spain") {
  
  pandemic <- c(1918, 1957, 2020)
  pandemic_affected <- c(
    seq(1918 + 1, 1918 + year_smooth),
    seq(1957 + 1, 1957 + year_smooth))
  
} else {
  
  pandemic <- c(1890, 1918, 1957, 2020)
  pandemic_affected <- c(
    seq(1890 + 1, 1890 + year_smooth),
    seq(1918 + 1, 1918 + year_smooth),
    seq(1957 + 1, 1957 + year_smooth))
  
}

## Starting year
START <- deaths_monthly %>% 
  summarize(MIN = min(Year))




# 1 - Excluding post-pandemic years, observed population data, until 2020 ----

## Loop
for (YEAR in (START$MIN+5):2020) {
  
  # Global model
  print(paste("Year:", YEAR, "Model: Global Serfling (Stan, NB)"))
  
  m_glo <- fn_global_serfling_nb_cmdstan(YEAR, 
                                         deaths_monthly, 
                                         pandemic_years = pandemic, 
                                         pop = "obs")
  
  if (YEAR == (START$MIN+5)) {
    results_month <- m_glo$excess_month
  } else {
    results_month <- bind_rows(results_month, 
                               m_glo$excess_month)
  }
  
  if (YEAR == (START$MIN+5)) {
    results_year <- m_glo$excess_year
  } else {
    results_year <- bind_rows(results_year, 
                              m_glo$excess_year)
  }
  
  # Age model
  print(paste("Year:", YEAR, "Model: Age Serfling (Stan, NB)"))
  
  m_age <- fn_age_serfling_nb_cmdstan(YEAR, 
                                      deaths_monthly, 
                                      deaths_yearly_age_sex, 
                                      pandemic_years = pandemic, 
                                      pop = "obs")
  
  if (YEAR == (START$MIN+5)) {
    results_month <- m_age$excess_month
  } else {
    results_month <- bind_rows(results_month, 
                               m_age$excess_month)
  }
  
  if (YEAR == (START$MIN+5)) {
    results_year <- m_age$excess_year
  } else {
    results_year <- bind_rows(results_year, 
                              m_age$excess_year)
  }
  
  if (YEAR == (START$MIN+5)) {
    results_age <- m_age$excess_age
  } else {
    results_age <- bind_rows(results_age, 
                             m_age$excess_age)
  }
  
  # Save
  write_rds(results_month, 
            paste0(path0, COUNTRY, "_results_month.Rds"))
  write_rds(results_year, 
            paste0(path0, COUNTRY, "_results_year.Rds"))
  write_rds(results_age, 
            paste0(path0, COUNTRY, "_results_age.Rds"))
  
  rm(m_glo, m_age)
  gc()
}






# 2 - Excluding post-pandemic years, observed population data, 2021 ----


## Global model

print("Global Serfling (Stan, NB)")

m_glo <- fn_m_glo_21(2021, deaths_monthly, pandemic_years = pandemic)

extract_month <- m_glo$pred_total_deaths %>% 
  select(Country, Year, Month, Deaths,
         pred, lower, upper, n_eff, Rhat,
         excess_month, excess_month_lower, excess_month_upper) %>% 
  mutate(Model = "Global Serfling (Stan, NB)",
         mutate(across(pred:excess_month_upper, round)))

results_month <- bind_rows(results_month, extract_month)

extract_year <- m_glo$pred_total_deaths %>% 
  filter(row_number() == 1) %>% 
  select(Country, Year, 
         excess_year, excess_year_lower, excess_year_upper) %>% 
  mutate(Model = "Global Serfling (Stan, NB)",
         mutate(across(excess_year:excess_year_upper, round)))

results_year <- bind_rows(results_year, extract_year)

## Age model

print("Age Serfling (Stan, NB)")

m_age <- fn_m_age_21(2021, deaths_monthly, deaths_yearly_age_sex, pandemic_years = pandemic)

extract_month <- m_age$pred_total_deaths %>% 
  select(Country, Year, Month, Deaths,
         pred, lower, upper, n_eff, Rhat,
         excess_month, excess_month_lower, excess_month_upper) %>% 
  mutate(Model = "Age Serfling (Stan, NB)",
         mutate(across(pred:excess_month_upper, round)))

results_month <- bind_rows(results_month, extract_month)

extract_year <- m_age$pred_total_deaths %>% 
  filter(row_number() == 1) %>% 
  select(Country, Year, 
         excess_year, excess_year_lower, excess_year_upper) %>% 
  mutate(Model = "Age Serfling (Stan, NB)",
         mutate(across(excess_year:excess_year_upper, round)))

results_year <- bind_rows(results_year, extract_year)

extract_age <- m_age$pred_grouped_deaths %>% 
  select(Country, Year, Age_cat, Deaths,
         pred, lower, upper,
         excess_year, excess_year_lower, excess_year_upper) %>% 
  mutate(Model = "Age Serfling (Stan, NB)",
         mutate(across(pred:excess_year_upper, round)))

results_age <- bind_rows(results_age, extract_age)

rm(extract_month, extract_year, extract_age)

write_rds(results_month, paste0(path0,"Switzerland_results_month.Rds"))
write_rds(results_year, paste0(path0,"Switzerland_results_year.Rds"))
write_rds(results_age, paste0(path0,"Switzerland_results_age.Rds"))



# 3 - Excluding post-pandemic years, expected population data, until 2020 ----

## Empty outputs
results_month <- tibble()
results_year <- tibble()
results_age <- tibble()

## Select years
START <- deaths_monthly %>% 
  summarize(MIN = min(Year))

## Loop
for (YEAR in (START$MIN+5):2020) {
  print(paste("     Analysing year", YEAR))
  
  ### Global model
  
  print("          Global Serfling (Stan, NB)")
  
  m_glo <- fn_m_glo(YEAR, deaths_monthly, pandemic_years = pandemic, pop="exp")
  
  extract_month <- m_glo$pred_total_deaths %>% 
    select(Country, Year, Month, Deaths,
           pred, lower, upper, n_eff, Rhat,
           excess_month, excess_month_lower, excess_month_upper) %>% 
    mutate(Model = "Global Serfling (Stan, NB)",
           mutate(across(pred:excess_month_upper, round)))
  
  results_month <- bind_rows(results_month, extract_month)
  
  extract_year <- m_glo$pred_total_deaths %>% 
    filter(row_number() == 1) %>% 
    select(Country, Year, 
           excess_year, excess_year_lower, excess_year_upper) %>% 
    mutate(Model = "Global Serfling (Stan, NB)",
           mutate(across(excess_year:excess_year_upper, round)))
  
  results_year <- bind_rows(results_year, extract_year)
  
  ### Age model
  
  print("          Age Serfling (Stan, NB)")
  
  m_age <- fn_m_age(YEAR, deaths_monthly, deaths_yearly_age_sex, pandemic_years = pandemic, pop="exp")
  
  extract_month <- m_age$pred_total_deaths %>% 
    select(Country, Year, Month, Deaths,
           pred, lower, upper, n_eff, Rhat,
           excess_month, excess_month_lower, excess_month_upper) %>% 
    mutate(Model = "Age Serfling (Stan, NB)",
           mutate(across(pred:excess_month_upper, round)))
  
  results_month <- bind_rows(results_month, extract_month)
  
  extract_year <- m_age$pred_total_deaths %>% 
    filter(row_number() == 1) %>% 
    select(Country, Year, 
           excess_year, excess_year_lower, excess_year_upper) %>% 
    mutate(Model = "Age Serfling (Stan, NB)",
           mutate(across(excess_year:excess_year_upper, round)))
  
  results_year <- bind_rows(results_year, extract_year)
  
  extract_age <- m_age$pred_grouped_deaths %>% 
    select(Country, Year, Age_cat, Deaths,
           pred, lower, upper,
           excess_year, excess_year_lower, excess_year_upper) %>% 
    mutate(Model = "Age Serfling (Stan, NB)",
           mutate(across(pred:excess_year_upper, round)))
  
  results_age <- bind_rows(results_age, extract_age)
  
  ### Save
  rm(extract_month, extract_year, extract_age)
  write_rds(results_month, paste0(path0,"Switzerland_results_month_exp.Rds"))
  write_rds(results_year, paste0(path0,"Switzerland_results_year_exp.Rds"))
  write_rds(results_age, paste0(path0,"Switzerland_results_age_exp.Rds"))
}






# 4 - Excluding post-pandemic years, observed population data, 2021 ----


## Global model

print("Global Serfling (Stan, NB)")

m_glo <- fn_m_glo_21(2021, deaths_monthly, pandemic_years = pandemic, pop="exp")

extract_month <- m_glo$pred_total_deaths %>% 
  select(Country, Year, Month, Deaths,
         pred, lower, upper, n_eff, Rhat,
         excess_month, excess_month_lower, excess_month_upper) %>% 
  mutate(Model = "Global Serfling (Stan, NB)",
         mutate(across(pred:excess_month_upper, round)))

results_month <- bind_rows(results_month, extract_month)

extract_year <- m_glo$pred_total_deaths %>% 
  filter(row_number() == 1) %>% 
  select(Country, Year, 
         excess_year, excess_year_lower, excess_year_upper) %>% 
  mutate(Model = "Global Serfling (Stan, NB)",
         mutate(across(excess_year:excess_year_upper, round)))

results_year <- bind_rows(results_year, extract_year)


## Age model

print("Age Serfling (Stan, NB)")

m_age <- fn_m_age_21(2021, deaths_monthly, deaths_yearly_age_sex, pandemic_years = pandemic, pop="exp")

extract_month <- m_age$pred_total_deaths %>% 
  select(Country, Year, Month, Deaths,
         pred, lower, upper, n_eff, Rhat,
         excess_month, excess_month_lower, excess_month_upper) %>% 
  mutate(Model = "Age Serfling (Stan, NB)",
         mutate(across(pred:excess_month_upper, round)))

results_month <- bind_rows(results_month, extract_month)

extract_year <- m_age$pred_total_deaths %>% 
  filter(row_number() == 1) %>% 
  select(Country, Year, 
         excess_year, excess_year_lower, excess_year_upper) %>% 
  mutate(Model = "Age Serfling (Stan, NB)",
         mutate(across(excess_year:excess_year_upper, round)))

results_year <- bind_rows(results_year, extract_year)

extract_age <- m_age$pred_grouped_deaths %>% 
  select(Country, Year, Age_cat, Deaths,
         pred, lower, upper,
         excess_year, excess_year_lower, excess_year_upper) %>% 
  mutate(Model = "Age Serfling (Stan, NB)",
         mutate(across(pred:excess_year_upper, round)))

results_age <- bind_rows(results_age, extract_age)

rm(extract_month, extract_year, extract_age)
write_rds(results_month, paste0(path0,"Switzerland_results_month_exp.Rds"))
write_rds(results_year, paste0(path0,"Switzerland_results_year_exp.Rds"))
write_rds(results_age, paste0(path0,"Switzerland_results_age_exp.Rds"))





# 5 - Including post-pandemic years, observed population data, until 2020 ----

## Empty outputs
results_month <- tibble()
results_year <- tibble()
results_age <- tibble()

## Select years
START <- deaths_monthly %>% 
  summarize(MIN = min(Year))

## Loop
for (YEAR in (START$MIN+5):2020) {
  print(paste("     Analysing year", YEAR))
  
  ### Global model
  
  print("          Global Serfling (Stan, NB)")
  
  m_glo <- fn_m_glo(YEAR, deaths_monthly, pandemic_years = NULL, pop="obs")
  
  extract_month <- m_glo$pred_total_deaths %>% 
    select(Country, Year, Month, Deaths,
           pred, lower, upper, n_eff, Rhat,
           excess_month, excess_month_lower, excess_month_upper) %>% 
    mutate(Model = "Global Serfling (Stan, NB)",
           mutate(across(pred:excess_month_upper, round)))
  
  results_month <- bind_rows(results_month, extract_month)
  
  extract_year <- m_glo$pred_total_deaths %>% 
    filter(row_number() == 1) %>% 
    select(Country, Year, 
           excess_year, excess_year_lower, excess_year_upper) %>% 
    mutate(Model = "Global Serfling (Stan, NB)",
           mutate(across(excess_year:excess_year_upper, round)))
  
  results_year <- bind_rows(results_year, extract_year)
  
  ### Age model
  
  print("          Age Serfling (Stan, NB)")
  
  m_age <- fn_m_age(YEAR, deaths_monthly, deaths_yearly_age_sex, pandemic_years = NULL, pop="obs")
  
  extract_month <- m_age$pred_total_deaths %>% 
    select(Country, Year, Month, Deaths,
           pred, lower, upper, n_eff, Rhat,
           excess_month, excess_month_lower, excess_month_upper) %>% 
    mutate(Model = "Age Serfling (Stan, NB)",
           mutate(across(pred:excess_month_upper, round)))
  
  results_month <- bind_rows(results_month, extract_month)
  
  extract_year <- m_age$pred_total_deaths %>% 
    filter(row_number() == 1) %>% 
    select(Country, Year, 
           excess_year, excess_year_lower, excess_year_upper) %>% 
    mutate(Model = "Age Serfling (Stan, NB)",
           mutate(across(excess_year:excess_year_upper, round)))
  
  results_year <- bind_rows(results_year, extract_year)
  
  extract_age <- m_age$pred_grouped_deaths %>% 
    select(Country, Year, Age_cat, Deaths,
           pred, lower, upper,
           excess_year, excess_year_lower, excess_year_upper) %>% 
    mutate(Model = "Age Serfling (Stan, NB)",
           mutate(across(pred:excess_year_upper, round)))
  
  results_age <- bind_rows(results_age, extract_age)
  
  ### Save
  rm(extract_month, extract_year, extract_age)
  write_rds(results_month, paste0(path0,"Switzerland_results_month_pand.Rds"))
  write_rds(results_year, paste0(path0,"Switzerland_results_year_pand.Rds"))
  write_rds(results_age, paste0(path0,"Switzerland_results_age_pand.Rds"))
}






# 6 - Including post-pandemic years, observed population data, 2021 ----


## Global model

print("Global Serfling (Stan, NB)")

m_glo <- fn_m_glo_21(2021, deaths_monthly, pandemic_years = NULL)

extract_month <- m_glo$pred_total_deaths %>% 
  select(Country, Year, Month, Deaths,
         pred, lower, upper, n_eff, Rhat,
         excess_month, excess_month_lower, excess_month_upper) %>% 
  mutate(Model = "Global Serfling (Stan, NB)",
         mutate(across(pred:excess_month_upper, round)))

results_month <- bind_rows(results_month, extract_month)

extract_year <- m_glo$pred_total_deaths %>% 
  filter(row_number() == 1) %>% 
  select(Country, Year, 
         excess_year, excess_year_lower, excess_year_upper) %>% 
  mutate(Model = "Global Serfling (Stan, NB)",
         mutate(across(excess_year:excess_year_upper, round)))

results_year <- bind_rows(results_year, extract_year)

## Age model

print("Age Serfling (Stan, NB)")

m_age <- fn_m_age_21(2021, deaths_monthly, deaths_yearly_age_sex, pandemic_years = NULL)

extract_month <- m_age$pred_total_deaths %>% 
  select(Country, Year, Month, Deaths,
         pred, lower, upper, n_eff, Rhat,
         excess_month, excess_month_lower, excess_month_upper) %>% 
  mutate(Model = "Age Serfling (Stan, NB)",
         mutate(across(pred:excess_month_upper, round)))

results_month <- bind_rows(results_month, extract_month)

extract_year <- m_age$pred_total_deaths %>% 
  filter(row_number() == 1) %>% 
  select(Country, Year, 
         excess_year, excess_year_lower, excess_year_upper) %>% 
  mutate(Model = "Age Serfling (Stan, NB)",
         mutate(across(excess_year:excess_year_upper, round)))

results_year <- bind_rows(results_year, extract_year)

extract_age <- m_age$pred_grouped_deaths %>% 
  select(Country, Year, Age_cat, Deaths,
         pred, lower, upper,
         excess_year, excess_year_lower, excess_year_upper) %>% 
  mutate(Model = "Age Serfling (Stan, NB)",
         mutate(across(pred:excess_year_upper, round)))

results_age <- bind_rows(results_age, extract_age)

rm(extract_month, extract_year, extract_age)

write_rds(results_month, paste0(path0,"Switzerland_results_month_pand.Rds"))
write_rds(results_year, paste0(path0,"Switzerland_results_year_pand.Rds"))
write_rds(results_age, paste0(path0,"Switzerland_results_age_pand.Rds"))









# 7 - Excluding post-pandemic years, observed population data, until 2020, exclude highest and lowest of last 7 years ----

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
START <- deaths_monthly %>% 
  summarize(MIN = min(Year))

## Loop
for (YEAR in (START$MIN+7):2020) {
  print(paste("     Analysing year", YEAR))
  
  ### Global model
  
  print("          Global Serfling (Stan, NB)")
  
  m_glo <- fn_m_glo_sst1(YEAR, deaths_monthly, pandemic_years = pandemic, pop="obs")
  
  extract_month <- m_glo$pred_total_deaths %>% 
    select(Country, Year, Month, Deaths,
           pred, lower, upper, n_eff, Rhat,
           excess_month, excess_month_lower, excess_month_upper) %>% 
    mutate(Model = "Global Serfling (Stan, NB)",
           mutate(across(pred:excess_month_upper, round)))
  
  results_month <- bind_rows(results_month, extract_month)
  
  extract_year <- m_glo$pred_total_deaths %>% 
    filter(row_number() == 1) %>% 
    select(Country, Year, 
           excess_year, excess_year_lower, excess_year_upper) %>% 
    mutate(Model = "Global Serfling (Stan, NB)",
           mutate(across(excess_year:excess_year_upper, round)))
  
  results_year <- bind_rows(results_year, extract_year)
  
  ### Age model
  
  print("          Age Serfling (Stan, NB)")
  
  m_age <- fn_m_age_sst1(YEAR, deaths_monthly, deaths_yearly_age_sex, pandemic_years = pandemic, pop="obs")
  
  extract_month <- m_age$pred_total_deaths %>% 
    select(Country, Year, Month, Deaths,
           pred, lower, upper, n_eff, Rhat,
           excess_month, excess_month_lower, excess_month_upper) %>% 
    mutate(Model = "Age Serfling (Stan, NB)",
           mutate(across(pred:excess_month_upper, round)))
  
  results_month <- bind_rows(results_month, extract_month)
  
  extract_year <- m_age$pred_total_deaths %>% 
    filter(row_number() == 1) %>% 
    select(Country, Year, 
           excess_year, excess_year_lower, excess_year_upper) %>% 
    mutate(Model = "Age Serfling (Stan, NB)",
           mutate(across(excess_year:excess_year_upper, round)))
  
  results_year <- bind_rows(results_year, extract_year)
  
  extract_age <- m_age$pred_grouped_deaths %>% 
    select(Country, Year, Age_cat, Deaths,
           pred, lower, upper,
           excess_year, excess_year_lower, excess_year_upper) %>% 
    mutate(Model = "Age Serfling (Stan, NB)",
           mutate(across(pred:excess_year_upper, round)))
  
  results_age <- bind_rows(results_age, extract_age)
  
  ### Save
  rm(extract_month, extract_year, extract_age)
  write_rds(results_month, paste0(path0,"Switzerland_results_month_sst1.Rds"))
  write_rds(results_year, paste0(path0,"Switzerland_results_year_sst1.Rds"))
  write_rds(results_age, paste0(path0,"Switzerland_results_age_sst1.Rds"))
}






# 8 - Excluding post-pandemic years, observed population data, 2021, exclude highest and lowest of last 7 years ----


## Global model

print("Global Serfling (Stan, NB)")

m_glo <- fn_m_glo_21_sst1(2021, deaths_monthly, pandemic_years = pandemic)

extract_month <- m_glo$pred_total_deaths %>% 
  select(Country, Year, Month, Deaths,
         pred, lower, upper, n_eff, Rhat,
         excess_month, excess_month_lower, excess_month_upper) %>% 
  mutate(Model = "Global Serfling (Stan, NB)",
         mutate(across(pred:excess_month_upper, round)))

results_month <- bind_rows(results_month, extract_month)

extract_year <- m_glo$pred_total_deaths %>% 
  filter(row_number() == 1) %>% 
  select(Country, Year, 
         excess_year, excess_year_lower, excess_year_upper) %>% 
  mutate(Model = "Global Serfling (Stan, NB)",
         mutate(across(excess_year:excess_year_upper, round)))

results_year <- bind_rows(results_year, extract_year)

## Age model

print("Age Serfling (Stan, NB)")

m_age <- fn_m_age_21_sst1(2021, deaths_monthly, deaths_yearly_age_sex, pandemic_years = pandemic)

extract_month <- m_age$pred_total_deaths %>% 
  select(Country, Year, Month, Deaths,
         pred, lower, upper, n_eff, Rhat,
         excess_month, excess_month_lower, excess_month_upper) %>% 
  mutate(Model = "Age Serfling (Stan, NB)",
         mutate(across(pred:excess_month_upper, round)))

results_month <- bind_rows(results_month, extract_month)

extract_year <- m_age$pred_total_deaths %>% 
  filter(row_number() == 1) %>% 
  select(Country, Year, 
         excess_year, excess_year_lower, excess_year_upper) %>% 
  mutate(Model = "Age Serfling (Stan, NB)",
         mutate(across(excess_year:excess_year_upper, round)))

results_year <- bind_rows(results_year, extract_year)

extract_age <- m_age$pred_grouped_deaths %>% 
  select(Country, Year, Age_cat, Deaths,
         pred, lower, upper,
         excess_year, excess_year_lower, excess_year_upper) %>% 
  mutate(Model = "Age Serfling (Stan, NB)",
         mutate(across(pred:excess_year_upper, round)))

results_age <- bind_rows(results_age, extract_age)

rm(extract_month, extract_year, extract_age)

write_rds(results_month, paste0(path0,"Switzerland_results_month_sst1.Rds"))
write_rds(results_year, paste0(path0,"Switzerland_results_year_sst1.Rds"))
write_rds(results_age, paste0(path0,"Switzerland_results_age_sst1.Rds"))

