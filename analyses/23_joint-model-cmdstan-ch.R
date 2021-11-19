# 0 - Set up ----

args = commandArgs(trailingOnly=TRUE)

## Libs - stan et al.
# install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

# check_cmdstan_toolchain()
# install_cmdstan(cores = 4)

## Libs - other
library(pacman)
p_load(tidyverse, magrittr,
       cmdstanr, posterior)

cmdstan_version()

## Counrtry 
COUNTRIES = c("Switzerland","Spain","Sweden")
COUNTRY = COUNTRIES[[as.numeric(args[[1]])]]

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
    seq(1957 + 1, 1957 + year_smooth),
    2021)
  
  
  
} else {
  
  pandemic <- c(1890, 1918, 1957, 2020)
  pandemic_affected <- c(
    seq(1890 + 1, 1890 + year_smooth),
    seq(1918 + 1, 1918 + year_smooth),
    seq(1957 + 1, 1957 + year_smooth),
    2021)
  
}

## Starting year
START <- deaths_monthly %>% 
  summarize(MIN = min(Year))





# 1 - Excluding post-pandemic years, observed population data ----

results_month <- tibble()
results_year <- tibble()
results_age <- tibble()

## Loop
for (YEAR in (START$MIN+5):2021) {
  
  # Global model
  print(paste("Loop 1.", "Year:", YEAR, "Model: Global Serfling (Stan, NB)"))
  
  m_glo <- fn_global_serfling_nb_cmdstan(YEAR, 
                                         deaths_monthly, 
                                         pandemic_years = pandemic, 
                                         pop = "obs",
                                         version = "last_5")
  
  
  results_month <- m_glo$pred_total_deaths %>% 
    dplyr::select(-starts_with("excess_year")) %>% 
    dplyr::mutate(Model = "Global Serfling (Stan, NB)") %>% 
    bind_rows(.,results_month)
  
  results_year <- m_glo$pred_total_deaths %>% 
    filter(row_number() == 1) %>% 
    select(Country, Year, starts_with("excess_year")) %>% 
    mutate(Model = "Global Serfling (Stan, NB)") %>% 
    bind_rows(.,results_year)
  
  # Age model
  
  if(! (COUNTRY=="Sweden" & YEAR==2021)) {
    
    print(paste("Loop 1.", "Year:", YEAR, "Model: Age Serfling (Stan, NB)"))
    
    m_age <- fn_age_serfling_nb_cmdstan(YEAR, 
                                        deaths_monthly, 
                                        deaths_yearly_age_sex, 
                                        pandemic_years = pandemic, 
                                        pop = "obs",
                                        version = "last_5")
    
    results_month <- m_age$pred_total_deaths %>% 
      dplyr::select(-starts_with("excess_year")) %>% 
      dplyr::mutate(Model = "Age Serfling (Stan, NB)") %>% 
      bind_rows(.,results_month)
    
    results_year <- m_age$pred_total_deaths %>% 
      filter(row_number() == 1) %>% 
      select(Country, Year, starts_with("excess_year")) %>% 
      mutate(Model = "Age Serfling (Stan, NB)") %>% 
      bind_rows(.,results_year)
    
    results_age <- m_age$pred_grouped_deaths %>% 
      mutate(Model = "Age Serfling (Stan, NB)") %>% 
      bind_rows(.,results_age)
    
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




# 2 - Excluding post-pandemic years, expected population data ----

results_month <- tibble()
results_year <- tibble()
results_age <- tibble()

## Loop
for (YEAR in 2020:2021) {
  
  # Global model
  print(paste("Loop 2.", "Year:", YEAR, "Model: Global Serfling (Stan, NB)"))
  
  m_glo <- fn_global_serfling_nb_cmdstan(YEAR, 
                                         deaths_monthly, 
                                         pandemic_years = pandemic, 
                                         pop = "exp",
                                         version = "last_5")
  
  
  results_month <- m_glo$pred_total_deaths %>% 
    dplyr::select(-starts_with("excess_year")) %>% 
    dplyr::mutate(Model = "Global Serfling (Stan, NB)") %>% 
    bind_rows(.,results_month)
  
  results_year <- m_glo$pred_total_deaths %>% 
    filter(row_number() == 1) %>% 
    select(Country, Year, starts_with("excess_year")) %>% 
    mutate(Model = "Global Serfling (Stan, NB)") %>% 
    bind_rows(.,results_year)
  
  # Age model
  
  if(! (COUNTRY=="Sweden" & YEAR==2021)) {
    
    print(paste("Loop 2.", "Year:", YEAR, "Model: Age Serfling (Stan, NB)"))
    
    m_age <- fn_age_serfling_nb_cmdstan(YEAR, 
                                        deaths_monthly, 
                                        deaths_yearly_age_sex, 
                                        pandemic_years = pandemic, 
                                        pop = "exp",
                                        version = "last_5")
    
    results_month <- m_age$pred_total_deaths %>% 
      dplyr::select(-starts_with("excess_year")) %>% 
      dplyr::mutate(Model = "Age Serfling (Stan, NB)") %>% 
      bind_rows(.,results_month)
    
    results_year <- m_age$pred_total_deaths %>% 
      filter(row_number() == 1) %>% 
      select(Country, Year, starts_with("excess_year")) %>% 
      mutate(Model = "Age Serfling (Stan, NB)") %>% 
      bind_rows(.,results_year)
    
    results_age <- m_age$pred_grouped_deaths %>% 
      mutate(Model = "Age Serfling (Stan, NB)") %>% 
      bind_rows(.,results_age)
    
  }
  
  # Save
  write_rds(results_month, 
            paste0(path0, COUNTRY, "_results_month_exp.Rds"))
  write_rds(results_year, 
            paste0(path0, COUNTRY, "_results_year_exp.Rds"))
  write_rds(results_age, 
            paste0(path0, COUNTRY, "_results_age_exp.Rds"))
  
  rm(m_glo, m_age)
  gc()
}




# 3 - Including post-pandemic years, observed population data, until 2020 ----

results_month <- tibble()
results_year <- tibble()
results_age <- tibble()

## Loop
for (YEAR in pandemic_affected) {
  
  # Global model
  print(paste("Loop 3.", "Year:", YEAR, "Model: Global Serfling (Stan, NB, pandemic)"))
  
  m_glo <- fn_global_serfling_nb_cmdstan(YEAR, 
                                         deaths_monthly, 
                                         pandemic_years = NULL, 
                                         pop = "obs",
                                         version = "last_5")
  
  results_month <- m_glo$pred_total_deaths %>% 
    dplyr::select(-starts_with("excess_year")) %>% 
    dplyr::mutate(Model = "Global Serfling (Stan, NB, pandemic)") %>% 
    bind_rows(.,results_month)
  
  results_year <- m_glo$pred_total_deaths %>% 
    filter(row_number() == 1) %>% 
    select(Country, Year, starts_with("excess_year")) %>% 
    mutate(Model = "Global Serfling (Stan, NB, pandemic)") %>% 
    bind_rows(.,results_year)
  
  # Age model
  
  if(! (COUNTRY=="Sweden" & YEAR==2021)) {
    
    print(paste("Loop 3.", "Year:", YEAR, "Model: Age Serfling (Stan, NB, pandemic)"))
    
    m_age <- fn_age_serfling_nb_cmdstan(YEAR, 
                                        deaths_monthly, 
                                        deaths_yearly_age_sex, 
                                        pandemic_years = NULL, 
                                        pop = "obs",
                                        version = "last_5")
    
    results_month <- m_age$pred_total_deaths %>% 
      dplyr::select(-starts_with("excess_year")) %>% 
      dplyr::mutate(Model = "Age Serfling (Stan, NB, pandemic)") %>% 
      bind_rows(.,results_month)
    
    results_year <- m_age$pred_total_deaths %>% 
      filter(row_number() == 1) %>% 
      select(Country, Year, starts_with("excess_year")) %>% 
      mutate(Model = "Age Serfling (Stan, NB, pandemic)") %>% 
      bind_rows(.,results_year)
    
    results_age <- m_age$pred_grouped_deaths %>% 
      mutate(Model = "Age Serfling (Stan, NB, pandemic)") %>% 
      bind_rows(.,results_age)
    
  }
  
  # Save
  write_rds(results_month, 
            paste0(path0, COUNTRY, "_results_month_pand.Rds"))
  write_rds(results_year, 
            paste0(path0, COUNTRY, "_results_year_pand.Rds"))
  write_rds(results_age, 
            paste0(path0, COUNTRY, "_results_age_pand.Rds"))
  
  rm(m_glo, m_age)
  gc()
}





# 4 - Excluding post-pandemic years, observed population data,  exclude highest and lowest of last 7 years ----


results_month <- tibble()
results_year <- tibble()
results_age <- tibble()

## Loop
for (YEAR in (START$MIN+7):2021) {
  
  # Global model
  print(paste("Loop 4.", "Year:", YEAR, "Model: Global Serfling (Stan, NB, last 7)"))
  
  m_glo <- fn_global_serfling_nb_cmdstan(YEAR, 
                                         deaths_monthly, 
                                         pandemic_years = pandemic, 
                                         pop = "obs",
                                         version = "last_7_trim")
  
  
  results_month <- m_glo$pred_total_deaths %>% 
    dplyr::select(-starts_with("excess_year")) %>% 
    dplyr::mutate(Model = "Global Serfling (Stan, NB, last 7)") %>% 
    bind_rows(.,results_month)
  
  results_year <- m_glo$pred_total_deaths %>% 
    filter(row_number() == 1) %>% 
    select(Country, Year, starts_with("excess_year")) %>% 
    mutate(Model = "Global Serfling (Stan, NB, last 7)") %>% 
    bind_rows(.,results_year)
  
  # Age model
  
  if(! (COUNTRY=="Sweden" & YEAR==2021)) {
    
    print(paste("Loop 4.", "Year:", YEAR, "Model: Age Serfling (Stan, NB, last 7)"))
    
    m_age <- fn_age_serfling_nb_cmdstan(YEAR, 
                                        deaths_monthly, 
                                        deaths_yearly_age_sex, 
                                        pandemic_years = pandemic, 
                                        pop = "obs",
                                        version = "last_7_trim")
    
    results_month <- m_age$pred_total_deaths %>% 
      dplyr::select(-starts_with("excess_year")) %>% 
      dplyr::mutate(Model = "Age Serfling (Stan, NB, last 7)") %>% 
      bind_rows(.,results_month)
    
    results_year <- m_age$pred_total_deaths %>% 
      filter(row_number() == 1) %>% 
      select(Country, Year, starts_with("excess_year")) %>% 
      mutate(Model = "Age Serfling (Stan, NB, last 7)") %>% 
      bind_rows(.,results_year)
    
    results_age <- m_age$pred_grouped_deaths %>% 
      mutate(Model = "Age Serfling (Stan, NB, last 7)") %>% 
      bind_rows(.,results_age)
    
  }
  
  # Save
  write_rds(results_month, 
            paste0(path0, COUNTRY, "_results_month_last_7_trim.Rds"))
  write_rds(results_year, 
            paste0(path0, COUNTRY, "_results_year_last_7_trim.Rds"))
  write_rds(results_age, 
            paste0(path0, COUNTRY, "_results_age_last_7_trim.Rds"))
  
  rm(m_glo, m_age)
  gc()
}