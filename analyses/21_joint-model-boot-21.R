library(pacman)
p_load(tidyverse, magrittr, 
       doParallel, foreach)

setwd("C:/projects/ISPM_excess-mortality/")

source("R/fn_global_serfling.R")

path0 = paste0("data/outputs_",Sys.Date(),"/")
dir.create(path0, showWarnings = FALSE)

set.seed(12345)
options(scipen = 999)
theme_set(theme_minimal())
registerDoParallel(cores = parallel::detectCores())

# Set up

## Data 

deaths_monthly <- read_rds("data/deaths_monthly.Rds") %>% 
  filter(Year >= 2016)

pandemic <- c(1890, 1918, 1957, 2020, 2021)

# Modelling 

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

for (COUNTRY in unique(deaths_monthly$Country)) {
  
  REG_DATA <- deaths_monthly %>% 
    filter(Country == COUNTRY) 
  
  print(paste("Analysing", COUNTRY))
  
  global_serfling <- fn_global_serfling(2021, REG_DATA, pandemic_years = pandemic)
  
  extract_month <- global_serfling$pred_total_deaths %>% 
    select(Country, Year, Month, Deaths, pred, lower, upper) %>% 
    mutate(excess_month = round(Deaths - pred),
           excess_month_upper = round(Deaths - upper),
           excess_month_lower = round(Deaths - lower)) %>% 
    mutate(Model = "Global Serfling")
  
  results_month <- bind_rows(results_month, extract_month)
  
  rm(extract_month)
  
  write_rds(results_month, paste0(path0, "All_results_month.Rds"))
  
} 

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

for (COUNTRY in unique(deaths_monthly$Country)) {
  
  REG_DATA <- deaths_monthly %>% 
    filter(Country == COUNTRY)
  
  print(paste("Analysing", COUNTRY))
  
  global_serfling <- fn_global_serfling(2021, REG_DATA, pandemic_years = NULL)
  
  extract_month <- global_serfling$pred_total_deaths %>% 
    select(Country, Year, Month, Deaths, pred, lower, upper) %>% 
    mutate(excess_month = round(Deaths - pred),
           excess_month_upper = round(Deaths - upper),
           excess_month_lower = round(Deaths - lower)) %>% 
    mutate(Model = "Global Serfling (pandemic)")
  
  results_month_pand <- bind_rows(results_month_pand, extract_month)
  
  rm(extract_month)
  
  write_rds(results_month_pand, paste0(path0, "All_results_month_pand.Rds"))
  
} 