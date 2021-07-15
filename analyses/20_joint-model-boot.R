# install.packages(c(“StanHeaders”, “rstan”), repos = "https://cloud.r-project.org/", dependencies = TRUE, type = "source")

# setwd("~/ISPM_excess-mortality/")

library(pacman)
p_load(tidyverse, magrittr, 
       doParallel, foreach, 
       rstan)

source("R/fn_global_serfling.R")

path0 = paste0("data/outputs_",Sys.Date(),"/")
dir.create(path0,showWarnings = FALSE)

set.seed(12345)
options(scipen = 999)
theme_set(theme_minimal())
registerDoParallel(cores = parallel::detectCores())
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Set up

## Data 

deaths_monthly <- read_rds("data/deaths_monthly.Rds")
deaths_yearly_age_sex <- read_rds("data/deaths_yearly_age_sex.Rds")

## Years

year_smooth <- 5

pandemic <- c(1890, 1918, 1957, 2020)

pandemic_affected <- c(seq(1890 + 1, 1890 + year_smooth),
                       seq(1957 + 1, 1957 + year_smooth),
                       seq(1918 + 1, 1918 + year_smooth))


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
    
    global_serfling <- fn_global_serfling(YEAR, REG_DATA, pandemic_years = pandemic)
    
    extract_month <- global_serfling$pred_total_deaths %>% 
      select(Country, Year, Month, Deaths, pred, lower, upper) %>% 
      mutate(excess_month = round(Deaths - pred),
             excess_month_upper = round(Deaths - upper),
             excess_month_lower = round(Deaths - lower)) %>% 
      mutate(Model = "Global Serfling")
    
    results_month <- bind_rows(results_month, extract_month)
    
    rm(extract_month)
    
    write_rds(results_month, paste0(path0,"All_results_month.Rds"))
    write_rds(results_year, paste0(path0,"All_results_year.Rds"))
    write_rds(results_age, paste0(path0,"All_results_age.Rds"))
  }
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

results_year_pand <- tibble(Country = character(), 
                            Year = double(), 
                            pred = double(), 
                            lower = double(), 
                            upper = double(),
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


for (COUNTRY in unique(deaths_monthly$Country)) {
# for (COUNTRY in c("Sweden")) {
  
  # #############################################
  # Data params preps
  
  REG_DATA <- deaths_monthly %>% 
    filter(Country == COUNTRY)
  
  AGE_DATA <- deaths_yearly_age_sex %>% 
    filter(Country == COUNTRY)
  
  print(paste("Analysing", COUNTRY))
  
  # Different pandemics for Spain - missing data on oldest
  if (Country == "Spain"){
    
    pandemic_affected <- c(seq(1957 + 1, 1957 + year_smooth),
                           seq(1918 + 1, 1918 + year_smooth))
    
  }
  
  for (YEAR in pandemic_affected) {
  # for (YEAR in 1919) {
    
    print(paste("     Analysing year", YEAR))
    
    # #############################################
    # Model 1
    print("          Global Serfling (pandemic)")
    
    global_serfling <- fn_global_serfling(YEAR, REG_DATA, pandemic_years = NULL)
    
    extract_month <- global_serfling$pred_total_deaths %>% 
      select(Country, Year, Month, Deaths, pred, lower, upper) %>% 
      mutate(excess_month = round(Deaths - pred),
             excess_month_upper = round(Deaths - upper),
             excess_month_lower = round(Deaths - lower)) %>% 
      mutate(Model = "Global Serfling (pandemic)")
    
    results_month_pand <- bind_rows(results_month_pand, extract_month)
    
    rm(extract_month)
    
    write_rds(results_month_pand, paste0(path0,"All_results_month_pand.Rds"))
    write_rds(results_year_pand, paste0(path0,"All_results_year_pand.Rds"))
    write_rds(results_age_pand, paste0(path0,"All_results_age_pand.Rds"))
  }
} 


