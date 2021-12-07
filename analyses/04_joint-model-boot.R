# #############################################
# #############################################
# #############################################
# Set up

library(pacman)
p_load(tidyverse, magrittr, 
       doParallel, foreach)

# for local runs outside of RStudio
# setwd("C:/projects/ISPM_excess-mortality/")

source("R/fn_global_serfling.R")

path0 = paste0("data/outputs_", Sys.Date(), "/")
dir.create(path0, showWarnings = FALSE)

set.seed(12345)
options(scipen = 999)
theme_set(theme_minimal())
registerDoParallel(cores = parallel::detectCores())

## Years

year_smooth <- 5

pandemic <- c(1890, 1918, 1957, 2020)

# Data 

deaths_monthly <- read_rds("data/deaths_monthly.Rds")

# #############################################
# #############################################
# #############################################
# Modelling 

# #############################################
# Excluding pandemic years

results_month_obs <- tibble(Country = character(), 
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
  
  YEARS <- deaths_monthly %>% 
    filter(Country == COUNTRY) %>% 
    summarize(MIN = min(Year))
  
  REG_DATA <- deaths_monthly %>% 
    filter(Country == COUNTRY)
  
  for (YEAR in (YEARS$MIN+5):2021) {
    
    print(paste("Loop 1 >>", COUNTRY, "::", YEAR))
    
    global_serfling <- fn_global_serfling(YEAR, REG_DATA, 
                                          pop = "obs",
                                          pandemic_years = pandemic)
    
    results_month_obs <- global_serfling$pred_total_deaths %>% 
      select(Country, Year, Month, Deaths, pred, lower, upper) %>% 
      mutate(excess_month = round(Deaths - pred),
             excess_month_upper = round(Deaths - upper),
             excess_month_lower = round(Deaths - lower)) %>% 
      mutate(Model = "Global Serfling") %>% 
      bind_rows(., results_month_obs)
    
    write_rds(results_month_obs, paste0(path0, "All_results_month.Rds"))
  }
} 

# #############################################
# Including pandemic years

results_month_pand_obs <- tibble(Country = character(), 
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
  
  YEARS <- deaths_monthly %>% 
    filter(Country == COUNTRY) %>% 
    summarize(MIN = min(Year))
  
  REG_DATA <- deaths_monthly %>% 
    filter(Country == COUNTRY)
  
  # Different pandemics for Spain - missing data on oldest
  if (COUNTRY == "Spain"){
    
    pandemic_affected <- c(seq(1918 + 1, 1918 + year_smooth),
                           seq(1957 + 1, 1957 + year_smooth))
    
  } else {
    
    pandemic_affected <- c(seq(1890 + 1, 1890 + year_smooth),
                           seq(1918 + 1, 1918 + year_smooth),
                           seq(1957 + 1, 1957 + year_smooth))
  }
  
  for (YEAR in pandemic_affected) {
    
    print(paste("Loop 3 >>", COUNTRY, "::", YEAR))
    
    global_serfling <- fn_global_serfling(YEAR, REG_DATA, 
                                          pop = "obs",
                                          pandemic_years = NULL)
    
    results_month_pand_obs <- global_serfling$pred_total_deaths %>% 
      select(Country, Year, Month, Deaths, pred, lower, upper) %>% 
      mutate(excess_month = round(Deaths - pred),
             excess_month_upper = round(Deaths - upper),
             excess_month_lower = round(Deaths - lower)) %>% 
      mutate(Model = "Global Serfling") %>% 
      bind_rows(., results_month_pand_obs)
    
    write_rds(results_month_pand_obs, paste0(path0, "All_results_month_pand.Rds"))
  }
} 