# ##################################################
# Additional outputs requested by Reviewer
# Computes probabilities used in the footnote of the Table 2
# Can be used to quantify the difference between 
# different yearly estimates of a country

# ##################################################
# 0 - Set up ----

## Libs - other
library(pacman)
p_load(tidyverse, magrittr,
       cmdstanr, posterior)

cmdstan_version()

## Counrtry 
COUNTRIES = c("Switzerland", "Spain", "Sweden")

## Funcs
source("R/fn_global_serfling_nb_cmdstan.R")
source("R/fn_age_serfling_nb_cmdstan.R")

## Smoothing window
year_smooth <- 5

# ##################################################
# 1 - Switzerland -----

COUNTRY = COUNTRIES[[1]]

pandemic <- c(1890, 1918, 1957, 2020)
pandemic_affected <- c(
  seq(1890 + 1, 1890 + year_smooth),
  seq(1918 + 1, 1918 + year_smooth),
  seq(1957 + 1, 1957 + year_smooth),
  2021)

deaths_monthly <- read_rds("data/deaths_monthly.Rds") %>% 
  filter(Country == COUNTRY)
deaths_yearly_age_sex <- read_rds("data/deaths_yearly_age_sex.Rds") %>% 
  filter(Country == COUNTRY) 

# 1957
m_age_1957 <- fn_age_serfling_nb_cmdstan(1957,
                                    deaths_monthly,
                                    deaths_yearly_age_sex,
                                    pandemic_years = pandemic,
                                    pop = "obs",
                                    version = "last_5")

S_age_1957 = m_age_1957$samples$draws("yearly_rel_excess_total_deaths") %>% 
  as_draws_df() %>% 
  pull(yearly_rel_excess_total_deaths)

# 2020
m_age_2020 <- fn_age_serfling_nb_cmdstan(2020,
                                         deaths_monthly,
                                         deaths_yearly_age_sex,
                                         pandemic_years = pandemic,
                                         pop = "obs",
                                         version = "last_5")

S_age_2020 = m_age_2020$samples$draws("yearly_rel_excess_total_deaths") %>% 
    as_draws_df() %>% 
  pull(yearly_rel_excess_total_deaths)

prob_2020_1957_CH = mean(S_age_2020 > S_age_1957)
round(prob_2020_1957_CH, digits = 4)


# ##################################################
# 2 - Spain -----

COUNTRY = COUNTRIES[[2]]

pandemic <- c(1918, 1957, 2020)
pandemic_affected <- c(
  seq(1918 + 1, 1918 + year_smooth),
  seq(1957 + 1, 1957 + year_smooth),
  2021)

deaths_monthly <- read_rds("data/deaths_monthly.Rds") %>% 
  filter(Country == COUNTRY)
deaths_yearly_age_sex <- read_rds("data/deaths_yearly_age_sex.Rds") %>% 
  filter(Country == COUNTRY) 

# 1957
m_age_1957 <- fn_age_serfling_nb_cmdstan(1957,
                                         deaths_monthly,
                                         deaths_yearly_age_sex,
                                         pandemic_years = pandemic,
                                         pop = "obs",
                                         version = "last_5")

S_age_1957  = m_age_1957$samples$draws("yearly_rel_excess_total_deaths") %>% 
  as_draws_df() %>% 
  pull(yearly_rel_excess_total_deaths)

# 2020
m_age_2020 <- fn_age_serfling_nb_cmdstan(2020,
                                         deaths_monthly,
                                         deaths_yearly_age_sex,
                                         pandemic_years = pandemic,
                                         pop = "obs",
                                         version = "last_5")
S_age_2020 = m_age_2020$samples$draws("yearly_rel_excess_total_deaths") %>% 
  as_draws_df() %>% 
  pull(yearly_rel_excess_total_deaths)

prob_2020_1957_ES = mean(S_age_2020 > S_age_1957)
round(prob_2020_1957_ES, digits = 4)


# ##################################################
# 3- Sweden -----

COUNTRY = COUNTRIES[[3]]

pandemic <- c(1890, 1918, 1957, 2020)
pandemic_affected <- c(
  seq(1890 + 1, 1890 + year_smooth),
  seq(1918 + 1, 1918 + year_smooth),
  seq(1957 + 1, 1957 + year_smooth),
  2021)

deaths_monthly <- read_rds("data/deaths_monthly.Rds") %>% 
  filter(Country == COUNTRY)
deaths_yearly_age_sex <- read_rds("data/deaths_yearly_age_sex.Rds") %>% 
  filter(Country == COUNTRY) 

# 1957
m_age_1957 <- fn_age_serfling_nb_cmdstan(1957,
                                         deaths_monthly,
                                         deaths_yearly_age_sex,
                                         pandemic_years = pandemic,
                                         pop = "obs",
                                         version = "last_5")

S_age_1957  = m_age_1957$samples$draws("yearly_rel_excess_total_deaths") %>% 
  as_draws_df() %>% 
  pull(yearly_rel_excess_total_deaths)

# 2020
m_age_2020 <- fn_age_serfling_nb_cmdstan(2020,
                                         deaths_monthly,
                                         deaths_yearly_age_sex,
                                         pandemic_years = pandemic,
                                         pop = "obs",
                                         version = "last_5")

S_age_2020 = m_age_2020$samples$draws("yearly_rel_excess_total_deaths") %>% 
  as_draws_df() %>% 
  pull(yearly_rel_excess_total_deaths)

prob_2020_1957_SE = mean(S_age_2020 > S_age_1957)
round(prob_2020_1957_SE, digits = 4)
