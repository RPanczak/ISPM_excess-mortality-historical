Spain_results_month <- read_rds("data/outputs_2021-07-16/Spain_results_month.Rds") %>% 
  filter(Year != 2020)
Spain_results_year <- read_rds("data/outputs_2021-07-16/Spain_results_year.Rds") %>% 
  filter(Year != 2020)
Spain_results_age <- read_rds("data/outputs_2021-07-16/Spain_results_age.Rds") %>% 
  filter(Year != 2020)

# #############################################
# Model 2

extract_month <- global_serfling_stan$pred_total_deaths %>% 
  select(Country, Year, Month, Deaths,
         pred, lower, upper, n_eff, Rhat,
         excess_month, excess_month_lower, excess_month_upper) %>% 
  mutate(Model = "Global Serfling (Stan, NB)",
         mutate(across(pred:excess_month_upper, round)))

Spain_results_month <- bind_rows(Spain_results_month, extract_month)

extract_year <- global_serfling_stan$pred_total_deaths %>% 
  filter(row_number() == 1) %>% 
  select(Country, Year, 
         excess_year, excess_year_lower, excess_year_upper) %>% 
  mutate(Model = "Global Serfling (Stan, NB)",
         mutate(across(pred:excess_year_upper, round)))

Spain_results_year <- bind_rows(Spain_results_year, extract_year)

rm(extract_month, extract_year)

# #############################################
# Model 3

extract_month <- age_serfling_nb_stan$pred_total_deaths %>% 
  select(Country, Year, Month, Deaths,
         pred, lower, upper, n_eff, Rhat,
         excess_month, excess_month_lower, excess_month_upper) %>% 
  mutate(Model = "Age Serfling (Stan, NB)",
         mutate(across(pred:excess_month_upper, round)))

Spain_results_month <- bind_rows(Spain_results_month, extract_month)

extract_year <- age_serfling_nb_stan$pred_total_deaths %>% 
  filter(row_number() == 1) %>% 
  select(Country, Year, 
         excess_year, excess_year_lower, excess_year_upper) %>% 
  mutate(Model = "Age Serfling (Stan, NB)",
         mutate(across(pred:excess_year_upper, round)))

Spain_results_year <- bind_rows(Spain_results_year, extract_year)

extract_age <- age_serfling_nb_stan$pred_grouped_deaths %>% 
  select(Country, Year, Age_cat, Deaths,
         pred, lower, upper,
         excess_year, excess_year_lower, excess_year_upper) %>% 
  mutate(Model = "Age Serfling (Stan, NB)",
         mutate(across(pred:excess_year_upper, round)))

Spain_results_age <- bind_rows(Spain_results_age, extract_age)

rm(extract_month, extract_year, extract_age)

write_rds(Spain_results_month, 
          "data/outputs_2021-07-16/Spain_results_month.Rds")
write_rds(Spain_results_year, 
          "data/outputs_2021-07-16/Spain_results_year.Rds")
write_rds(Spain_results_age,
          "data/outputs_2021-07-16/Spain_results_age.Rds")


