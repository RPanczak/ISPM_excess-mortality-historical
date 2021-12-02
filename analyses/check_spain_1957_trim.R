library(pacman)
p_load(tidyverse, scales)


merge_mort <- function(data){
  
  data %>% 
    mutate(Pandemic = case_when(
      (Year >= 1889 & Year <= 1892) ~ 1890,
      (Year >= 1917 & Year <= 1920) ~ 1918,
      (Year >= 1956 & Year <= 1959) ~ 1957,
      (Year >= 2019 & Year <= 2021) ~ 2020
    )) %>% 
    relocate(Country) %>% 
    mutate(Country = fct_relevel(factor(Country), 
                                 "Switzerland", "Sweden", "Spain"))
}

data <- bind_rows(
  
  merge_mort(bind_rows(
    read_rds("data/outputs_2021-11-23/Sweden_results_month.Rds"),
    read_rds("data/outputs_2021-11-24/Spain_results_month.Rds"),
    read_rds("data/outputs_2021-11-23/Switzerland_results_month.Rds")
  )) %>% 
    filter(Model == "Age Serfling (Stan, NB)") %>% 
    mutate(Model = "Five") %>% 
    mutate(Date = anytime::anydate(paste0(Year, "-", Month))) %>% 
    select(-n_eff, -Rhat) %>% 
    arrange(Country, Year, Month) ,
  
  merge_mort(bind_rows(
    read_rds("data/outputs_2021-12-01/Sweden_results_month_last_7_notrim.Rds"),
    read_rds("data/outputs_2021-12-01/Spain_results_month_last_7_notrim.Rds"),
    read_rds("data/outputs_2021-12-01/Switzerland_results_month_last_7_notrim.Rds")
  )) %>% 
    filter(Model == "Age Serfling (Stan, NB, last 7 no trim)") %>% 
    mutate(Model = "Seven, notrim") %>% 
    mutate(Date = anytime::anydate(paste0(Year, "-", Month))) %>% 
    select(-n_eff, -Rhat) %>% 
    arrange(Country, Year, Month), 
  
  merge_mort(bind_rows(
    read_rds("data/outputs_2021-11-23/Sweden_results_month_last_7_trim.Rds"),
    read_rds("data/outputs_2021-11-24/Spain_results_month_last_7_trim.Rds"),
    read_rds("data/outputs_2021-11-23/Switzerland_results_month_last_7_trim.Rds")
  )) %>% 
    filter(Model == "Age Serfling (Stan, NB, last 7)") %>% 
    mutate(Model = "Seven, trim") %>% 
    select(-n_eff, -Rhat) %>% 
    mutate(Date = anytime::anydate(paste0(Year, "-", Month))) %>% 
    arrange(Country, Year, Month) 
  
) 

data %>% 
  filter(Country=="Spain") %>% 
  filter(Year >= 1956 & Year <= 1958) %>% 
  mutate(Deaths = ifelse(Model == "Five", Deaths, 0)) %>% 
  ggplot(aes(x = Date)) +
  geom_col(aes(y = Deaths), alpha = 0.33) + 
  geom_pointrange(aes(y = pred_total_deaths, 
                      ymin = pred_total_deaths_lower , 
                      ymax = pred_total_deaths_upper,
                      color = Model), 
                  position = position_dodge(width = 20)) + 
  facet_grid(vars(Year)) +
  theme_light()

data_age <- bind_rows(
  
  merge_mort(bind_rows(
    read_rds("data/outputs_2021-11-23/Sweden_results_age.Rds"),
    read_rds("data/outputs_2021-11-24/Spain_results_age.Rds"),
    read_rds("data/outputs_2021-11-23/Switzerland_results_age.Rds")
  )) %>% 
    filter(Model == "Age Serfling (Stan, NB)") %>% 
    mutate(Model = "Five") %>% 
    arrange(Country, Year, Age_cat) ,
  
  merge_mort(bind_rows(
    read_rds("data/outputs_2021-12-01/Sweden_results_age_last_7_notrim.Rds"),
    read_rds("data/outputs_2021-12-01/Spain_results_age_last_7_notrim.Rds"),
    read_rds("data/outputs_2021-12-01/Switzerland_results_age_last_7_notrim.Rds")
  )) %>% 
    filter(Model == "Age Serfling (Stan, NB, last 7 no trim)") %>% 
    mutate(Model = "Seven, notrim") %>% 
    arrange(Country, Year, Age_cat), 
  
  merge_mort(bind_rows(
    read_rds("data/outputs_2021-11-23/Sweden_results_age_last_7_trim.Rds"),
    read_rds("data/outputs_2021-11-24/Spain_results_age_last_7_trim.Rds"),
    read_rds("data/outputs_2021-11-23/Switzerland_results_age_last_7_trim.Rds")
  )) %>% 
    filter(Model == "Age Serfling (Stan, NB, last 7)") %>% 
    mutate(Model = "Seven, trim") %>% 
    arrange(Country, Year, Age_cat) 
  
) 

data_age %>% 
  filter(Country=="Spain") %>% 
  filter(Year >= 1956 & Year <= 1958) %>% 
  mutate(Deaths = ifelse(Model == "Five", Deaths, 0)) %>% 
  ggplot(aes(x = Age_cat)) +
  geom_col(aes(y = Deaths), alpha = 0.33) + 
  geom_pointrange(aes(y = pred_grouped_deaths, 
                      ymin = pred_grouped_deaths_lower , 
                      ymax = pred_grouped_deaths_upper,
                      color = Model), 
                  position = position_dodge(width = 0.9)) +
  facet_grid(vars(Year)) +
  theme_light()

data_age %>% 
  filter(Country=="Spain") %>% 
  filter(Year >= 1957-7 & Year <= 1957) %>% 
  ggplot(aes(x = Age_cat)) +
  geom_col(aes(y = Population_obs, fill = factor(Year)), position = "dodge") +
  theme_light()

