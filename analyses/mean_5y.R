# using monthly data without age
test1 <- deaths_monthly %>% 
  arrange(Country, Month, Year) %>% 
  group_by(Country, Month) %>% 
  mutate(mean_5y = slider::slide_dbl(Deaths, mean, 
                                     na.rm = TRUE, .complete = TRUE,
                                     .before = 5, .after = -1)) %>% 
  ungroup() %>% 
  arrange(Country, Year, Month) %>% 
  mutate(excess = as.integer(Deaths - mean_5y)) %>% 
  group_by(Country, Year) %>% 
  summarise(excess = sum(excess)) %>% 
  ungroup()

# using yearly data with age
test2 <- deaths_yearly_age_sex %>% 
  select(-Population_obs, -Population_exp) %>% 
  arrange(Country, Age_cat, Year) %>% 
  group_by(Country, Age_cat) %>% 
  mutate(mean_5y = slider::slide_dbl(Deaths, mean, 
                                     na.rm = TRUE, .complete = TRUE,
                                     .before = 5, .after = -1)) %>% 
  ungroup() %>% 
  arrange(Country, Year, Age_cat) %>% 
  mutate(excess = as.integer(Deaths - mean_5y)) %>% 
  group_by(Country, Year) %>% 
  summarise(excess = sum(excess)) %>% 
  ungroup()
