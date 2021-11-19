# using monthly data without age
deaths_monthly <- read_rds("data/deaths_monthly.Rds") %>% 
  select(-starts_with("Population"), -starts_with("si_"), -starts_with("co_")) %>% 
  filter(! is.na(Deaths))

# using yearly data with age
deaths_yearly_age_sex <- read_rds("data/deaths_yearly_age_sex.Rds") %>% 
  select(-starts_with("Population")) %>% 
  filter(Year >= 1851)

deaths_monthly %>% 
  filter(Year == 2021 & Country == "Switzerland") %>% 
  summarise(Excess = sum(Deaths, na.rm = TRUE))

deaths_yearly_age_sex %>% 
  filter(Year == 2021 & Country == "Switzerland") %>% 
  summarise(Excess = sum(Deaths, na.rm = TRUE))

result_long <- bind_rows(
  
  deaths_monthly %>% 
    arrange(Country, Month, Year) %>% 
    group_by(Country, Month) %>% 
    mutate(mean_5y = slider::slide_dbl(Deaths, mean, 
                                       na.rm = TRUE, .complete = TRUE,
                                       .before = 5, .after = -1)) %>% 
    ungroup() %>% 
    arrange(Country, Year, Month) %>% 
    mutate(excess = as.integer(Deaths - mean_5y)) %>% 
    group_by(Country, Year) %>% 
    summarise(Excess = sum(excess)) %>% 
    mutate(Type = "month") %>% 
    ungroup(),
  
  deaths_yearly_age_sex %>% 
    arrange(Country, Age_cat, Year) %>% 
    group_by(Country, Age_cat) %>% 
    mutate(mean_5y = slider::slide_dbl(Deaths, mean, 
                                       na.rm = TRUE, .complete = TRUE,
                                       .before = 5, .after = -1)) %>% 
    ungroup() %>% 
    arrange(Country, Year, Age_cat) %>% 
    mutate(excess = as.integer(Deaths - mean_5y)) %>% 
    group_by(Country, Year) %>% 
    summarise(Excess = sum(excess)) %>% 
    mutate(Type = "age") %>% 
    ungroup()
  
) %>% 
  filter(! is.na(Excess))

result_long %>% 
  filter(Year == 2021 & Country == "Switzerland") 

result_long %>% 
  # 2021 age has to go away since it's based on 6 mo only!
  filter(! (Type == "age" & Year == 2021)) %>% 
  ggplot(aes(x = Year, y = Excess, color = Type)) +
  geom_line() + 
  facet_grid(vars(Country), scales = "free_y")

result_wide <- left_join(
  
  deaths_monthly %>% 
    arrange(Country, Month, Year) %>% 
    group_by(Country, Month) %>% 
    mutate(mean_5y = slider::slide_dbl(Deaths, mean, 
                                       na.rm = TRUE, .complete = TRUE,
                                       .before = 5, .after = -1)) %>% 
    ungroup() %>% 
    arrange(Country, Year, Month) %>% 
    mutate(excess = as.integer(Deaths - mean_5y)) %>% 
    group_by(Country, Year) %>% 
    summarise(Excess_month = sum(excess)) %>% 
    ungroup(),
  
  deaths_yearly_age_sex %>% 
    arrange(Country, Age_cat, Year) %>% 
    group_by(Country, Age_cat) %>% 
    mutate(mean_5y = slider::slide_dbl(Deaths, mean, 
                                       na.rm = TRUE, .complete = TRUE,
                                       .before = 5, .after = -1)) %>% 
    ungroup() %>% 
    arrange(Country, Year, Age_cat) %>% 
    mutate(excess = as.integer(Deaths - mean_5y)) %>% 
    group_by(Country, Year) %>% 
    summarise(Excess_age = sum(excess)) %>% 
    ungroup()
  
)

result_wide %>% 
  # 2021 has to go away since it's based on 6 mo only!
  filter(! (Year == 2021)) %>%   
  ggplot(aes(x = Year, y = Excess_month - Excess_age)) +
  geom_line() + 
  scale_y_continuous()+
  facet_grid(vars(Country), scales = "free_y")

result_wide %>% 
  # 2021 has to go away since it's based on 6 mo only!
  filter(! (Year == 2021)) %>%   
  ggplot(aes(x = Year, y = (Excess_month - Excess_age) / Excess_month)) +
  geom_line() + 
  scale_y_continuous(labels = scales::percent)+
  facet_grid(vars(Country), scales = "free_y")
