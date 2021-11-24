monthly_data %>% 
  filter(Year>2012) %>% 
  filter(Year<2020) %>% 
  ggplot() +
  geom_line(aes(x=Month,y=Deaths,colour=as.factor(Year))) +
  geom_pointrange(data=results_month,aes(x=Month,y=pred_total_deaths,ymax=pred_total_deaths_upper,ymin=pred_total_deaths_lower)) +
  facet_grid(~Model)


yearly_data %>% 
  filter(Year>2012) %>% 
  filter(Year<2020) %>% 
  ggplot() +
  geom_point(aes(x=Age_cat,y=Deaths,colour=as.factor(Year))) +
  geom_pointrange(data=results_age,aes(x=Age_cat,y=pred_grouped_deaths,ymax=pred_grouped_deaths_upper,ymin=pred_grouped_deaths_lower)) +
  facet_grid(~Model)

yearly_data %>% 
  filter(Year>2012) %>% 
  # filter(Year<2020) %>% 
  ggplot() +
  geom_point(aes(x=Age_cat,y=Population_exp,colour=as.factor(Year)),position=position_dodge(.5)) 
