results_age %>% 
  filter(Year %in% pandemic_year, Age_cat=="[0,10)") %>%  
  mutate(Lifeyearlost70 = number((yearly_excess_grouped_lifelost70/Population) * 100000, 
                                 accuracy = 5L, big.mark = " "),
         Lifeyearlost70_CI = paste0("(",
                                    number(yearly_excess_grouped_lifelost70_lower/Population * 100000,
                                           accuracy = 5L, big.mark = " "),
                                    " to ",
                                    number(yearly_excess_grouped_lifelost70_upper/Population * 100000,
                                           accuracy = 5L, big.mark = " "),
                                    ")"
         )
  )  %>% 
  select(Year, Country, Lifeyearlost70, Lifeyearlost70_CI)