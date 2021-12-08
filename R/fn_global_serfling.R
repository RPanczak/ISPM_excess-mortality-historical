# function for the global Serfling model

# pred_year = 2021
# monthly_data = deaths_monthly %>% filter(Country == "Spain")
# pop = "obs"
# pandemic_years = pandemic
# p = 0.95

source("R/fn_boot_pi.R")

fn_global_serfling = function(pred_year, monthly_data, pop = "obs", pandemic_years, p = 0.95) {
  
  # select last 5 years
  dd = dplyr::filter(monthly_data, Year >= pred_year - 5, Year < pred_year)
  
  # remove special year (e.g. 1918 because of the flu pandemic)
  dd %<>% dplyr::filter(!(Year %in% pandemic_years))
  
  # select population
  if(pop == "obs") dd$Population = dd$Population_obs
  if(pop == "exp") dd$Population = dd$Population_exp
  
  # model
  mm = glm(Deaths ~ Year + si_one + si_two + co_one + co_two, 
           offset = log(Population),
           data = dd, 
           family = poisson(link = "log"))
  
  # get prediction
  pp = dplyr::filter(monthly_data, Year == pred_year)
  
  # remove second half of 2021
  if(pred_year == 2021) pp %<>% filter(! is.na(Deaths))
  
  if(pop=="obs") pp$Population = pp$Population_obs
  if(pop=="exp") pp$Population = pp$Population_exp
  
  pp = bind_cols(pp, boot_pi(mm, pp, 1000, p))
  
  return(list(model = mm, pred_total_deaths = pp))
}
