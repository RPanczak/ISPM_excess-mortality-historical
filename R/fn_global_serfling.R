# function for the global Serfling model

source("R/fn_boot_pi.R")

fn_global_serfling = function(pred_year, monthly_data, ignore_year=TRUE, p = 0.99) {
  
  # select last 5 years
  dd = dplyr::filter(monthly_data, Year >= pred_year - 5, Year < pred_year)
  
  # remove special year (e.g. 1918 because of the flu pandemic)
  if(ignore_year == TRUE & pred_year %in% pandemic_affected) {
    dd %<>% dplyr::filter(Year != pandemic)
  }
  
  # model
  mm = glm(Deaths ~ Year + si_one + si_two + co_one + co_two, 
           offset = log(Population),
           data = dd, 
           family = poisson(link="log"))
  # get prediction
  pp = dplyr::filter(monthly_data, Year == pred_year)
  pp = bind_cols(pp, boot_pi(mm, pp, 1000, p))
  return(list(model = mm, pred_total_deaths = pp))
}
