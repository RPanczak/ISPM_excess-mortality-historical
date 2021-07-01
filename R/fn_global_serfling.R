# function for the global Serfling model

fn_global_serfling = function(pred_year, monthly_data, ignore_year = NULL) {
  # compute periodic component
  monthly_data %<>% dplyr::mutate(si_one = sin(2*pi*Month/12),
                        si_two = sin(4*pi*Month/12),
                        co_one = cos(2*pi*Month/12),
                        co_two = cos(4*pi*Month/12))
  # select last 5 years
  dd = dplyr::filter(monthly_data, Year >= pred_year - 5, Year < pred_year)
  # remove special year (e.g. 1918 because of the flu pandemic)
  if(!is.null(ignore_year)) {
    dd %<>% dplyr::filter(Year != ignore_year)
  }
  # model
  mm = glm(Deaths ~ Year + si_one + si_two + co_one + co_two, 
           offset = log(Population),
           data = dd, 
           family = poisson(link="log"))
  # get prediction
  pp = dplyr::filter(monthly_data, Year == pred_year)
  pp = bind_cols(pp,boot_pi(mm, pp, 1000, 0.99))
  return(list(model=mm,pred_total_deaths=pp))
}
