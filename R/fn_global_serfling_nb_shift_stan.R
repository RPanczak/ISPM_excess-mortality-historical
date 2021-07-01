# function for the global Serfling model, negative binomial version

# pred_year = 1918
# monthly_data = deaths_monthly
# ignore_year=NULL
# prior=10
# prior_intercept=10

fn_global_serfling_nb_shift_stan = function(pred_year, monthly_data, ignore_year=NULL, prior=10, prior_intercept=10) {
  require(rstan)
  options(mc.cores = parallel::detectCores())
  # format
  monthly_data %<>% arrange(Month,Year)
  # select last 5 years
  dd = dplyr::filter(monthly_data, Year >= pred_year - 5, Year < pred_year)
  # remove special year (e.g. 1918 because of the flu pandemic)
  if(!is.null(ignore_year)) {
    dd %<>% dplyr::filter(Year != ignore_year)
  }
  # extract prediction data
  pp = dplyr::filter(monthly_data, Year == pred_year)
  # format data into multi-dimensional arrays
  years = unique(dd$Year)
  years = years - min(years) + 1
  months = unique(dd$Month)
  I = length(years)
  J = length(months)
  total_deaths = array(dd$Deaths, dim=c(I,J))
  total_population = array(dd$Population, dim=c(I,J))
  predyear_total_deaths = array(pp$Deaths, dim=J)
  predyear_total_population = array(pp$Population, dim=J)
  # transform data into list
  dd_list = list(I=I,J=J,
                 years=years,
                 months=months,
                 total_deaths = total_deaths,
                 total_population = total_population,
                 predyear_total_deaths = predyear_total_deaths,
                 predyear_total_population = predyear_total_population)
  # set priors (default is normal(0,10))
  dd_list$p_beta = prior
  dd_list$p_alpha = prior_intercept
  # sampling
  ss = stan(file="stan/global_serfling_nb_shift.stan",
            data=dd_list,
            chains=4,
            iter=2000)
  # diagnostics
  # check_hmc_diagnostics(ss)
  # get prediction
  pp = summary(ss, pars="pred_total_deaths",probs=c(0.01,.5,0.99))[[1]] %>%
    as_tibble() %>%
    bind_cols(pp) %>%
    dplyr::rename(pred=`50%`,lower=`1%`,upper=`99%`) %>%
    dplyr::select(Year,Month,Date,Deaths,Population,pred,lower,upper)
  qq = summary(ss, pars="excess_total_deaths",probs=c(0.01,.5,0.99))[[1]] %>%
    as_tibble() %>%
    dplyr::select(excess_month=`50%`,excess_month_lower=`1%`,excess_month_upper=`99%`)
  rr = summary(ss, pars="yearly_excess_total_deaths",probs=c(0.01,.5,0.99))[[1]] %>%
    as_tibble() %>%
    dplyr::select(excess_year=`50%`,excess_year_lower=`1%`,excess_year_upper=`99%`)
  return(bind_cols(pp,qq,rr))
}
