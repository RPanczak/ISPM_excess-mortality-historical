# function for the global Serfling model, negative binomial version

# pred_year = YEAR
# monthly_data = deaths_monthly
# pandemic_years =  pandemic
# pop = "exp"
# prior=10
# prior_intercept=10

fn_global_serfling_nb_stan = function(pred_year, monthly_data, pandemic_years, pop="obs", prior=10, prior_intercept=10, p=0.95) {
  
  require(rstan)
  # select population
  if(pop=="obs") monthly_data$Population = monthly_data$Population_obs
  if(pop=="exp") monthly_data$Population = monthly_data$Population_exp
  
  # select last 5 years
  dd = dplyr::filter(monthly_data, Year >= pred_year - 5, Year < pred_year)
  dd %<>% arrange(Month,Year)
  
  # remove special year (e.g. 1918 because of the flu pandemic)
  dd %<>% dplyr::filter(!(Year %in% pandemic_years))
  
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
  # compiling and save compiled model (need to recompile on a new machine)
  mm = stan_model(file="stan/global_serfling_nb.stan", save_dso = FALSE)
  # sampling
  ss = sampling(mm,
            data=dd_list,
            chains=4,
            iter=2000,
            refresh=0,
            save_warmup = FALSE)
  # get prediction
  lp = (1 - p) / 2
  up = 1 - lp
  pp = summary(ss, pars="pred_total_deaths",probs=c(lp,.5,up))[[1]] %>%
    as_tibble() %>%
    bind_cols(pp) %>%
    dplyr::rename(pred=1,lower=4,upper=6) %>%
    dplyr::select(Country,Year,Month,Date,Deaths,Population,pred,lower,upper,n_eff,Rhat)
  pp = summary(ss, pars="excess_total_deaths",probs=c(lp,.5,up))[[1]] %>%
    as_tibble() %>%
    dplyr::select(excess_month=1,excess_month_lower=4,excess_month_upper=6) %>%
    bind_cols(pp,.)
  pp = summary(ss, pars="yearly_excess_total_deaths",probs=c(lp,.5,up))[[1]] %>%
    as_tibble() %>%
    dplyr::select(excess_year=1,excess_year_lower=4,excess_year_upper=6) %>%
    bind_cols(pp,.)
  return(list(samples=ss,pred_total_deaths=pp))
}
