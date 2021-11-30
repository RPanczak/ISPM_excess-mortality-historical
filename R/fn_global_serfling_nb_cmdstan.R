# function for the global Serfling model, negative binomial version

# pred_year = 1988
# monthly_data = deaths_monthly
# pandemic_years =  pandemic
# pop = "obs"
# version = "last_5"
# prior = 10
# prior_intercept = 10
# 
# # for monthly vs weekly runs
# COUNTRY = "Switzerland"
# pred_year = 2005
# monthly_data = deaths_monthly

fn_global_serfling_nb_cmdstan = function(pred_year, monthly_data, pandemic_years, pop="obs", version="last_5", prior=10, prior_intercept=10, p=0.95) {
  
  require(cmdstanr)
  require(posterior)
  
  # select population
  if(pop=="obs") monthly_data$Population = monthly_data$Population_obs
  if(pop=="exp") monthly_data$Population = monthly_data$Population_exp
  
  # select last X years
  if(version=="last_5") {
    # select last 5 years
    dd = dplyr::filter(monthly_data, Year >= pred_year - 5, Year < pred_year)
    # remove special year (e.g. 1918 because of the flu pandemic)
    dd %<>% dplyr::filter(!(Year %in% pandemic_years))
    # arrange
    dd %<>% dplyr::arrange(Month,Year)
  }
  if(version=="last_7_trim") {
    # select last 7 years
    dd = dplyr::filter(monthly_data, Year >= pred_year - 7, Year < pred_year)
    # remove special year (e.g. 1918 because of the flu pandemic)
    dd %<>% dplyr::filter(!(Year %in% pandemic_years))
    # trim highest and lowest
    num_year = length(unique(dd$Year))
    dd %<>% 
      dplyr::group_by(Year) %>% 
      dplyr::mutate(TotDeaths=sum(Deaths)) %>% 
      dplyr::group_by(Month) %>% 
      dplyr::mutate(Rank=dplyr::dense_rank(TotDeaths)) %>% 
      dplyr::filter(Rank %in% 2:(num_year-1)) %>% 
      dplyr::arrange(Month,Year)
  }
  if(version=="last_7_notrim") {
    # select last 7 years
    dd = dplyr::filter(monthly_data, Year >= pred_year - 7, Year < pred_year)
    # remove special year (e.g. 1918 because of the flu pandemic)
    dd %<>% dplyr::filter(!(Year %in% pandemic_years))
  }
  if(pred_year!=2021) {
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
    
    # compile model
    global_serfling_nb <- cmdstan_model("stan/global_serfling_nb.stan",
                                        quiet = TRUE,
                                        force_recompile = FALSE)
  }
  if(pred_year==2021) {
    # extract prediction data excluding last 6 mo of 2021
    pp = dplyr::filter(monthly_data, Year == pred_year) %>% 
      dplyr::filter(!is.na(Deaths))
    
    # format data into multi-dimensional arrays
    years = unique(dd$Year)
    years = years - min(years) + 1
    months = unique(dd$Month)
    months2 = unique(pp$Month) # for 2021
    I = length(years)
    J = length(months)
    J2 = length(months2) # for 2021
    total_deaths = array(dd$Deaths, dim=c(I,J))
    total_population = array(dd$Population, dim=c(I,J))
    predyear_total_deaths = array(pp$Deaths, dim=J2)
    predyear_total_population = array(pp$Population, dim=J2)
    # transform data into list
    dd_list = list(I=I,J=J,
                   J2=J2, # for 2021
                   years=years,
                   months=months,
                   months2=months2, # for 2021
                   total_deaths = total_deaths,
                   total_population = total_population,
                   predyear_total_deaths = predyear_total_deaths,
                   predyear_total_population = predyear_total_population)
    
    # compile model
    global_serfling_nb <- cmdstan_model("stan/global_serfling_nb_21.stan",
                                        quiet = TRUE,
                                        force_recompile = FALSE)
  }
  
  # set priors (default is normal(0,10))
  dd_list$p_beta = prior
  dd_list$p_alpha = prior_intercept
  
  # sampling
  ss = global_serfling_nb$sample(
    data = dd_list, 
    seed = 12345, 
    chains = 4, 
    iter_sampling = 2000,
    parallel_chains = 4,
    refresh = 0,
    save_warmup = FALSE,
    show_messages = FALSE)
  
  # get prediction
  lp = (1 - p) / 2
  up = 1 - lp
  pp = posterior::summarise_draws(ss$draws("pred_total_deaths"), "mean", ~quantile(.x, probs = c(lp,up)),"rhat","ess_bulk") %>% 
    bind_cols(pp) %>%
    dplyr::rename(pred=mean,lower=3,upper=4) %>%
    dplyr::select(Country, Year,Month,Date,Deaths,Population,
                  pred_total_deaths=pred,pred_total_deaths_lower=lower,pred_total_deaths_upper=upper,
                  n_eff=ess_bulk,Rhat=rhat)
  pp = posterior::summarise_draws(ss$draws("excess_total_deaths"), "mean", ~quantile(.x, probs = c(lp,up))) %>%
    dplyr::select(excess_month=mean,excess_month_lower=3,excess_month_upper=4) %>%
    bind_cols(pp,.)
  pp = posterior::summarise_draws(ss$draws("rel_excess_total_deaths"), "mean", ~quantile(.x, probs = c(lp,up))) %>%
    dplyr::select(rel_excess_month=mean,rel_excess_month_lower=3,rel_excess_month_upper=4) %>%
    bind_cols(pp,.)
  pp = posterior::summarise_draws(ss$draws("yearly_pred_total_deaths"), "mean", ~quantile(.x, probs = c(lp,up))) %>%
    dplyr::select(yearly_pred_total_deaths=mean,yearly_pred_total_deaths_lower=3,yearly_pred_total_deaths_upper=4) %>%
    bind_cols(pp,.)
  pp = posterior::summarise_draws(ss$draws("yearly_excess_total_deaths"), "mean", ~quantile(.x, probs = c(lp,up))) %>%
    dplyr::select(yearly_excess_total_deaths=mean,yearly_excess_total_deaths_lower=3,yearly_excess_total_deaths_upper=4) %>%
    bind_cols(pp,.)
  pp = posterior::summarise_draws(ss$draws("yearly_rel_excess_total_deaths"), "mean", ~quantile(.x, probs = c(lp,up))) %>%
    dplyr::select(yearly_rel_excess_total_deaths=mean,yearly_rel_excess_total_deaths_lower=3,yearly_rel_excess_total_deaths_upper=4) %>%
    bind_cols(pp,.)
  

return(list(samples=ss,pred_total_deaths=pp))
}
