# function for the global Serfling model, negative binomial version
# based on fn_global_serfling_nb_cmdstan.R
# adapted to work with weekly data

# # for monthly vs weekly runs
# COUNTRY = "Spain"
# pred_year = 2012
# pandemic_years =  pandemic
# pop = "obs"
# prior = 10
# prior_intercept = 10
# weekly_data = deaths_weekly

fn_global_serfling_nb_week_cmdstan = function(pred_year, weekly_data, pandemic_years, pop="obs", prior=10, prior_intercept=10, p=0.95) {
  
  require(cmdstanr)
  require(posterior)
  
  # select population
  if(pop=="obs") weekly_data$Population = weekly_data$Population_obs
  if(pop=="exp") weekly_data$Population = weekly_data$Population_exp
  
  # select last 5 years
  dd = dplyr::filter(weekly_data, Year >= pred_year - 5, Year < pred_year)
  # remove special year (e.g. 1918 because of the flu pandemic)
  dd %<>% dplyr::filter(!(Year %in% pandemic_years))
  # arrange
  dd %<>% dplyr::arrange(Week, Year)
  
  if(pred_year!=2021) {
    # extract prediction data
    pp = dplyr::filter(weekly_data, Year == pred_year)
    
    # format data into multi-dimensional arrays
    years = unique(dd$Year)
    years = years - min(years) + 1
    weeks = unique(dd$Week)
    I = length(years)
    J = length(weeks)
    total_deaths = array(dd$Deaths, dim=c(I,J))
    total_population = array(dd$Population, dim=c(I,J))
    predyear_total_deaths = array(pp$Deaths, dim=J)
    predyear_total_population = array(pp$Population, dim=J)
    
    # transform data into list
    dd_list = list(I=I,J=J,
                   years=years,
                   weeks=weeks,
                   total_deaths = total_deaths,
                   total_population = total_population,
                   predyear_total_deaths = predyear_total_deaths,
                   predyear_total_population = predyear_total_population)
    
    # compile model
    global_serfling_nb <- cmdstan_model("stan/global_serfling_nb_week.stan",
                                        quiet = TRUE,
                                        force_recompile = FALSE)
  }
  if(pred_year==2021) {
    # extract prediction data excluding last 6 mo of 2021
    pp = dplyr::filter(weekly_data, Year == pred_year) %>% 
      dplyr::filter(!is.na(Deaths))
    
    # format data into multi-dimensional arrays
    years = unique(dd$Year)
    years = years - min(years) + 1
    weeks = unique(dd$Week)
    weeks2 = unique(pp$Week) # for 2021
    I = length(years)
    J = length(weeks)
    J2 = length(weeks2) # for 2021
    total_deaths = array(dd$Deaths, dim=c(I,J))
    total_population = array(dd$Population, dim=c(I,J))
    predyear_total_deaths = array(pp$Deaths, dim=J2)
    predyear_total_population = array(pp$Population, dim=J2)
    # transform data into list
    dd_list = list(I=I,J=J,
                   J2=J2, # for 2021
                   years=years,
                   weeks=weeks,
                   weeks2=weeks2, # for 2021
                   total_deaths = total_deaths,
                   total_population = total_population,
                   predyear_total_deaths = predyear_total_deaths,
                   predyear_total_population = predyear_total_population)
    
    # compile model
    global_serfling_nb <- cmdstan_model("stan/global_serfling_nb_21_week.stan",
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
    dplyr::select(Country,Year,Week,Deaths,Population,pred,lower,upper,n_eff=ess_bulk,Rhat=rhat)
  pp = posterior::summarise_draws(ss$draws("excess_total_deaths"),"mean", ~quantile(.x, probs = c(lp,up))) %>% 
    dplyr::select(excess_week=mean,excess_week_lower=3,excess_week_upper=4) %>%
    bind_cols(pp,.)
  pp = posterior::summarise_draws(ss$draws("yearly_excess_total_deaths"),"mean",~quantile(.x, probs = c(lp,up))) %>% 
    dplyr::select(excess_year=mean,excess_year_lower=3,excess_year_upper=4) %>%
    bind_cols(pp,.)
  
  return(list(samples=ss,pred_total_deaths=pp))
}
