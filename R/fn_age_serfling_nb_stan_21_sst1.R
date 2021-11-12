# function for the joint Serfling model, negative binomial version

# pred_year = 2021
# monthly_data = filter(deaths_monthly, Country == "Switzerland")
# yearly_data = filter(deaths_yearly_age_sex, Country == "Switzerland")
# pandemic_years =  c(1890, 1918, 1957, 2020, 2021)
# prior=10
# prior_intercept=10
# p=0.95

fn_age_serfling_nb_stan_21_sst1 = function(pred_year, monthly_data, yearly_data, pandemic_years, pop="obs", prior=10, prior_intercept=10, p=0.95) {
  
  require(rstan)
  options(mc.cores = parallel::detectCores())
  
  # select population
  if(pop=="obs") { 
    monthly_data$Population = monthly_data$Population_obs
    yearly_data$Population = yearly_data$Population_obs
  }
  if(pop=="exp") {
    monthly_data$Population = monthly_data$Population_exp
    yearly_data$Population = yearly_data$Population_exp
  }
  
  # select last 7 years
  dd = dplyr::filter(monthly_data, Year >= pred_year - 7, Year < pred_year)
  ee = dplyr::filter(yearly_data, Year >= pred_year - 7, Year < pred_year) 
  
  # remove highest and lowest
  dd %<>% 
    dplyr::group_by(Year) %>% 
    dplyr::mutate(TotDeaths=sum(Deaths)) %>% 
    dplyr::group_by(Month) %>% 
    dplyr::mutate(Rank=dense_rank(TotDeaths)) %>% 
    dplyr::filter(Rank %in% 2:6) %>% 
    dplyr::arrange(Month,Year)
  ee %<>% 
    dplyr::group_by(Year) %>% 
    dplyr::mutate(TotDeaths=sum(Deaths)) %>% 
    dplyr::group_by(Age_cat) %>% 
    dplyr::mutate(Rank=dense_rank(TotDeaths)) %>% 
    dplyr::filter(Rank %in% 2:6) %>% 
    dplyr::arrange(Age_cat,Year)
  
  # remove special year (e.g. 1918 because of the flu pandemic)
  dd %<>% dplyr::filter(!(Year %in% pandemic_years))
  ee %<>% dplyr::filter(!(Year %in% pandemic_years))
  
  # extract prediction data excluding last 6 mo of 2021
  pp = dplyr::filter(monthly_data, Year == pred_year) %>% 
    dplyr::filter(!is.na(Deaths))
  qq = dplyr::filter(yearly_data, Year == pred_year)
  
  # format data into multi-dimensional arrays
  years = unique(dd$Year)
  years = years - min(years) + 1
  months = unique(dd$Month)
  months2 = unique(pp$Month)
  I = length(years)
  J = length(months)
  J2 = length(months2)
  K = length(unique(ee$Age_cat))
  total_deaths = array(dd$Deaths, dim=c(I,J))
  total_population = array(dd$Population, dim=c(I,J))
  grouped_deaths = round(array(ee$Deaths, dim=c(I,K)))
  grouped_population = array(ee$Population, dim=c(I,K))
  predyear_total_deaths = array(pp$Deaths, dim=J2)
  predyear_total_population = array(pp$Population, dim=J2)
  predyear_grouped_deaths = round(array(qq$Deaths, dim=K))
  predyear_grouped_population = qq$Population
  # transform data into list
  dd_list = list(I=I,J=J,J2=J2,K=K,
                 years=years,
                 months=months,
                 months2=months2,
                 total_deaths = total_deaths,
                 total_population = total_population,
                 grouped_deaths = round(grouped_deaths),
                 grouped_population = grouped_population,
                 predyear_total_deaths = predyear_total_deaths,
                 predyear_total_population = predyear_total_population,
                 predyear_grouped_deaths = predyear_grouped_deaths,
                 predyear_grouped_population = predyear_grouped_population)
  # set priors (default is normal(0,10))
  dd_list$p_beta = prior
  dd_list$p_alpha = prior_intercept
  # compiling and save compiled model (need to recompile on a new machine)
  mm = stan_model(file="stan/age_serfling_nb_21.stan", save_dso = TRUE)
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
    dplyr::select(Country, Year,Month,Date,Deaths,Population,pred,lower,upper,n_eff,Rhat)
  pp = summary(ss, pars="excess_total_deaths",probs=c(lp,.5,up))[[1]] %>%
    as_tibble() %>%
    dplyr::select(excess_month=1,excess_month_lower=4,excess_month_upper=6) %>%
    bind_cols(pp,.)
  pp = summary(ss, pars="yearly_excess_total_deaths",probs=c(lp,.5,up))[[1]] %>%
    as_tibble() %>%
    dplyr::select(excess_year=1,excess_year_lower=4,excess_year_upper=6) %>%
    bind_cols(pp,.)
  qq = summary(ss, pars="pred_grouped_deaths",probs=c(lp,.5,up))[[1]] %>%
    as_tibble() %>%
    dplyr::select(pred=1,lower=4,upper=6) %>%
    bind_cols(qq,.) 
  qq = summary(ss, pars="excess_grouped_deaths",probs=c(lp,.5,up))[[1]] %>%
    as_tibble() %>%
    dplyr::select(excess_year=1,excess_year_lower=4,excess_year_upper=6) %>%
    bind_cols(qq,.) 
  return(list(samples=ss,pred_total_deaths=pp,pred_grouped_deaths=qq))
}


if(FALSE) {
  # population data
  ggplot(ee) +
    geom_point(aes(x=Age_cat,y=Population)) +
    geom_point(data=qq,aes(x=Age_cat,y=Population),col="firebrick")
  
  # check computations
  # here something goes bonkers with 12 vs 6 :/ # JR: solved using sampling() instead of stan()
  uu = sampling(mm,
                data=dd_list,
                chains=1,
                iter=1)
  uu = extract(uu)
  
  lin = uu$lin[1,,,]
  exp_lin = uu$exp_lin[1,,,]
  exp_lin_total_deaths = uu$exp_lin_total_deaths[1,,]
  exp_lin_total_deaths
  apply(exp_lin,c(1,2),sum)
  
  exp_lin_grouped_deaths = uu$exp_lin_grouped_deaths[1,,]
  exp_lin_grouped_deaths
  apply(exp_lin,c(1,3),sum)
  
  prop_lin_grouped_deaths = uu$prop_lin_grouped_deaths[1,,]
  prop_lin_grouped_deaths
  exp_lin_grouped_deaths / apply(exp_lin_grouped_deaths,1,sum)
  
  pred_lin = uu$pred_lin[1,,]
  exp_pred_lin = uu$exp_pred_lin[1,,]
  exp_pred_lin_total_deaths = uu$exp_pred_lin_total_deaths[1,]
  exp_pred_lin_total_deaths
  apply(exp_pred_lin,1,sum)

  exp_pred_lin_grouped_deaths = uu$exp_pred_lin_grouped_deaths[1,]
  exp_pred_lin_grouped_deaths
  apply(exp_pred_lin,2,sum)
  
  prop_pred_lin_grouped_deaths = uu$prop_pred_lin_grouped_deaths[1,]
  prop_pred_lin_grouped_deaths
  exp_pred_lin_grouped_deaths / sum(exp_pred_lin_grouped_deaths)
  
  pred_total_deaths = uu$pred_total_deaths[1,]
  pred_grouped_deaths = uu$pred_grouped_deaths[1,]
  sum(pred_total_deaths)
  sum(pred_grouped_deaths)
  
  # diagnostics
  check_hmc_diagnostics(ss)
  get_bfmi(ss)
  stan_trace(ss,"alpha")
  stan_dens(ss,"alpha",separate_chains = TRUE)
  print(ss,pars=c("alpha"))
  stan_trace(ss,"beta_year")
  stan_dens(ss,"beta_year",separate_chains = TRUE)
  print(ss,pars=c("beta_year"))
  stan_trace(ss,"beta_periodic")
  stan_dens(ss,"beta_periodic",separate_chains = TRUE)
  print(ss,pars=c("beta_periodic"))
  
  # posterior predictive checks
  pp2 = pp
  dd2 = arrange(dd,Year,Month)
  
  summary(ss,pars="exp_lin_total_deaths")[[1]] %>%
    as_tibble() %>%
    bind_cols(dd2) %>%
    ggplot() +
    geom_ribbon(aes(x=Date,ymin=`2.5%`,ymax=`97.5%`),alpha=.4) +
    geom_line(aes(x=Date,y=`50%`)) +
    geom_point(aes(x=Date,y=Deaths)) +
    geom_ribbon(data=pp2,aes(x=Date,ymin=lower,ymax=upper),alpha=.4,fill="dodgerblue") +
    geom_line(data=pp2,aes(x=Date,y=pred,colour="dodgerblue")) +
    geom_point(data=pp2,aes(x=Date,y=Deaths),colour="firebrick") 
  
  qq2 = qq
  ee2 = arrange(ee, Year, Age_cat)
  
  summary(ss,pars="exp_lin_grouped_deaths")[[1]] %>%
    as_tibble() %>%
    bind_cols(ee2) %>%
    ggplot() +
    geom_col(aes(x=Age_cat,y=Deaths),fill=NA,colour="black",position=position_dodge(.8)) +
    geom_pointrange(aes(x=Age_cat,y=`50%`, ymin=`2.5%`,ymax=`97.5%`),
                    fill="white",shape=21, position=position_dodge(.8)) +
    geom_col(data=qq,aes(x=Age_cat,y=Deaths),fill=NA,colour="firebrick",position=position_dodge(.8)) +
    geom_pointrange(data=qq,aes(x=Age_cat,y=pred, ymin=lower,ymax=upper),
                    fill="dodgerblue",shape=21, position=position_dodge(.8)) +
    facet_wrap(~Year)
  
  ee3 = ee2 %>%
    group_by(Year) %>%
    mutate(prop_Deaths=Deaths/sum(Deaths))
  
  summary(ss,pars="prop_lin_grouped_deaths")[[1]] %>%
    as_tibble() %>%
    bind_cols(ee3) %>%
    ggplot() +
    geom_pointrange(aes(x=Year,y=`50%`, ymin=`2.5%`,ymax=`97.5%`,colour=Age_cat),
                    alpha=.4,position=position_dodge(.8)) +
    geom_point(aes(x=Year,y=prop_Deaths,fill=Age_cat),shape=21,position=position_dodge(.8))
  
  # expected deaths
  ll = list(samples=ss,pred_total_deaths=pp,pred_grouped_deaths=qq)
  
  source("R/fn_plot_global_serfling.R")
  fn_plot_global_serfling(ll, title="Expected deaths for 2020", ylim=c(0,10000))
  source("R/fn_plot_age_serfling.R")
  fn_plot_age_serfling(list(samples=ss,pred_total_deaths=pp,pred_grouped_deaths=qq))
}


