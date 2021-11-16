# function for the joint Serfling model, negative binomial version

# pred_year = 2020
# monthly_data = deaths_monthly
# yearly_data = deaths_yearly_age_sex
# pandemic_years =  c(1890, 1918, 1957, 2020, 2021)
# pop = "obs"
# prior = 10
# prior_intercept = 10
# p = 0.95

fn_age_serfling_nb_cmdstan = function(pred_year, monthly_data, yearly_data, pandemic_years, pop="obs", prior=10, prior_intercept=10, p=0.95) {
  
  require(cmdstanr)
  
  # compile model
  age_serfling_nb <- cmdstan_model("stan/age_serfling_nb.stan",
                                   quiet = FALSE,
                                   force_recompile = FALSE)
  
  # age_serfling_nb$print()
  # age_serfling_nb$exe_file()
  # age_serfling_nb$check_syntax()
  # age_serfling_nb$check_syntax(pedantic = TRUE)
  
  # select population
  if(pop=="obs") { 
    monthly_data$Population = monthly_data$Population_obs
    yearly_data$Population = yearly_data$Population_obs
  }
  if(pop=="exp") {
    monthly_data$Population = monthly_data$Population_exp
    yearly_data$Population = yearly_data$Population_exp
  }
  
  # select last 5 years
  dd = dplyr::filter(monthly_data, Year >= pred_year - 5, Year < pred_year)
  dd %<>% dplyr::arrange(Month, Year)
  ee = dplyr::filter(yearly_data, Year >= pred_year - 5, Year < pred_year) 
  ee %<>% dplyr::arrange(Age_cat, Year)
  
  # remove special year (e.g. 1918 because of the flu pandemic)
  dd %<>% dplyr::filter(!(Year %in% pandemic_years))
  ee %<>% dplyr::filter(!(Year %in% pandemic_years))
  
  # extract prediction data
  pp = dplyr::filter(monthly_data, Year == pred_year)
  qq = dplyr::filter(yearly_data, Year == pred_year)
  
  # format data into multi-dimensional arrays
  years = unique(dd$Year)
  years = years - min(years) + 1
  months = unique(dd$Month)
  I = length(years)
  J = length(months)
  K = length(unique(ee$Age_cat))
  total_deaths = array(dd$Deaths, dim=c(I,J))
  grouped_deaths = round(array(ee$Deaths, dim=c(I,K)))
  predyear_total_deaths = array(pp$Deaths, dim=J)
  predyear_total_population = array(pp$Population, dim=J)
  predyear_grouped_deaths = round(array(qq$Deaths, dim=K))
  predyear_grouped_population = qq$Population
  total_population = array(dd$Population, dim=c(I,J))
  grouped_population = array(ee$Population, dim=c(I,K))
  predyear_total_population = array(pp$Population, dim=J)
  
  # transform data into list
  dd_list = list(I=I,J=J,K=K,
                 years=years,
                 months=months,
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
  
  # sampling
  ss = age_serfling_nb$sample(
    data = dd_list, 
    seed = 12345, 
    chains = 4, 
    iter_sampling = 2000,
    parallel_chains = 4,
    refresh = 0,
    save_warmup = FALSE
  )
  
  # median too?
  ss = ss$summary() %>% 
    dplyr::select(variable, mean, 
                  # median,
                  q5, q95)
  
  # base = pp %>% 
  #   select(-si_one, -si_two, -co_one, -co_two)
  
  excess_month = 
    dplyr::bind_cols(
      
      # predicted
      ss %>%
        dplyr::filter(stringr::str_detect(variable, 
                                          stringr::fixed("pred_total_deaths["))) %>%
        dplyr::rename(pred_month = mean,
                      pred_month_lower = q5,
                      pred_month_upper  = q95) %>% 
        dplyr::select(-variable), 
      
      # excess 
      ss %>% 
        dplyr::filter(stringr::str_detect(variable, 
                                          stringr::fixed("excess_total_deaths["))) %>% 
        dplyr::rename(excess_month = mean,
                      excess_month_lower = q5,
                      excess_month_upper  = q95) %>% 
        dplyr::select(-variable) ) %>% 
    dplyr::mutate(Year = as.integer(pred_year),
                  Month = as.integer(1:12),
                  Model = "Age Serfling (Stan, NB)")
  
  excess_year = ss %>% 
    dplyr::filter(stringr::str_detect(variable, 
                                      "yearly_excess_total_deaths")) %>% 
    dplyr::mutate(Year = as.integer(pred_year),
                  Model = "Age Serfling (Stan, NB)") %>% 
    dplyr::rename(excess_year = mean,
                  excess_year_lower = q5,
                  excess_year_upper  = q95)  %>% 
    dplyr::select(-variable)
  
  excess_age = 
    dplyr::bind_cols(
      
      # predicted
      ss %>%
        dplyr::filter(stringr::str_detect(variable, 
                                          stringr::fixed("pred_grouped_deaths["))) %>%
        dplyr::rename(pred_month = mean,
                      pred_month_lower = q5,
                      pred_month_upper  = q95) %>% 
        dplyr::select(-variable), 
      
      # excess 
      ss %>% 
        dplyr::filter(stringr::str_detect(variable, 
                                          stringr::fixed("excess_grouped_deaths["))) %>% 
        dplyr::rename(excess_month = mean,
                      excess_month_lower = q5,
                      excess_month_upper  = q95) %>% 
        dplyr::select(-variable) ) %>% 
    dplyr::mutate(Year = as.integer(pred_year),
                  Age_cat = factor(1:10, labels = levels(ee$Age_cat)),
                  Model = "Age Serfling (Stan, NB)")
  
  return(list(
    # base = base,
    excess_month = excess_month, 
    excess_year = excess_year, 
    excess_age = excess_age)) 
}

if(FALSE) {
  
  # ss$cmdstan_diagnose()
  # ss$cmdstan_summary()
  # fit_mle <- age_serfling_nb$optimize(data = dd_list, seed = 12345) 
  # fit_mle$summary()
  # fit_vb <- age_serfling_nb$variational(data = dd_list, seed = 12345, output_samples = 4000) 
  # fit_vb$summary()
  
}

if(FALSE) {
  # population data
  ggplot(ee) +
    geom_point(aes(x=Age_cat,y=Population)) +
    geom_point(data=qq,aes(x=Age_cat,y=Population),col="firebrick")
  
  # check computations
  uu = stan(file="stan/age_serfling_nb.stan",
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
  fn_plot_global_serfling(ll, title="Expected deaths for 2020", ylim=c(0,10000))
  fn_plot_age_serfling(list(samples=ss,pred_total_deaths=pp,pred_grouped_deaths=qq))
}


