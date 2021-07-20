fn_prepare_monthly_weekly_data <- function(Country.data, data.timespan){
  # load data
  
  if(data.timespan=="Week"){
    deaths_data <- read_rds("data/mortality_org/stmf_week.Rds") %>%
      filter(Country == Country.data) %>%
      filter(!Year == 2021)
  }
  else {
    deaths_data <- read_rds("data/mortality_org/stmf_month.Rds") %>%
      filter(Country == Country.data) %>%
      filter(!Year == 2021)
  }
  
  # define years
  year_smooth <- 5
  year_from <- min(deaths_data$Year)
  year_reg <- year_from + year_smooth
  year_max <- 2020
  
  # load population data
  if(Country.data=="Switzerland") {
    pop_year <- read_rds("data/BfS/ch_pop_year.Rds") %>% 
      filter(Year >= 2000) %>% 
      mutate(Population = round((pop_jan + pop_dec) / 2)) %>% 
      select(-pop_jan, -pop_dec)
    
    pop_2020 <- read.px("data-raw/BfS/px-x-0102020000_202.px") %>% 
      as_tibble() %>% remove_empty() %>% clean_names() %>% 
      filter(demografische_komponente %in% c("Bestand am 1. Januar", "Bestand am 31. Dezember")) %>% 
      filter(alter == "Alter - Total") %>% 
      filter(geschlecht == "Geschlecht - Total") %>% 
      filter(kanton == "Schweiz") %>% 
      filter(staatsangehorigkeit_kategorie == "Staatsangehörigkeit (Kategorie) - Total") %>% 
      select(demografische_komponente, value) %>% 
      summarise(Population = round(mean(value))) %>% 
      mutate(Year = 2020) %>% relocate(Year)
    
    pop_year <- pop_year %>%
      bind_rows(pop_2020)
    
    rm(pop_2020)
  }
  if(Country.data=="Spain") {
    pop_year<- read_rds("data/INE/es_pop_year.Rds") %>% 
      filter(Year >= 2000)
  }
  if(Country.data=="Sweden") {
    pop_year<- read_rds("data/SCB/se_pop_year.Rds") %>% 
      filter(Year >= 2000)
   
  }
  
  # merge population 
  if(data.timespan=="Week"){
    deaths_data %<>% 
      left_join(pop_year, by = "Year") %>%
      mutate(Date = Week_start,
             si_one = sin(2*pi*Week/(365.25/7)),
             si_two = sin(2*pi*Week/(365.25/2/7)),
             co_one = cos(2*pi*Week/(365.25/7)),
             co_two = cos(2*pi*Week/(365.25/2/7)),
             Deaths = as.integer(round(Deaths)),
             Population = as.integer(round(Population)))
  }
  else{
    deaths_data %<>% 
      left_join(pop_year, by = "Year") %>%
      mutate(si_one = sin(2*pi*Month/12),
             si_two = sin(4*pi*Month/12),
             co_one = cos(2*pi*Month/12),
             co_two = cos(4*pi*Month/12)) %>% 
      mutate(Deaths = as.integer(round(Deaths))) %>% 
      mutate(Population = as.integer(round(Population)))
  }
  
  rm(pop_year)
  
  # run Marcel's regression approach
  registerDoParallel(cores = 4)
  
  boot_pi <- function(model, pdata, n, p) {
    odata <- model$data
    lp <- (1 - p) / 2
    up <- 1 - lp
    seeds <- round(runif(n, 1, 1000), 0)
    boot_y <- foreach(i = 1:n, .combine = rbind) %dopar% {
      set.seed(seeds[i])
      bdata <- odata[sample(seq(nrow(odata)), size = nrow(odata), replace = TRUE), ]
      bpred <- predict(update(model, data = bdata), type = "response", newdata = pdata)
      rpois(length(bpred), lambda = bpred)
    }
    boot_ci <- t(apply(boot_y, 2, quantile, c(lp, up)))
    return(data.frame(pred = predict(model, newdata = pdata, type = "response"), 
                      lower = boot_ci[, 1], upper = boot_ci[, 2]))
  }
  
  
  print(paste("Analysing", Country.data))
  
  for (YEAR in year_reg:year_max){
    
    print(paste("   year", YEAR))
    
    reg_data <- deaths_data %>% 
      filter(Year >= YEAR - year_smooth & Year < YEAR) 
    
    pred_data <- deaths_data %>% 
      filter(Year == YEAR) %>%
      mutate(
        fit = NA_real_,
        lpi = NA_real_,
        upi = NA_real_
        )
    
    # summary(reg_data$Year)
    # summary(pred_data$Year)
    
    # all data
    summary(timespan <- glm(Deaths ~ Year + 
                              si_one + si_two + co_one + co_two, 
                            offset = log(Population),
                            data = reg_data, 
                            family = "poisson"))
    
    predict <- boot_pi(timespan, pred_data, 1000, 0.99)
    
    pred_data <- pred_data %>%
      mutate(
        fit = predict$pred,
        lpi = predict$lower,
        upi = predict$upper
      )
    
    # flu hc excluded
    if (YEAR == year_reg) {
      expected_deaths <- pred_data
    } else {
      expected_deaths <- bind_rows(expected_deaths, pred_data)
    }
    write_rds(expected_deaths,paste0("data/mortality_org/expected_death_",Country.data,"_",data.timespan,"_stmf.Rds"))
  }
}