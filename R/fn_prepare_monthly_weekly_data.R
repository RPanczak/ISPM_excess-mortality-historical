fn_prepare_monthly_weekly_data <- function(Country.data,data.timespan){
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
bfs_pop_year <- read_rds("data/BfS/bfs_pop_year.Rds") %>% 
  filter(Year >= min(deaths_data$Year)) %>% 
  mutate(Population = round((pop_jan + pop_dec) / 2)) %>% 
  select(-pop_jan, -pop_dec)

pop_2020_pred <- predict(
  lm(Population ~ Year, 
     data = bfs_pop_year %>% filter(Year >= 2010)),
  new = tibble(Year = 2020))

bfs_pop_year <- bfs_pop_year %>%
  add_row(Year=2020, Population=pop_2020_pred)
}
if(Country.data=="Spain") {
bfs_pop_year<- read_rds("data/INE/spain_deaths_month.Rds") %>% 
  filter(Year >= 2000)%>%
  filter(Month==12)%>%
  select( Year, Population)
}
else{
  bfs_pop_year<- read_rds("data/SCB/sweden_deaths_month.Rds") %>% 
    filter(Year >= 2000)%>%
    filter(Month==12)%>%
    select( Year, Population)
}

# merge population 
if(data.timespan=="Week"){
deaths_data %<>% 
  left_join(bfs_pop_year) %>%
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
  left_join(bfs_pop_year) %>%
  mutate(si_one = sin(2*pi*Month/12),
         si_two = sin(4*pi*Month/12),
         co_one = cos(2*pi*Month/12),
         co_two = cos(4*pi*Month/12)) %>% 
  mutate(Deaths = as.integer(round(Deaths))) %>% 
  mutate(Population = as.integer(round(Population)))
}

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


for (YEAR in year_reg:year_max){
  
  print(paste("Analysing year", YEAR))
  
  reg_data <- deaths_data %>% 
    filter(Year >= YEAR - year_smooth & Year < YEAR) 
  
  pred_data <- deaths_data %>% 
    filter(Year == YEAR) %>%
    mutate(
      fit = NA_real_,
      lpi = NA_real_,
      upi = NA_real_,
      fit_flu_hc = NA_real_,
      lpi_flu_hc = NA_real_,
      upi_flu_hc = NA_real_,
      fit_flu = NA_real_,
      lpi_flu = NA_real_,
      upi_flu = NA_real_)
  
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
      upi = predict$upper,
      fit_flu_hc = predict$pred,
      lpi_flu_hc = predict$lower,
      upi_flu_hc = predict$upper,
      fit_flu = predict$pred,
      lpi_flu = predict$lower,
      upi_flu = predict$upper
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



