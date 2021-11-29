fn_comparison_monthly_weekly <- function(Country.data) {
  if(Country.data=="Switzerland"){
    stmf_month <- readRDS("data/mortality_org/expected_death_Switzerland_Month_stmf.Rds") %>%
      select(Year, Deaths, fit) %>%
      filter(Year > 2004) %>%
      mutate(Year = as.factor(Year))%>%
      dplyr::group_by(Year) %>%
      dplyr::summarise(EM_month=round(sum(fit),0),
                Deaths_month=sum(Deaths))
    
    stmf_week <- readRDS("data/mortality_org/expected_death_Switzerland_Week_stmf.Rds") %>%
      select(Year, Deaths, fit) %>%
      filter(Year > 2004) %>%
      mutate(Year = as.factor(Year))%>%
      dplyr::group_by(Year) %>%
      dplyr::summarise(EM_week=round(sum(fit),0),
                Deaths_week=sum(Deaths)) 
    
    stmf_both <- stmf_month %>%
      left_join(stmf_week) %>%
      mutate(Excess_death_month = Deaths_month - EM_month,
             Excess_death_week = Deaths_week - EM_week,
             Diff_death=Deaths_month-Deaths_week,
             Diff_Excess_death = Excess_death_month - Excess_death_week,
             P_excess_death_m=round((Excess_death_month/EM_month)*100,1),
             P_excess_death_w=round((Excess_death_week/EM_week)*100,1),
             Difference = Excess_death_month - Excess_death_week,
             Perc_diff = P_excess_death_m - P_excess_death_w) %>%
      select(Year, Expected_Mortality_m =EM_month, Expected_Mortality_w =EM_week,Deaths_m=Deaths_month,Deaths_w=Deaths_week, 
             Diff_death, Diff_Excess_Mortality=Diff_Excess_death,Percent_excess_mortality_m = P_excess_death_m,
             Percent_excess_mortality_w = P_excess_death_w,
             Difference, Perc_diff)
  }
  
  if(Country.data=="Spain"){
    stmf_month <- readRDS("data/mortality_org/expected_death_Spain_Month_stmf.Rds") %>%
      select(Year, Deaths, fit) %>%
      filter(Year > 2004) %>%
      mutate(Year = as.factor(Year))%>%
      dplyr::group_by(Year) %>%
      dplyr::summarise(EM_month=round(sum(fit),0),
                       Deaths_month=sum(Deaths))
    
    stmf_week <- readRDS("data/mortality_org/expected_death_Spain_Week_stmf.Rds") %>%
      select(Year, Deaths, fit) %>%
      filter(Year > 2004) %>%
      mutate(Year = as.factor(Year))%>%
      dplyr::group_by(Year) %>%
      dplyr::summarise(EM_week=round(sum(fit),0),
                       Deaths_week=sum(Deaths)) 
    
    stmf_both <- stmf_month%>%
      left_join(stmf_week) %>%
      mutate(Excess_death_month = Deaths_month - EM_month,
             Excess_death_week = Deaths_week - EM_week,
             Diff_death=Deaths_month-Deaths_week,
             Diff_Excess_death = Excess_death_month - Excess_death_week,
             P_excess_death_m=round((Excess_death_month/EM_month)*100,1),
             P_excess_death_w=round((Excess_death_week/EM_week)*100,1),
             Difference = Excess_death_month - Excess_death_week,
             Perc_diff = P_excess_death_m - P_excess_death_w) %>%
      select(Year, Expected_Mortality_m =EM_month, Expected_Mortality_w =EM_week,Deaths_m=Deaths_month,Deaths_w=Deaths_week, 
             Diff_death, Diff_Excess_Mortality=Diff_Excess_death,Percent_excess_mortality_m = P_excess_death_m,
             Percent_excess_mortality_w = P_excess_death_w,
             Difference, Perc_diff)
  }
  
  if(Country.data=="Sweden"){
    stmf_month <- readRDS("data/mortality_org/expected_death_Sweden_Month_stmf.Rds") %>%
      select(Year, Deaths, fit) %>%
      filter(Year > 2004) %>%
      mutate(Year = as.factor(Year))%>%
      dplyr::group_by(Year) %>%
      dplyr::summarise(EM_month=round(sum(fit),0),
                       Deaths_month=sum(Deaths))
    
    stmf_week <- readRDS("data/mortality_org/expected_death_Sweden_Week_stmf.Rds") %>%
      select(Year, Deaths, fit) %>%
      filter(Year > 2004) %>%
      mutate(Year = as.factor(Year))%>%
      dplyr::group_by(Year) %>%
      dplyr::summarise(EM_week=round(sum(fit),0),
                       Deaths_week=sum(Deaths)) 
    
    stmf_both <- stmf_month%>%
      left_join(stmf_week) %>%
      mutate(Excess_death_month = Deaths_month - EM_month,
             Excess_death_week = Deaths_week - EM_week,
             Diff_death=Deaths_month-Deaths_week,
             Diff_Excess_death = Excess_death_month - Excess_death_week,
             P_excess_death_m=round((Excess_death_month/EM_month)*100,1),
             P_excess_death_w=round((Excess_death_week/EM_week)*100,1),
             Difference = Excess_death_month - Excess_death_week,
             Perc_diff = P_excess_death_m - P_excess_death_w) %>%
      select(Year, Expected_Mortality_m =EM_month, Expected_Mortality_w =EM_week,Deaths_m=Deaths_month,Deaths_w=Deaths_week, 
             Diff_death, Diff_Excess_Mortality=Diff_Excess_death,Percent_excess_mortality_m = P_excess_death_m,
             Percent_excess_mortality_w = P_excess_death_w,
             Difference, Perc_diff)
    
  }
  
  return(stmf_both)
  
}