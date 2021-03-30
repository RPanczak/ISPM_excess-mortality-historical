# Demo R code for "Forecasting Imminent Deaths" 

rm(list=ls())
# Set working directory
setwd()

# Load packages
library(data.table)
library(ggplot2)
library(dplyr)

# Load data
# Source HMD 
data <- read.table("data.txt", sep="")
data.dt <- data.table(data)

# Compute later/earlier ratio
# Earlier death counts = sum of deaths from week 27 through week 10 by epi_year, age_group, sex and 
# country_code
# Later death counts = sum of deaths from week 11 through week 26 by epi_year, age_group, sex and 
# country_code
data.dt[, later_earlier_ratio := later_death_counts/earlier_death_counts, 
        by = list(epi_year, age_group, sex, country_code)]

# Average later/earlier ratio of 12 epiyears prior epiyear 2019/2020
data.dt[, average_later_earlier_ratio := mean(later_earlier_ratio[-c(seq(13, 520, by=13))]), 
        by = list(age_group, sex, country_code)]


# Illustration of later/earlier ratios (upsilon) by age_group, sex, country_code and epiyear
data.fig <- subset(data.dt, epi_year <= "2018/2019")
scaleFUN <- function(x) sprintf("%.2f", x)
upsilon <- ggplot(data = data.fig, 
                  aes(x = epi_year, y=later_earlier_ratio,
                      group=age_group, col=age_group))+
  geom_point(size=06) +
  geom_line(data = data.fig,aes(x=epi_year, y=average_later_earlier_ratio), alpha=0.5, size=3) +
  facet_grid(country_code+sex~age_group) +
  scale_y_continuous(labels=scaleFUN) +
  theme_minimal()+
  labs(x="Epiyear", y="Later/earlier ratio") +
  theme(title = element_text(size = 22, face = "bold"), 
        axis.text=element_text(size=18),
        plot.title = element_text(size=22),
        strip.text = element_text(size=20),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.text=element_text(size=20),
        legend.position = "bottom") + 
  coord_flip() +
  labs(color="Age group") 
ggsave("upsilon.pdf", plot = upsilon, width = 20, height =24)


# Illustration of deaths in later vs. earlier segments of epiyears by age_group, sex, and country_code
later_vs_earlier_deaths <- ggplot(data = data.fig, 
                                  aes(x = earlier_death_counts, 
                                      y= later_death_counts,
                                      group=sex, 
                                      col=sex)) +
  geom_point(size=06) +
  geom_smooth(se=FALSE, method=lm, size=02, alpha=0.5) +
  facet_wrap(~country_code+age_group, scales="free") +
  theme_minimal()+
  labs(x="Earlier death counts", y="Later death counts") +
  theme(title = element_text(size = 22, face = "bold"), 
        axis.text=element_text(size=20),
        plot.title = element_text(size=22),
        strip.text = element_text(size=20),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.text=element_text(size=20),
        legend.position = "bottom") + 
  labs(color="Sex") 
ggsave("later_vs_earlier_deaths.pdf", plot = later_vs_earlier_deaths , width = 24, height =13)

# Calculate regression slopes
data.dt %>% 
  group_by(age_group, sex, country_code) %>% 
  do({
    mod = lm(later_death_counts ~ 0 + earlier_death_counts, data = .)
    data.frame(Slope = coef(mod)[1])
  })

# Shortcast of expected deaths from week 11 through week 26 of epiyear 2019/2020
# in absence of Covid-19
data.dt[epi_year >= "2019/2020", expected_later_death_counts := earlier_death_counts * average_later_earlier_ratio,
        by= list(age_group, sex, country_code)]

# Compute excess deaths
data.dt[epi_year >= "2019/2020", excess_death_counts := later_death_counts - expected_later_death_counts,
        by= list(age_group, sex, country_code)]

# Observed, expected and excess deaths in epi_year 2019/2020 from week 11 through week 26 by 
# age_group, sex, and country_code
data.dt_DNK_SWE <- subset(data.dt, country_code %in% c("DNK", "SWE"))
table.2 <- data.dt_DNK_SWE[epi_year >= "2019/2020", list(later_deaths = round(later_death_counts), 
                                                         expected_later_deaths = round(expected_later_death_counts), 
                                                         excess_deaths = round(excess_death_counts)),
                           by = list(age_group, sex, country_code)]

# Observed, expected and excess deaths in epi_year 2019/2020 from week 11 through week 26 aggregated # by country_code
table.1 <- data.dt_DNK_SWE[epi_year >= "2019/2020", 
                           list(aggr_later_death_counts = sum(later_death_counts),
                                aggr_expected_later_death_counts = round(sum(expected_later_death_counts)),
                                aggr_excess_deaths = sum(later_death_counts) - round(sum(expected_later_death_counts)),
                                aggr_excess_observed = (sum(later_death_counts) - round(sum(expected_later_death_counts)))/sum(later_death_counts)),
                           by= list(country_code)]


# Compute 95% prediction intervals via bootstrapping
# Later/earlier ratios to draw from
upsilons <- matrix(data.dt_DNK_SWE[epi_year < "2019/2020", list(later_earlier_ratio),
                                   by = list(epi_year, age_group, sex, country_code)]$later_earlier_ratio, ncol=20)
# Earlier death counts registered in epiyear 2019-20
earlier_death_counts_2019.20 <- data.dt_DNK_SWE[epi_year >= "2019/2020", list(earlier_death_counts),
                                                by = list(age_group, sex, country_code)]$earlier_death_counts

n.sim = 10000
lambda = NULL
y = matrix(0, nrow= n.sim, ncol=20)
S = NULL

t1=Sys.time()
for ( i in 1:20){
  for (k in 1:n.sim){
    S = sample(x=upsilons[, i], size=1,  replace = T)
    # Compute expected deaths 
    lambda = (earlier_death_counts_2019.20[i]*S) 
    # Draw from Poisson
    y[k,i] <- rpois(n=1, lambda = lambda)
  }
}
t2=Sys.time()
t2-t1

# Observed deaths in epi_year 2019/2020 from week 11 through week 26 by age_group, sex, and 
# country_code
later_death_counts_2019.20 <- matrix(data.dt_DNK_SWE[epi_year >= "2019/2020", list(later_death_counts), 
                                                     by = list(age_group, sex, country_code)]$later_death_counts, nrow=5)

# Distribution excess 95% prediction intervals
PI = matrix(0, nrow=20, ncol=2)
colnames(PI) <- c("PI_low", "PI_high")
rownames(PI) <- c("DNK F 0","DNK F 15", "DNK F 65", "DNK F 75", "DNK F 85",
                  "DNK M 0","DNK M 15", "DNK M 65", "DNK M 75", "DNK M 85",
                  "SWE F 0","SWE F 15", "SWE F 65", "SWE F 75", "SWE F 85",
                  "SWE M 0","SWE M 15", "SWE M 65", "SWE M 75", "SWE M 85")
for (i in 1:20){
  PI[i,] = quantile(as.vector(later_death_counts_2019.20)[i] - y[,i], probs=c(0.025, 0.975))
}

# Comparison to 5-years average method
# Use data prior epiyear 2019-20, i.e. in the absence of Covid-19
data.dt_DNK_SWE_comparison <- data.dt_DNK_SWE[epi_year < "2019/2020", ]

# Shortcast of expected deaths from week 11 through week 26 
# with five years average death counts method 
# by age group, sex and country for epiyears 2012-13 through 2018-19
data.dt_DNK_SWE_comparison[ , five.average_later_death_counts := frollmean(later_death_counts,5),
                            by= list(age_group, sex, country_code)]
data.dt_DNK_SWE_comparison[, shifted.five.average_later_death_counts := shift(five.average_later_death_counts,1), 
                           by = list(age_group, sex, country_code)]

# Shortcast of expected deaths from week 11 through week 26 
# with earlier/later ratio method 
# by age group, sex and country for epiyears 2012-13 through 2018-19
data.dt_DNK_SWE_comparison[, later_earlier_ratio := later_death_counts/earlier_death_counts, 
                           by = list(epi_year, age_group, sex, country_code)]

data.dt_DNK_SWE_comparison[, average_later_earlier_ratio := frollmean(later_earlier_ratio,5), 
                           by = list(age_group, sex, country_code)]
data.dt_DNK_SWE_comparison[, shifted.average_later_earlier_ratio := shift(average_later_earlier_ratio,1), 
                           by = list(age_group, sex, country_code)]

data.dt_DNK_SWE_comparison[, expected_later_earlier_ratio_death_counts := earlier_death_counts * shifted.average_later_earlier_ratio, 
                           by = list(age_group, sex, country_code)]

# Calculate RMSE
data.dt_DNK_SWE_comparison[, RMSE_upsilon := sqrt(mean((later_death_counts - expected_later_earlier_ratio_death_counts)^2, na.rm=T)),
                           by = list(age_group, sex, country_code)]

data.dt_DNK_SWE_comparison[, RMSE_five := sqrt(mean((later_death_counts - shifted.five.average_later_death_counts)^2, na.rm=T)),
                           by = list(age_group, sex, country_code)]

table.3 <- data.dt_DNK_SWE_comparison[, list(RMSE.u = unique(round(RMSE_upsilon)), 
                                             RMSE.5 = unique(round(RMSE_five))),
                                      by = list(age_group, sex, country_code)]
