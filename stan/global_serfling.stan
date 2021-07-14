data {
  // past years
  int I; // number of years (generally 5 except if some year is ignored)
  int J; // number of months (12)
  vector[I] years;
  vector[J] months;
  int total_deaths[I,J];
  int total_population[I,J];
  // prediction year
  int predyear_total_deaths[J];
  int predyear_total_population[J];
  // priors
  real p_alpha;
  real p_beta;
}
parameters {
  real alpha;
  real beta_year;
  real beta_periodic[4]; 
}
transformed parameters {
  real lin[I,J]; // linear predictor
  real<lower=0> exp_lin[I,J];
  for(i in 1:I) 
    for(j in 1:J)
      lin[i,j] = alpha + 
        beta_year*years[i] + 
        beta_periodic[1]*sin(2*pi()*months[j]/12) + 
        beta_periodic[2]*sin(4*pi()*months[j]/12) + 
        beta_periodic[3]*cos(2*pi()*months[j]/12) + 
        beta_periodic[4]*cos(4*pi()*months[j]/12) + 
        log(total_population[i,j]);
  exp_lin = exp(lin);
}
model {
  // priors
  alpha ~ normal(0,p_alpha);
  beta_year ~ normal(0,p_beta);
  beta_periodic ~ normal(0,p_beta);
  // log-likelihood
  for(i in 1:I) 
    for(j in 1:J)
      target += poisson_lpmf(total_deaths[i,j] | exp_lin[i,j]);
}
generated quantities {
  real pred_lin[J];
  real exp_pred_lin[J];
  int pred_total_deaths[J];
  int excess_total_deaths[J];
  int yearly_excess_total_deaths;
  for(j in 1:J) {
    pred_lin[j] = alpha + 
      beta_year*(years[I]+1) + 
        beta_periodic[1]*sin(2*pi()*months[j]/12) + 
        beta_periodic[2]*sin(4*pi()*months[j]/12) + 
        beta_periodic[3]*cos(2*pi()*months[j]/12) + 
        beta_periodic[4]*cos(4*pi()*months[j]/12) + 
      log(predyear_total_population[j]);
    exp_pred_lin[j] = exp(pred_lin[j]);
    if(exp_pred_lin[j] < 2^28) // avoid overflow
      pred_total_deaths[j] = poisson_rng(exp_pred_lin[j]);
    else
      pred_total_deaths[j] = -1; 
    excess_total_deaths[j] = predyear_total_deaths[j] - pred_total_deaths[j];
    yearly_excess_total_deaths = sum(excess_total_deaths);
  }
}
