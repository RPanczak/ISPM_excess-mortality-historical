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
transformed data {
  int overflow_limit = 10000000;
}
parameters {
  real alpha;
  real beta_year;
  real beta_periodic[4]; 
  real<lower=0> phi_inv;
}
transformed parameters {
  real lin[I,J]; // linear predictor
  real<lower=0> exp_lin[I,J];
  real phi = 1./phi_inv;
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
  phi_inv ~ cauchy(0,5);
  // log-likelihood
  for(i in 1:I) 
    for(j in 1:J)
      target += neg_binomial_2_lpmf(total_deaths[i,j] | exp_lin[i,j], phi);
}
generated quantities {
  // linear predictors
  real pred_lin[J];
  real exp_pred_lin[J];
  
  // predictions
  int pred_total_deaths[J];
  int yearly_pred_total_deaths;
  int excess_total_deaths[J];
  int yearly_excess_total_deaths;
  real rel_excess_total_deaths[J];
  real yearly_rel_excess_total_deaths;
  
  // compute linear predictors
  for(j in 1:J) {
    pred_lin[j] = alpha + 
      beta_year*(years[I]+1) + 
        beta_periodic[1]*sin(2*pi()*months[j]/12) + 
        beta_periodic[2]*sin(4*pi()*months[j]/12) + 
        beta_periodic[3]*cos(2*pi()*months[j]/12) + 
        beta_periodic[4]*cos(4*pi()*months[j]/12) + 
      log(predyear_total_population[j]);
    exp_pred_lin[j] = exp(pred_lin[j]);
  }
  
  // compute predictions
  for(j in 1:J) {
    if(exp_pred_lin[j] < overflow_limit && phi > 1e-3) // avoid overflow
      pred_total_deaths[j] = neg_binomial_2_rng(exp_pred_lin[j], phi);
    else
      pred_total_deaths[j] = overflow_limit; 
    excess_total_deaths[j] = predyear_total_deaths[j] - pred_total_deaths[j];
    rel_excess_total_deaths[j] = (predyear_total_deaths[j] - pred_total_deaths[j]) / (0.0+pred_total_deaths[j]);
  }
  yearly_pred_total_deaths = sum(pred_total_deaths);
  yearly_excess_total_deaths = sum(excess_total_deaths);
  yearly_rel_excess_total_deaths = (sum(predyear_total_deaths) - sum(pred_total_deaths)) / (0.0+sum(pred_total_deaths));
}
