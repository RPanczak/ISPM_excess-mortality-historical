data {
  // past years
  int I; // number of years (generally 5 except if some year is ignored)
  int J; // number of months (12)
  int K; // number of age groups (10)
  vector[I] years;
  vector[J] months;
  int total_deaths[I,J];
  int total_population[I,J];
  int grouped_deaths[I,K];
  int grouped_population[I,K];
  // prediction year
  int predyear_total_deaths[J];
  int predyear_total_population[J];
  int predyear_grouped_deaths[K];
  int predyear_grouped_population[K];
  // priors
  real p_alpha;
  real p_beta;
}
transformed data {
  int overflow_limit = 10000000;
}
parameters {
  real alpha[K];
  real beta_year;
  real beta_periodic[4]; 
  real<lower=0> phi_inv;
}
transformed parameters {
  real lin[I,J,K]; // linear predictor
  real<lower=0> exp_lin[I,J,K];
  real<lower=0> exp_lin_total_deaths[I,J];
  real<lower=0> exp_lin_grouped_deaths[I,K];
  simplex[K] prop_lin_grouped_deaths[I];
  real phi = 1./phi_inv;
  
  // for every stratum
  for(i in 1:I) 
    for(j in 1:J)
      for(k in 1:K) 
          lin[i,j,k] = alpha[k] + 
            beta_year*years[i] + 
            beta_periodic[1]*sin(2*pi()*months[j]/12) + 
            beta_periodic[2]*sin(4*pi()*months[j]/12) + 
            beta_periodic[3]*cos(2*pi()*months[j]/12) + 
            beta_periodic[4]*cos(4*pi()*months[j]/12) + 
            log(grouped_population[i,k]);
  exp_lin = exp(lin);
  
  // sum by month/year
  for(i in 1:I) 
    for(j in 1:J)
      exp_lin_total_deaths[i,j] = sum(to_array_1d(exp_lin[i,j,]));
  
  // sum by age cat/year
  for(i in 1:I) 
    for(k in 1:K) 
      exp_lin_grouped_deaths[i,k] = sum(to_array_1d(exp_lin[i,,k]));
        
  // compute proportions by year
  for(i in 1:I) 
    prop_lin_grouped_deaths[i] = ( to_vector(to_array_1d(exp_lin_grouped_deaths[i,])) ) ./ sum( to_vector(to_array_1d(exp_lin_grouped_deaths[i,])) );
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
      target += neg_binomial_2_lpmf(total_deaths[i,j] | exp_lin_total_deaths[i,j], phi);
  
  for(i in 1:I) 
    target += multinomial_lpmf(to_array_1d(grouped_deaths[i,]) | prop_lin_grouped_deaths[i]);
}
generated quantities {
  // linear predictors
  real pred_lin[J,K];
  real<lower=0> exp_pred_lin[J,K];
  real<lower=0> exp_pred_lin_total_deaths[J];
  real<lower=0> exp_pred_lin_grouped_deaths[K];
  simplex[K] prop_pred_lin_grouped_deaths;
  
  // predictions from month
  int pred_total_deaths[J];
  int yearly_pred_total_deaths;
  int excess_total_deaths[J];
  int yearly_excess_total_deaths;
  real rel_excess_total_deaths[J];
  real yearly_rel_excess_total_deaths;
  
  // predictions from age groups
  int pred_grouped_deaths[K];
  int yearly_pred_grouped_deaths;
  int excess_grouped_deaths[K];
  real excess_grouped_lifelost70[K];
  int yearly_excess_grouped_deaths;
  real yearly_excess_grouped_lifelost70;
  real rel_excess_grouped_deaths[K];
  real yearly_rel_excess_grouped_deaths;

  // compute linear predictors
  for(j in 1:J) 
    for(k in 1:K) 
      pred_lin[j,k] = alpha[k] + 
        beta_year*(years[I]+1) + 
        beta_periodic[1]*sin(2*pi()*months[j]/12) + 
        beta_periodic[2]*sin(4*pi()*months[j]/12) + 
        beta_periodic[3]*cos(2*pi()*months[j]/12) + 
        beta_periodic[4]*cos(4*pi()*months[j]/12) + 
        log(predyear_grouped_population[k]);
  exp_pred_lin = exp(pred_lin);
  for(j in 1:J)
    exp_pred_lin_total_deaths[j] = sum(to_array_1d(exp_pred_lin[j,]));
  for(k in 1:K) 
    exp_pred_lin_grouped_deaths[k] = sum(to_array_1d(exp_pred_lin[,k]));
  prop_pred_lin_grouped_deaths = ( to_vector(to_array_1d(exp_pred_lin_grouped_deaths)) ) ./ sum( to_vector(to_array_1d(exp_pred_lin_grouped_deaths)) );
  
  // compute predictions from month
  for(j in 1:J) {
    if(exp_pred_lin_total_deaths[j] < overflow_limit && phi > 1e-3) // avoid overflow
      pred_total_deaths[j] = neg_binomial_2_rng(exp_pred_lin_total_deaths[j], phi);
    else 
      pred_total_deaths[j] = overflow_limit;
    excess_total_deaths[j] = predyear_total_deaths[j] - pred_total_deaths[j];
    rel_excess_total_deaths[j] = (predyear_total_deaths[j] - pred_total_deaths[j]) / (0.0+pred_total_deaths[j]);
  }
  yearly_pred_total_deaths = sum(pred_total_deaths);
  yearly_excess_total_deaths = sum(excess_total_deaths);
  yearly_rel_excess_total_deaths = (sum(predyear_total_deaths) - sum(pred_total_deaths)) /  (0.0+sum(pred_total_deaths));
  
  // compute predictions from age group
  pred_grouped_deaths = multinomial_rng(prop_pred_lin_grouped_deaths, sum(pred_total_deaths));
  for(k in 1:K) {
    excess_grouped_deaths[k] = predyear_grouped_deaths[k] - pred_grouped_deaths[k];
    excess_grouped_lifelost70[k] = excess_grouped_deaths[k] * (k<8 ? (70.0-(k*10.0)+5.0) : 0.0) ;
    rel_excess_grouped_deaths[k] = (predyear_grouped_deaths[k] - pred_grouped_deaths[k]) / (0.0+pred_grouped_deaths[k]);
  }
  yearly_pred_grouped_deaths = sum(pred_grouped_deaths);
  yearly_excess_grouped_deaths = sum(excess_grouped_deaths);
  yearly_excess_grouped_lifelost70 = sum(excess_grouped_lifelost70);
  yearly_rel_excess_grouped_deaths = (sum(predyear_grouped_deaths) - sum(pred_grouped_deaths)) / (0.0+sum(pred_grouped_deaths));
}
