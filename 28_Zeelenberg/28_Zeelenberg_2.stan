// Zeelenberg

// NEED TO DESCRIBE BETTER WHAT ALPHA VS PHI IS

data { 
  int<lower=0> ns;     // number of people
  int<lower=0> nb;     // number of trials w both priming
  int<lower=0> nn;     // number of trials w no priming
  int<lower=0> sb[ns]; // number of successes w both priming
  int<lower=0> sn[ns]; // number of successes w no priming
}

parameters {
  real<lower=0> mu;                    // pop-level average performance in no priming (for ind level phin)
  real<lower=0> muprior;                    // pop-level average performance in no priming (for ind level phin)
  real<lower=0> sigma;        // pop-level variance in performance in no priming (for ind level phin)
  real<lower=0> sigmaprior;        // pop-level variance in performance in no priming (for ind level phin)
  
  vector[ns] phin;                     // ind level performance rate (z-scores) in no priming
    
  real<lower=0> delta;                 // pop-level standardized difference
  real<lower=0> deltaprior;            // pop-level standardized difference (prior)
  real<lower=0> sigmadelta;        // pop-level variance in performance in no priming (for ind level phin)
  real<lower=0> sigmadeltaprior;        // pop-level variance in performance in no priming (for ind level phin)
  
  vector[ns] delta_ind;                    // ind level difference in performance rate (both minus no)

} 

transformed parameters {
  vector<lower=0,upper=1>[ns] thetab; // ind-level theta for both priming
  vector<lower=0,upper=1>[ns] thetan; // ind-level theta for no priming
  vector[ns] phib;                    // ind-level performance rate (z-scores) in both priming
  real CohenD; 
  real CohenDprior;                   // standardized effect size
  
  phib = phin + delta_ind;            // performance (z-scores) in both is performance in no priming + alpha
  
  // Probit transformation
  for (i in 1:ns) {
    thetab[i] = Phi(phib[i]); // from zscores to probability
    thetan[i] = Phi(phin[i]); // from zscores to probability
  }
  
  CohenD = delta / sigmadelta;
  CohenDprior = deltaprior / sigmadeltaprior;
  
}

model{
  // Priors
  mu ~ normal(0, 1);            // pop-level average performance in no priming 
  muprior ~ normal(0, 1);            // pop-level average performance in no priming 
  sigma ~ normal(0, 1);            // pop-level average performance in no priming 
  sigmaprior ~ normal(0, 1);            // pop-level average performance in no priming 
  // Priming Effect
  delta ~ normal(0, .2);         // mean standardized difference
  deltaprior ~ normal(0, .2);    // mean standardized difference prior
  sigmadelta ~ normal(0, .2);            // pop-level average performance in no priming 
  sigmadeltaprior ~ normal(0, .2);            // pop-level average performance in no priming 
  
  // Individual Parameters
  delta_ind ~ normal(delta, sigmadelta);
  phin ~ normal(mu, sigma); // each participant comes from the population distribution
  // Data
  sb ~ binomial(nb, thetab); // both priming model
  sn ~ binomial(nn, thetan); //  no priming model
}

