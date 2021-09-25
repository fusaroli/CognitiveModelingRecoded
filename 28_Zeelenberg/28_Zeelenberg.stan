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
  real<lower=0> sigmaalpha;   // pop-level variance for difference in performance btw conditions
  real<lower=0> sigmaalphaprior;   // pop-level variance for difference in performance btw conditions
  
  vector[ns] alpha;                    // ind level difference in performance rate (both minus no)

} 

transformed parameters {

  real<lower=0> mualpha;              // pop level non standardized mean difference, derived from alpha
  real<lower=0> mualphaprior;              // pop level non standardized mean difference, derived from alpha
  vector<lower=0,upper=1>[ns] thetab; // ind-level theta for both priming
  vector<lower=0,upper=1>[ns] thetan; // ind-level theta for no priming
  vector[ns] phib;                    // ind-level performance rate (z-scores) in both priming
  
  mualphaprior = deltaprior * sigmaalphaprior;       // transform standardized difference in non standardized difference
  mualpha = delta * sigmaalpha;       // transform standardized difference in non standardized difference
  phib = phin + alpha;                // performance (z-scores) in both is performance in no priming + alpha
  
  // Probit transformation
  for (i in 1:ns) {
    thetab[i] = Phi(phib[i]); // from zscores to probability
    thetan[i] = Phi(phin[i]); // from zscores to probability
  }
}

model{
  // Priors
  mu ~ normal(0, 1);            // pop-level average performance in no priming 
  muprior ~ normal(0, 1);            // pop-level average performance in no priming 
  sigma ~ normal(0,1);
  sigmaprior ~ normal(0,1);
  // Priming Effect
  delta ~ normal(0, 1);         // mean standardized difference
  deltaprior ~ normal(0, 1);    // mean standardized difference prior
  sigmaalpha ~ normal(0,1);
  sigmaalphaprior ~ normal(0,1);
  
  // Individual Parameters
  alpha ~ normal(mualpha, sigmaalpha);
  phin ~ normal(mu, sigma); // each participant comes from the population distribution
  // Data
  sb ~ binomial(nb, thetab); // both priming model
  sn ~ binomial(nn, thetan); //  no priming model
}

