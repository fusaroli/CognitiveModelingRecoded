//
// This Stan model does something
//

// The input data is a vector 'y' of length 'N'.
data {
  
  int<lower = 1> nsubjs;         // n of participants (row)
  int<lower = 1> nstim[nsubjs];  // n of stimuli (columns excluding NAs/-99)
  int n[nsubjs,28];              // n of times the participant is exposed to the stimulus
  vector[nsubjs] xmean;          // mean intensity by subject
  int r[nsubjs,28];              // n of times the participant judges the stimulus as longer
  int x[nsubjs,28];              // stimulus intensity
  
}

// The parameters accepted by the model. 
parameters {
  real mualpha;                             // pop level intercept estimate
  real<lower=0> sigmaalpha;                 // pop level variance of intercept estimate
  real mubeta;                              // pop level slope estimate                   
  real<lower=0> sigmabeta;                  // pop level variance of slope estimate 
   
  real alpha[nsubjs];                       // individual level intercept estimate
  real beta[nsubjs];                        // individual level slope estimate
  
  real muphi;                               // pop level probability of contaminant (not paying attention)
  real<lower=0> sigmaphi;                   // pop level variance in probability of contaminant (not paying attention)
  
  vector[nsubjs] probitphi;                 // individual level probability of contaminant
  matrix<lower=0,upper=1>[nsubjs,28] pi;    //  success rate when contaminant process is on (not sure why it's by trial by participant)
  
}


transformed parameters {
  matrix[nsubjs, 28] theta;             // probability of success given intensity of the stimulus
  vector[2] lp_parts[nsubjs,28];        // probability of contaminant by participant by trial 
  vector<lower=0,upper=1>[nsubjs] phi;  // probability of contaminant by participant
  
  
  for (i in 1:nsubjs)
    phi[i] = Phi(probitphi[i]);         // 
    
  for (i in 1:nsubjs) {
    for (j in 1:nstim[i]) {
      theta[i,j] = inv_logit(alpha[i] + beta[i] * (x[i,j] - xmean[i]));  // model 

      lp_parts[i,j,1] = log1m(phi[i]) + binomial_lpmf(r[i,j] | n[i,j], theta[i,j]); // identifying 
      lp_parts[i,j,2] = log(phi[i]) + binomial_lpmf(r[i,j] | n[i,j], pi[i,j]);
    }
  }
}

// The model to be estimated. 
model {
  mualpha ~ normal(0,1);           // population level mean intercept (in log odds)
  sigmaalpha ~ normal(0,1);        // population level mean variance of the intercept (in log odds)
  
  mubeta ~ normal(0,.1);           // population level mean slope (in log odds)
  sigmabeta ~ normal(0,.1);        // population level mean variance of the slope (in log odds)
  
  alpha ~ normal(mualpha,sigmaalpha); // Individual level mean estimate for intercept (in log odds)
  
  beta ~ normal(mubeta,sigmabeta); // Individual level mean estimate for slope (in log odds)
  
  muphi ~ normal(0, 1); 
  sigmaphi ~ normal(0, 1); 
  
  probitphi ~ normal(muphi, sigmaphi);
  
  
  for (i in 1:nsubjs) 
    pi[i] ~ beta(1, 1);  // can be removed
  
  for (i in 1:nsubjs)
    for (j in 1:nstim[i])
      target +=log_sum_exp(lp_parts[i,j]);
  
}

generated quantities {
  
  real mualphaprior;
  real<lower=0> sigmaalphaprior;
  real mubetaprior;
  real<lower=0> sigmabetaprior;
  real alphaprior;
  real betaprior;
  real muphiprior;
  real<lower=0> sigmaphiprior;
  real probitphiprior;
  int<lower=0,upper=1> z[nsubjs,28];
  
  mualphaprior = normal_rng(0,1);
  sigmaalphaprior = normal_rng(0,1);
  mubetaprior = normal_rng(0,.1);
  sigmabetaprior = normal_rng(0,.1);
  alphaprior = normal_rng(mualphaprior,sigmaalphaprior);
  betaprior = normal_rng(mubetaprior,sigmabetaprior);
  muphiprior = normal_rng(0, 1); 
  sigmaphiprior = normal_rng(0, 1); 
  probitphiprior = normal_rng(muphiprior, sigmaphiprior);
  
  for (i in 1:nsubjs) {
    for (j in 1:nstim[i]) {  
      vector[2] prob;
      
      prob = softmax(lp_parts[i,j]);
      z[i,j] = bernoulli_rng(prob[2]);
    }
  }
}
