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
  real mualpha;                   // 
  real<lower=0> sigmaalpha;
  real mubeta;
  real<lower=0> sigmabeta;
  real mualphaprior;
  real<lower=0> sigmaalphaprior;
  real mubetaprior;
  real<lower=0> sigmabetaprior;
  
  real alpha[nsubjs];
  real beta[nsubjs];
  real alphaprior;
  real betaprior;
  
  
  
}

transformed parameters {
  matrix[nsubjs, 28] theta;
  
  // Retention Rate At Each Lag For Each Subject Decays Exponentially
  for (i in 1:nsubjs) {
    for (j in 1:nstim[i]) {
      theta[i,j] = inv_logit(alpha[i] + beta[i] * (x[i,j] - xmean[i]));
    }
  }

}

// The model to be estimated. 
model {
  mualpha ~ normal(0,1);           // population level mean intercept (in log odds)
  mualphaprior ~ normal(0,1);
  sigmaalpha ~ normal(0,1);        // population level mean variance of the intercept (in log odds)
  sigmaalphaprior ~ normal(0,1);
  
  mubeta ~ normal(0,.1);           // population level mean slope (in log odds)
  mubetaprior ~ normal(0,.1);
  sigmabeta ~ normal(0,.1);        // population level mean variance of the slope (in log odds)
  sigmabetaprior ~ normal(0,.1);
  
  alpha ~ normal(mualpha,sigmaalpha); // Individual level mean estimate for intercept (in log odds)
  alphaprior ~ normal(mualphaprior,sigmaalphaprior);
  
  beta ~ normal(mubeta,sigmabeta); // Individual level mean estimate for slope (in log odds)
  betaprior ~ normal(mubetaprior,sigmabetaprior);
  
  for (i in 1:nsubjs){
    for (j in 1:nstim[i]){
      r[i,j] ~ binomial(n[i,j], theta[i,j]);
    }
  }
  
}

generated quantities{
  
}

