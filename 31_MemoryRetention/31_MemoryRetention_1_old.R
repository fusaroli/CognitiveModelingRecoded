pacman::p_load(
  tidyverse,
  here,
  cmdstanr,
  posterior,
  brms,
  bayesplot
)

t     <- c(1, 2, 4, 7, 12, 21, 35, 59, 99, 200)
nt    <- length(t)
slist <- 1:4
ns    <- length(slist)

k1 <- as.matrix(c(18, 18, 16, 13, 9, 6, 4, 4, 4, NA,
               17, 13,  9,  6, 4, 4, 4, 4, 4, NA,
               14, 10,  6,  4, 4, 4, 4, 4, 4, NA,
               NA, NA, NA, NA,NA,NA,NA,NA,NA, NA), nrow=ns, ncol=nt, byrow=T)

k <- k1[1:(ns - 1), 1:(nt - 1)]   # Excluding NAs (for Stan solution)

n <- 18

data <- list(k=as.matrix(k), n=n, t=t, ns=ns, nt=nt) # To be passed on to Stan

myinits <- list(
  list(alpha=.5, beta=.1))


stan_file <- write_stan_file("
// Retention With No Individual Differences

data { 
  int ns;
  int nt;
  int k[ns - 1,nt - 1];
  int t[nt];
  int n;
}

parameters {
  real<lower=0,upper=1> alpha;
  real<lower=0,upper=1> beta;
} 

transformed parameters {
  matrix<lower=0,upper=1>[ns,nt] theta;
  
  // Retention Rate At Each Lag For Each Subject Decays Exponentially
  for (i in 1:ns) {
    for (j in 1:nt) {
      theta[i,j] = fmin(1, exp(-alpha * t[j]) + beta);
    }
  }
}

model {
  // Priors
  alpha ~ beta(1, 1);  // can be removed
  beta ~ beta(1, 1);  // can be removed
  
  // Observed Data
  for (i in 1:(ns - 1))
    for (j in 1:(nt - 1))
      k[i,j] ~ binomial(n, theta[i,j]);
}

generated quantities {
  int<lower=0,upper=n> predk[ns,nt];
  
  // Predicted Data
  for (i in 1:ns)
    for (j in 1:nt)
      predk[i,j] = binomial_rng(n, theta[i,j]);
}")

mod <- cmdstan_model(stan_file, cpp_options = list(stan_threads = TRUE), pedantic = TRUE)

samples <- mod$sample(
  data = data,
  seed = 123,
  chains = 2,
  parallel_chains = 2,
  threads_per_chain = 2,
  iter_warmup = 2000,
  iter_sampling = 2000,
  refresh = 500,
  #init = myinits,
  max_treedepth = 20,
  adapt_delta = 0.99,
)
