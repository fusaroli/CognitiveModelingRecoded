pacman::p_load(
  cmdstanr,
  posterior,
  here,
  job,
  brms
)


c <- c(rnorm(200, 30, 5), rnorm(200, 50, 5))

n <- length(c)
t <- 1:n

data <- list(c=c, n=n, t=t) # to be passed on to Stan

myinits <- list(
  list(mu=c(40, 40), sigma = 5, tau = n / 2))

stan_file <- write_stan_file("
data { 
  int n;
  vector[n] t;
  vector[n] c;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  vector[2] mu;
  real<lower=0> sigma;
  real<lower=0,upper=n> tau;
  vector[2] muprior;
  real<lower=0> sigmaprior;
} 

model { 
  // Group Means
  mu ~ normal(40, 10);
  muprior ~ normal(40, 10);
  // Standard deviation
  sigma ~ normal(0, 10);
  sigmaprior ~ normal(0, 10);
  tau ~ uniform(0, 400);
    
  // Which Side is Time of Change Point?
  // Data Come From A Gaussian
  for (i in 1:n) {
    if ((t[i] - tau) < 0.0)
      c[i] ~ normal(mu[1], sigma);
    else 
      c[i] ~ normal(mu[2], sigma);
  }
}
")

mod <- cmdstan_model(stan_file, cpp_options = list(stan_threads = TRUE), pedantic = TRUE)


job::job({
  samples <- mod$sample(
    data = data,
    seed = 123,
    chains = 1,
    parallel_chains = 1,
    threads_per_chain = 1,
    iter_warmup = 300,
    iter_sampling = 300,
    refresh = 10,
    init = myinits,
    max_treedepth = 20,
    adapt_delta = 0.8,
  )
})

df <- tibble(c=c, t=t)

bform <- bf(
  c ~ Intercept1 * step(change - t) +  # Section 1
    Intercept2 * step(t - change),  # Section 2,
  Intercept1 + Intercept2 ~ 1,  # Fixed intercept and slopes
  change ~ 1,  # Per-person changepoints around pop mean
  nl = TRUE
)

# Priors
bprior <- prior(normal(40, 10), class = "b", coef="Intercept", nlpar = "Intercept1") +
  prior(normal(40, 10), class = "b", coef="Intercept", nlpar = "Intercept2") +
  prior(normal(0, 10), class="sigma") +
  prior(uniform(0, 1178), class = "b", coef="Intercept", nlpar = "change") # Within observed range



# Fit it!
job::job({
m <- brm(
  bform,
  df,
  family = gaussian,
  prior = bprior,
  sample_prior = T,
  seed = 123,
  chains = 1,
  cores = 1,
  iter = 500,
  refresh = 10,
  backend = "cmdstanr",
  threads = threading(2),
  control = list(
    max_treedepth = 20,
    adapt_delta = 0.8)
)})

stancode(m)
