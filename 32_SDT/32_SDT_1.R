dataset <- 1

if (dataset == 1) {  # Demo
  k <- 3 # number of cases
  data <- matrix(c(70, 50, 30, 50,
                   7,  5,  3,  5, 
                   10,  0,  0, 10), nrow=k, ncol=4, byrow=TRUE)
}

if (dataset == 2) {  # Lehrner et al. (1995) data 
  k <- 3 # number of cases
  data <- matrix(c(148, 29, 32, 151,
                   150, 40, 30, 140,
                   150, 51, 40, 139), nrow=k, ncol=4, byrow=TRUE)
}

hits <- data[, 1]
falsealarms <- data[, 2]
missed <- data[, 3]
correctrejections <- data[, 4]
signal <- hits + missed
noise <- falsealarms + correctrejections

data <- list(hits = hits, falsealarms = falsealarms, signal=signal, noise = noise, k = k) # To be passed on to Stan

file <- file.path(here("32_SDT", "32_SDT_1.stan"))
mod <- cmdstan_model(file, cpp_options = list(stan_threads = TRUE), pedantic = TRUE)

samples <- mod$sample(
  data = data,
  seed = 123,
  chains = 2,
  parallel_chains = 2,
  threads_per_chain = 2,
  iter_warmup = 2000,
  iter_sampling = 2000,
  refresh = 500,
  max_treedepth = 20,
  adapt_delta = 0.99,
)

# Extract posterior samples and include sampling of the prior:
draws_df <- as_draws_df(samples$draws()) 

ggplot(draws_df)+
  geom_density(aes(criterionprior), fill="red", alpha=0.3) +
  geom_density(aes(`criterion[1]`), fill="blue", alpha=0.3) +
  geom_density(aes(`criterion[2]`), fill="blue", alpha=0.3) +
  geom_density(aes(`criterion[3]`), fill="blue", alpha=0.3) +
  theme_classic()

ggplot(draws_df)+
  geom_density(aes(`discriminabilityprior`), fill="red", alpha=0.3) +
  geom_density(aes(`discriminability[1]`), fill="blue", alpha=0.3) +
  geom_density(aes(`discriminability[2]`), fill="blue", alpha=0.3) +
  geom_density(aes(`discriminability[3]`), fill="blue", alpha=0.3) +
  theme_classic()

ggplot(draws_df)+
  geom_point(aes(criterionprior, discriminabilityprior), color="red", alpha=0.05) +
  geom_point(aes(`criterion[2]`, `discriminability[2]`), color="green", alpha=0.1) +
  geom_point(aes(`criterion[1]`, `discriminability[1]`), color="blue", alpha=0.1) +
  geom_point(aes(`criterion[3]`, `discriminability[3]`), color="black", alpha=0.1) +
  theme_classic()
