pacman::p_load(
  tidyverse,
  here,
  cmdstanr,
  posterior,
  tidybayes,
  brms,
  bayesplot,
  patchwork
)

t     <- c(1, 2, 4, 7, 12, 21, 35, 59, 99, 200)
nt    <- length(t)
slist <- 1:4
ns    <- length(slist)

k1 <- matrix(c(18, 18, 16, 13, 9, 6, 4, 4, 4, NA,
               17, 13,  9,  6, 4, 4, 4, 4, 4, NA,
               14, 10,  6,  4, 4, 4, 4, 4, 4, NA,
               NA, NA, NA, NA,NA,NA,NA,NA,NA, NA), nrow=ns, ncol=nt, byrow=T)

k <- k1[1:(ns - 1), 1:(nt - 1)]   # Excluding NAs (for Stan solution)

n <- 18

data <- list(k=k, n=n, t=t, ns=ns, nt=nt) # To be passed on to Stan

df <- tibble(
  successes = as.vector(k1),
  trials = 18,
  timeLag = rep(1:10, each=4),
  ID = rep(1:4, 10))

data_file <- tempfile(fileext = ".json")
write_stan_json(data, data_file)

file <- file.path(here("31_MemoryRetention", "31_MemoryRetention_1.stan"))
mod <- cmdstan_model(file, cpp_options = list(stan_threads = TRUE), pedantic = TRUE)

samples <- mod$sample(
  data = data_file,
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
  geom_density(aes(alphaprior), fill="red", alpha=0.3) +
  geom_density(aes(alpha), fill="blue", alpha=0.3) +
  theme_classic()

ggplot(draws_df)+
  geom_density(aes(betaprior), fill="red", alpha=0.3) +
  geom_density(aes(beta), fill="blue", alpha=0.3) +
  theme_classic()

ggplot(draws_df)+
  geom_point(aes(alphaprior, betaprior), color="red", alpha=0.3) +
  geom_point(aes(alpha, beta), color="blue", alpha=0.3) +
  theme_classic()

## 

p <- ggplot(draws_df, aes(alpha, beta)) +
  geom_point(alpha=0.1) +
  xlab("alpha") +
  ylab("beta") +
  geom_rug(size=0.1) +
  theme_classic()

p1 <- ggExtra::ggMarginal(p, type="histogram", fill = "lightblue")

# Predictive checks as in the book (STILL TO FIX)

# extract thetaprior

prior_df <- draws_df %>% 
  spread_draws(priorpredk[timeLag]) %>% 
  group_by(timeLag, priorpredk) %>%
  summarize(prop=n()/4000)

priorPredCheck <- ggplot(prior_df,aes(timeLag,priorpredk,size=prop)) +
  geom_point(aes(timeLag,priorpredk,size=prop),shape=0) + guides(size="none") +
  ylab("Success Rate") +
  xlab("Time Lag") +
  scale_x_continuous(minor_breaks=NULL,breaks=seq(0,10,1)) +
  scale_y_continuous(minor_breaks=NULL,breaks=seq(0,18,1)) +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  theme_classic()

post_df <- draws_df %>% 
  spread_draws(postpredk[ID, timeLag]) %>% 
  group_by(ID, timeLag, postpredk) %>%
  summarize(prop=n()/4000)

postPredCheck <-  ggplot(post_df, aes(timeLag, postpredk)) +
  geom_point(shape=0, size=7*post_df$prop) + guides(size="none") +
  ylab("Success Rate") +
  xlab("Time Lag") +
  scale_x_continuous(minor_breaks=NULL,breaks=seq(0,10,1)) +
  scale_y_continuous(minor_breaks=NULL,breaks=seq(0,18,1)) +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  theme_classic() +
  facet_wrap(.~ ID) +
  geom_point(data=df,aes(timeLag, successes), size=3, alpha=0.5) +
  geom_line(data=df,aes(timeLag, successes))
  

priorPredCheck / postPredCheck