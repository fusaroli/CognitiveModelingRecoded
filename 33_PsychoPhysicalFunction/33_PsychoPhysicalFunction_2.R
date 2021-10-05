pacman::p_load(
  here,
  tidyverse,
  cmdstanr,
  posterior,
  brms
)

x <- as.matrix(read.table(here("33_PsychoPhysicalFunction", "data_x.txt"), sep="\t"))
n <- as.matrix(read.table(here("33_PsychoPhysicalFunction", "data_n.txt"), sep="\t"))
r <- as.matrix(read.table(here("33_PsychoPhysicalFunction", "data_r.txt"), sep="\t"))
rprop <- as.matrix(read.table(here("33_PsychoPhysicalFunction", "data_rprop.txt"), sep="\t"))

xmean <- c(318.888, 311.0417, 284.4444, 301.5909, 
           296.2000, 305.7692, 294.6429, 280.3571)
nstim <- c(27, 24, 27, 22, 25, 26, 28, 28)
nsubjs <- 8


df <- tibble(
  duration = as.vector(r), 
  trials = as.vector(n),
  judgement = as.vector(rprop),
  intensity = as.vector(x),
  ID = rep(1:8, 28))

for (id in unique(df$ID)){
  df$intensity[df$ID==id] = df$intensity[df$ID==id]-mean(df$intensity[df$ID==id])
}

x[is.na(x)] = -99  # transforming because Stan won't accept NAs 
n[is.na(n)] = -99  # transforming because Stan won't accept NAs 
r[is.na(r)] = -99  # transforming because Stan won't accept NAs 



# to be passed on to Stan
data <- list(x=x, xmean=xmean, n=n, r=r, nsubjs=nsubjs, nstim=nstim) 
data_file <- tempfile(fileext = ".json")
write_stan_json(data, data_file)





file <- file.path(here("33_PsychoPhysicalFunction", "33_PsychoPhysicalFunction_2.stan"))
mod <- cmdstan_model(file, cpp_options = list(stan_threads = TRUE), pedantic = TRUE)

samples <- mod$sample(
  data = data_file,
  seed = 123,
  chains = 2,
  parallel_chains = 2,
  threads_per_chain = 2,
  iter_warmup = 2000,
  iter_sampling = 2000,
  init=0.5,
  refresh = 500,
  max_treedepth = 20,
  adapt_delta = 0.99,
)

draws_df <- as_draws_df(samples$draws()) 

ggplot(draws_df)+
  geom_density(aes(mualphaprior), fill="red", alpha=0.3) +
  geom_density(aes(`mualpha`), fill="blue", alpha=0.3) +
  theme_classic()

ggplot(draws_df)+
  geom_density(aes(`mubetaprior`), fill="red", alpha=0.3) +
  geom_density(aes(`mubeta`), fill="blue", alpha=0.3) +
  theme_classic()

ggplot(draws_df)+
  geom_density(aes(sigmaalphaprior), fill="red", alpha=0.3) +
  geom_density(aes(`sigmaalpha`), fill="blue", alpha=0.3) +
  theme_classic()

ggplot(draws_df)+
  geom_density(aes(`sigmabetaprior`), fill="red", alpha=0.3) +
  geom_density(aes(`sigmabeta`), fill="blue", alpha=0.3) +
  theme_classic()


ggplot(draws_df)+
  geom_point(aes(mualphaprior, sigmaalphaprior), color="red", alpha=0.05) +
  geom_point(aes(`mualpha`, `sigmaalpha`), color="green", alpha=0.1) +
  theme_classic()

ggplot(draws_df)+
  #geom_point(aes(mubetaprior, sigmabetaprior), color="red", alpha=0.05) +
  geom_point(aes(`mubeta`, `sigmabeta`), color="green", alpha=0.1) +
  theme_classic()

ggplot(draws_df)+
  #geom_point(aes(mubetaprior, sigmabetaprior), color="red", alpha=0.05) +
  geom_point(aes(`mubeta`, `mualpha`), color="green", alpha=0.1) +
  theme_classic()


# Extract diagnostics
samples$cmdstan_diagnose() # summary
diagnostics_df <- as_draws_df(samples$sampler_diagnostics())
print(diagnostics_df)

draws_df$divergence=diagnostics_df$divergent__
div <- draws_df[draws_df$divergence==1,]


ggplot(draws_df)+
  geom_point(aes(`mualpha`, `sigmaalpha`), alpha=0.1) +
  geom_point(data=div, aes(`mualpha`, `sigmaalpha`), color="red") +
  theme_classic()

ggplot(draws_df)+
  geom_point(aes(`mubeta`, `sigmabeta`), alpha=0.1) +
  geom_point(data=div, aes(`mubeta`, `sigmabeta`), color="red") +
  theme_classic()



ggplot(draws_df, aes(.iteration, mualpha, color=as.factor(.chain))) +
  geom_line() +
  theme_classic()
ggplot(draws_df, aes(.iteration, sigmaalpha, color=as.factor(.chain))) +
  geom_line() +
  theme_classic()
ggplot(draws_df, aes(.iteration, mubeta, color=as.factor(.chain))) +
  geom_line() +
  theme_classic()
ggplot(draws_df, aes(.iteration, sigmabeta, color=as.factor(.chain))) +
  geom_line() +
  theme_classic()

dd <-  draws_df %>% spread_draws(theta[ID,trial]) %>% group_by(trial,ID) %>% summarize(theta=mean(theta, na.rm=T))
df$theta <- dd$theta
## Plots
ggplot(df) +
  geom_point(aes(intensity, judgement)) +
  geom_line(aes(intensity, theta))+
  theme_classic() +
  facet_wrap(.~ID)

ggplot(df, aes(judgement, theta))+
  geom_point()


