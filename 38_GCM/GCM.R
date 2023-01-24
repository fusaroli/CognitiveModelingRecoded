pacman::p_load(
  tidyverse,
  here,
  cmdstanr,
  posterior
)

load("GCM/KruschkeData.Rdata")  # Load Kruschke's data

x <- y
y <- apply(y, 1, sum)
t <- n * nsubj

data <- list(y = y, 
             nstim = nstim, 
             t = t, 
             a = a, 
             d1 = d1, 
             d2 = d2) # To be passed on to Stan

## Specify where the model is
file <- file.path(here("GCM.stan"))
mod <- cmdstan_model(file, cpp_options = list(stan_threads = TRUE))

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

samples$summary()  # summary, same as print(samples)

# Extract posterior samples 
draws_df <- as_draws_df(samples$draws()) 
ggplot(draws_df) +
  geom_density(aes(w_prior), fill = "red", alpha = 0.3) +
  geom_density(aes(`w`), fill = "blue", alpha = 0.3) +
  xlab("Weight") +
  ylab("Posterior Density") +
  theme_classic()

## MISSING FULL PRIOR AND POSTERIOR PREDICTIVE CHECKS AS WELL AS PRETTY PICTURE
#### Figure 17.3 ####
# plot(c, w, xlim=c(0, 5), ylim=c(0,1), xlab="Generalization", pch=4, cex=.4,
#      ylab="Attention Weight")
# 
# #### Figure 17.4 ####
# breaks <- seq(0, t, by=2)
# 
# windows(10, 5)
# par(mgp=c(2, 1, 0), mar=c(4, 4, 2, 2) + .1)
# plot(NA, xlim=c(0.5, 8.5), ylim=c(0, 320), xlab="Stimulus", yaxt="n", xaxt="n",
#      ylab="Category Decision")
# axis(side=1, 1:8)
# axis(side=2, c(0, t), c("B", "A"))
# 
# for (i in 1:nstim) {
#   counts=hist(predy[, i], plot=FALSE, breaks=breaks)$counts
#   breaks=hist(predy[, i], plot=FALSE, breaks=breaks)$breaks
#   
#   segments(i - counts * .003, breaks, i + counts * .003, breaks, col="gray",
#            lwd=3.5, lend=2)
# }
# apply(x * 40, 2, lines, lty=3, col="gray")
# lines(apply(x * 40, 1, mean), lwd=3)


