## GCM 3

load("GCM/KruschkeData.Rdata")  # Load Kruschke's data

y <- t(y)  # Transpose matrix (for simpler Stan implementation)        

# To be passed on to Stan
data <- list(y = y, 
             nstim = nstim, 
             nsubj = nsubj, 
             n = n, 
             a = a, 
             d1 = d1, 
             d2 = d2) 


## Specify where the model is
file <- file.path(here("GCM3.stan"))
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

samples$summary()

# Now the values for the monitored parameters are in the "samples" object, 
# ready for inspection.
# For better estimates increase number of iterations or run optimized  
# model in file GCM_3_optimized_Stan.R

draws_df <- as_draws_df(samples$draws()) 
z <- draws_df$z
predyg <- draws_df$predyg

############# Figure 17.9 ################
z1 <- c()
z2 <- c()
z3 <- c()
for (i in 1:40) {
  z1[i] <- sum(z[, i] == 1) / length(z[, 1])
  z2[i] <- sum(z[, i] == 2) / length(z[, 2])
  z3[i] <- sum(z[, i] == 3) / length(z[, 3])
}

ord1 <- order(z1, decreasing = TRUE)
ord2 <- order(z2, decreasing = TRUE)
ord3 <- order(z3, decreasing = TRUE)
ord1 <- ord1[z1[ord1] > .5]
ord2 <- ord2[z2[ord2] > .5]
ord3 <- ord3[z3[ord3] > .5]
ord <- c(ord3, ord2, ord1)

windows(7, 4)
plot(z1[ord], ylim = c(0, 1), type = "b", pch = 0, ylab = "Membership Probability",
     xlab = "Subject")
lines(z2[ord], type = "b", pch = 1)
lines(z3[ord], type = "b", pch = 2)
legend("center", c("Contaminant", "Attend Position", "Attend Height"),
       pch = c(2:0))

############# Figure 17.10 ################
textMain <- c("Attend Height", "Attend Position", "Contaminant")
textXlab <- c("", "", "Stimulus")
textYlab <- c("", "", "Category")
nsamples <- length(predyg[, 1, 1])
squaresize <- 8
whichGroup <- apply(cbind(z1, z2, z3), 1, which.max)
xaxes <- c("n", "n", "s")

windows(14,7)
layout(matrix(1:3, 1, 3))
par(cex.main = 1.5, mar = c(5, 5, 4, 1) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5,
    font.lab = 2, cex.axis = 1.3)

for (p in 3:1) {  # loop over the plots
  plot(0, type = "n", main = textMain[p], xlab = textXlab[p], ylab = textYlab[p], 
       xlim = c(1, 8), ylim = c(0, 8), xaxt = xaxes[p], yaxt = "n")
  if (p == 3)
    Axis(side = 2, at = c(0, 8), labels = c("B", "A"), las = 2, cex = 2)
  
  for (n in 1:40) {  # plotting observed data
    if (whichGroup[n] == p)
      lines(y[n, ], type = "l", col = "gray", lwd = 1.5)
  }
  
  # plotting mean of observed data
  tmpMean <- apply(y[whichGroup == p, ], 2, mean)
  lines(tmpMean, lwd = 4)
  
  for (i in 1:8) {  # plotting posterior predictive distributions
    for (j in 0:8) {
      tmp <- sum(predyg[, p, i] == j) / nsamples
      if (tmp > 0)
        points(i, j, pch = 0, cex = squaresize * sqrt(tmp), lwd = 1.2)  
    }
  }
}