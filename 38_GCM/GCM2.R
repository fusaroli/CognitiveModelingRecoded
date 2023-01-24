### GCM 2

load("GCM/KruschkeData.Rdata")  # Load Kruschke's data

data <- list(y = y, 
             nstim = nstim, 
             nsubj = nsubj, 
             n = n,
             a = a, 
             d1 = d1, 
             d2 = d2) # To be passed on to Stan


## Specify where the model is
file <- file.path(here("GCM2.stan"))
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

#### Figure 17.5 ####
# windows(12, 7)
# layout(matrix(1:40, 5, 8, byrow=TRUE))
# par(mar=c(1, 1, 2, 1) + .1, oma=c(3, 3, 0, 0), mgp=c(3, .3, 0))
# 
# for (i in 1:40) {
#   plot(y[, i], type="l", ylim=c(0, 8), xaxt="n", yaxt="n", main=i, ylab="",
#        xlab="")
#   if (i == 33) {
#     axis(side=1, at=1:8, tick=FALSE, line=0)
#     axis(side=2, at=c(0, 8), labels=c("B", "A"), tick=FALSE)
#   }
# }
# mtext("Stimulus", side=1, line=.8, at=.035, adj=0, cex=1, outer=TRUE)  
# mtext("Category", side=2, line=.8, at=.035, adj=0, cex=1, outer=TRUE)  
# 
# #### Figure 17.7 ####
# c <- extract(samples)$c
# w <- extract(samples)$w
# 
# cMean <- apply(c, 2, mean)
# wMean <- apply(w, 2, mean)
# keep=sample(1:length(c[, 1]), size=20)
# 
# par(cex.lab=1.2)
# plot("", xlim=c(0, 4), ylim=c(0,1), xlab="Generalization", xaxs="i", yaxs="i",
#      ylab="Attention Weight")
# 
# for (i in 1:nsubj) {
#   for (j in 1:length(keep)) {
#     segments(cMean[i], wMean[i], c[keep[j], i], w[keep[j], i], col="gray")
#   }
# }
# points(cMean, wMean, pch=16)
# 
# for (i in c(3, 31, 33))
#   text(cMean[i], wMean[i], pos=4, labels = i, cex=1.3)

