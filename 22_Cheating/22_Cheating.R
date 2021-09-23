pacman::p_load(
  cmdstanr,
  posterior,
  here,
  job,
  brms,
  tidyverse,
  ggbeeswarm
)


cheat.dat  <- read.table(here("22_Cheating", "cheat.csv"), header=F, sep=",")
cheatt.dat <- read.table(here("22_Cheating", "cheatt.csv"), header=F, sep="")
truth <- cheatt.dat$V1  # truth = 1 if cheater
k <- apply(cheat.dat, 1, sum)  # total correct per participant
p <- length(k)  # number of people
n <- 40         # total trials

data <- list(p=p, k=k, n=n, truth=truth) # To be passed on to Stan

file <- file.path(here("22_Cheating", "22_Cheating.stan"))
mod <- cmdstan_model(file, cpp_options = list(stan_threads = TRUE), pedantic = TRUE)

## N.B. divergences relate to the priors
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

samples

samples$summary()  # summary, same as print(samples)
## BLABLA DIAGNOSTICS

## PLOTS TO be CONVERTED
# plot 6.9
#make the two panel plot:
windows(width=8,height=6) #this command works only under Windows!
layout(matrix(c(1,2),2,1))
layout.show(2)
par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5,
    font.lab = 2, cex.axis = 1.3, bty = "n", las=1)
bins <- c(-1:n)+.5
bonafide <- hist(k[truth==0], breaks=bins, plot=F)$counts
cheat    <- hist(k[truth==1], breaks=bins, plot=F)$counts

counts <- rbind(bonafide, cheat)
barplot(counts, main=" ", xlab=" ", col=c("grey","white"),
        legend.text = c("Bona Fide","Cheater"), args.legend = list(x="topleft"),
        beside=TRUE, axes=F)
# bottom panel:
par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5,
    font.lab = 2, cex.axis = 1.3, bty = "n", las=1)
pc.line <- array()
for (i in 1:41) {
  pc.line[i] <- mean((k>=(i-1))==truth)
}

dev.new() # so the plot below does not overwrite the plot above

plot(c(0:40), pc.line, type="l", lwd=2, xlim=c(0,40), ylim=c(0.4,1), 
     xlab="Number of Items Recalled Correctly", 
     ylab=" ", axes=F)
axis(1, at=c(0,seq(from=5,by=5,to=40)))
axis(2, at=c(.5,.75,1))
par(las=0)
mtext("Prop. Correct",side=2, line=2.5,cex=1.5)
# Now add the distribution:
pc.dens <- density(pc)
polygon(c(0,pc.dens$y,0,0), c(pc.dens$x[1]-.01,pc.dens$x,pc.dens$x[1]+.01,
                              pc.dens$x[1]-.01), col="green")

# plot 6.10
windows()
par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5,
    font.lab = 2, cex.axis = 1.3, bty = "n", las=1)
plot(k,summary(samples)$summary[paste("z[", 1:118, "]", sep=""), 1], ylim=c(0,1),
     xlim=c(0,n), lwd=2, pch=4, xlab= "Number of Items Recalled Correctly", 
     ylab="Cheater Classification") 
# in the code, z=0 is bonafide and z=1 is cheating
# so z gives the prob of being assigned to cheating group

# BRMS
df <- tibble(
  truth = cheatt.dat$V1,  # truth = 1 if cheater
  k = apply(cheat.dat, 1, sum), # total correct per participant
  n = 40, # total trials
  ID = as.factor(seq(p)))
        

## Basic model

cheating_f <- bf(
  k | trials(n) ~ 1 + (1|p|ID),
  theta1 ~ 0 + ID
)

mix1 <- mixture(binomial, binomial, order="none")

get_prior(cheating_f, df, mix1)

priormix1 <- c(
  prior(normal(0,1.5), class= Intercept, dpar=mu1),
  prior(normal(0,1.5), class= Intercept, dpar=mu2),
  prior(normal(0,1), class= sd, dpar=mu1),
  prior(normal(0,1), class= sd, dpar=mu2),
  prior(normal(0,1.5), class= b, dpar=theta1)
)

m1 <- brm(
  cheating_f,
  df,
  mix1,
  prior=priormix1,
  sample_prior = T,
  chains = 2,
  cores = 2,
  iter = 4000,
  refresh = 200,
  backend = "cmdstanr",
  threads = threading(2),
  control = list(
    max_treedepth = 20,
    adapt_delta = 0.99)
)
m1