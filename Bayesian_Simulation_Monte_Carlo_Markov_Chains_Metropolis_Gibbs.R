# Markov Chain simulation

library("markovchain")
library(diagram)
library(expm)
library(pracma)
library(MASS)
library(ks)


# simulate discrete Markov chains according to transition matrix P
run.mc.sim <- function( P, num.iters = 50 ) {
  
  # number of possible states
  num.states <- nrow(P)
  
  # stores the states X_t through time
  states     <- numeric(num.iters)
  
  # initialize variable for first state 
  states[1]    <- 1
  
  for(t in 2:num.iters) {
    
    # probability vector to simulate next state X_{t+1}
    p  <- P[states[t-1], ]
    
    ## draw from multinomial and determine state
    states[t] <-  which(rmultinom(1, 1, p) == 1)
  }
  return(states)
}
# found manually
# initialise transition matrix
P <- t(matrix(c(0.16, 0.14, 0.17, 0.16, 0.19, 0.18, 
                0.16, 0.16, 0.17, 0.17, 0.18, 0.16,
                0.17, 0.17, 0.17, 0.17, 0.16, 0.17,
                0.17, 0.16, 0.16, 0.18, 0.17, 0.16,
                0.17, 0.16, 0.17, 0.16, 0.16, 0.18,
                0.17, 0.16, 0.17, 0.18, 0.17, 0.16), nrow=6, ncol=6))
P

num.chains     <- 50
num.iterations <- 500

# each column stores the sequence of states for a single chains
chain.states  <- matrix(NA, ncol=num.chains, nrow=num.iterations)

# simulate chains
for(c in seq_len(num.chains)){
  chain.states[,c] <- run.mc.sim(P)
}
matplot(chain.states, type='l', lty=1, col=1:5, ylim=c(0,7), ylab='state', xlab='time')
abline(h=1, lty=3)
abline(h=3, lty=3)

# create the DTMC
tmA <- matrix(c(0.36, 0.34, 0.30, 0.32, 0.32, 0.36, 0.36, 0.33, 0.31),nrow = 3, byrow = TRUE)
tmA
dtmcA <- new("markovchain",transitionMatrix=tmA, states=c("2000", "2005", "2010"), name="MarkovChain A") 
dtmcA

# The numbers in this transition matrix were found manually
#  The transition matrix  (by rows)  is defined as follows: 
#      2000 2005 2010
# 2000 0.36 0.34 0.30
# 2005 0.32 0.32 0.36
# 2010 0.36 0.33 0.31

plot(tmA)

stateNames <- c("2000", "2005", "2010")
row.names(tmA) <- stateNames; colnames(tmA) <- stateNames
plotmat(tmA,pos = c(1,2), 
        lwd = 1, box.lwd = 2, 
        cex.txt = 0.8, 
        box.size = 0.1, 
        box.type = "circle", 
        box.prop = 0.5,
        box.col = "light blue",
        arr.length=.1,
        arr.width=.1,
        self.cex = .6,
        self.shifty = -.01,
        self.shiftx = .14,
        main = "Markov Chain")
# It is possible to simulate states distribution after n-steps
initialState<-c(0,1,0)
steps<-2
finalState<-initialState*dtmcA^steps #using power operator
finalState
#       2000 2005   2010
#[1,] 0.3472 0.33 0.3228
# As well as steady states distribution
steadyStates(dtmcA)
#           2000      2005      2010
# [1,] 0.3467933 0.3301663 0.3230404

# 10 years prediction
#       2020      2025      2030
# 2020 0.3467933 0.3301663 0.3230404
# 2025 0.3467933 0.3301663 0.3230404
# 2030 0.3467933 0.3301663 0.3230404


stateNames <- c("2020", "2025", "2030")
Oz10 <- matrix(c(0.34, 0.32, 0.32, 0.35, 0.33, 0.32, 0.35, 0.33, 0.32),
             nrow=3, byrow=TRUE)
row.names(Oz10) <- stateNames; colnames(Oz10) <- stateNames
Oz10

plotmat(Oz,pos = c(1,2), 
        lwd = 1, box.lwd = 2, 
        cex.txt = 0.8, 
        box.size = 0.1, 
        box.type = "circle", 
        box.prop = 0.5,
        box.col = "light blue",
        arr.length=.1,
        arr.width=.1,
        self.cex = .4,
        self.shifty = -.01,
        self.shiftx = .13,
        main = "Prediction Matrix")

x1 <- matrix(c(0,0,1),nrow=1, byrow=TRUE)
# predict following year
x1 %*% Oz
#      2000 2005 2010
# 2006
#[1,] 0.32 0.32 0.36
#2011
#[1,] 0.36 0.33 0.31

# predicting 10 years from 2010 ie 2020
Oz10 <- Oz %^% 10
Oz10
Oz10=round(Oz10,3)

# predictions found by Bayesian model
#       2020      2025      2030
# 2020 0.3467933 0.3301663 0.3230404
# 2025 0.3467933 0.3301663 0.3230404
# 2030 0.3467933 0.3301663 0.3230404

plotmat(Oz10,pos = c(1,2), 
        lwd = 1, box.lwd = 2, 
        cex.txt = 0.8, 
        box.size = 0.12, 
        box.type = "circle", 
        box.prop = 0.7,
        box.col = "light blue",
        arr.length=.4,
        arr.width=.2,
        self.cex = .6,
        self.shifty = -.01,
        self.shiftx = .17,
        main = "Markov Chain Transition Matrix")
-----------------------------------------------------------------------------------------------
  
  #METROPOLIS WITHIN GIBBS

-----------------------------------------------------------------------------------------------
  n <- 200
m <- rep(20, n)
x <- rnorm(n, 0, 1)
X <- model.matrix(~ x)
Beta.true <- c(-1, 1)
mean.true <- plogis(X %*% Beta.true)
kappa.true <- 1
Pi.true <- c(1,3)/4
d <- ncol(X)
J <- length(Pi.true)
y <- r.mixlink.binom(n, mean.true, Pi.true, kappa.true, m)

# ----- Run Metropolis-within-Gibbs sampler -----
hyper <- list(VBeta = diag(1000, d), alpha.Pi = rep(1, J),
              kappa.a = 1, kappa.b = 1/2)
gibbs.out <- gibbs.mixlink(y, X, R = 10, burn = 5, thin = 1,
                           invlink = plogis, report.period = 100, Pi.init = c(1,9)/10,
                           proposal.VBeta = solve(t(X) %*% X), proposal.VPi = diag(0.25^2, J-1),
                           proposal.Vkappa = 0.5^2, proposal.Vpsi = diag(0.5^2, J),
                           hyper = hyper, family = "binomial", trials = m)

print(gibbs.out)
DIC(gibbs.out)

# Metropolis-in-Gibbs samples can get 'stuck' in certain regions of the support, 
# especially when there are multiple modes and/or significant correlation among the random variables. 
# This is not as much a problem for Metropolis sampling.
# Metropolis sampling can produce fewer unique samples due to the poor approximation 
# of the proposal density to the target density. 
# This occurs more often for high-dimensional target densities.

# Metropolis and Metropolis-in-Gibbs sampling algorithms, 
# which are useful for sampling probability densities for which the normalizing constant is difficult 
# to calculate, are irregular, or have high dimension (Metropolis-in-Gibbs).
