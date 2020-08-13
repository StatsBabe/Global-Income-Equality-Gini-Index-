getwd()
setwd("C:/Users/Owner#/Documents/Diss")

#---------------
# LOAD PACKAGES
#---------------

library(blme) # Bayesian Linear Mixed Effects Models
library(MASS) # Support Functions

trust<- read.csv("Gini007.csv", stringsAsFactors = FALSE, header = TRUE, na.strings = c('NA',''))
head(trust)
str(trust)
# check for missing values (none)
sapply(trust, function(x) sum(is.na(x)))

# create a dataframe
trust.df<-as.data.frame(trust)
rm(trust)
# recognise the headers
attach(trust.df)

# check what the dataframe looks like
head(trust.df)
str(trust.df)
  
lm_fit<-lm(LE~Gini, data=trust.df)
lm_fit # re-run to check all in order
confint(lm_fit) # confidence intervals to check

#------------------------------------------

# CREATE BAYESIAN SIMPLE LINEAR REGRESSION

#------------------------------------------

QR<-lm_fit$qr
df.residual<-lm_fit$df.residual
R<-qr.R(QR) ## R component
coef<-lm_fit$coef
Vb<-chol2inv(R) ## variance(unscaled)
s2<-(t(lm_fit$residuals)%*%lm_fit$residuals)
s2<-s2[1,1]/df.residual

## function to compute the bayesian analog of the frequentist lm_fit
## using non-informative priors and Monte Carlo scheme
## based on N samples

bayesfit<-function(lm_fit,N){
  QR<-lm_fit$qr
  df.residual<-lm_fit$df.residual
  R<-qr.R(QR) ## R component
  coef<-lm_fit$coef
  Vb<-chol2inv(R) ## variance(unscaled)
  s2<-(t(lmfit$residuals)%*%lmfit$residuals)
  s2<-s2[1,1]/df.residual
  
  ## sample residual variance
  sigma<-df.residual*s2/rchisq(N,df.residual)
  coef.sim<-sapply(sigma,function(x) mvrnorm(1,coef,Vb*x))
  ret<-data.frame(t(coef.sim))
  names(ret)<-names(lm_fit$coef)
  ret$sigma<-sqrt(sigma)
  ret
}                      

Bayes.sum<-function(x)
{
  c("mean"=mean(x),
    "se"=sd(x),
    "t"=mean(x)/sd(x),
    "median"=median(x),
    "CrI"=quantile(x,prob=0.025),
    "CrI"=quantile(x,prob=0.975)
  )
}

set.seed(1234)  ## reproducible sim
lmfit <- lm(LE ~ Gini)
bf<-bayesfit(lmfit,10000)
t(apply(bf,2,Bayes.sum))

#                mean       se         t    median    CrI.2.5% CrI.97.5%
# (Intercept) 13.249811 1.7487790  7.576607 13.23355  9.817033  16.69132
# Gini         7.674463 2.1691554  3.537996  7.68791  3.367462  11.95234
# sigma       12.845947 0.7347008 17.484597 12.81267 11.506178  14.38228

summary(lmfit)

#Call:
#  lm(formula = LE ~ Gini)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-18.120  -9.559  -4.389   9.626  32.341 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   13.259      1.740   7.619 2.42e-12 ***
#  Gini           7.660      2.152   3.560 0.000494 ***
  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 12.79 on 154 degrees of freedom
#Multiple R-squared:  0.07602,	Adjusted R-squared:  0.07002 
#F-statistic: 12.67 on 1 and 154 DF,  p-value: 0.0004944
  
trt<-predict(lmfit) # Bayesian
ctl<-predict(lm_fit) # frequentist

compare <- cbind(ctl, trt)

compare.df<-as.data.frame(compare)
rm(compare)
# recognise the headers
attach(compare.df)

head(compare.df)
str(compare.df)

ggplot(compare.df, aes(x=ctl, y=trt)) + 
  geom_point(color='#2980B9', size = 4) + 
  geom_smooth(method=lm, color='#2C3E50')

#------------------------------------------------------

# COMPARING PRIOR AND POSTERIOR, USING GAMMA PRIOR

#------------------------------------------------------

# assign a gamma prior
# alpha = median life expectancy = 13.4
# beta = agg Gini = 6158.022 = 6158
# create Gamma (16, 6158) dist

alpha=13.40; beta=6158
yobs=1; ex=17.6 # mean life expectancy
y=0:10
lam=alpha/beta
py=dpois(y, lam*ex)*dgamma(lam, shape=alpha, 
                           rate = beta)/dgamma(lam, shape=alpha+y,
                                               rate= beta + ex)
cbind(y, round(py, 3))

#y      
#[1,]  0 0.962
#[2,]  1 0.037
#[3,]  2 0.001
#[4,]  3 0.000
#[5,]  4 0.000
#[6,]  5 0.000
#[7,]  6 0.000
#[8,]  7 0.000
#[9,]  8 0.000
#[10,]  9 0.000
#[11,] 10 0.000

lambdaA=rgamma(1000, shape = alpha + yobs, rate = beta + ex)

alpha=13.4; beta=6158
yobs=1; ex=39.26 # mean Gini 
y=0:10
lam=alpha/beta
py=dpois(y, lam*ex)*dgamma(lam, shape=alpha, 
                           rate = beta)/dgamma(lam, shape=alpha+y,
                                               rate= beta + ex)
cbind(y, round(py, 3))

      #y      
#[1,]  0 0.918
#[2,]  1 0.078
#[3,]  2 0.004
#[4,]  3 0.000
#[5,]  4 0.000
#[6,]  5 0.000
#[7,]  6 0.000
#[8,]  7 0.000
#[9,]  8 0.000
#[10,]  9 0.000
#[11,] 10 0.000

lambdaB=rgamma(1000, shape = alpha + yobs, rate = beta + ex)

par(mfrow = c(1, 2)) 

plot(density(lambdaB), main = "Gini Index",
     xlab= "lambdaB", lwd = 3)
curve (dgamma(x, shape = alpha, rate=beta), add=TRUE)
legend("topright", legend = c("prior", "posterior"), lwd=c(1,3))

plot(density(lambdaA), main = "Life Expectancy",
     xlab= "lambdaA", lwd = 3)
curve (dgamma(x, shape = alpha, rate=beta), add=TRUE)
legend("topright", legend = c("prior", "posterior"), lwd=c(1,3))

------------------------------------------------------------------------------------------
  
# GIBBS SAMPLER WITHOUT BAYESIAN INFERENCE (to solve analytically intractable problem)

------------------------------------------------------------------------------------------

set.seed(1212)
m = 100000; x = y = numeric(m); x[1] = y[1] = 0  # initial conditions
rho = 0.34; sgm = sqrt(1 - rho^2)                # correlation and standard deviation

for (i in 2:m) {                                 # Gibbs Sampler Loop
  x[i] = rnorm(1, rho*y[i-1], sgm)
  y[i] = rnorm(1, rho*x[i], sgm)
}

alpha.1 = beta.1 = .01
alpha.2 = .015; beta.2 = .012
x1 = alpha.1 +beta.1*x          # x1 ~ N(alpha.1, beta.1)
y1 = alpha.2 +beta.2*y          # x2 ~ N(alpha.2, beta.2)

c(mean(x), mean(y)); c(mean(x1),mean(y1))
# [1] 0.009958309 0.014986108

c(sd(x),sd(y)); c(sd(x1),sd(y1))
# [1] 0.009945211 0.012029835

# check they are correlated - should get same corr
c(cor(x,y),cor(x1,y1)) 
# [1] 0.3383534 0.3383534
# yes, they do, all on track

a= 9; b= .5                     # Assign weights from prior/posterior plots above
w = a*x1 + b*y1; mean(w); sd(w) # Overall integral 

# a = 9 b = .5
# [1] 0.09711784
# [1] 0.09171688

par(mfrow=c(2,2), pty="s") # 2 x 2 array of square plots
plot(x[1:50], y[1:50], type="l")
plot(x1, y1, pch=".", main = "Gini, LE, Green = Potential Correlation");points(x1[w>0], y1[w>0], pch=".", col="green4")
plot(x[1:1000], type="l"); acf(y)

---------------------------------------------------------------------------

# GIBBS SAMPLER WITH BAYESIAN MODEL
  
---------------------------------------------------------------------------
# 101 above average Gini out of 156 

# Assumptions and Parameters
# {S=1} Gini is high; {S=0} Gini is low
# {R=1} LE is high;   {R=0} LE is low


eta = 0.90;    # Sensitivity
theta = 0.95;  # Specificity
r = 101;       # Above average Gini coefficient
n = 156;       # Total number of countries
tau = r/n      # Estimate of rejection rate P{R=1}
print(tau)
# 0.6474359

psi = (tau + theta - 1)/(eta + theta -1)
psi
# me 0.7028658

tau.l = tau - 1.96 * sqrt(tau*(1-tau)/n)
tau.u = tau + 1.96 * sqrt(tau*(1-tau)/n)
round(c(tau.l, tau.u),3)
# me [1] 0.572 0.722

alpha.0 = 1; beta.0 = 1        # Prior Information
n = 156; r = 101               # Data

alpha.n = alpha.0 +r           # Posterior parameters
beta.n = beta.0 + n - r

alpha.n/(alpha.n+beta.n) 
# me [1] 0.6455696

qbeta(c(.025,.975),alpha.n,beta.n) 
# me [1] 0.5695886 0.7180595

N = 10000            # Number of iterations
Nb = 2000; N1 = N+1  # Burn-in

psi = numeric(N) 
psi[1] = .5          # Initial value

# Gibbs Sampler Loop
for(i in 2:N) {
  tau=psi[i-1]*eta+(1-psi[i-1])* (1-theta)        
  X = rbinom(1, r, psi[i-1]* eta/tau)
  Y = rbinom(1, n-r, psi[i-1]*(1-eta)/(1-tau))
  psi[i] = rbeta(1, alpha.0+X+Y, beta.0+n-X-Y)
}

mean(psi[Nb:N])
# [1] 0.7014006

par(mfrow=c(2,2))
hist(psi)
plot(1:N,cumsum(psi)/(1:N),type="l",ylab= "psi", ylim=c(0.68,0.72))
plot(psi,type='p',pch='.',ylim=c(0.58,0.72))
acf(psi)

psi.sort <- sort(psi)
L = as.integer(.05/2*(N-Nb))
U = as.integer((1-.05/2)*(N-Nb))

psi.L = psi.sort[L]
psi.U = psi.sort[U]
round(c(psi.L,psi.U),3)
# [1] 0.608 0.735
