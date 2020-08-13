rm(list = ls())

library(tidyverse)

# MARS
library(earth)

trust<-read.csv("Gini4.0.csv", stringsAsFactors = FALSE, header = TRUE, na.strings=c('NA',''))
str(trust)

# Set the seed for replication purposes

set.seed(123)

# Then, randomly sampling from the full dataset to generate a training set and a test set

rs <- sample(1:dim(trust)[1],size=round(dim(trust)[1]/5),replace=F)

length(rs) # Number of integers

table(rs) 

training.set <- trust[-rs,] # Bigger, to give a better chance to our model...
test.set <- trust[rs,] 

dim(training.set)
# 45 7  76%
dim(test.set)
# 11 7  24%

preProcValues<-preProcess(training.set, method= c("center", "scale"))
preProcValues
# Created from 45 samples and 7 variables

#Pre-processing:
#  - centered (6)
#  - ignored (1)
#  - scaled (6)

preProcValuestest<-preProcess(test.set, method= c("center", "scale"))
preProcValuestest
# Created from 11 samples and 7 variables

# Pre-processing:
# - centered (6)
#  - ignored (1)
#  - scaled (6)

mars <- earth(
  LE ~ .,  
  data = training.set   
)

summary(mars)

#Call: earth(formula=LE~., data=training.set)


# coefficients
# (Intercept)            84.03190
# CountryNetherlands     10.29143
# CountrySouthAfrica    -10.52967
# CountryZimbabwe       -15.17612
# h(0.19-Health)        -35.40582
# h(Health-0.19)        -23.05336
# h(0.9-Tfam)           -27.75571
# h(Tneigh-0.14)        -50.14155
# h(Tper-0.22)           43.71222

#Selected 9 of 22 terms, and 7 of 49 predictors
#Termination condition: GRSq -Inf at 22 terms
#Importance: Tfam, CountryZimbabwe, Tneigh, Tper, CountrySouthAfrica, ...
# Number of terms at each degree of interaction: 1 8 (additive model)
#GCV 20.98246    RSS 365.5611    GRSq 0.6353021    RSq 0.8523124
print(mars)
# Selected 9 of 22 terms, and 7 of 49 predictors
# Termination condition: GRSq -Inf at 22 terms
# Importance: Tfam, CountryZimbabwe, Tneigh, Tper, CountrySouthAfrica, ...
# Number of terms at each degree of interaction: 1 8 (additive model)
# GCV 20.98246    RSS 365.5611    GRSq 0.6353021    RSq 0.8523124

# GCV is like AIC, the smaller it is, the better the model

library(splines)

plot(mars, which = 1)

install.packages("ISLR")
require(ISLR)

# Call:
#  lm(formula = LE ~ bs(Gini, knots = c(25, 40, 60)), data = training.set)

# Residuals:
#  Min      1Q  Median      3Q     Max 
# -19.1600  -2.7395   0.2412   4.3400  10.6127 

#Coefficients:
#                                     Estimate  St Error t value  Pr(>|t|)    
#  (Intercept)                        78.060      2.094  37.270   <2e-16 ***
# bs(Gini, knots = c(25, 40, 60))1    36.994     62.560   0.591   0.5578    
# bs(Gini, knots = c(25, 40, 60))2    -5.896     14.736  -0.400   0.6913    
# bs(Gini, knots = c(25, 40, 60))3     1.347      9.209   0.146   0.8845    
# bs(Gini, knots = c(25, 40, 60))4   -2.475      11.172  -0.222   0.8258    
# bs(Gini, knots = c(25, 40, 60))5  -31.267      15.172  -2.061   0.0462 *  
# bs(Gini, knots = c(25, 40, 60))6  -18.760       6.946  -2.701   0.0103 *  
  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 6.623 on 38 degrees of freedom
# Multiple R-squared:  0.3266,	Adjusted R-squared:  0.2202 
# F-statistic: 3.071 on 6 and 38 DF,  p-value: 0.01501

Ginilims<-range(trust$Gini)
#Generating Test Data
Gini.grid<-seq(from=Ginilims[1], to = Ginilims[2])

plot(Gini.grid,col="grey",xlab="Gini",ylab="Life Expectancy")
points(Gini.grid,predict(fit,newdata = list(Gini=Gini.grid)),col="darkgreen",lwd=2,type="l")
#adding cutpoints
abline(v=c(25,40,60),lty=2,col="darkgreen")

install.packages("splines")
library(splines)

cubic.model=lm(LE~bs(Gini,knots=c(25,40,60)),data=trust)

attach(trust)
agelims=range(Gini)#get the upper and under border
agelims
#0 65

age.grid=seq(from=agelims[1],to=agelims[2])#0 - 65
pred=predict(cubic.model,newdata=list(Gini=Gini.grid),se=T)#nee

plot(Gini, LE, col="gray")#plot the scatter plot of age and wage
title("Cubic Splines")
lines(Gini.grid,pred$fit,lwd=2)#plot the fitted spline curve  #line width
lines(Gini.grid,pred$fit+2*pred$se,lty="dashed")#+2 sigma  #line type
lines(Gini.grid,pred$fit-2*pred$se,lty="dashed")#

dim(bs(LE,knots=c(25,40,60))) 
# 56   6
dim(bs(LE,df=6))
# 56 6
attr(bs(LE,df=6),"knots") 
#   25%   50%   75% 
#  73.85 77.20 81.30

head(ns(LE,df=4))
#                 1           2         3            4
# [1,] 0.0177942553  0.30318205 0.3314222  0.347601479
# [2,] 0.0627047502  0.47490658 0.2854622  0.176926423
# [3,] 0.0627047502  0.47490658 0.2854622  0.176926423
# [4,] 0.2302913303  0.57058812 0.2044450 -0.005324467
# [5,] 0.0007488007  0.01712443 0.3855304  0.596596354
# [6,] 0.0000000000 -0.15067633 0.4136750  0.737001313

natural.model=lm(LE~ns(LE,df=4),data=trust) #choose 4 knots as a example
pred2=predict(natural.model,newdata=list(Gini=Gini.grid),se=T)
plot(Gini,LE, cex=.5,col="darkgrey")
title("Natural Splines")

smooth.1=smooth.spline(Gini,LE,df=6) #specify the df, not accurate,will display warning
smooth.2=smooth.spline(Gini,LE,cv=TRUE)# use cross validation method
smooth.2$df  
# [1] 13.66558 #the final df achieved by cross validation

plot(Gini, LE, xlim=agelims,cex=.5,col="darkgrey")
title("Smoothing Spline")
lines(smooth.1,col="red",lwd=2)
lines(smooth.2,col="blue",lwd=2)#cross validation
legend("topright",legend=c("6 DF","13.6 DF"),col=c("red","blue"),lty=1,lwd=2,cex=.8)


