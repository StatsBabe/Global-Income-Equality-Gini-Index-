library(MASS)       #for the shuttle data set
library(caret)      #for data preparation
library(neuralnet)  #for NN model
library(vcd)
library(nnet)

getwd()

trust<-read.csv("Gini4.0.csv", stringsAsFactors = FALSE, header = TRUE, na.strings=c('NA',''))
str(trust)
dim(trust)
# 56  7

# Then, randomly sampling from the full dataset to generate a training set and a test set

rs <- sample(1:dim(trust)[1],size=round(dim(trust)[1]/5),replace=F)

length(rs) # Number of integers

table(rs) 

train <- trust[-rs,] # Bigger, to give a better chance to our model...
test <- trust[rs,] 

train<-training.set
test<-test.set

# The next step is simply (1) running the linear model in the training set and 
# (2) predicting the value of our dependent variable in the test set using the predict() function. 
# Performing cross-validation is as simple as that!

starting.model <- lm(LE ~ Gini+Health+Tfam+Tneigh+Tper, data = training.set)
summary(starting.model)

pout <- predict(starting.model,newdata=test.set)
pout
#       31       15       51       14        3       42       50       43       37 
# 72.71187 78.84716 67.26637 73.65699 75.36840 70.52807 69.17252 79.96471 72.74251 
# 53       25 
#71.88234 76.66583 
# What we are really looking for is the answer to this question: was cross-validation effective? 
# Let's take a look at (1) the mean difference between our prediction and the actual data and 
# (2) how strongly correlated our prediction is with the value of actual data.

# Displaying the main difference between the expected response values from our regression on the training set 
# and the actual response values within the training set
mean(abs(pout-test.set$LE),na.rm=T)
# [1] 5.642282

# # How strongly correlated are our expected values with the actual values? 
cor(pout,test.set$LE,use="pairwise.complete.obs")
# [1] 0.665452

library(nnet)
library(ggplot2)
library(NeuralNetTools)
library(lattice)
library(MASS)       #for the shuttle data set
library(caret)      #for data prepration
library(neuralnet)  #for NN model
library(vcd)        #for data visualization

dim(train)
dim(test)

# keep this short-cut!
n <- names(train)
form <- as.formula(paste("use~", paste(n[!n %in% "use"], collapse = "+")))
form

neuralnet(LE ~ Gini+Health+Tfam+Tneigh+Tper, data = train)

#         Country   LE Gini Health Tfam Tneigh Tper
#1      Australia 84.5 34.0   0.32 0.81   0.06 0.40
#2    Netherlands 83.3 28.9   0.15 0.59   0.16 0.19
#4  United States 81.4 41.1   0.28 0.69   0.08 0.30
#5      Singapore 86.0  0.0   0.27 0.82   0.18 0.31
#6      Hong_Kong 86.8  0.0   0.15 0.84   0.12 0.30
#7         Sweden 83.9 26.1   0.31 0.89   0.29 0.43
#8        S_Korea 85.0  0.0   0.15 0.83   0.11 0.16
#9       Slovenia 83.4 24.9   0.22 0.86   0.14 0.21
#10         Spain 85.3 35.8   0.20 0.94   0.27 0.38
#11       Estonia 81.3 32.7   0.12 0.89   0.20 0.24
#12        Cyprus 82.4  0.0   0.40 0.90   0.17 0.22
#13         Qatar 79.9  0.0   0.50 0.91   0.35 0.43
#16         Chile 84.5 50.8   0.19 0.87   0.16 0.19
#17        Kuwait 75.8  0.0   0.42 0.86   0.29 0.39
#18       Belarus 77.2 26.5   0.05 0.88   0.18 0.17
#19        Russia 75.8 39.7   0.05 0.87   0.18 0.20
#20       Romania 78.3 27.3   0.15 0.81   0.09 0.10
#21       Uruguay 80.6 41.3   0.26 0.85   0.23 0.22
#22    Kazakhstan 74.1 28.6   0.14 0.93   0.24 0.23
#23      Malaysia 77.1 46.2   0.38 0.83   0.14 0.19
#24     Mauritius 74.0  0.0   0.43 0.70   0.13 0.18
#26        Turkey 78.5 40.0   0.19 0.94   0.37 0.32
#27        Mexico 79.2 48.1   0.26 0.82   0.13 0.15
#28        Brazil 78.3 52.7   0.24 0.70   0.11 0.10
#29       Georgia 78.4 41.4   0.14 0.91   0.27 0.15
#30    Azerbaijan 74.0 33.0   0.16 0.65   0.16 0.16
#32       Ukraine 75.9 24.8   0.06 0.92   0.23 0.18
#33       Algeria 77.2  0.0   0.22 0.86   0.24 0.23
#34          Peru 77.2 45.3   0.13 0.79   0.07 0.11
#35       Armenia 78.6 30.3   0.11 0.96   0.16 0.20
#36         China 77.3 37.0   0.24 0.86   0.19 0.13
#38         Libya 74.6  0.0   0.51 0.94   0.38 0.40
#39       Tunisia 77.3 35.8   0.28 0.95   0.38 0.34
#40         Egypt 73.4 30.8   0.14 0.99   0.56 0.61
#41     Palestine 74.9 34.5   0.28 0.87   0.16 0.20
#44  South Africa 59.3 65.0   0.42 0.76   0.24 0.20
#45    Kyrgyzstan 74.6 33.4   0.18 0.96   0.29 0.20
#46          Iraq 71.7 29.5   0.16 0.97   0.38 0.26
#47       Morocco 75.1 40.9   0.26 0.82   0.13 0.15
#48         India 69.5 33.6   0.27 0.91   0.34 0.30
#49         Ghana 62.3 42.8   0.50 0.67   0.13 0.15
#52      Zimbabwe 58.9  0.0   0.47 0.81   0.16 0.14
#54         Haiti 64.9 59.2   0.17 0.18   0.04 0.09
#55        Rwanda 67.0 50.8   0.34 0.79   0.48 0.32
#56    Cen_Africa 52.6 56.3   0.00 0.00   0.00 0.00

#$exclude
#NULL

#$net.result
#$net.result[[1]]
#[,1]
#1  76.2511
#2  76.2511
#4  76.2511
#5  76.2511
#6  76.2511
#7  76.2511
#8  76.2511
#9  76.2511
#10 76.2511
#11 76.2511
#12 76.2511
#13 76.2511
#16 76.2511
#17 76.2511
#18 76.2511
#19 76.2511
#20 76.2511
#21 76.2511
#22 76.2511
#23 76.2511
#24 76.2511

# to 52 same

#$weights
#$weights[[1]]
#$weights[[1]][[1]]
#[,1]
#[1,] 15.203339
#[2,]  1.381547
#[3,] 16.022738
#[4,] 17.692482
#[5,] 16.828214
#[6,] 16.869171

# $weights[[1]][[2]]
#[,1]
#[1,] 38.45881
#[2,] 37.79229


#$startweights
#$startweights[[1]]
#$startweights[[1]][[1]]
#[,1]
#[1,] -1.2650612
#[2,] -0.6868529
#[3,] -0.4456620
#[4,]  1.2240818
#[5,]  0.3598138
#[6,]  0.4007715

#$startweights[[1]][[2]]
#[,1]
#[1,]  0.1106827
#[2,] -0.5558411

#$result.matrix
#[,1]
#error                 1.237616e+03
#reached.threshold     2.938464e-04
#steps                 4.120000e+02
#Intercept.to.1layhid1 1.520334e+01
#Gini.to.1layhid1      1.381547e+00
#Health.to.1layhid1    1.602274e+01
#Tfam.to.1layhid1      1.769248e+01
#Tneigh.to.1layhid1    1.682821e+01
#Tper.to.1layhid1      1.686917e+01
#Intercept.to.LE       3.845881e+01
#1layhid1.to.LE        3.779229e+01

summary(fit)

#Call:
#  lm(formula = LE ~ bs(Gini, knots = c(25, 40, 60)), data = training.set)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-19.1600  -2.7395   0.2412   4.3400  10.6127 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                        78.060      2.094  37.270   <2e-16 ***
# bs(Gini, knots = c(25, 40, 60))1   36.994     62.560   0.591   0.5578    
# bs(Gini, knots = c(25, 40, 60))2   -5.896     14.736  -0.400   0.6913    
# bs(Gini, knots = c(25, 40, 60))3    1.347      9.209   0.146   0.8845    
# bs(Gini, knots = c(25, 40, 60))4   -2.475     11.172  -0.222   0.8258    
# bs(Gini, knots = c(25, 40, 60))5  -31.267     15.172  -2.061   0.0462 *  
# bs(Gini, knots = c(25, 40, 60))6  -18.760      6.946  -2.701   0.0103 *  
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 6.623 on 38 degrees of freedom
# Multiple R-squared:  0.3266,	Adjusted R-squared:  0.2202 
# F-statistic: 3.071 on 6 and 38 DF,  p-value: 0.01501

print(fit)

#Call:
#  lm(formula = LE ~ bs(Gini, knots = c(25, 40, 60)), data = training.set)

#Coefficients:
#  (Intercept)  bs(Gini, knots = c(25, 40, 60))1  
#78.060                            36.994  
#bs(Gini, knots = c(25, 40, 60))2  bs(Gini, knots = c(25, 40, 60))3  
#-5.896                             1.347  
#bs(Gini, knots = c(25, 40, 60))4  bs(Gini, knots = c(25, 40, 60))5  
#-2.475                           -31.267  
#bs(Gini, knots = c(25, 40, 60))6  
#-18.760  

