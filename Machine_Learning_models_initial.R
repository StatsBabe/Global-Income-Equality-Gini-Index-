# =================================================================================

# DOWNLOAD LIBRARIES (OTHERS DOWNLOADED LATER AS APPROPRIATE)

#==================================================================================

getwd()
# basic packages install
library(tidyverse)
library(plyr)

#=====================================================================================

# INITIAL DATA EXPLORATION (inc stepwise linear regression, diagnostic plots, PCA)

#=====================================================================================

trust_set<-read.csv("Gini8.0.csv", stringsAsFactors = FALSE, header = TRUE, na.strings=c('NA',''))
dim(trust_set)
# 56 12
head(trust_set)
names(trust_set)
# "Country"   "LE"        "Gini"      "Trust"     "Health"    "SC"        "Tfam"      "Tneigh"    "Tper"     
# [10] "Tfirst"    "Treligion" "Tnat" 

# check for missing values
sapply(trust_set, function(x) sum(is.na(x)))
#Country        LE      Gini     Trust    Health        SC      Tfam    Tneigh 
#     0         0         0         0         0         0         0         0 
#Tper    Tfirst Treligion      Tnat 
#0         0         0         0 

summary(trust_set)
# all covariates seem to be on roughly equivalent scales

install.packages("Amelia")
library(Amelia)
missmap(trust_set, main = "Missingness Map Test")

#corrmatrix
pairs(trust_set[,c(2,3,4,5,6,7,8,9,10,11,12)],col=2:12)
# general overall scatter in correlation plots between all covariates

glimpse(trust_set)

# linear regression

simple.model <- lm(LE ~ 1, data = trust_set)
summary(simple.model)
#Call:
#lm(formula = LE ~ 1, data = trust_set)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-23.038  -2.138   1.062   5.162  10.662 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  76.1382     0.9734   78.22   <2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 7.219 on 54 degrees of freedom

# plot basic regression with line of best fit (which is not fitting well!)

ggplot(data = trust_set, aes(x = Gini, y = LE)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

# Regression to the mean
ggplot(data = trust_set, aes(x = Gini, y = LE)) +
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  geom_smooth(method = "lm", se = FALSE)


# CLUSTER DENDROGRAM OF GINI
d <- dist(trust_set, method= "euclidean")
H.fit <- hclust(d, method="ward")
par(mfrow = c(1, 1)) 
plot(H.fit) # display dendogram
groups <- cutree(H.fit, k=5)
rect.hclust(H.fit, k=3, border="red") # divide into 3 groups,

# multiple linear regression

glm.model <- glm(LE ~ Gini+Trust+Health+SC+Tfam+Tneigh+Tper+Tfirst+Treligion+Tnat, data = trust_set)
summary(glm.model)
#Call:
#  glm(formula = LE ~ Gini + Trust + Health + SC + Tfam + Tneigh + 
#        Tper + Tfirst + Treligion + Tnat, data = trust_set)

#Deviance Residuals: 
#    Min        1Q    Median        3Q       Max  
#-11.7485#   -3.0249    0.3976    3.6283    7.8325  

#Coefficients:
#           Estimate Std. Error t value   Pr(>|t|)    
#(Intercept)  77.06574    6.28216  12.267 8.53e-16 ***
#Gini         -0.05707    0.04634  -1.232 0.224655    
#Trust        -2.22542    5.05009  -0.441 0.661609    
#Health      -23.74332    8.41878  -2.820 0.007170 ** 
#SC           -1.28424    6.37633  -0.201 0.841309    
#Tfam         10.46369    7.07581   1.479 0.146318    
#Tneigh      -45.30611   11.20623  -4.043 0.000209 ***
#Tper         44.45334   10.53614   4.219 0.000121 ***
#Tfirst      -24.90636   52.63512  -0.473 0.638418    
#Treligion   -71.13992   45.84073  -1.552 0.127852    
#Tnat         61.62450   42.94681   1.435 0.158385   
 
---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# (Dispersion parameter for gaussian family taken to be 25.66569)
  
# Null deviance: 2814.2  on 54  degrees of freedom
# Residual deviance: 1129.3  on 44  degrees of freedom
# AIC: 346.29

# Number of Fisher Scoring iterations: 2

# stepwise multiple linear regresssion

stepAIC(starting.model, scope = list(upper = starting.model, lower = simple.model), direction = "forward")

final.model <- lm(LE ~ Gini+Health+Tfam+Tneigh+Tper, data = trust_set)
summary(final.model)
#Call:
#  lm(formula = LE ~ Gini + Health + Tfam + Tneigh + Tper, data = trust_set)

# Residuals:
#  Min       1Q   Median       3Q      Max 

final.model <- glm(LE ~ Gini+Health+Tfam+Tneigh+Tper, data = trust_set)
summary(final.model)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-12.4827  -3.9490  -0.0762   3.9089   7.3562 

#Coefficients:
#               Estimate  Std. Error  t value Pr(>|t|)    
#(Intercept)    74.32005    5.81537  12.780  < 2e-16 ***
#  Gini         -0.07628    0.04344  -1.756   0.0853 .  
# Health       -26.01760    5.88667  -4.420 5.47e-05 ***
#  Tfam         13.63602    6.72291   2.028   0.0480 *  
#  Tneigh      -47.70565    9.23872  -5.164 4.40e-06 ***
#  Tper         41.99011    9.63138   4.360 6.67e-05 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 5.01 on 49 degrees of freedom
# Multiple R-squared:  0.563,	Adjusted R-squared:  0.5184 
# F-statistic: 12.63 on 5 and 49 DF,  p-value: 6.753e-08

install.packages("leaps")
library(leaps)
attach(trust_set)
leaps<-regsubsets(LE ~ Gini+Trust+Health+SC+Tfam+Tneigh+Tper+Tfirst+Treligion+Tnat, data = trust_set,nbest=10)
# view results
summary(leaps)
# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
plot(leaps,scale="r2")
# plot statistic by subset size
install.packages("car")
library(car)
subsets(leaps, statistic="rsq") 

# Diagnostic plots
trust_set.x <- data.matrix(trust_set[, 1:10])
trust_set.y <- trust_set[, "LE"]
pairs(trust_set, main = "trust_set")
summary(fm1 <- lm(LE ~ ., data = trust_set))
opar <- par(mfrow = c(2, 2), oma = c(0, 0, 1.1, 0),
            mar = c(4.1, 4.1, 2.1, 1.1))
plot(fm1)
par(opar)

# Other functions in linear model
coefficients(final.model) # model coefficients
confint(final.model, level=0.95) # CIs for model parameters

#                  2.5 %       97.5 %
#(Intercept)  62.6336321  86.00645980
#Gini         -0.1635740   0.01101533
#Health      -37.8473191 -14.18788929
#Tfam          0.1258225  27.14622376
#Tneigh      -66.2715590 -29.13974671
#Tper         22.6351202  61.34510252

fitted(final.model) # predicted values
#       1        2        3        4        5        6        7        8        9 
#88.37980 76.60340 75.94378 82.08947 82.90675 88.74402 80.62087 83.20610 80.56293 
#10       11       12       13       14       15       16       17       18 
#82.27930 81.37616 77.31329 75.07879 75.30600 77.19914 77.71027 77.66114 81.54877 
#19       20       21       22       23       24       25       26       27 
#81.66522 79.28566 74.26128 79.38586 73.52648 74.03418 80.12331 74.92913 75.16475 
#28       29       30       31       32       33       34       35       36 
#72.55251 73.34639 75.58894 71.17111 79.99832 78.53152 79.53428 83.00255 73.37511 
#37       38       39       40       41       42       43       44       45 
#71.96812 72.53683 73.40703 80.72664 77.03194 69.53246 79.68877 65.74654 74.74311 
#46       47       48       49       50       51       52       53       54 
#73.92321 75.71397 73.51820 67.27941 68.62122 63.96401 71.38266 71.19297 69.70669 
#55 
#62.90965 

residuals(final.model) # residuals
anova(final.model) # anova table
vcov(final.model) # covariance matrix for model parameters
influence(final.model) # regression diagnostics 
predictions<-fitted(final.model)
mse <- mean((trust_set$LE - predictions)^2)
print(mse)
# 22.35988
sqrt(mse)
# rmse =  4.728623

# diagnostic plots to check heteroscedascity, normality
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(final.model)
# general scatter, approximately normally distributed

# PCA
PCA<-prcomp(trust_set[c("Gini", "Health", "Tfam", "Tneigh", "Tper")], scale = T)
summary(PCA)
# Importance of components:
#PC1    PC2    PC3    PC4     PC5
#Standard deviation     1.4992 1.0606 0.9379 0.7080 0.49665
#Proportion of Variance 0.4495 0.2250 0.1759 0.1003 0.04933
#Cumulative Proportion  0.4495 0.6745 0.8504 0.9507 1.00000

par(mfrow = c(1, 1))
screeplot(PCA)

barplot(100*summary(PCA)$importance[2,], ylab="Percentage Variance")

# fab ordination plot
biplot(PCA)

#=================================================================================

# MACHINE LEARNING EXPLORATORY (WITHOUT TRAINING AND TEST SETS AND METRICS)

#=================================================================================

# MARS
install.packages("earth")
# load the package
library(earth)

# fit the model
modelMARS <- earth(LE~., trust_set)
modelMARS

#Selected 12 of 26 terms, and 10 of 64 predictors
#Termination condition: GRSq -10 at 26 terms
#Importance: Health, Gini, Tneigh, Tper, CountryZimbabwe, CountryHaiti, Treligion, ...
#Number of terms at each degree of interaction: 1 11 (additive model)
#GCV 21.6674    RSS 403.4076    GRSq 0.5918008    RSq 0.8566543

# summarize the importance of input variables
evimp(modelMARS)
#                    nsubsets   gcv    rss
#Health                11     100.0  100.0
#Gini                   9      69.7   74.1
#Tneigh                 9      65.4   70.3
#Tper                   9      65.4   70.3
#CountryZimbabwe        7      45.9   53.3
#CountryHaiti           6      37.0   46.0
#Treligion              5      21.9   37.5
#Tfam                   4      18.5   32.7
#CountryYemen           2      22.7   24.5
#CountryUruguay         1      14.9   16.6

# make predictions
predictions <- predict(modelMARS, trust_set)
# summarize accuracy
mse <- mean((trust_set$LE - predictions)^2)
print(mse)
# 7.334684
sqrt(mse)
# 2.708262

# SVM
install.packages("kernlab")
# load the package
library("kernlab")
# fit model
modelSVM <- ksvm(LE~., trust_set)
modelSVM
# Support Vector Machine object of class "ksvm" 

# SV type: eps-svr  (regression) 
# parameter : epsilon = 0.1  cost C = 1 

# Gaussian Radial Basis kernel function. 
# Hyperparameter : sigma =  0.0823508113197531 

# Number of Support Vectors : 52 

# Objective Function Value : -20.5632 
# Training error : 0.228829 

# summarize the fit
summary(modelSVM)
# Length  Class   Mode 
# 1   ksvm     S4 
# make predictions
predictions <- predict(modelSVM, trust_set)
# summarize accuracy
mse <- mean((trust_set$LE - predictions)^2)
print(mse)
# 11.92552
sqrt(mse)
# 3.453335

# k nearest neighbour
install.packages("caret")
# load the package
library(caret)
# fit model
modelKNN <- knnreg(trust_set[,2:12], trust_set[,12], k=3)
modelKNN
# 10-nearest neighbour regression model
# summarize the fit
summary(modelKNN)
#         Length Class  Mode   
# learn   2      -none- list   
# k       1      -none- numeric
# theDots 0      -none- list  
# make predictions
predictions <- predict(modelKNN, trust_set[,2:12])
predictions
# [1] 0.030 0.042 0.045 0.044 0.054 0.054 0.042 0.054 0.042 0.038 0.029 0.054 0.054
#[14] 0.029 0.042 0.043 0.054 0.046 0.032 0.047 0.044 0.039 0.040 0.054 0.054 0.045
#[27] 0.043 0.043 0.044 0.040 0.034 0.050 0.054 0.043 0.034 0.032 0.045 0.054 0.033
#[40] 0.040 0.036 0.034 0.028 0.042 0.035 0.042 0.032 0.037 0.044 0.040 0.043 0.053
#[53] 0.038 0.042 0.031
# summarize accuracy
mse <- mean((trust_set$LE - predictions)^2)
print(mse)
# 5841.764
sqrt(mse)
# 76.43143
# exactly the same as glm/lm

# CART
install.packages("rpart")
library(rpart) 
par(mfrow = c(1,1))
modelCART <- rpart(LE ~ Gini+Health+Tfam+Tneigh+Tper, trust_set) 
par(xpd = TRUE) 
plot(modelCART, compress = TRUE) 
text(modelCART, use.n = TRUE) 
modelCART

# n= 56 

# node), split, n, deviance, yval
# * denotes terminal node

# 1) root 55 2814.2300 76.13818  
# 2) Health>=0.41 8  690.3588 67.23750 *
# 3) Health< 0.41 47 1382.2170 77.65319  
# 6) Tneigh>=0.31 10  186.7240 71.74000 *
# 7) Tneigh< 0.31 37  751.3324 79.25135  
#14) Tper< 0.235 28  499.5411 77.91786  
#28) Gini>=32.9 16  269.5600 76.35000 *
#29) Gini< 32.9 12  138.2092 80.00833 *
#15) Tper>=0.235 9   47.1000 83.40000 *

# LM Assessing R2 shrinkage using 10-Fold Cross-Validation
install.packages("bootstrap")
library(bootstrap)
# After cross-validation, sum the MSE for each fold, divide by the number of observations, and take the square root to get the cross-validated standard error of estimate.

# You can assess R2 shrinkage via K-fold cross-validation. Using the crossval() function from the bootstrap package, do the following: 
  # define functions
theta.fit <- function(x,y){lsfit(x,y)}
theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef}

# matrix of predictors
X <- as.matrix(trust_set[c("Gini", "Health", "Tfam", "Tneigh", "Tper")])
# vector of predicted values
y <- as.matrix(trust_set[c("LE")])
results <- crossval(X,y,theta.fit,theta.predict,ngroup=10)

cor(y,results$cv.fit)**2 # cross-validated R2 
# LE 0.4338585

#SVM
install.packages("e1071")
library(e1071)

#Create a data frame of the data
train=data.frame(X,y)
#Plot the dataset
plot(train,pch=16)
#Linear regression
model <- lm(y ~ X, train)

#Linear model has a residuals part which we can extract and directly calculate rmse
error <- model$residuals 
lm_error <- sqrt(mean(error^2)) 
lm_error # RMSE  4.728623

#Fit a model. The function syntax is very similar to lm function
model_svm <- svm(y ~ X, train)
#Use the predictions on the data
pred <- predict(model_svm, train)
#Plot the predictions and the plot to see model fit

##Calculate parameters of the SVM model
#Find value of b
b = model_svm$rho
b
# 0.418037

mse <- mean((y - pred)^2)
print(mse)
# 15.01626
sqrt(mse)
# RMSE for SVM Model 3.875082

# perform a grid search
svm_tune <- tune(svm, y ~ X, data = trust_set,
                 ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:12))
)
print(svm_tune)

#Parameter tuning of 'svm':

# - sampling method: 10-fold cross validation 

#- best parameters:
# epsilon cost
# best performance: 7.134636 

#The best model
best_mod <- svm_tune$best.model
best_mod_pred <- predict(best_mod, train) 

error_best_mod <- train$y - best_mod_pred 

# tune method randomly shuffles the data unless you set a seed
best_mod_RMSE <- sqrt(mean(error_best_mod^2))
#       1        2        3        4        5        6        7        8        9 
#83.77939 82.57740 82.57572 80.67908 85.27734 87.52295 84.61891 85.72141 82.67552 
#10       11       12       13       14       15       16       17       18 
#86.02284 80.30408 81.67683 79.17877 80.67688 80.82004 83.77650 76.52418 77.03003 
#19       20       21       22       23       24       25       26       27 
#76.52031 79.02336 79.87629 76.18046 76.37547 73.27731 82.02556 77.77803 79.87239 
#28       29       30       31       32       33       34       35       36 
#77.58093 79.12439 74.72391 75.96921 75.17766 76.48190 76.47590 77.87878 76.57609 
#37       38       39       40       41       42       43       44       45 
#78.55740 75.32370 76.57829 74.12087 75.62300 71.07912 75.76180 60.02286 73.87663 
#46       47       48       49       50       51       52       53       54 
#70.97625 75.82287 70.09328 63.02151 66.47555 53.82249 59.62317 65.92505 65.62278 
#55 
#66.27650 

plot(svm_tune) # gives big blue square showing cost vs epsilon
# darker is better
# best coefficients are cost around 4000, epsilon 0.2 or less





