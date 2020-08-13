# DOWLOAD DATA

trust<-read.csv("Gini8.0.csv", header = TRUE)
str(trust)

# Split into Train and Validation sets
# Training Set : Test Set = 76 : 24 (random)
set.seed(100)
train <- sample(nrow(trust), 0.76*nrow(trust), replace = FALSE)
training <- trust[train,]
testing <- trust[-train,]

head(training)
head(testing)

dim(training)
# 41 12
dim(testing)
# 14 12

# BASIC REGRESSION MODEL

lm_fit<-lm(LE~Health+Gini+Tfam+Tneigh+Tper,data=training)
summary(lm_fit)
predictions<-predict(lm_fit,newdata=testing)
error<-sqrt((sum((testing$LE-predictions)^2))/nrow(testing))
error

# 5.093477

library(foreach)
length_divisor<-4
iterations<-1000
predictions<-foreach(m=1:iterations,.combine=cbind) %do% {
  training_positions <- sample(nrow(training), size=floor((nrow(training)/length_divisor)))
  train_pos<-1:nrow(training) %in% training_positions
  lm_fit<-lm(LE~Health+Gini+Tfam+Tneigh+Tper,data=training[train_pos,])
  predict(lm_fit,newdata=testing)
}
predictions<-rowMeans(predictions)
error<-sqrt((sum((testing$LE-predictions)^2))/nrow(testing))
error

# error for linear model = 5.093477

# BAGGING MODEL

bagging<-function(training,testing,length_divisor=4,iterations=1000)
{
  predictions<-foreach(m=1:iterations,.combine=cbind) %do% {
    training_positions <- sample(nrow(training), size=floor((nrow(training)/length_divisor)))
    train_pos<-1:nrow(training) %in% training_positions
    lm_fit<-lm(LE~Health+Gini+Tfam+Tneigh+Tper,data=training[train_pos,])
    predict(lm_fit,newdata=testing)
  }
  rowMeans(predictions)
}
predictions<-rowMeans(predictions)
error<-sqrt((sum((testing$LE-predictions)^2))/nrow(testing))
error

# 4.512025
# therefore bagging (and other ensemble methods have reduced the error)

# BOOSTING MODEL

plot(predictions)
library(mboost)

mbout <- glmboost(LE ~ .,data=training)
lmout <- lm(LE ~ .,data=training)

print(mbout)

# Generalized Linear Models Fitted via Gradient Boosting

# Squared Error (Regression) 

# Loss function: (y - f)^2 

# Number of boosting iterations: mstop = 100 
# Step size:  0.1 
# Offset:  76.16341 

#Coefficients: 
#  (Intercept)    CountryAustralia        CountryChile       CountryCyprus 
#5.60607793          1.85121840          2.61988019          0.54046398 
#CountryGhana        CountryHaiti    CountryHong_Kong        CountryIndia 
#-7.55943644         -8.31711061          1.20394191         -1.78413499 
#CountryIraq   CountryKazakhstan      CountryNigeria        CountryQatar 
#-0.55966628         -0.52267691        -16.22822578          0.55315233 
#CountrySouth Africa  CountrySpain       CountrySweden     CountryZimbabwe 
#-10.23012699          4.00918929          1.10990233        -13.58512108 
#Gini               Trust              Health                  SC 
#-0.04389544          2.75472290         -4.73987814         -3.87248458 
#Tneigh 
#-4.55496735 

plot(mbout)



