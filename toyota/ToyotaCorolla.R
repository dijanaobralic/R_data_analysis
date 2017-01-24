
library(reshape2) # For resahping the data
library(dplyr) # For data preperation
library(e1071) # Skewness and Kurtosis
library(car) # For the Levenes test
library(ggplot2) # For nice plotting
library(leaps) # Exhaustive search for the best subsets of the variables in x for predicting y
library(broom) # For residual analysis (augment())
library(psych) # descriptive statistics

# -------------------------PREPARE DATA----------------------------------

# load the file
toyota.df <- read.csv("ToyotaCorolla.csv",header = TRUE)

# check if data loaded correctly
head(toyota.df)

# check types of variables in the data set
str(toyota.df)
summary(toyota.df$FuelType)

# regression takes only numerical values
## convert FuelType from categorical to numerical


# Diesel = 0
# Petrol = 1
# CNG = 2

toyota.df$FuelType = ifelse(toyota.df$FuelType =="Diesel",0,
                            ifelse(toyota.df$FuelType=="Petrol",1,
                                   ifelse(toyota.df$FuelType=="CNG",2,
                                          toyota.df$FuelType)))


# check if categorical "FuelType" data is converted into numerical
head(toyota.df)


#--------------------------DESCRIPTIVE STATISTICS---------------------------

# PREDICTED VARIABLE:
# Price - The price of the car ($)

# PREDICTOR VARIABLES:
# Age - The age of the car in month 
# KM - Accumulated kilometer on the odometer 
# FuelType - Type of fuel used 
# HP - Horsepower of the car 
# MetColor - Whether the car has metallic (1) or not (0) 
# Automatic - Whether the car has automatic transmission (1) or not (0) 
# CC - Cylinder volumen in cubic centimeter 
# Doors - Number of doors 
# Weight - Weight of the car in kilogram

toyota.desc <- describe(toyota.df)

# become familiar with data
plot(Price ~ Age, data = toyota.df,col="blue")
plot(Price ~ KM, data = toyota.df,col="blue")
plot(Price ~ FuelType, data = toyota.df,col="blue")
plot(Price ~ HP, data = toyota.df,col="blue")
plot(Price ~ MetColor, data = toyota.df,col="blue")
plot(Price ~ Automatic, data = toyota.df,col="blue")

#------------------------------REGRESSION ANALYSIS-------------------------------

# first regression model using all data
## if p-value < 0.05 then variable is significant for the model
### p value > 0.05 for MetColor and Doors -> not significant for the model
toyota.m1 <- lm(formula = Price ~ ., data = toyota.df)
toyota.m1.summary <- summary(toyota.m1)
toyota.m1.summary

## Getting the confident intervals
toyota.m1.confint <- confint(toyota.m1,level = 0.95)
toyota.m1.confint

## Check with a correlation matrix if predictor variables are themselves related

toyota.cor <- cor(toyota.df)
toyota.cor

# caclulate rsq, rsq(adj) and Cp to see which variables to remove from the model

x <- toyota.df[,2:10] # Independent variables
y <- toyota.df[,1] # Dependent variables

toyota.out <- summary(regsubsets(x, y, nbest = 1, nvmax = ncol(x),force.in = NULL, force.out = NULL, method = "exhaustive"))
toyota.regtab <- cbind(toyota.out$which,toyota.out$rsq, toyota.out$adjr2, toyota.out$cp) # Stich things together
colnames(toyota.regtab) <- c("(Intercept)","Age","KM","FuelType", "HP", "MetColor", "Automatic","CC","Doors","Weight",
                             "R-Sq", "R-Sq (adj)", "Cp") # Add header
print(toyota.regtab)

## model 7 gives the highest r-Sq(adj) 
### exclude MetColor and Doors from the model

# Create a second model without MetColor and Doors
toyota.m2 <- lm(formula = Price ~ Age + KM + FuelType + HP + Automatic + CC + Weight, data = toyota.df) 
toyota.m2.summary <- toyota.m2
print(toyota.m2.summary)

## Getting the confident intervals
toyota.m2.confint <- confint(toyota.m2)
print(toyota.m2.confint)

#------------------------------COMPARE MODELS----------------------------------

# Check which model (m1 or m2) is better

n <- length(toyota.df$Price) # Get the number of elements
diff <- dim(n) # Set the dimension of the container object
percdiff <- dim(n) # Set the dimension of the container object

# Create a loop to test each combination options of the elements in our data
for (k in 1:n) {
  train1 <- c(1:n)
  
  # the R expression "train1[train1 != k]" picks from train1 those
  # elements that are different from k and stores those elements in the
  # object train.
  # For k = 1, train consists of elements that are different from 1; that
  # is 2, 3, ..., n.
  train <- train1[train1 != k]
  
  # Create the linar model for the all but one element
  m1 <- lm(Price ~ ., data = toyota.df[train,])
  
  # Predict the missing value based on the model
  pred <- predict(m1, newdat = toyota.df[-train,])
  
  # What is the real value
  obs <- toyota.df$Price[-train]
  
  # Calculate the delta between observed and predicted
  diff[k] <- obs - pred
  
  # Calculate the relative difference between observed and predicted
  percdiff[k] <- abs(diff[k]) / obs
}

toyota.m1.me <- mean(diff) # mean error
toyota.m1.rmse <- sqrt(mean(diff**2)) # root mean square error
toyota.m1.mape <- 100*(mean(percdiff)) # mean absolute percent error

n <- length(toyota.df$Price)
diff <- dim(n)
percdiff <- dim(n)
for (k in 1:n) {
  train1 <- c(1:n)
  train <- train1[train1 !=k ]
  m2 <- lm(Price ~ Age + KM + FuelType + HP + Automatic + CC + Weight, data = toyota.df[train,])
  pred <- predict(m2, newdat = toyota.df[-train,])
  obs <- toyota.df$Price[-train]
  diff[k] <- obs - pred
  percdiff[k] <- abs(diff[k]) / obs
}

toyota.m2.me <- mean(diff)
toyota.m2.rmse <- sqrt(mean(diff**2))
toyota.m2.mape <- 100*(mean(percdiff))


toyota.m1.me   # mean error
toyota.m1.rmse # root mean square error
toyota.m1.mape # mean absolute percent error

toyota.m2.me   # mean error
toyota.m2.rmse # root mean square error
toyota.m2.mape # mean absolute percent error
#-----------------------------------PREDICT PRICE------------------------

# m2 seems to be optimal model
## for this prediction use m1

toyota.predict.lm <- lm(Price ~ Age + FuelType + HP + MetColor + Automatic + CC + Doors, data = toyota.df)

toyota.predict.var <- data.frame(Age=12, FuelType=1, HP=185, MetColor=1, Automatic=0,CC=2000,Doors=4)
                                   
predicted.price <- predict(toyota.predict.lm,toyota.predict.var)

print(predicted.price)



#---------------------------------------GRAPH----------------------------------

# Check if the assumptions are met
## Create data frame with residuals
toyota.f <- fortify(toyota.m1)

## Linearity
### Residual vs Fitted Plot
p1 <- ggplot(toyota.f, aes(x = .fitted, y = .resid)) +
  geom_point() +
  stat_smooth(method = "loess") +
  geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals") +
  ggtitle("Residual vs Fitted Plot")


## Normality
### Normal Q-Q Plot
p2 <- ggplot(toyota.f, aes(x = qqnorm(.stdresid)[[1]], y = .stdresid)) +
  geom_point(na.rm = TRUE) +
  geom_abline() +
  xlab("Theoretical Quantiles") +
  ylab("Standardized Residuals") +
  ggtitle("Normal Q-Q")

toyota.skew <- skewness(toyota.f$.resid)
toyota.kurt <- kurtosis(toyota.f$.resid)

## Equal variance
### Scale-Location Plot
p3 <- ggplot(toyota.f, aes(x = .fitted, y = sqrt(abs(.stdresid)))) +
  geom_point(na.rm=TRUE) +
  stat_smooth(method = "loess", na.rm = TRUE) +
  xlab("Fitted Value") +
  ylab(expression(sqrt("|Standardized residuals|"))) +
  ggtitle("Scale-Location")


## Independence
# Perform a Durbin-Watson F-test for autocorrelation
toyota.dw <- dwtest(m1)

## Outlier influance
### Cook's Distance Histogram
p4 <- ggplot(toyota.f, aes(x = seq_along(.cooksd), y = .cooksd)) +
  geom_bar(stat="identity", position="identity") +
  xlab("Obs. Number") +
  ylab("Cook's distance") +
  ggtitle("Cook's distance")

p5 <- ggplot(toyota.f, aes(x =.hat, y = .stdresid)) +
  geom_point(aes(size=.cooksd), na.rm=TRUE) +
  stat_smooth(method="loess", na.rm=TRUE) +
  xlab("Leverage") +
  ylab("Standardized Residuals") +
  ggtitle("Residual vs Leverage Plot") +
  scale_size_continuous("Cook's Distance", range = c(1,5)) +
  theme(legend.position="bottom")

## Save Plots
ggsave("graphs/linearityAssumption.pdf", p1)
ggsave("graphs/normalityAssumption.pdf", p2)
ggsave("graphs/equalVarianceAssumptions.pdf", p3)
ggsave("graphs/outlierInfluance1Assumptions.pdf", p4)
ggsave("graphs/outlierInfluance2Assumptions.pdf", p4)