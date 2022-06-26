library(tidyverse)
library(glmnet)
library(car)
houses <- read.csv("https://raw.githubusercontent.com/aquijanoruiz/R_projects/master/kaggle_house_prices/data/train.csv")

hist(houses$SalePrice, freq = FALSE, main = "Histogram of house prices")
summary(houses$SalePrice)

# relationship between price and year a house was build
scatterplot(SalePrice ~ YearBuilt, data=houses,  xlab="Year Built", ylab="Sale Price", grid=FALSE)

# columns with NAs
colSums(is.na(houses))

# class of each column
sapply(houses, FUN = class)
str(houses)

#####################################
####        cleaning data        ####
#####################################

houses <- houses[ , colSums(is.na(houses)) == 0] # removes columns with NAs

# dichotomize function
dichotomize <- function(data,var) {
  for(x in var) {
    l <- length(data)
    data$v <- 1
    data <- spread(data, key = x, value = v, fill = 0, sep = "_")
    data <- data[, -l] # eliminates one column for colinearity
  }
  return(data)
}

# variables to dichotomize
binvar <- names(houses)[sapply(houses, FUN = is.character)] # character variable names
binvar <- c(binvar, c("MSSubClass", "OverallQual", "OverallCond","YearBuilt", 
                      "YearRemodAdd", "YrSold", "MoSold"))
houses <- dichotomize(houses, binvar)

#####################################
####    splitting the dataset    ####
#####################################

x <- houses[, -which(colnames(houses) %in% c("Id", "SalePrice"))]; x <- as.matrix(x)
y <- houses[,"SalePrice"]
y <- log(y) # log of price

# 2/3rds of the data will be used for training and 1/3 of the data will be used for testing
set.seed(25)
train_rows <- sample(1:nrow(houses), 0.66*nrow(houses))
x.train <- x[train_rows, ]
x.test <- x[-train_rows, ]

y.train <- y[train_rows]
y.test <- y[-train_rows]

#####################################
####      ridge and lasso        ####
#####################################

# ridge regression
set.seed(25)
alpha0.fit <- cv.glmnet(x = x.train, y = y.train, type.measure="mse", alpha=0, family="gaussian")
alpha0.predicted <- predict(alpha0.fit, s=alpha0.fit$lambda.1se, newx=x.test)
mean((alpha0.predicted - y.test)^2) # mse

# lasso regression
set.seed(25)
alpha1.fit <- cv.glmnet(x = x.train, y = y.train, type.measure="mse", alpha=1, family="gaussian")
alpha1.predicted <- predict(alpha1.fit, s=alpha1.fit$lambda.1se, newx=x.test)
mean((alpha1.predicted - y.test)^2) # mse

#####################################
####       eslastic net          ####
#####################################

# elastic net (50% lasso 50% ridge)
set.seed(25)
alpha0.5.fit <- cv.glmnet(x.train, y.train, type.measure="mse", alpha=0.5, family="gaussian")
alpha0.5.predicted <- predict(alpha0.5.fit, s=alpha0.5.fit$lambda.1se, newx=x.test)
mean((y.test - alpha0.5.predicted)^2)

list.of.fits <- list()
set.seed(25)
for (i in 0:10) {
  fit.name <- paste0("alpha", i/10)
  list.of.fits[[fit.name]] <-
    cv.glmnet(x.train, y.train, type.measure="mse", alpha=i/10, 
              family="gaussian")
}

results <- data.frame()
for (i in 0:10) {
  fit.name <- paste0("alpha", i/10)
  
  # y hat
  predicted <- 
    predict(list.of.fits[[fit.name]], 
            s=list.of.fits[[fit.name]]$lambda.1se, newx=x.test)
  
  mse <- mean((y.test - predicted)^2) # mean square error
  
  ## store the results
  temp <- data.frame(alpha=i/10, mse=mse, fit.name=fit.name)
  results <- rbind(results, temp)
}

results
plot(results$alpha, results$mse, xlab = "alpha", ylab = "mse", main = "Elastic net tuning")