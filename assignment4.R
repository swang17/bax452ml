rm(list = ls())
setwd('Desktop/ml')

library('freqparcoord')
data("mlb")
head(mlb)

########## 1.22.1 Building Cross-Validation Functions for Linear and K-NN Models ##########

### partitioning of data
xvalpart <- function(data, p){
  n <- nrow(data)
  ntrain <- round(p*n)
  trainidxs <- sample(1:n, ntrain, replace = FALSE)
  list(train = data[trainidxs,],
       valid = data[-trainidxs,])
}


### cross validation: linear model case ###
# arguments:
# data: full data
# ycol: column number of resp. var.
# predvars: column numbers of predictors
# p: proportion for training set
# meanabs: see 'values' below
# value: if meanabs is TRUE, the mean absolute 
# prediction error ; otherwise , an R list 
# containing pred. , real Y 

xvallm <- function(data, ycol, predvars, p, meanabs = TRUE){
  tmp <- xvalpart(data, p)
  train <- tmp$train
  valid <- tmp$valid
  # fit model to training data
  trainy <- train[, ycol]
  trainpreds <- train[, predvars]
  # using matrix form in lm call
  trainpreds <- as.matrix(trainpreds)
  lmout <- lm(trainy ~ trainpreds)
  # apply fitted model to validation data;
  # note that %*% works only on matrices, not dataframes
  validpreds <- as.matrix(valid[, predvars])
  predy <- cbind(1, validpreds)%*% coef(lmout)
  realy <- valid[,ycol]
  if (meanabs) return (mean(abs(predy-realy)))
  list(predy = predy, realy = realy)
  
}

# try out the cross-validation function with mlb data
for (i in 1:10){
  print(xvallm(mlb, 5, c(4,6), 2/3))
}

### cross validatoin: k-nn case ###
library(regtools)
# arguments:
#   data: full data
#   ycol: column number of resp. var.
#   k: number of nearest neighbors
#   p: prop. for training set
#   meanabs: see 'value' below
#   value: if meanabs is TRUE, the mean absolute
#         prediction error ; otherwise , an R list 
#         containing pred . , real Y

xvalknn <-  function(data, ycol , predvars , k , p , meanabs=TRUE){
  # cull out just Y and the Xs 
  data <-  data[ ,c( predvars , ycol ) ] 
  ycol <-  length( predvars ) + 1 
  tmp <-  xvalpart (data, p) 
  train <-  tmp$train 
  valid <- tmp$valid 
  valid <-  as.matrix(valid ) 
  xd <-  preprocessx( train [ ,-ycol] , k) 
  kout <-  knnest( train [ ,ycol ] , xd , k) 
  predy <-  predict(kout, valid[ ,-ycol ] ,TRUE) 
  realy <-   valid[ , ycol] 
  if ( meanabs ) return(mean(abs(predy-realy)))
  list( predy = predy , realy = realy )
}

set.seed(9999)

# try out the cross-validation function with mlb data
for (i in 1:10){
  print(xvalknn (mlb, 5, c(4,6), 25, 2/3) )
}


### Comparison ###
# In this case, the results for both models are similar
 
#####################################################################

########## 1.22.2 Linear Regression with Interaction Terms ##########
rm(list = ls())
library('freqparcoord')
data("prgeng")
prgeng$age2 <- prgeng$age^2
edu <- prgeng$educ
prgeng$ms <- as.integer(edu == 14)
prgeng$phd <- as.integer(edu==16)
prgeng$fem <- prgeng$sex - 1
tmp <- prgeng[edu>=13,]
pe <- tmp[,c(1,12,9,13,14,15,8)]
# add new interaction terms
pe$agegen <- pe$age*pe$fem
pe$age2gen <- pe$age2 * pe$fem
#pe <- as.matrix(pe)

# run linear model
model = lm(wageinc ~ age+age2+agegen+age2gen+wkswrkd+ms+phd+fem, data = pe)
summary(model)
# Effect of a female, 32-year-old, Master's Degree holder is 
effect = 4715.271*32 + 32^2 *(-49.081) -2503.678 * 32 +27.607 * 32^2 +9810.033+41739
effect

#####################################################################################

########## 1.22.3 Test Feasibility of Indirect Methods  ##########
rm(list = ls())
data = read.csv('bodyfat.csv')
lmdata = data[, c(5:18)]
model = lm(density~ ., data = lmdata)
summary(model)
# The model captures more than 70% of variation in density, having a considerably high predicting power
# Indirect Method in this case is feasible

#####################################################################################

########## 1.22.4   ##########

# National mean height of people is the weighted average of the gender means, 
# where the weight for each gender being its proportion to the national population

# The overall proportion of people taller than 70 inches is the weighted average of gender proportions
# where the weight for each gender being its proportion to the national population
#####################################################################################

########## 2.14.1   ##########
rm(list = ls())
library('freqparcoord')
data("prgeng")
prgeng$age2 <- prgeng$age^2
edu <- prgeng$educ
prgeng$ms <- as.integer(edu == 14)
prgeng$phd <- as.integer(edu==16)
prgeng$fem <- prgeng$sex - 1
tmp <- prgeng[edu>=13,]
pe <- tmp[,c(1,12,9,13,14,15,8)]
pe$msfem <- pe$ms * pe$fem
pe$phdfem <- pe$phd * pe$fem

model = lm(wageinc ~., data = pe)
model_sum = summary(model)
model_sum

# calculate confidence interval for beta6 & beta7
beta6 <- model_sum$coefficients['fem',]
beta7 <- model_sum$coefficients['msfem',]
t95 <- qt(0.975, nrow(pe)-1)
beta6_h <- beta6[1] + t95*beta6[2]
beta6_l <- beta6[1] - t95*beta6[2]
beta7_h <- beta7[1] + t95*beta7[2]
beta7_l <- beta7[1] - t95*beta7[2]

sprintf('The 95 percent confidence interval for beta6 is %f and %f', beta6_l, beta6_h)
sprintf('The 95 percent confidence interval for beta6+beta7 is %f and %f', beta6_l +beta7_l, beta6_h + beta7_h)

#####################################################################################
########## 2.14.2   ##########
rm(list = ls())
day <- read.csv('day.csv')
day$temp2 <- day$temp^2
day$clearday <- as.integer(day$weathersit == 1)
bike <- lm(registered ~ temp + temp2 + workingday + clearday + yr, data = day)
bike_summ <- summary(bike)
t_value <- qt(0.975, nrow(day)-1)
yr <- bike_summ$coefficients['yr',]
yr_l <- yr[1] - t_value * yr[2]
yr_h <- yr[1] + t_value * yr[2]

sprintf('The 95 percent confidence interval for beta6 is %f and %f', yr_l, yr_h)















