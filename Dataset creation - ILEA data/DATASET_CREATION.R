##############################

# This file creates a transformed datasets in RData format

# Input: split number and lambda

# File names are created automatically in the loop

# The train-test split is done  separately within each task

# This file is for ILEA schools data, 
# but can be easily adjusted for other datasets.

# The data are corrected and cleaned first; details below


##############################

library(dummies)
library(reshape2)

# Read the input of the split number
args = commandArgs(trailingOnly=TRUE)
inputs <- as.numeric(unlist(strsplit(args," ")))
split <- inputs[1]
lambda <- inputs[2]

# Set the seed to be the split number
# Lambda is not included in the seed, because we want to keep the split constant across lambdas.

set.seed(split)


# Train-test split for a chosen ratio within each task
train_test_split_mtl <- function(data, splitratio = 0.75){
  data_train <- data.frame()
  data_test <- data.frame()
  for (l in unique(data$school)){
    set <- data[data$school == l,]
    sampled = sample(1:nrow(set), nrow(set)*splitratio)
    set_train = set[sampled,]
    set_test = set[-sampled,]
    data_train <- rbind(data_train, set_train)
    data_test <- rbind(data_test, set_test)
  }
  data_split <- list()
  data_split[['train']] <- data_train
  data_split[['test']] <- data_test
  return(data_split)
}


# Folder to the data
setwd("/projects/gzhang2016227/data-schools-lambda-manysplits/")


df <- read.csv(file = 'ILEA567.csv', header = TRUE)


# One-hot encoding

numeric_features = c('school','fsm','vr1percent','gender')
categorical_features = c('year','vrband','ethnicity','schoolgender','schooldenom')


v1 <- dummy(df$year, sep = "_")
v2 <- dummy(df$vrband, sep = "_")
v3 <- dummy(df$ethnicity, sep = "_")
v4 <- dummy(df$schoolgender, sep = "_")
v5 <- dummy(df$schooldenom, sep = "_")

df_categorical <- cbind(v1,v2,v3,v4,v5)

data <- cbind(df['score'], df[numeric_features], df_categorical)


# str(data)
# 'data.frame':	15362 obs. of  29 variables:
# ...

# 1 column for y, 1 column for school (task), 27 columns for variables



####################
## Data correction

# 1. Find student 5333 and change its gender to male (since 44 is an all-male school)
# simplify the task by putting gender 0 (male) to everyone in school 44

data[data$school == 44,'gender'] <- 0

# 2. For all observations with vrband=0, change vrband to 1 and remove the column vrband_0

data[data$vrband_0 == 1,'vrband_1'] <- 1
data <- subset(data, select = -vrband_0)



###############################
# Data transformation

###############
# LAMBDA PART

create_BL_lambda <- function(l, n, d, lambda){
  BL <- matrix(0, (n+1)*d, d)
  BL[1:d,] <- sqrt(1-lambda)*diag(d)
  BL[(l*d+1):((l+1)*d),] <- sqrt(lambda*n)*diag(d)
  return(BL)
}


create_BLs_lambda <- function(n, d, lambda){
  BLs <- list()
  for (l in 1:n){
    BLs[[l]] <- create_BL_lambda(l, n, d, lambda)
  }
  return(BLs)
}


###############
# TRANSFORM FUNCTION
# CREATES BOTH LAMBDA AND C TRANSFORMS


transform_data <- function(data, n, d, lambda){
  
  # This function creates datasets for training and testing.
  # For each lambda, datasets will be different.
  # The train-test split is done before transformation,
  # so that the split is the same for every lambda.
  # n - number of tasks
  # d - number of variables
  
  
  # Train-test split without regards to task:
  #sample = sample.split(data$score, SplitRatio = .75)
  #data_train = subset(data, sample == TRUE)
  #data_test = subset(data, sample == FALSE)
  
  
  # Train-test split with a constant ration in each task:
  data_split <- train_test_split_mtl(data, splitratio = 0.75)
  data_train <- data_split[['train']]
  data_test <- data_split[['test']]
  
  
  dataset_train <- list()
  dataset_test <- list()
  
  BLs <- create_BLs_lambda(n, d, lambda)
  
  df_train <- data.frame()
  df_test <- data.frame()
  
  # The following loop goes through all tasks, selecting data from that task and
  # multiplying with the matrix BL for that task.
  # These products are combined into full datasets.
  for (l in 1:n){
    BL <- BLs[[l]]
    
    # BX product for training set
    score <- data_train[data_train$school == l,'score']
    obs_train <- data.matrix(data_train[data_train$school == l,3:ncol(data)])
    BL_X_train <- t(BL %*% t(obs_train))
    transformed_train <- cbind(score, school=l, BL_X_train)
    df_train <- rbind(df_train,transformed_train)
    
    #  BX product for test set
    score <- data_test[data_test$school == l,'score']
    obs_test <- data.matrix(data_test[data_test$school == l,3:ncol(data)])
    BL_X_test <- t(BL %*% t(obs_test))
    transformed_test <- cbind(score, school=l, BL_X_test)
    df_test <- rbind(df_test,transformed_test)
  }
  
  dataset_train[[toString(lambda)]] <- df_train
  dataset_test[[toString(lambda)]] <- df_test
  
  dataset_lambda <- list()
  dataset_lambda[['train']] <- dataset_train
  dataset_lambda[['test']] <- dataset_test
  
  dataset_train <- list()
  dataset_test <- list()
  
  df_output <- list()
  df_output[['raw']] <- data_split
  df_output[['meanreg_lambda']] <- dataset_lambda

  return(df_output)
  
}




# Define n - number of tasks, d - number of variables,
# and create the dataset.

n <- 139
d <- ncol(data)-2


df_output <- transform_data(data, n, d, lambda)

setwd("/projects/gzhang2016227/data-schools-lambda-manysplits/data/")
filename <- paste(c("data_lambda_split_",toString(split),"_lambda_",toString(lambda),".RData"), collapse="")
save(df_output, file=filename)


