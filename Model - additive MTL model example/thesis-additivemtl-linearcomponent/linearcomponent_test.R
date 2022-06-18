suppressMessages(library(e1071))
suppressMessages(library(randomForest))
suppressMessages(library(emdbook))
suppressMessages(library(plyr))
suppressMessages(library(dplyr))
suppressMessages(library(plotly))
suppressMessages(library(ggplot2))
suppressMessages(library(reshape2))

options(warning.length = 100L)
options(scipen=999)

# Read the input from the Rscript call
args = commandArgs(trailingOnly=TRUE)
inputs <- as.numeric(unlist(strsplit(args," ")))

lambda <- inputs[1]

# This is a collection of optimal choices of cost and gamma for Gaussian kernel SVR, cross-validated on the raw response variable (without deleting linear components first)
parameters<-data.frame(lambda=c(0,0.01,0.1),cost=c(90,7500,5500),gamma=c(0.0002,0.0000005,0.0000003))

cost <- parameters[parameters$lambda == lambda, "cost"]
gamma <- parameters[parameters$lambda == lambda, "gamma"]


# Explained variance function, definition 1
explained_var_1 <- function(y_test, y_test_pred) {
  data_variance <- (1/length(y_test)) * sum((y_test-mean(y_test))^2)
  unexplained_var <- (1/length(y_test)) * sum((y_test_pred-y_test)^2)
  explained_var <- data_variance - unexplained_var
  percentage_explained_var <- (explained_var/data_variance)*100
  return(percentage_explained_var)
}


# Combined estimate function
comb_est <- function(y_pred_0, y_pred_1, theta){
  ce <- theta*y_pred_0 + (1-theta)*y_pred_1
  return(ce)
}


###################################
###   DATA PREPARATION PART
###################################

# Folder to the data
setwd("/projects/gzhang2016227/data-schools-nosplits/data/")

# The data transformed with MTL kernel, with a chosen lambda:
filename <- paste(c("data_lambda_",toString(lambda),".RData"), collapse="")
load(filename)
data <- df_output$meanreg_lambda[[toString(lambda)]]


# The raw data only:
#filename <- paste(c("data_split_raw_corr_",toString(split),".RData"), collapse="")
#load(filename)
#data_train <- df_split$train



# Extracting mean, sd and median for each task's training, test and full sets
# NOTE: it is possible to add any other statistics here as you like
#taskstatistics <- data.frame()
#for (i in unique(data$school)){
#  y_task <- data[data$school == i, 'score']
#  taskstatistics <- rbind(taskstatistics, 
#                          data.frame(task=i, 
#                                     mean_task=mean(y_task), sd_task=sd(y_task), 
#                                     median_task=median(y_task)))
#}



# Creating a per-task standardized response variable in training set, called "score_std"
#for (i in unique(data$school)){
#  y_task <- data[data$school == i, 'score']
#  data[data$school == i, 'score_std'] <- (y_task - mean(y_task))/sd(y_task)
#}

# NOTE that now the training set has 1 more column than 
# the test set since we've added standardized response variable.

# removing the school column as it is not a predictor
#X <- subset(data, select=-c(score,school,score_std))
#y <- data$score
#y_std <- data$score_std
  
  
X <- subset(data, select=-c(score,school))
y <- data$score


###################################
###        MODELING PART
###################################

# # # # #
# CASE 1 ::: OLS vs additive(OLS+SVM) 
# # # # #

ev_df <- data.frame()
y_adjustment <- y
y_pred <- c(0)

loops <- 2

for (loop in 1:loops){
  # In this loop, the adjustment is initialized as y, and is modeled and updated continuously
  
  #######################################
  # Linear predictions explained variance
  
  model_linear <- lm(y_adjustment~as.matrix(X))  
  y_adjustment <- predict(model_linear, as.data.frame(X))  
  
  y_pred <- y_pred + unname(y_adjustment)
  ev_linear <- explained_var_1(y, y_pred)

  # Get ready for the next step
  
  y_adjustment <- y - y_pred
  
  #######################################
  # Additive MTL predictions explained variance
  
  model = svm(y=y_adjustment, x=X, type="eps-regression", kernel="radial", cost=cost, gamma=gamma)  
  y_adjustment <- predict(model, X)
  
  # Only the fitted values from the single-task model are recorded, in order to be used for combined estimate later
  y_pred <- y_pred + y_adjustment
  ev_additive <- explained_var_1(y, y_pred)

  # Record the results  
  ev_df <- rbind(ev_df, data.frame(experiment="ols", lambda=lambda, loop=loop, ev_linear=ev_linear, ev_additive = ev_additive))
  
  # Get ready for the next step
  y_adjustment <- y - y_pred

}


setwd("/projects/gzhang2016227/nikolay/thesis-additivemtl-linearcomponent/")
filename <- paste("results.csv", sep="")  
write.table(ev_df, file=filename, append=TRUE, quote=FALSE, sep=",", row.names=FALSE, col.names=!file.exists(filename))






