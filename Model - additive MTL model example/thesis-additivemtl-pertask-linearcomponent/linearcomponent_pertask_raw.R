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

task <- inputs[1]


cost_raw <- 1000
gamma_raw <- 10

cost_adj <- 100
gamma_adj <- 100


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
filename <- paste(c("data_lambda_0.1.RData"), collapse="")
load(filename)
data <- df_output$raw


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


ev_df <- data.frame()


  
X_dev <- data[data$school!=task,]
X_dev <- subset(X_dev, select=-c(score,school))
y_dev <- data[data$school!=task,]$score

# SVR Only
model = svm(y_dev, x=X_dev, type="eps-regression", kernel="radial", cost=cost_raw, gamma=gamma_raw)  
y_pred <- predict(model, X)
ev_svr <- explained_var_1(y, y_pred)


# OLS Only
model = lm(y_dev~.,data=X_dev)
y_pred <- predict(model, X)
ev_lin <- explained_var_1(y, y_pred)




# OLS+SVR

# Adjustments for training (since the model cannot be trained on task "task")
# Choose only those adjustments where "task" is not included
y_pred_dev <- predict(model, X_dev)
y_adjustment_dev <- y_dev - y_pred_dev

model = svm(y=y_adjustment_dev, x=X_dev, type="eps-regression", kernel="radial", cost=cost_adj, gamma=gamma_adj)  
y_adjustment_pred <- predict(model, X)
y_pred <- y_pred + y_adjustment_pred

ev_all <- explained_var_1(y, y_pred)


# Record the results  
ev_df <- rbind(ev_df, data.frame(task=task, ev_all=ev_all, ev_lin=ev_lin, ev_svr=ev_svr))


setwd("/projects/gzhang2016227/nikolay/thesis-additivemtl-pertask-linearcomponent/")
filename <- paste("results_raw.csv", sep="")  
write.table(ev_df, file=filename, append=TRUE, quote=FALSE, sep=",", row.names=FALSE, col.names=!file.exists(filename))






