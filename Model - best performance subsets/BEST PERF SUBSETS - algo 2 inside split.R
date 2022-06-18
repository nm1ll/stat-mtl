

# In this file:

# As described in report 8, section "Best performance subsets combinations"

# EVERYTHING IS DONE BY SPLIT

# DELETED RANDOM FOREST

##################


library(e1071)
library(randomForest)
library(emdbook)
library(plyr)
library(dplyr)
library(plotly)
library(ggplot2)
library(reshape2)
library(ggpubr)

#options(warning.length = 8170L)
options(warning.length = 100L)
options(scipen=999)
memory.limit(150000)


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



# Folder to the data
setwd("C:/Users/nmil/Dropbox/_UNM_/RESEARCH/DATASETS/regression - Schools")



######################################
######################################
######################################
######################################
#####  CODE  #####
## Algorithm:
# For each task, select all tasks which explain it best, within maximum at distance TAU.
# Train single-task learning model on these tasks, and predict for this task ONLY.
# Repeat for all tasks!
# Later, tune TAU to get the best result. Values = (5,10,15,20)


# Define number of splits to load
# From 1 to 10
splits <- 10

# Parameter tau from the report
tau <- 20

expvar_svr <- data.frame()
results_tau <- data.frame()

for (split in 1:splits){
  start_time <- Sys.time()
  cat('#######################\n')
  cat('Split number',split,'\n')
  
  cat('Loading the data\n')
  filename <- paste(c("data_split_raw_",toString(split),".RData"), collapse="")
  load(filename)
  data_split_raw <- df_output[['raw']]
  
  d <- ncol(data_split_raw$train)-2
  n <- length(unique(data_split_raw$train$school))
  
  
  cat('#######################\n')
  cat('Cross validation by task\n')
  data_train_raw <- data_split_raw[['train']]
  data_test_raw <- data_split_raw[['test']]
  
  cv_svr_it <- data.frame()
  
  # Loop through tasks for cross validation in each individual school
  for (i in unique(data_train_raw$school)){
    data_train_school_raw <- data_train_raw[data_train_raw$school == i, ]
    data_test_school_raw <- data_test_raw[data_test_raw$school == i, ]
    
    # 'SVR Independent task learning case
    tc <- tune.control(cross = 5)
    tunesvm_it = tune.svm(y=data_train_school_raw[,1], x=data_train_school_raw[,-c(1,2)], 
                          type="eps-regression", kernel="linear", cost=seq(from=0.1, to=10,length.out=30),
                          tunecontrol = tc)
    cv_svr_it <- rbind(cv_svr_it,data.frame(task=i, C=tunesvm_it$best.parameters))
    
  }
  
  
  cat('#######################\n')
  cat('Diagnostic step\n')
  
  
  ev_matrix_svr <- data.frame()
  
  
  # Loop through tasks for training and predictions for each combination of schools
  for (i in unique(data_train_raw$school)){
    data_train_school_raw <- data_train_raw[data_train_raw$school == i, ]
    
    # 'SVR Independent task case
    cost_it <- cv_svr_it[cv_svr_it$task==i,'cost']
    modelsvm_it = svm(y=data_train_school_raw[,1], x=data_train_school_raw[,-c(1,2)], type="eps-regression", kernel="linear", cost=cost_it)
    
    for (j in unique(data_train_raw$school)){
      
      data_train_school_raw <- data_train_raw[data_train_raw$school == j, ]
      
      y_train_school_pred_svr_it = predict(modelsvm_it, data_train_school_raw[,-c(1,2)])
      ev_svr_it_train <- explained_var_1(data_train_school_raw[,1], y_train_school_pred_svr_it)
      
      # Recording the results of explained variance:
      
      result_svr <- data.frame(split=split, school_train=i, school_test=j, ev_svr_it_train=ev_svr_it_train)
      ev_matrix_svr <- rbind(ev_matrix_svr, result_svr)
    }
  }
  
  # Cast into matrix
  ev_svr <- acast(ev_matrix_svr, school_train~school_test, value.var="ev_svr_it_train")
  
  # Create clusters of tasks according to tau
  clusters_svr = list()
  for (i in 1:139){
    evs_diag <- subset(ev_matrix_svr, school_test==i)$ev_svr_it_train
    ev_max <- max(evs_diag)
    clust <- which(evs_diag > (ev_max-tau))
    clusters_svr[i] <- list(clust)
  }
  
  
  cat('#######################\n')
  cat('Training and testing step\n')
  
  
  y_svr <- data.frame()
  
  for (i in 1:139){
    
    # SVR - select data
    clust_svr <- unlist(clusters_svr[i])
    data_train_cluster_svr <- data_train_raw[data_train_raw$school %in% clust_svr,]
    
    data_test_cluster <- data_test_raw[data_test_raw$school == i,]
    
    # SVR: 5-fold cross validation for SVR
    tc <- tune.control(cross = 5)
    tunesvm_svr = tune.svm(y=data_train_cluster_svr[,1], x=data_train_cluster_svr[,-c(1,2)], 
                               type="eps-regression", kernel="linear", cost=seq(from=0.1, to=10,length.out=8),
                               tunecontrol = tc)
    
    cost_cv <- tunesvm_svr$best.parameters$cost
    
    
    # SVR training and predicting
    modelsvm_cluster = svm(y=data_train_cluster_svr[,1], x=data_train_cluster_svr[,-c(1,2)], type="eps-regression", kernel="linear", cost=cost_cv)
    y_pred_test_svr = predict(modelsvm_cluster, data_test_cluster[,-c(1,2)])
    y_pred_test_svr <- data.frame(y_test = data_test_cluster[,c(1)],
                                  y_test_pred = y_pred_test_svr)
    
    y_svr <- rbind(y_svr, y_pred_test_svr)
    
    
  }
  
  # Measure and record explained variance
  
  ev_svr_final <- explained_var_1(y_svr$y_test, y_svr$y_test_pred)
  
  expvar_svr <- rbind(expvar_svr,data.frame(split=split, ev=ev_svr_final))
  
  end_time <- Sys.time()
  cat('Time taken for this split:\n')
  print(end_time - start_time)

}

results_tau <- rbind(results_tau, data.frame(tau=tau,
                                             ev_svr_mod=mean(expvar_svr$ev)))







