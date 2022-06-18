

# In this file:

# As described in report 8, section "Best performance subsets combinations"

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


# COUNT THE TIME
ptm <- proc.time()


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


create_BLs_all_lambda <- function(n, d, lambdas){
  BLs_all <- list()
  for (lambda in lambdas){
    BLs_all[[toString(lambda)]] <- create_BLs_lambda(n, d, lambda)
  }
  return(BLs_all)
}



transform_data <- function(data, n, d, lambdas){
  
  # This function creates datasets for training and testing.
  # For each lambda, datasets will be different.
  # The train-test split is done before transformation,
  # so that the split is the same for every lambda.
  # n - number of tasks
  # d - number of variables
  
  BLs_all_lambda <- create_BLs_all_lambda(n, d, lambdas)
  
  # Train-test split without regards to task:
  #sample = sample.split(data$score, SplitRatio = .75)
  #data_train = subset(data, sample == TRUE)
  #data_test = subset(data, sample == FALSE)
  
  
  # Train-test split with a constant ration in each task:
  data_train <- data[['train']]
  data_test <- data[['test']]
  
  
  
  dataset_train <- list()
  dataset_test <- list()
  
  for (lambda in lambdas){
    BLs <- BLs_all_lambda[[toString(lambda)]]
    df_train <- data.frame()
    df_test <- data.frame()
    
    # The following loop goes through all tasks, selecting data from that task and
    # multiplying with the matrix BL for that task.
    # These products are combined into full datasets.
    for (l in 1:n){
      BL <- BLs[[l]]
      
      # BX product for training set
      scores <- data_train[data_train$school == l,'score']
      school <- data_train[data_train$school == l,'school']
      obs_train <- data.matrix(data_train[data_train$school == l,3:ncol(data_train)])
      BL_X_train <- t(BL %*% t(obs_train))
      transformed_train <- cbind(scores, school, BL_X_train)
      df_train <- rbind(df_train,transformed_train)
      
      #  BX product for test set
      scores <- data_test[data_test$school == l,'score']
      school <- data_test[data_test$school == l,'school']
      obs_test <- data.matrix(data_test[data_test$school == l,3:ncol(data_train)])
      BL_X_test <- t(BL %*% t(obs_test))
      transformed_test <- cbind(scores, school, BL_X_test)
      df_test <- rbind(df_test,transformed_test)
    }
    
    dataset_train[[toString(lambda)]] <- df_train
    dataset_test[[toString(lambda)]] <- df_test
  }
  dataset_lambda <- list()
  dataset_lambda[['train']] <- dataset_train
  dataset_lambda[['test']] <- dataset_test
  
  return(dataset_lambda)
  
}


###############################


# Define number of splits to load
# From 1 to 10
splits <- 10

# Load "splits" number of precomputed datasets, use raw data:
df_splits_raw <- list()
for (split in 1:splits){
  filename <- paste(c("data_split_",toString(split),".RData"), collapse="")
  load(filename)
  df_splits_raw[[split]] <- df_output[['raw']]
}


# DEFINE LAMBDA HERE:
# lambda <- 0.5


d <- ncol(df_splits_raw[[1]]$train)-2
n <- length(unique(df_splits_raw[[1]]$train$school))

# df_splits_transformed <- list()
# for (split in 1:splits){
#   data <- df_splits_raw[[split]]
#   df_out <- transform_data(data, n, d, lambda)
#   df_splits_transformed[[split]] <- df_out
# }


# Cost for SVR single task case:

cost_st <- 1.96

######################################
######################################
######################################
######################################
#####  CROSS-VALIDATION BY TASK  #####

cv_svr_it <- data.frame()


for (split in 1:splits){
  cat('#######################\n')
  cat('Cross validation - Split number',split,'\n')
  
  data_split_raw <- df_splits_raw[[split]]
  data_train_raw <- data_split_raw[['train']]
  data_test_raw <- data_split_raw[['test']]
  
  # Loop through tasks for cross validation in each individual school
  for (i in unique(data_train_raw$school)){
    data_train_school_raw <- data_train_raw[data_train_raw$school == i, ]
    data_test_school_raw <- data_test_raw[data_test_raw$school == i, ]
    
    # 'SVR Independent task learning case
    tc <- tune.control(cross = 5)
    tunesvm_it = tune.svm(y=data_train_school_raw[,1], x=data_train_school_raw[,-c(1,2)], 
                          type="eps-regression", kernel="linear", cost=seq(from=0.1, to=10,length.out=30),
                          tunecontrol = tc)
    cv_svr_it <- rbind(cv_svr_it, data.frame(split=split, task=i, C=tunesvm_it$best.parameters))
    
  }
}


# Average the estimates across the splits:

cv_svr_it_avg <- cv_svr_it %>%
  group_by(task) %>%
  summarise_at(vars("cost"), mean)


############################
############################
############################
############################
#####  DIAGNOSTIC STEP #####


ev_matrix_svr <- data.frame()
ev_matrix_rf <- data.frame()
# y_train_school_pred_svr_it_diag <- data.frame()
# y_test_school_pred_svr_it_diag <- data.frame()
# y_train_school_pred_rf_it_diag <- data.frame()
# y_test_school_pred_rf_it_diag <- data.frame()

# Run the training procedure for each split

for (split in 1:splits){
  cat('#######################\n')
  cat('Diagnostic step - Split number',split,'\n')
  
  data_split_raw <- df_splits_raw[[split]]
  data_train_raw <- data_split_raw[['train']]
  data_test_raw <- data_split_raw[['test']]
  
  
  # Loop through tasks for training and predictions for each combination of schools
  for (i in unique(data_train_raw$school)){
    data_train_school_raw <- data_train_raw[data_train_raw$school == i, ]
    data_test_school_raw <- data_test_raw[data_test_raw$school == i, ]
    
    
    # 'SVR Independent task case
    
    cost_it <- cv_svr_it_avg[cv_svr_it_avg$task==i,'cost']
    modelsvm_it = svm(y=data_train_school_raw[,1], x=data_train_school_raw[,-c(1,2)], type="eps-regression", kernel="linear", cost=cost_it)
    
    # RF Independent task case
    
    modelrf_it = randomForest(x = data_train_school_raw[,-c(1,2)], y = data_train_school_raw[,1])
    
    
    
    for (j in unique(data_train_raw$school)){
      
      data_train_school_raw <- data_train_raw[data_train_raw$school == j, ]
      data_test_school_raw <- data_test_raw[data_test_raw$school == j, ]
      
      # SVR
      
      y_train_school_pred_svr_it = predict(modelsvm_it, data_train_school_raw[,-c(1,2)])
      ev_svr_it_train <- explained_var_1(data_train_school_raw[,1], y_train_school_pred_svr_it)
      
      y_test_school_pred_svr_it = predict(modelsvm_it, data_test_school_raw[,-c(1,2)])
      ev_svr_it_test <- explained_var_1(data_test_school_raw[,1], y_test_school_pred_svr_it)
      
      # RF
      y_train_school_pred_rf_it = predict(modelrf_it, data_train_school_raw[,-c(1,2)])
      ev_rf_it_train <- explained_var_1(data_train_school_raw[,1], y_train_school_pred_rf_it)
      
      y_test_school_pred_rf_it = predict(modelrf_it, data_test_school_raw[,-c(1,2)])
      ev_rf_it_test <- explained_var_1(data_test_school_raw[,1], y_test_school_pred_rf_it)
      
      # Recording the results of explained variance:
      
      result_svr <- data.frame(split=split, school_train=i, school_test=j, ev_svr_it_train=ev_svr_it_train, ev_svr_it_test=ev_svr_it_test)
      result_rf <- data.frame(split=split, school_train=i, school_test=j, ev_rf_it_train=ev_rf_it_train, ev_rf_it_test=ev_rf_it_test)
      
      ev_matrix_svr <- rbind(ev_matrix_svr, result_svr)
      ev_matrix_rf <- rbind(ev_matrix_rf, result_rf)
      
      # Saving the fitted values from individual tasks
      # WARNING: DRAMATICALLY SLOWS DOWN THE CODE 
      
      # y_train_school_pred_svr_it_diag <- rbind(y_train_school_pred_svr_it_diag,
      #                                          data.frame(split=split, school_train=i, school_test=j,
      #                                               y_train = data_train_school_raw[,1],
      #                                               y_train_school_pred_svr_it = y_train_school_pred_svr_it))
      # y_test_school_pred_svr_it_diag <- rbind(y_test_school_pred_svr_it_diag,
      #                                         data.frame(split=split, school_train=i, school_test=j,
      #                                              y_test=data_test_school_raw[,1],
      #                                              y_test_school_pred_svr_it=y_test_school_pred_svr_it))
      # y_train_school_pred_rf_it_diag <- rbind(y_train_school_pred_rf_it_diag,
      #                                         data.frame(split=split, school_train=i, school_test=j,
      #                                              y_train = data_train_school_raw[,1],
      #                                              y_train_school_pred_rf_it=y_train_school_pred_rf_it))
      # y_test_school_pred_rf_it_diag <- rbind(y_test_school_pred_rf_it_diag,
      #                                        data.frame(split=split, school_train=i, school_test=j,
      #                                             y_test=data_test_school_raw[,1],
      #                                             y_test_school_pred_rf_it=y_test_school_pred_rf_it))

      
    }
  }
}


# Average across splits - SVR
ev_matrix_svr_summary <- ev_matrix_svr %>%
  group_by(school_train, school_test) %>%
  summarise_at(vars("ev_svr_it_train", "ev_svr_it_test"), mean)


# Average across splits - RF
ev_matrix_rf_summary <- ev_matrix_rf %>%
  group_by(school_train, school_test) %>%
  summarise_at(vars("ev_rf_it_train", "ev_rf_it_test"), mean)



# Stop time counting
ptm2 <- proc.time() - ptm
ptm2


# Cast into matrix
ev_svr <- acast(ev_matrix_svr_summary, school_train~school_test, value.var="ev_svr_it_train")
ev_rf <- acast(ev_matrix_rf_summary, school_train~school_test, value.var="ev_rf_it_train")


# SVR: For each task, check which model was the best (should be the one base its own task)
ev_svr_best <- data.frame()
for (i in 1:139){
  j <- which.max(ev_svr[,i])
  ev_svr_best <- rbind(ev_svr_best, data.frame(target_task=i, best_model=j))
}

# SVR: Number of mismatches:
sum(ev_svr_best$target_task!=ev_svr_best$best_model)


# RF: For each task, check which model was the best (should be the one base its own task)
ev_rf_best <- data.frame()
for (i in 1:139){
  j <- which.max(ev_rf[,i])
  ev_rf_best <- rbind(ev_rf_best, data.frame(target_task=i, best_model=j))
}
# RF: Number of mismatches:
sum(ev_rf_best$target_task!=ev_rf_best$best_model)


############################################################
############################################################
############################################################
############################################################
# OLD BROKEN METHOD. TOO MANY CLUSTERS AT FIRST, AND MANY TASKS HAVE THEIR OWN CLUSTER AT THE END.
# ###############
# # Clustering tasks
# 
# ## Algorithm:
# # For each task, select itself and all tasks which explain it better than itself,
# # and tasks that yield explained variance lower than its own, maximum at distance ALPHA.
# # Continue this loop until all tasks are clustered.
# # Later, tune ALPHA to get the best result. Values = (1,2,3,4,5,10)
# 
# # SVR
# alpha <- 10
# indices <- 1:139
# clusters_svr = list()
# 
# j = 0
# while (length(indices) != 0){
#   i <- indices[1]
#   j = j+1
#   ev_task <- subset(ev_matrix_svr_summary, school_test==i)$ev_svr_it_train
#   ev_diag <- subset(ev_matrix_svr_summary, school_train==i & school_test==i)$ev_svr_it_train
#   clust <- which(ev_task > (ev_diag-alpha))
#   clust <- intersect(clust, indices)
#   clusters_svr[j] <- list(clust)
#   indices <- setdiff(indices, clust)
# }
############################################################
############################################################
############################################################
############################################################

###############
# Clustering tasks

## Algorithm:
# For each task, select all tasks which explain it best, within maximum at distance TAU.
# Train single-task learning model on these tasks, and predict for this task ONLY.
# Repeat for all tasks!
# Later, tune TAU to get the best result. Values = (5,10,15,20)


tau <- 10

results_tau <- data.frame()

# SVR
clusters_svr = list()

for (i in 1:139){
  evs_diag <- subset(ev_matrix_svr_summary, school_test==i)$ev_svr_it_train
  ev_max <- max(evs_diag)
  clust <- which(evs_diag > (ev_max-tau))
  clusters_svr[i] <- list(clust)
}


# RF
clusters_rf = list()

for (i in 1:139){
  evs_diag <- subset(ev_matrix_rf_summary, school_test==i)$ev_rf_it_train
  ev_max <- max(evs_diag)
  clust <- which(evs_diag > (ev_max-tau))
  clusters_rf[i] <- list(clust)
}


expvar_svr <- data.frame()
expvar_rf <- data.frame()

for (split in 1:splits){
  
  data_split_raw <- df_splits_raw[[split]]
  data_train_raw <- data_split_raw[['train']]
  data_test_raw <- data_split_raw[['test']]
  
  y_svr <- data.frame()
  y_rf <- data.frame()
  
  for (i in 1:139){
    
    # SVR - select data
    clust_svr <- unlist(clusters_svr[i])
    data_train_cluster_svr <- data_train_raw[data_train_raw$school %in% clust_svr,]
    
    # RF - select data
    clust_rf <- unlist(clusters_rf[i])
    data_train_cluster_rf <- data_train_raw[data_train_raw$school %in% clust_rf,]
    
    
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
    
    
    # RF training and predicting
    
    modelrf_cluster = randomForest(x=data_train_cluster_rf[,-c(1,2)],y=data_train_cluster_rf[,1], ntree = 1000)
    y_pred_test_rf = predict(modelrf_cluster, data_test_cluster[,-c(1,2)])
    y_pred_test_rf <- data.frame(y_test = data_test_cluster[,c(1)],
                                  y_test_pred = y_pred_test_rf)
    
    y_rf <- rbind(y_rf, y_pred_test_rf)
    
  }
  
  # Measure and record explained variance
  
  ev_svr_final <- explained_var_1(y_svr$y_test, y_svr$y_test_pred)
  ev_rf_final <- explained_var_1(y_rf$y_test, y_rf$y_test_pred)
  
  expvar_svr <- rbind(expvar_svr,data.frame(split=split, ev=ev_svr_final))
  expvar_rf <- rbind(expvar_rf,data.frame(split=split, ev=ev_rf_final))
  
}

results_tau <- rbind(results_tau, data.frame(tau=tau,
                                                 ev_svr_mod=mean(expvar_svr$ev),
                                                 ev_rf_mod=mean(expvar_rf$ev)))



#####################################
######### REMEDIAL MEASURES #########

###
# Combinations of SVR single task learning and RF single task learning

expvar_svr_good_rf_bad <- data.frame()
expvar_rf_good_svr_bad <- data.frame()


for (split in 1:splits){
  data_split_raw <- df_splits_raw[[split]]
  data_train_raw <- data_split_raw[['train']]
  data_test_raw <- data_split_raw[['test']]
  
  data_train_good_cluster <- data_train_raw[data_train_raw$school %in% weighted_ev_good_cluster$task,]
  data_train_bad_cluster <- data_train_raw[data_train_raw$school %in% weighted_ev_bad_cluster$task,]
  data_test_good_cluster <- data_test_raw[data_test_raw$school %in% weighted_ev_good_cluster$task,]
  data_test_bad_cluster <- data_test_raw[data_test_raw$school %in% weighted_ev_bad_cluster$task,]
  
  # # 5-fold cross validation for SVR on good and bad clusters
  # tc <- tune.control(cross = 5)
  # tunesvm_st_good = tune.svm(y=data_train_good_cluster[,1], x=data_train_good_cluster[,-c(1,2)], 
  #                       type="eps-regression", kernel="linear", cost=seq(from=0.1, to=10,length.out=30),
  #                       tunecontrol = tc)
  # 
  # tc <- tune.control(cross = 5)
  # tunesvm_st_bad = tune.svm(y=data_train_good_cluster[,1], x=data_train_good_cluster[,-c(1,2)], 
  #                       type="eps-regression", kernel="linear", cost=seq(from=0.1, to=10,length.out=30),
  #                       tunecontrol = tc)
  
  # Set cost for good and bad clusters
  
  cost_good <- 1.96
  cost_bad <- 1.96
  
  
  modelsvm_st_good = svm(y=data_train_good_cluster[,1], x=data_train_good_cluster[,-c(1,2)], type="eps-regression", kernel="linear", cost=cost_good)
  modelrf_st_good = randomForest(x = data_train_good_cluster[,-c(1,2)], y = data_train_good_cluster[,1], ntree = 1000)
  modelsvm_st_bad = svm(y=data_train_bad_cluster[,1], x=data_train_bad_cluster[,-c(1,2)], type="eps-regression", kernel="linear", cost=cost_bad)
  modelrf_st_bad = randomForest(x = data_train_bad_cluster[,-c(1,2)], y = data_train_bad_cluster[,1], ntree = 1000)
  
  
  y_pred_test_good_svr = predict(modelsvm_st_good, data_test_good_cluster[,-c(1,2)])
  y_pred_test_good_rf = predict(modelrf_st_good, data_test_good_cluster[,-c(1,2)])
  y_pred_test_bad_svr = predict(modelsvm_st_bad, data_test_bad_cluster[,-c(1,2)])
  y_pred_test_bad_rf = predict(modelrf_st_bad, data_test_bad_cluster[,-c(1,2)])
  
  # Predictions combinations
  
  y_test <- c(data_test_good_cluster[,1], data_test_bad_cluster[,1])
  rf_good_svr_bad <- c(y_pred_test_good_rf, y_pred_test_bad_svr)
  svr_good_rf_bad <- c(y_pred_test_good_svr, y_pred_test_bad_rf)
  
  ev_rf_good_svr_bad <- explained_var_1(y_test, rf_good_svr_bad)
  ev_svr_good_rf_bad <- explained_var_1(y_test, svr_good_rf_bad)
  
  expvar_rf_good_svr_bad <- rbind(expvar_rf_good_svr_bad,data.frame(split=split, ev=ev_rf_good_svr_bad))
  expvar_svr_good_rf_bad <- rbind(expvar_svr_good_rf_bad,data.frame(split=split, ev=ev_svr_good_rf_bad))
}

mean(expvar_rf_good_svr_bad$ev)
mean(expvar_svr_good_rf_bad$ev)




# Average across splits
weighted_ev_1_summary <- mod_ev_1 %>%
  group_by(task) %>%
  summarise_at(vars("ev"), mean)

# Result
mean(weighted_ev_1_summary$ev)
















######################################
### EXPLAINED VARIANCE DIAGNOSTICS ###



# Average explained variance for each model (based on the training set) and task across the splits:
ev_matrix_svr_avg <- ev_matrix_svr %>%
  group_by(school_train, school_test) %>%
  summarise_at(vars("ev_svr_it_train", "ev_svr_it_test"), mean)


ev_matrix_rf_avg <- ev_matrix_rf %>%
  group_by(school_train, school_test) %>%
  summarise_at(vars("ev_rf_it_train", "ev_rf_it_test"), mean)


# Heatmaps

ev_matrix_svr_avg_mod <- ev_matrix_svr_avg
ev_matrix_svr_avg_mod[ev_matrix_svr_avg_mod$ev_svr_it_train < 0,'ev_svr_it_train'] <- 
  replace(ev_matrix_svr_avg_mod[ev_matrix_svr_avg_mod$ev_svr_it_train < 0,'ev_svr_it_train'], 
          ev_matrix_svr_avg_mod[ev_matrix_svr_avg_mod$ev_svr_it_train < 0,'ev_svr_it_train']<0,
          NA)

ev_matrix_svr_avg_mod[ev_matrix_svr_avg_mod$ev_svr_it_test < 0,'ev_svr_it_test'] <- 
  replace(ev_matrix_svr_avg_mod[ev_matrix_svr_avg_mod$ev_svr_it_test < 0,'ev_svr_it_test'], 
          ev_matrix_svr_avg_mod[ev_matrix_svr_avg_mod$ev_svr_it_test < 0,'ev_svr_it_test']<0,
          NA)


ev_matrix_rf_avg_mod <- ev_matrix_rf_avg
ev_matrix_rf_avg_mod[ev_matrix_rf_avg_mod$ev_rf_it_train < 0,'ev_rf_it_train'] <- 
  replace(ev_matrix_rf_avg_mod[ev_matrix_rf_avg_mod$ev_rf_it_train < 0,'ev_rf_it_train'], 
          ev_matrix_rf_avg_mod[ev_matrix_rf_avg_mod$ev_rf_it_train < 0,'ev_rf_it_train']<0,
          NA)

ev_matrix_rf_avg_mod[ev_matrix_rf_avg_mod$ev_rf_it_test < 0,'ev_rf_it_test'] <- 
  replace(ev_matrix_rf_avg_mod[ev_matrix_rf_avg_mod$ev_rf_it_test < 0,'ev_rf_it_test'], 
          ev_matrix_rf_avg_mod[ev_matrix_rf_avg_mod$ev_rf_it_test < 0,'ev_rf_it_test']<0,
          NA)




p_svr_train <- ggplot(ev_matrix_svr_avg_mod,aes(y=school_test,x=school_train,fill=ev_svr_it_train))+
  geom_tile(colour="white",size=0.25)+
  theme_grey(base_size=8)+
  theme(
    #bold font for legend text
    legend.text=element_text(face="bold"),
    #set thickness of axis ticks
    axis.ticks=element_line(size=0.4),
    #remove plot background
    plot.background=element_blank(),
    #remove plot border
    panel.border=element_blank())+
  scale_fill_continuous(na.value="black",
                        low = "royalblue4", high = "white",
                        breaks=seq(0,72,10),
                        limits = c(0, 72))+
  scale_y_discrete(name = "Task used for prediction - training set")+
  scale_x_discrete(name = "Task used for training - training set")+
  ggtitle('SVR - Individual task models predictive heatmap', subtitle = waiver())+
  labs(fill = "Explained variance")+
  geom_tile()



p_svr_test <- ggplot(ev_matrix_svr_avg_mod,aes(y=school_test,x=school_train,fill=ev_svr_it_test))+
  geom_tile(colour="white",size=0.25)+
  theme_grey(base_size=8)+
  theme(
    #bold font for legend text
    legend.text=element_text(face="bold"),
    #set thickness of axis ticks
    axis.ticks=element_line(size=0.4),
    #remove plot background
    plot.background=element_blank(),
    #remove plot border
    panel.border=element_blank())+
  scale_fill_continuous(na.value="black",
                        low = "royalblue4", high = "white",
                        breaks=seq(0,72,10),
                        limits = c(0, 72))+
  scale_y_discrete(name = "Task used for prediction - test set")+
  scale_x_discrete(name = "Task used for training - training set")+
  ggtitle('SVR - Individual task models predictive heatmap', subtitle = waiver())+
  labs(fill = "Explained variance")+
  geom_tile()



p_rf_train <- ggplot(ev_matrix_rf_avg_mod,aes(y=school_test,x=school_train,fill=ev_rf_it_train))+
  geom_tile(colour="white",size=0.25)+
  theme_grey(base_size=8)+
  theme(
    #bold font for legend text
    legend.text=element_text(face="bold"),
    #set thickness of axis ticks
    axis.ticks=element_line(size=0.4),
    #remove plot background
    plot.background=element_blank(),
    #remove plot border
    panel.border=element_blank())+
  scale_fill_continuous(na.value="black",
                        low = "royalblue4", high = "white",
                        breaks=seq(0,72,10),
                        limits = c(0, 72))+
  scale_y_discrete(name = "Task used for prediction - training set")+
  scale_x_discrete(name = "Task used for training - training set")+
  ggtitle('Random forest - Individual task models predictive heatmap', subtitle = waiver())+
  labs(fill = "Explained variance")+
  geom_tile()


p_rf_test <- ggplot(ev_matrix_rf_avg_mod,aes(y=school_test,x=school_train,fill=ev_rf_it_test))+
  geom_tile(colour="white",size=0.25)+
  theme_grey(base_size=8)+
  theme(
    #bold font for legend text
    legend.text=element_text(face="bold"),
    #set thickness of axis ticks
    axis.ticks=element_line(size=0.4),
    #remove plot background
    plot.background=element_blank(),
    #remove plot border
    panel.border=element_blank())+
  scale_fill_continuous(na.value="black",
                        low = "royalblue4", high = "white",
                        breaks=seq(0,72,10),
                        limits = c(0, 72))+
  scale_y_discrete(name = "Task used for prediction - test set")+
  scale_x_discrete(name = "Task used for training - training set")+
  ggtitle('Random forest - Individual task models predictive heatmap', subtitle = waiver())+
  labs(fill = "Explained variance")+
  geom_tile()




ggarrange(p_svr_train, p_svr_test, p_rf_train, p_rf_test,
          ncol = 2, nrow = 2)
  





