suppressMessages(library(e1071))
suppressMessages(library(randomForest))
suppressMessages(library(emdbook))
suppressMessages(library(plyr))
suppressMessages(library(dplyr))
suppressMessages(library(plotly))
suppressMessages(library(ggplot2))
suppressMessages(library(gridExtra))
suppressMessages(library(reshape2))
suppressMessages(library(nortest))
suppressMessages(library(lmtest))
suppressMessages(library(car))

options(warning.length = 100L)
options(scipen=999)
options(warn=-1)


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

setwd("C:/Users/nmil/Dropbox/_UNM_/RESEARCH/DATASETS/regression - Schools/")


filename <- ("data_split_raw_corr_1.RData")
load(filename)

data <- rbind(df_split$train,df_split$test)

X <- subset(data, select=-c(score,school))
y <- data$score

df_orig <- cbind(score=y, X)



ev_df <- data.frame()
resids_list <- list()
resid_stats <- data.frame()

for(task in 1:139){
  X_task <- data[data$school == task,]
  X_task <- subset(X_task, select=-c(score,school))
  y_task <- data[data$school == task,'score']
  df <- cbind(score=y_task, X_task)
  
  model = lm(score~. , data=df)
  
  y_pred <- predict(model, X_task)
  resids <- y_task - y_pred
  
  
  
  resids_list[[task]] <- data.frame(res=resids, fitted=y_pred)
  
  mse <- mean((y_task - y_pred)^2)
  #mse <- mean(y_task / y_pred)
    
  resid_max <- max(abs(resids_list[[task]]$res))
  
  resid_stat <- resid_max / sqrt(mse)
  
  resid_stats <- rbind(resid_stats, data.frame(task=task, resid_stat=resid_stat, resid_max=resid_max, mse=mse))
  
  # Deleted explained variance
  ev_task <- explained_var_1(y, unname(y_pred))
  ev_df <- rbind(ev_df, data.frame(task=task, ev_task=ev_task))
  
  
  
}



ggplot(resid_stats, aes(x=resid_stat)) +
  geom_density(fill="gray")+
  labs(x="Task max absolute residual / square root of task MSE", y = "Density")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))





