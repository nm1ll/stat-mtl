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

diffs_full <- c()

mean_diff <- data.frame()

for (split in 1:10){
  
  filename <- paste0("data_split_raw_corr_",split,".RData")
  load(filename)
  
  data <- rbind(df_split$train,df_split$test)
  
  data_train <- df_split$train
  data_test <- df_split$test
  
  X <- subset(data, select=-c(score,school))
  y <- data$score
  
  df_orig <- cbind(score=y, X)
  
  X_train <- subset(data_train, select=-c(score,school))
  y_train <- data_train$score
  
  X_test <- subset(data_test, select=-c(score,school))
  y_test <- data_test$score
  
  
  diffs_full <- c(diffs_full, mean(y_test)-mean(y_train))
  
  
  for(task in 1:139){
    
    X_train_task <- data_train[data_train$school == task,]
    X_train_task <- subset(X_train_task, select=-c(score,school))
    y_train_task <- data_train[data_train$school == task,'score']
    
    X_test_task <- data_test[data_test$school == task,]
    X_test_task <- subset(X_test_task, select=-c(score,school))
    y_test_task <- data_test[data_test$school == task,'score']
    
    mean_diff <- rbind(mean_diff, data.frame(split=split,task=task, test_train_diff=mean(y_test_task - y_train_task)))
    
  }
}



mean_diff_avg  <- mean_diff %>% 
  group_by(task) %>%
  summarise(across(test_train_diff, mean, na.rm = TRUE))


# FOR 1 SPLIT
ggplot(mean_diff[mean_diff$split==1,], aes(x=test_train_diff)) +
  geom_density(fill="gray")+
  labs(x="mean(y_test_task) - mean(y_train_task)", y = "Density")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(limits = c(-10, 10))+
  scale_y_continuous(limits = c(0, 0.55))


# FOR ALL SPLITS
ggplot(mean_diff_avg, aes(x=test_train_diff)) +
  geom_density(fill="gray")+
  labs(x="mean(y_test_task) - mean(y_train_task)", y = "Density")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(limits = c(-10, 10))+
  scale_y_continuous(limits = c(0, 0.55))

#hist(mean_diff$test_train_diff)


# For tasks
t1 <- ggplot(mean_diff[mean_diff$task==1,], aes(x=test_train_diff)) +
  geom_density(fill="gray")+
  labs(x="Difference of test and train mean", y = "Density")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(limits = c(-8, 8))+
  scale_y_continuous(limits = c(0, 0.7))

t2 <- ggplot(mean_diff[mean_diff$task==2,], aes(x=test_train_diff)) +
  geom_density(fill="gray")+
  labs(x="Difference of test and train mean", y = "Density")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(limits = c(-8, 8))+
  scale_y_continuous(limits = c(0, 0.7))


t3 <- ggplot(mean_diff[mean_diff$task==3,], aes(x=test_train_diff)) +
  geom_density(fill="gray")+
  labs(x="Difference of test and train mean", y = "Density")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(limits = c(-8, 8))+
  scale_y_continuous(limits = c(0, 0.7))


t4 <- ggplot(mean_diff[mean_diff$task==4,], aes(x=test_train_diff)) +
  geom_density(fill="gray")+
  labs(x="Difference of test and train mean", y = "Density")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(limits = c(-8, 8))+
  scale_y_continuous(limits = c(0, 0.7))


t5 <- ggplot(mean_diff[mean_diff$task==5,], aes(x=test_train_diff)) +
  geom_density(fill="gray")+
  labs(x="Difference of test and train mean", y = "Density")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(limits = c(-8, 8))+
  scale_y_continuous(limits = c(0, 0.7))


t6 <- ggplot(mean_diff[mean_diff$task==6,], aes(x=test_train_diff)) +
  geom_density(fill="gray")+
  labs(x="Difference of test and train mean", y = "Density")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(limits = c(-8, 8))+
  scale_y_continuous(limits = c(0, 0.7))


t7 <- ggplot(mean_diff[mean_diff$task==7,], aes(x=test_train_diff)) +
  geom_density(fill="gray")+
  labs(x="Difference of test and train mean", y = "Density")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(limits = c(-8, 8))+
  scale_y_continuous(limits = c(0, 0.7))


t8 <- ggplot(mean_diff[mean_diff$task==8,], aes(x=test_train_diff)) +
  geom_density(fill="gray")+
  labs(x="Difference of test and train mean", y = "Density")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(limits = c(-8, 8))+
  scale_y_continuous(limits = c(0, 0.7))


t9 <- ggplot(mean_diff[mean_diff$task==9,], aes(x=test_train_diff)) +
  geom_density(fill="gray")+
  labs(x="Difference of test and train mean", y = "Density")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(limits = c(-8, 8))+
  scale_y_continuous(limits = c(0, 0.7))


grid.arrange(t1,t2,t3,t4,t5,t6,t7,t8,t9, ncol=3)



