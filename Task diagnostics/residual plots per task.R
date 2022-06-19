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

for(task in 1:9){
  X_task <- data[data$school == task,]
  X_task <- subset(X_task, select=-c(score,school))
  y_task <- data[data$school == task,'score']
  df <- cbind(score=y_task, X_task)
  
  model = lm(score~. , data=df)
  
  y_pred <- predict(model, X_task)
  resids <- y_task - y_pred
  
  
  resids_list[[task]] <- data.frame(res=resids, fitted=y_pred)
  
  # Deleted explained variance
  ev_task <- explained_var_1(y, unname(y_pred))
  ev_df <- rbind(ev_df, data.frame(task=task, ev_task=ev_task))
  
  
  
}

idx <- 1
plot_df <- data.frame(count=1:length(resids_list[[idx]]), res=resids_list[[idx]])



# For tasks
idx <- 1
t1 <- ggplot(resids_list[[idx]], aes(y=res, x=fitted))+
  geom_point()+
  labs(y="Residual", x = "Fitted value")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept=0)+
  scale_x_continuous(limits = c(-1, 51))+
  scale_y_continuous(limits = c(-25, 35))

idx <- 2
t2 <- ggplot(resids_list[[idx]], aes(y=res, x=fitted))+
  geom_point()+
  labs(y="Residual", x = "Fitted value")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept=0)+
  scale_x_continuous(limits = c(-1, 51))+
  scale_y_continuous(limits = c(-25, 35))


idx <- 3
t3 <- ggplot(resids_list[[idx]], aes(y=res, x=fitted))+
  geom_point()+
  labs(y="Residual", x = "Fitted value")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept=0)+
  scale_x_continuous(limits = c(-1, 51))+
  scale_y_continuous(limits = c(-25, 35))


idx <- 4
t4 <- ggplot(resids_list[[idx]], aes(y=res, x=fitted))+
  geom_point()+
  labs(y="Residual", x = "Fitted value")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept=0)+
  scale_x_continuous(limits = c(-1, 51))+
  scale_y_continuous(limits = c(-25, 35))


idx <- 5
t5 <- ggplot(resids_list[[idx]], aes(y=res, x=fitted))+
  geom_point()+
  labs(y="Residual", x = "Fitted value")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept=0)+
  scale_x_continuous(limits = c(-1, 51))+
  scale_y_continuous(limits = c(-25, 35))


idx <- 6
t6 <- ggplot(resids_list[[idx]], aes(y=res, x=fitted))+
  geom_point()+
  labs(y="Residual", x = "Fitted value")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept=0)+
  scale_x_continuous(limits = c(-1, 51))+
  scale_y_continuous(limits = c(-25, 35))


idx <- 7
t7 <- ggplot(resids_list[[idx]], aes(y=res, x=fitted))+
  geom_point()+
  labs(y="Residual", x = "Fitted value")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept=0)+
  scale_x_continuous(limits = c(-1, 51))+
  scale_y_continuous(limits = c(-25, 35))

idx <- 8
t8 <- ggplot(resids_list[[idx]], aes(y=res, x=fitted))+
  geom_point()+
  labs(y="Residual", x = "Fitted value")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept=0)+
  scale_x_continuous(limits = c(-1, 51))+
  scale_y_continuous(limits = c(-25, 35))


idx <- 9
t9 <- ggplot(resids_list[[idx]], aes(y=res, x=fitted))+
  geom_point()+
  labs(y="Residual", x = "Fitted value")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept=0)+
  scale_x_continuous(limits = c(-1, 51))+
  scale_y_continuous(limits = c(-25, 35))

grid.arrange(t1,t2,t3,t4,t5,t6,t7,t8,t9, ncol=3)

