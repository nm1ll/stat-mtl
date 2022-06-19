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
#args = commandArgs(trailingOnly=TRUE)
#inputs <- as.numeric(unlist(strsplit(args," ")))

lambda <- 0.1
#task <- inputs[2]
cost <- 100
gamma <- 0.1

gev <- 52.6045457638946

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

# "NOT IN", a set negation function
`%ni%` <- Negate(`%in%`)

###################################
###   DATA PREPARATION PART
###################################

# Folder to the data
setwd("C:/Users/nmil/Dropbox/_UNM_/RESEARCH/DATASETS/regression - Schools/no_splits_meanreg/")

# The data transformed with MTL kernel, with a chosen lambda:
filename <- paste(c("data_lambda_",toString(lambda),".RData"), collapse="")
load(filename)
data <- df_output$meanreg_lambda[[toString(lambda)]]


X_full <- subset(data, select=-c(score,school))
y_full <- data$score


# Read the DEV values for all tasks
setwd("C:/Users/nmil/Dropbox/_UNM_/RESEARCH/DATASETS/regression - Schools/")
filename <- "results_1_diag.csv"
devs <- read.csv(filename, header=TRUE)

devs$ev_dev_std <- scale(devs$ev_dev)

devs$ev_d <- gev - devs$ev_dev


kernel_choices <- c("gaussian", "epanechnikov", "rectangular",
                   "triangular", "biweight",
                   "cosine", "optcosine")

rej_list <- list()

sign_level <- 0.10

#k <- "gaussian"

for(k in kernel_choices){
  # Fit kernel density estimator
  d_dens <- density(x = devs$ev_d, bw = "nrd0", kernel = k)
  
  # Finding the 5% percentile (closest value to the right)
  cutoff_index = tail(which(cumsum(d_dens$y/sum(d_dens$y)) <= sign_level), n=1)+1
  cutoff <- d_dens$x[cutoff_index]
  
  # 1 is task is in the rejection region, 0 otherwise
  tasks_status <- as.numeric(devs$ev_d < cutoff)
  
  # Numbers of tasks in the rejection region
  tasks_rej <- which(tasks_status == 1)
  
  rej_list[[k]] <- tasks_rej
}



## Plot section
d_dens <- density(x = devs$ev_d, bw = "nrd0", kernel = "gaussian")
plot(d_dens, main="", xlab = "")

polygon(d_dens, col = "lightgray")
polygon(c(d_dens$x[d_dens$x <= cutoff], cutoff), c(d_dens$y[d_dens$x <= cutoff], 0),
        col = rgb(1, 0, 0, alpha = 0.5), border = "red", main = "")
abline(v=mean(devs$ev_d), col="blue", lty=2)

# NO EDITING FROM HERE
# NO EDITING FROM HERE
# NO EDITING FROM HERE
# NO EDITING FROM HERE
# NO EDITING FROM HERE
# BUT, NEED TO SELECT THE NOT-DELETED TASKS, TRAIN THE MODEL ON THEM, AND MEASURE EV ON ALL TASKS (PREDICT ON FULL X)

# Select the tasks which are NOT deleted

k <- "gaussian"
tasks_rej <- rej_list[[k]]



X_dev <- data[data$school %ni% tasks_rej,]
X_dev <- subset(X_dev, select=-c(score,school))
y_dev <- data[data$school %ni% tasks_rej,]$score

model = svm(y=y_dev, x=X_dev, type="eps-regression", kernel="radial", cost=cost, gamma=gamma)  
y_full_pred <- predict(model, X_full)
ev_dev <- explained_var_1(y_full, y_full_pred)

ev_df <- data.frame(task=task, lambda=lambda, cost=cost, gamma=gamma, ev_dev=ev_dev)




setwd("/projects/gzhang2016227/nikolay/thesis-devtest-full-tdist/")
filename <- paste("results_1_diag.csv", sep="")  
write.table(ev_df, file=filename, append=TRUE, quote=FALSE, sep=",", row.names=FALSE, col.names=!file.exists(filename))



