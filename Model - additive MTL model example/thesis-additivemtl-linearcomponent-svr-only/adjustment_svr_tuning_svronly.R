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
loop <- inputs[2]
cost <- inputs[3]
gamma <- inputs[4]



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

X <- subset(data, select=-c(score,school))
y <- data$score

model = svm(y=y, x=X, type="eps-regression", kernel="radial", cost=cost, gamma=gamma)  
y_pred <- predict(model, X)

  
ev_svr <- explained_var_1(y, y_pred)

ev_df <- data.frame(lambda=lambda, cost=cost, gamma=gamma, ev_svr=ev_svr)

setwd("/projects/gzhang2016227/nikolay/thesis-additivemtl-linearcomponent-svr-only/")
filename <- paste("results.csv", sep="")  
write.table(ev_df, file=filename, append=TRUE, quote=FALSE, sep=",", row.names=FALSE, col.names=!file.exists(filename))



