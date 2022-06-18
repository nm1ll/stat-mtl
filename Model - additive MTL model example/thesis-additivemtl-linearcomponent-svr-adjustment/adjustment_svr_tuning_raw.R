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

loop <- inputs[1]
cost <- inputs[2]
gamma <- inputs[3]



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

X <- subset(data, select=-c(score,school))
y <- data$score


# Load the adjustment to tune
setwd("/projects/gzhang2016227/nikolay/thesis-additivemtl-linearcomponent-svr-adjustment/")
filename <- paste(c("adjustment_raw_loop",toString(loop),".RData"), collapse="")
load(filename)

y_adjustment <- adjustment$y_adjustment
y_pred <- adjustment$y_pred


model = svm(y=y_adjustment, x=X, type="eps-regression", kernel="radial", cost=cost, gamma=gamma)  
y_adjustment <- predict(model, X)
  

y_pred <- y_pred + y_adjustment
ev_additive <- explained_var_1(y, y_pred)

ev_df <- data.frame(loop=loop, cost=cost, gamma=gamma, ev_additive=ev_additive)

setwd("/projects/gzhang2016227/nikolay/thesis-additivemtl-linearcomponent-svr-adjustment/")
filename <- paste("results_raw_loop",toString(loop),".csv", sep="")  
write.table(ev_df, file=filename, append=TRUE, quote=FALSE, sep=",", row.names=FALSE, col.names=!file.exists(filename))











