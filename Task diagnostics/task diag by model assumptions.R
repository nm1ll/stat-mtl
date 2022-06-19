suppressMessages(library(e1071))
suppressMessages(library(randomForest))
suppressMessages(library(emdbook))
suppressMessages(library(plyr))
suppressMessages(library(dplyr))
suppressMessages(library(plotly))
suppressMessages(library(ggplot2))
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
diag_indep <- data.frame()
diag_equalvar <- data.frame()
diag_normality <- data.frame()
diag_dfs <- data.frame()
diag_cooks <- data.frame()


for(task in 1:139){
  X_dev <- data[data$school != task,]
  X_dev <- subset(X_dev, select=-c(score,school))
  y_dev <- data[data$school != task,'score']
  df_dev <- cbind(score=y_dev, X_dev)
  
  model = lm(score~. , data=df_dev)
  
  y_pred <- predict(model, X)
  resids <- y - y_pred
  
  # Deleted explained variance
  ev_dev <- explained_var_1(y, unname(y_pred))
  ev_df <- rbind(ev_df, data.frame(task=task, ev_dev=ev_dev))
  
  # The null hypothesis states that the errors are not auto-correlated with themselves (they are independent). 
  # Thus, if we achieve a p-value > 0.05, we would fail to reject the null hypothesis. 
  # This would give us enough evidence to state that our independence assumption is met!
  dwt <- durbinWatsonTest(model)
  diag_indep <- rbind(diag_indep, data.frame(task=task, dwt_stat=dwt$r, dwt_pval=dwt$p))
  
  # BP TEST
  # H0: equal variance for all Y given X's
  bpt <- bptest(model, studentize = FALSE) # H0: equal variance for all Y given X's
  
  diag_equalvar <- rbind(diag_equalvar, data.frame(task=task, 
                                                   bp_stat=bpt$statistic, bp_pval = bpt$p.value))
  
  
  # SHAPIRO TEST doesn't work (max sample size is 5000)
  # shapiro.test(resids)
  
  
  # H0: residuals are Normally distributed
  # ANDERSON-DARLING NORMALITY TEST
  adt <- ad.test(resids)
  # ONE-SAMPLE KOLMOGOROV-SMIRNOV TEST
  kst <- ks.test(resids,y='pnorm',alternative='two.sided')
  diag_normality <- rbind(diag_normality, data.frame(task=task, 
                                                     ad_stat=adt$statistic, ad_pval = adt$p.value,
                                                     ks_stat=kst$statistic, ks_pval = kst$p.value))
  # DF-stats count per deleted task
  count_dffits <- length(which(dffits(model) > 1))
  count_dfbetas <- length(which(dfbetas(model) > 1))
  diag_dfs <- rbind(diag_dfs,data.frame(task=task, 
                                           dffits_count=count_dffits,
                                           dfbetas_count=count_dfbetas))
  
  # How many Cook's distance marked observations are there per each deleted task
  cd <- cooks.distance(model)
  sum_fit <- summary(model)
  highcook <- which(cd > qf(0.5, sum_fit$df[1], sum_fit$df[2]))
  diag_cooks <- rbind(diag_cooks, data.frame(task=task, cd_count=length(highcook)))
  
}



# Durbin-Watson test
ggplot(diag_indep, aes(x=dwt_stat)) +
  geom_density(fill="gray")+
  labs(x="Durbin-Watson statistic", y = "Density")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

# Breusch-Pagan
ggplot(diag_equalvar, aes(x=bp_stat)) +
  geom_density(fill="gray")+
  labs(x="Breusch-Pagan statistic", y = "Density")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

# Anderson-Darling test for normality
ggplot(diag_normality, aes(x=ad_stat)) +
  geom_density(fill="gray")+
  labs(x="Anderson-Darling statistic", y = "Density")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

# One Sample Kolmogorov-Smirnov Test
ggplot(diag_normality, aes(x=ks_stat)) +
  geom_density(fill="gray")+
  labs(x="Kolmogorov-Smirnov statistic", y = "Density")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))


hist(diag_dfs$dffits_count)
hist(diag_dfs$dfbetas_count)
hist(diag_cooks$cd_count)

# # NOT USEFUL SINCE CATEGORICAL VARIABLES ARE ENCODED WITH ALL LEVELS SEPARATELY, AND
# # THEY ARE LINEARLY DEPENDENT
# vif(model)



# SEEMS OK
aa <- lm.influence(model)
xoutliers <- which(aa$hat > .333)



