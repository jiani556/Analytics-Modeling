#+eval=FALSE
# IP uscrime.txt germancredit.txt

#Question 10.1 Using the same crime data set uscrime.txt as in Questions 8.2 and 9.1,
#Find the best model you can using (a) a regression tree model, and (b) a random forest model.
library(randomForest)
library(tree)
library(rpart)
library(rpart.plot)
library(tibble)
library(ggplot2)
library(dplyr)
library(tidyverse)

df<-read.delim("data/uscrime.txt", header = TRUE, sep = "\t")

#scale
fn <- function(x) scale(x,  scale = TRUE)
df_scaled<-as.data.frame(lapply(df[,-16], fn))
df_scaled$Crime<-df$Crime

#splite train and test
set.seed(666)
g <- sample(1:2,size=nrow(df_scaled),replace=TRUE,prob=c(0.7,0.3))
train <- df_scaled[g==1,]
test <- df_scaled[g==2,]

# Fit model:regression tree
set.seed(123)
reg_tree <- rpart(
  formula = Crime ~ .,
  data    = train,
  method  = "anova"
)
rpart.plot(reg_tree)
plotcp(reg_tree)

predict(reg_tree, test, type = 'vector')
test_residials<-predict(reg_tree, test, type = 'vector')-test$Crime
MSE_test_reg<-mean(test_residials^2) #MSE Train

# Tune model:regression tree
rgt_tune <- rpart(
  formula = Crime ~ .,
  data    = df,
  method  = "anova",
  control = list(minsplit = 5, maxdepth = 12, xval = 2)
)
rpart.plot(rgt_tune)
plotcp(rgt_tune)

predict(rgt_tune, test, type = 'vector')
test_residials2<-predict(rgt_tune, test, type = 'vector')-test$Crime
MSE_test_rgt<-mean(test_residials2^2) #MSE Train

# Fit glm model: random forest
set.seed(123)
rf_model <- randomForest(
  formula = Crime ~ .,
  data    = train,
  xtest   = test[,-16],
  ytest   = test[,16]
)

rf_model$importance
which.min(rf_model$mse)
rf_model$mse[which.min(rf_model$mse)]
rf_model$test$mse[which.min(rf_model$mse)]

MSE_train_rf <- rf_model$mse
MSE_test_rf <- rf_model$test$mse

#plot error
# compare error
tibble::tibble(
  `RF TRAIN MSE` = MSE_train_rf,
  `RF TEST MSE` = MSE_test_rf,
  ntrees = 1:rf_model$ntree
) %>%
  gather(Metric, MSE, -ntrees) %>%
  ggplot(aes(ntrees, MSE, color = Metric)) +
  geom_line() +
  scale_y_continuous(labels = scales::dollar) +
  xlab("Number of trees")


##tuning mtry
set.seed(123)
rf_tune<- tuneRF(
  x          = train[,-16],
  y          = train[,16],
  ntreeTry   = 500,
  mtryStart  = 5,
  stepFactor = 1.3,
  improve    = 0.01,
  trace      = FALSE      # to not show real-time progress
)

set.seed(123)
rf_model2 <- randomForest(
  formula = Crime ~ .,
  data    = train,
  xtest   = test[,-16],
  ytest   = test[,16],
  mtry=6
)

which.min(rf_model2$mse)
rf_model2$mse[which.min(rf_model2$mse)]
rf_model2$test$mse[which.min(rf_model2$mse)]

MSE_train_rf2 <- rf_model2$mse
MSE_test_rf2<- rf_model2$test$mse

#plot error
# compare error
tibble::tibble(
  `RF TRAIN MSE` = MSE_train_rf2,
  `RF TEST MSE` = MSE_test_rf2,
  ntrees = 1:rf_model2$ntree
) %>%
  gather(Metric, MSE, -ntrees) %>%
  ggplot(aes(ntrees, MSE, color = Metric)) +
  geom_line() +
  scale_y_continuous(labels = scales::dollar) +
  xlab("Number of trees")

#Question 10.3 Using the GermanCredit data set, use logistic regression
# 10.3.1. Find a good predictive model for whether credit applicants are good credit risks or not.
df2<-read.delim("/Users/jianilong/Documents/Gatech/CS/isye6501/hw7/germancredit.txt", header = FALSE, sep = " ")
#### Display Head Lines ####
head(df2,2)
#### Show summary, number of row of last column##
nrow(df2)
sum(df2$V21-1)/nrow(df2)
ncol(df2)
for (i in attributes(df2)$names ){
  print(paste0('Name:',i,'   Class:',class(df2[[i]]),'   nlevels:',length(unique(df2[[i]]))))
  print(summary(df2[[i]]))
}
sapply(df2,function(x) sum(is.na(x)))

numerc_var<-c("V2","V5","V13")
fn <- function(x) scale(x,  scale = TRUE)
f_scaled<-as.data.frame(lapply(df2[,numerc_var], fn))
f_scaled$V21=df2$V21-1
df2_scaled<-cbind(subset(df2, select = -c(V2,V5,V13,V21)),f_scaled)

#splite train and test
set.seed(666)
g2 <- sample(1:2,size=nrow(df2_scaled),replace=TRUE,prob=c(0.7,0.3))
train2 <- df2_scaled[g==1,]
test2 <- df2_scaled[g==2,]

library(pROC)
cal_ROC <- function(y_pred, y_real, dateset=NULL)
{ outcome <- as.numeric(factor(y_real))-1
  pos <- sum(outcome) # total known positives
  neg <- sum(1-outcome) # total known negatives
  pos_probs <- outcome*y_pred # probabilities for known positives
  neg_probs <- (1-outcome)*y_pred # probabilities for known negatives
  true_pos <- sapply(y_pred,
                     function(x) sum(pos_probs>=x)/pos) # true pos. rate
  false_pos <- sapply(y_pred,
                     function(x) sum(neg_probs>=x)/neg)
  if (is.null(dateset))
    result <- data.frame(true_pos, false_pos)
  else
    result <- data.frame(true_pos, false_pos, dateset)
  result %>% arrange(false_pos, true_pos)
}

# Fit glm model: gaussian model
logit_model<-glm(V21~.,family = binomial(link = "logit"),train2)
logit_test_pred<-predict(logit_model,test2,type="response")
ROC.train <- cal_ROC(y_pred=logit_model$fitted.values,
                      y_real=train2$V21,
                      dateset="train")
ROC.test <- cal_ROC(y_pred=logit_test_pred,
                     y_real=test2$V21,
                     dateset="test")
ROCs <- rbind(ROC.train, ROC.test)
auc_test<-auc(test2$V21,logit_test_pred)
auc_train<-auc(train2$V21,logit_model$fitted.values)
ggplot(ROCs, aes(x=false_pos, y=true_pos, color=dateset)) +
  geom_line() + xlim(0, 0.25)+
  ggtitle(paste0("AUC_Train: ",round(auc_train, digits = 2),"  AUC_Test: ",round(auc_test, digits = 2)))

# 10.3.2. Determine a good threshold probability based on your model.

summary_table <- data.frame(row.names=paste0("th",c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)),
                            threshold= c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))
for (th in c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)){
      col<-paste0("th",th)
      fitted<-(logit_test_pred>=th)
      table(test2$V21,fitted)
      summary_table[col,"tn"] = table(test2$V21,fitted)[1]
      summary_table[col,"fn"] = table(test2$V21,fitted)[2]
      summary_table[col,"fp"] = table(test2$V21,fitted)[3]
      summary_table[col,"tp"] = table(test2$V21,fitted)[4]
}
summary_table$P=summary_table$tp+summary_table$fp
summary_table$N=summary_table$tn+summary_table$fn
summary_table$TPR=summary_table$tp/(summary_table$tp+summary_table$fn)
summary_table$TNR=summary_table$tn/(summary_table$tn+summary_table$fp)
summary_table$Error=(summary_table$fp+summary_table$fn)/(summary_table$P+summary_table$N)
summary_table$Cost=summary_table$fp*5+summary_table$fn

plot(summary_table$TPR~summary_table$threshold , type="b" , bty="l" , xlab="Threshold" ,
     ylab="Rate" , col=rgb(0.2,0.4,0.1,0.7) , lwd=2 , pch=17, ylim=c(0,1))+
  lines(summary_table$TNR~summary_table$threshold, col=rgb(0.8,0.4,0.1,0.7),lwd=2,pch=19,type="b")+
  axis(side=1, at=seq(0.1, 0.9, by=0.1), labels =c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9) )+
  title("True Positive Rate (Sensitivity) and True Negative Rate (Specificity)
        with Different Threshold",adj =0,cex.main=0.9)+
  legend("bottomleft",
         legend = c("True Positive Rate", "True Negative Rate"),
         col = c(rgb(0.2,0.4,0.1,0.7),
                 rgb(0.8,0.4,0.1,0.7)),
         pch = c(17,19),
         bty = "n",
         pt.cex = 1,
         cex = 0.8,
         text.col = "black",
         horiz = F ,
         inset = c(0.15, 0.15))

plot(summary_table$Error~summary_table$threshold, type="b" , bty="l" , xlab="Threshold" ,
     ylab="Error" , lwd=2  )+
  axis(side=1, at=seq(0.1, 0.9, by=0.1), labels =c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9) )+
  title("Error with Different Threshold",adj =0,cex.main=0.9)

plot(summary_table$Cost~summary_table$threshold, type="b" , bty="l" , xlab="Threshold" ,
     ylab="Cost" , lwd=2  )+
  axis(side=1, at=seq(0.1, 0.9, by=0.1), labels =c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9) )+
  title("Cost with Different Threshold",adj =0,cex.main=0.9)
