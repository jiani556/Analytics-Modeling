#+eval=FALSE
# ISYE 6501 Intro Analytics Modeling - HW8
# IP uscrime.txt

# Loading and examining data
df<-read.delim("data/uscrime.txt", header = TRUE, sep = "\t")
fn <- function(x) scale(x,  scale = TRUE)
df_scaled<-as.data.frame(lapply(df[,-16], fn))
df_scaled$Crime<-df$Crime

#splite train and test
set.seed(666)
g <- sample(1:2,size=nrow(df_scaled),replace=TRUE,prob=c(0.7,0.3))
train <- df_scaled[g==1,]
test <- df_scaled[g==2,]

# Fit glm model: gaussian model backward stepwise&both
full_model<-glm(Crime~.,family = gaussian,train)
MSE_train_full<-mean(full_model$residuals^2) #MSE Train
full_test<-predict(full_model,test,type="response")
full_residials<-full_test-test$Crime
MSE_test_full<-mean(full_residials^2) #MSE Train

model_step_bw <- step(full_model,direction = "backward",test = "F")
model_step_both <- step(full_model,direction = "both",test = "F")

glm_model1<-glm(Crime~M + Ed + Po1 + Po2 + U1 + U2 + Ineq + Prob + Time,family = gaussian,train)
MSE_train_m1<-mean(glm_model1$residuals^2) #MSE Train
confint(glm_model1) # 95% CI for the coefficients
p1_test<-predict(glm_model1,test,type="response")
p1_residials<-p1_test-test$Crime
MSE_test_m1<-mean(p1_residials^2) #MSE Train

# Fit glm model: lasso gaussian model 10-fold cross validation
library(glmnet)
set.seed(123)
lasso_glm<-cv.glmnet(as.matrix(train[,c(1:15)]), train$Crime, family = "gaussian", alpha=1,
                     weights = NULL, offset = NULL, lambda = NULL,
                     type.measure = c("default", "mse", "deviance", "class", "auc", "mae","C"),
                     nfolds = 10, foldid = NULL, alignment = c("lambda",  "fraction"),
                     grouped = TRUE, keep = FALSE, parallel = FALSE,
                     gamma = c(0, 0.25, 0.5, 0.75, 1), relax = FALSE, trace.it = 0)
lasso_glm
plot(lasso_glm)

coef(lasso_glm, s = "lambda.min")
lasso_min_p<-predict(lasso_glm, as.matrix(test[,c(1:15)]), s = "lambda.min")
p2_residials<-lasso_min_p-test$Crime
MSE_test_m2<-mean(p2_residials^2) #MSE Train

coef(lasso_glm, s = "lambda.1se")
lasso_1se_p<-predict(lasso_glm, as.matrix(test[,c(1:15)]), s = "lambda.1se")
p3_residials<-lasso_1se_p-test$Crime
MSE_test_m3<-mean(p3_residials^2) #MSE Train

# Fit glm model: Elastic net model 10-fold cross validation
set.seed(123)
elnet_glm<-cv.glmnet(as.matrix(train[,c(1:15)]), train$Crime, family = "gaussian", alpha=0.5,
                     weights = NULL, offset = NULL, lambda = NULL,
                     type.measure = c("default", "mse", "deviance", "class", "auc", "mae","C"),
                     nfolds = 10, foldid = NULL, alignment = c("lambda",  "fraction"),
                     grouped = TRUE, keep = FALSE, parallel = FALSE,
                     gamma = c(0, 0.25, 0.5, 0.75, 1), relax = FALSE, trace.it = 0)
elnet_glm

coef(elnet_glm, s = "lambda.min")
elnet_min_p<-predict(elnet_glm, as.matrix(test[,c(1:15)]), s = "lambda.min")
p4_residials<-elnet_min_p-test$Crime
MSE_test_m4<-mean(p4_residials^2) #MSE Train

coef(elnet_glm, s = "lambda.1se")
elnet_1se_p<-predict(elnet_glm, as.matrix(test[,c(1:15)]), s = "lambda.1se")
p5_residials<-elnet_1se_p-test$Crime
MSE_test_m5<-mean(p5_residials^2) #MSE Train

# Fit glm model: lasso gaussian model 10-fold cross validation

set.seed(123)
ridge_glm<-cv.glmnet(as.matrix(train[,c(1:15)]), train$Crime, family = "gaussian", alpha=0,
                     weights = NULL, offset = NULL, lambda = NULL,
                     type.measure = c("default", "mse", "deviance", "class", "auc", "mae","C"),
                     nfolds = 10, foldid = NULL, alignment = c("lambda",  "fraction"),
                     grouped = TRUE, keep = FALSE, parallel = FALSE,
                     gamma = c(0, 0.25, 0.5, 0.75, 1), relax = FALSE, trace.it = 0)
ridge_glm

coef(ridge_glm, s = "lambda.min")
ridge_min_p<-predict(ridge_glm, as.matrix(test[,c(1:15)]), s = "lambda.min")
p6_residials<-ridge_min_p-test$Crime
MSE_test_m6<-mean(p6_residials^2) #MSE Train
