#+eval=FALSE
# IP uscrime.txt

# Loading and examining data
df<-read.delim("data/uscrime.txt", header = TRUE, sep = "\t")
#### Display Head Lines ####
head(df,2)
#### Show summary, number of row of last column##
summary(df$Crime)
nrow(df)
ncol(df)
for (i in attributes(df)$names ){
  print(paste0('Name:',i,'   Class:',class(df[[i]]),'   nlevels:',length(unique(df[[i]]))))
  print(summary(df[[i]]))
}

library("ggplot2")
library("ggpubr")

for (i in c(1:15)){
  assign(paste0("g", i),
         ggscatter(df, x = colnames(df)[i], y = "Crime",
         add = "reg.line", conf.int = TRUE,
         cor.coef = TRUE, cor.method = "pearson",
         xlab = colnames(df)[i], ylab = "Crime")
          )
}

library(gridExtra)
grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12, g13, g14, g15,  nrow = 5)

#splite train and validation set
set.seed(666)
g <- sample(1:2,size=nrow(df),replace=TRUE,prob=c(0.7,0.3))
train <- df[g==1,]
test <- df[g==2,]

# Fit glm model: gaussian model
glm_model1<-glm(Crime~.,family = gaussian,train)
glm_model1
MSE_train<-mean(glm_model1$residuals^2) #MSE Train
confint(glm_model1) # 95% CI for the coefficients

p1_test<-predict(glm_model1,test,type="response")
p1_residials<-p1_test-test$Crime
MSE_test<-mean(p1_residials^2) #MSE Train


# Fit glm model: gaussian model leave one out cross validation
library(glmnet)
lasso_glm<-cv.glmnet(as.matrix(df[,c(1:15)]), df$Crime, family = "gaussian",
                           weights = NULL, offset = NULL, lambda = NULL,
                           type.measure = c("default", "mse", "deviance", "class", "auc", "mae","C"),
                           nfolds = 10, foldid = NULL, alignment = c("lambda",  "fraction"),
                           grouped = TRUE, keep = FALSE, parallel = FALSE,
                           gamma = c(0, 0.25, 0.5, 0.75, 1), relax = FALSE, trace.it = 0)
lasso_glm
plot(lasso_glm)
coef(lasso_glm, s = "lambda.min")
coef(lasso_glm, s = "lambda.1se")

       ggscatter(df, x = "Po1", y = "Po2",
                 add = "reg.line", conf.int = TRUE,
                 cor.coef = TRUE, cor.method = "pearson",
                 xlab = "Po1", ylab = "Po2")


new_data<- matrix(1:15, nrow = 1, dimnames = list(1, colnames(df)[c(1:15)]))
new_data[1,]<-c(14.0, 0, 10.0, 12.0, 15.5, 0.640, 94.0, 150, 1.1, 0.120, 3.6, 3200, 20.1, 0.04, 39.0)

predict(lasso_glm,new_data, s = "lambda.min")
predict(lasso_glm,new_data, s = "lambda.1se")
