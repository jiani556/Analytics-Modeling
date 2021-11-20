#+eval=FALSE
# ISYE 6501 Intro Analytics Modeling - HW10
# IP breast-cancer-wisconsin.data.txt 
library(mice)
# Loading and examining data
df<-read.delim("/Users/jianilong/Documents/Gatech/CS/isye6501/hw10/breast-cancer-wisconsin.data.txt", header = FALSE, sep = ",")
#### Display Head Lines ####
head(df,2)
#### Show summary, number of row of last column##
nrow(df)
ncol(df)
df[df=="?"]<-NA
sapply(df, function(x) sum(is.na(x)))
df$V7<-as.numeric(df$V7)
df$Dep_Var<-(df$V11-2)/2
df<- df[order(df$V1),]

#1.	Use the mean/mode imputation method to impute values for the missing data.#
df_mean<-df
for(i in 1:ncol(df_mean)){
  df_mean[is.na(df_mean[,i]), i] <- mean(df_mean[,i], na.rm = TRUE)
}
df_mean <- df_mean[order(df_mean$V1),]

#2.Use regression to impute values for the missing data. .#
df_rg_t<-df[! is.na(df$V7),]
df_rg_p<-df[is.na(df$V7),]

rg<-glm(V7~V2+V3+V4+V5+V6+V8+V9+V10,family = gaussian,df_rg_t) 
rg
MSE_train<-mean(rg$residuals^2) 
df_rg_p$V7<-predict(rg,df_rg_p,type="response")
sd(df_rg_p$V7)
df_rg<-rbind(df_rg_t,df_rg_p)
df_rg <- df_rg[order(df_rg$V1),]
#3.Use regression with perturbation to impute values for the missing data. #

imp <- mice(df[,c(2:10)], method="norm.predict")
df_rgp<-complete(imp)
df_rgp$V1<-df$V1
df_rgp$Dep_Var<-df$Dep_Var
df_rgp <- df_rgp[order(df_rgp$V1),]
mean(df_rgp[miss,'V7'])
sd(df_rgp[miss,'V7'])
miss<-c(24,41,140,146,159,165,236,250,276,293,295,298,316,322,412,618)
    #4.	(Optional) Compare the results and quality of classification models (e.g., SVM, KNN) build using 
# (1) the data sets from questions 1,2,3;
# (2) the data that remains after data points with missing values are removed; 
# and (3) the data set when a binary variable is introduced to indicate missing values. 

set.seed(123)
g <- sample(1:2,size=nrow(df),replace=TRUE,prob=c(0.7,0.3)) 
df_mean_t <- df_mean[g==1,]
df_mean_v <- df_mean[g==2,]

library(kernlab)
cost<-c(10^(-3:3)) ## Create a table to record models results 
summary_df_mean <- data.frame(cost= c(-3:3))
indep_var<-c('V2','V3','V4','V5','V6','V8','V9','V10','V7')
for (c in cost){
  ## Create classifier with different C value
  svm <- ksvm(
    as.matrix(df_mean_t[,indep_var]), as.factor(df_mean_t[,'Dep_Var']), type="C-svc", kernel="vanilladot", C=c,
    scaled=TRUE ) 
  for (x in 1:10){
    summary_df_mean[cost==c,"Dataset"]="DF_Mean"
    summary_df_mean[cost==c,"Error_Train"]=as.numeric(svm@error)
    summary_df_mean[cost==c,"Error_Test"]= sum((df_mean_v$Dep_Var-as.numeric(predict(svm,df_mean_v[,indep_var]))+1)^2)/nrow(df_mean_v)
    cf_mx<-table(df_mean_v$Dep_Var, predict(svm,df_mean_v[,indep_var]))
    summary_df_mean[cost==c,"TPR"]=cf_mx[4]/(cf_mx[4]+cf_mx[2]) ## True positive rate
    summary_df_mean[cost==c,"TNR"]=cf_mx[1] /(cf_mx[1]+cf_mx[3]) ## Ture negative rate 
  }
}

df_rg_t<- df_rg[g==1,]
df_rg_v <- df_rg[g==2,]

summary_df_rg <- data.frame(cost= c(-3:3))
indep_var<-c('V2','V3','V4','V5','V6','V8','V9','V10','V7')
for (c in cost){
  ## Create classifier with different C value
  svm <- ksvm(
    as.matrix(df_rg_t[,indep_var]), as.factor(df_rg_t[,'Dep_Var']), type="C-svc", kernel="vanilladot", C=c,
    scaled=TRUE ) 
  for (x in 1:10){
    summary_df_rg[cost==c,"Dataset"]="DF_Regression"
    summary_df_rg[cost==c,"Error_Train"]=as.numeric(svm@error)
    summary_df_rg[cost==c,"Error_Test"]= sum((df_rg_v$Dep_Var-as.numeric(predict(svm,df_rg_v[,indep_var]))+1)^2)/nrow(df_rg_v)
    cf_mx<-table(df_rg_v$Dep_Var, predict(svm,df_rg_v[,indep_var]))
    summary_df_rg[cost==c,"TPR"]=cf_mx[4]/(cf_mx[4]+cf_mx[2]) ## True positive rate
    summary_df_rg[cost==c,"TNR"]=cf_mx[1] /(cf_mx[1]+cf_mx[3]) ## Ture negative rate 
  }
}

df_rgp_t <- df_rgp[g==1,]
df_rpg_v <- df_rgp[g==2,]

summary_df_rgp <- data.frame(cost= c(-3:3))
indep_var<-c('V2','V3','V4','V5','V6','V8','V9','V10','V7')
for (c in cost){
  ## Create classifier with different C value
  svm <- ksvm(
    as.matrix(df_rgp_t[,indep_var]), as.factor(df_rgp_t[,'Dep_Var']), type="C-svc", kernel="vanilladot", C=c,
    scaled=TRUE ) 
  for (x in 1:10){
    summary_df_rgp[cost==c,"Dataset"]="DF_Reg_Perturbation"
    summary_df_rgp[cost==c,"Error_Train"]=as.numeric(svm@error)
    summary_df_rgp[cost==c,"Error_Test"]= sum((df_rpg_v$Dep_Var-as.numeric(predict(svm,df_rpg_v[,indep_var]))+1)^2)/nrow(df_rpg_v)
    cf_mx<-table(df_rpg_v$Dep_Var, predict(svm,df_rpg_v[,indep_var]))
    summary_df_rgp[cost==c,"TPR"]=cf_mx[4]/(cf_mx[4]+cf_mx[2]) ## True positive rate
    summary_df_rgp[cost==c,"TNR"]=cf_mx[1] /(cf_mx[1]+cf_mx[3]) ## Ture negative rate 
  }
}

df_rm_t <- na.omit(df[g==1,])
df_rm_v <- na.omit(df[g==2,])

summary_df_rm <- data.frame(cost= c(-3:3))
indep_var<-c('V2','V3','V4','V5','V6','V8','V9','V10','V7')
for (c in cost){
  ## Create classifier with different C value
  svm <- ksvm(
    as.matrix(df_rm_t[,indep_var]), as.factor(df_rm_t[,'Dep_Var']), type="C-svc", kernel="vanilladot", C=c,
    scaled=TRUE ) 
  for (x in 1:10){
    summary_df_rm[cost==c,"Dataset"]="DF_Removed"
    summary_df_rm[cost==c,"Error_Train"]=as.numeric(svm@error)
    summary_df_rm[cost==c,"Error_Test"]= sum((df_rm_v$Dep_Var-as.numeric(predict(svm,df_rm_v[,indep_var]))+1)^2)/nrow(df_rm_v)
    cf_mx<-table(df_rm_v$Dep_Var, predict(svm,df_rm_v[,indep_var]))
    summary_df_rm[cost==c,"TPR"]=cf_mx[4]/(cf_mx[4]+cf_mx[2]) ## True positive rate
    summary_df_rm[cost==c,"TNR"]=cf_mx[1] /(cf_mx[1]+cf_mx[3]) ## Ture negative rate 
  }
}

df_flag<-df
df_flag$V7_miss_flag <- as.numeric(is.na(df_flag$V7))
df_flag[is.na(df_flag)]<-0

df_flag_t <- df_flag[g==1,]
df_flag_v <- df_flag[g==2,]


summary_df_flag <- data.frame(cost= c(-3:3))
indep_var<-c('V2','V3','V4','V5','V6','V8','V9','V10','V7','V7_miss_flag')
for (c in cost){
  ## Create classifier with different C value
  svm <- ksvm(
    as.matrix(df_flag_t[,indep_var]), as.factor(df_flag_t[,'Dep_Var']), type="C-svc", kernel="vanilladot", C=c,
    scaled=TRUE ) 
  for (x in 1:10){
    summary_df_flag[cost==c,"Dataset"]="DF_Flag"
    summary_df_flag[cost==c,"Error_Train"]=as.numeric(svm@error)
    summary_df_flag[cost==c,"Error_Test"]= sum((df_flag_v$Dep_Var-as.numeric(predict(svm,df_flag_v[,indep_var]))+1)^2)/nrow(df_flag_v)
    cf_mx<-table(df_flag_v$Dep_Var, predict(svm,df_flag_v[,indep_var]))
    summary_df_flag[cost==c,"TPR"]=cf_mx[4]/(cf_mx[4]+cf_mx[2]) ## True positive rate
    summary_df_flag[cost==c,"TNR"]=cf_mx[1] /(cf_mx[1]+cf_mx[3]) ## Ture negative rate 
  }
}

summary<-rbind(summary_df_mean,summary_df_rg,summary_df_rgp,summary_df_rm,summary_df_flag)
summary_c1<-summary[cost==1,]

p3<-ggplot(summary_c1, aes(x=Dataset, y=Error_Test)) + 
   geom_bar(stat = "identity",color="blue", fill=rgb(0.1,0.4,0.5,0.7)) + coord_flip()+xlab("Dataset") +ylab("Error") +
   labs(title ="Test Error with cost=1")

plot(summary_c1$Error_Test~summary_c1$Dataset, type="b", bty="l", xlab="Cost",
     ylab="Error",col=rgb(0.2,0.4,0.1,0.7) , lwd=2 , pch=17, ylim=c(0,0.1))

+ 
  lines(summary_df_mean$Error_Test~summary_df_mean$cost, col=rgb(0.8,0.4,0.1,0.7),lwd=2,pch=19,type="b")+
  title("Replace with Mean - Train and Test Error with Different cost",adj =0,cex.main=0.9)+
  legend("bottomleft", legend = c("Train Error", "Test Error"), col = c(rgb(0.2,0.4,0.1,0.7), 
                                                                        rgb(0.8,0.4,0.1,0.7)), pch = c(17,19), bty = "n", pt.cex = 1, cex = 0.8, text.col = "black", horiz = F)
library(gridExtra)
library(ggplot2)
p1<-plot(summary_df_mean$Error_Train~summary_df_mean$cost, type="b", bty="l", xlab="Cost",
        ylab="Error",col=rgb(0.2,0.4,0.1,0.7) , lwd=2 , pch=17, ylim=c(0,0.1))+ 
  lines(summary_df_mean$Error_Test~summary_df_mean$cost, col=rgb(0.8,0.4,0.1,0.7),lwd=2,pch=19,type="b")+
  title("Replace with Mean - Train and Test Error with Different cost",adj =0,cex.main=0.9)+
  legend("bottomleft", legend = c("Train Error", "Test Error"), col = c(rgb(0.2,0.4,0.1,0.7), 
         rgb(0.8,0.4,0.1,0.7)), pch = c(17,19), bty = "n", pt.cex = 1, cex = 0.8, text.col = "black", horiz = F)

p2<-plot(summary_df_rg$Error_Train~summary_df_rg$cost, type="b", bty="l", xlab="Cost",
         ylab="Error",col=rgb(0.2,0.4,0.1,0.7) , lwd=2 , pch=17, ylim=c(0,0.1))+ 
  lines(summary_df_rg$Error_Test~summary_df_rg$cost, col=rgb(0.8,0.4,0.1,0.7),lwd=2,pch=19,type="b")+
  title("Replace with Regression - Train and Test Error with Different cost",adj =0,cex.main=0.9)+
  legend("bottomleft", legend = c("Train Error", "Test Error"), col = c(rgb(0.2,0.4,0.1,0.7), 
                                                                        rgb(0.8,0.4,0.1,0.7)), pch = c(17,19), bty = "n", pt.cex = 1, cex = 0.8, text.col = "black", horiz = F )

p3<-plot(summary_df_rgp$Error_Train~summary_df_rgp$cost, type="b", bty="l", xlab="Cost",
         ylab="Error",col=rgb(0.2,0.4,0.1,0.7) , lwd=2 , pch=17, ylim=c(0,0.1))+ 
  lines(summary_df_rgp$Error_Test~summary_df_rgp$cost, col=rgb(0.8,0.4,0.1,0.7),lwd=2,pch=19,type="b")+
  title("Replace with Regression & perturbation - Train and Test Error with Different cost",adj =0,cex.main=0.9)+
  legend("bottomleft", legend = c("Train Error", "Test Error"), col = c(rgb(0.2,0.4,0.1,0.7), 
                                                                        rgb(0.8,0.4,0.1,0.7)), pch = c(17,19), bty = "n", pt.cex = 1, cex = 0.8, text.col = "black", horiz = F )


p4<-plot(summary_df_rm$Error_Train~summary_df_rm$cost, type="b", bty="l", xlab="Cost",
         ylab="Error",col=rgb(0.2,0.4,0.1,0.7) , lwd=2 , pch=17, ylim=c(0,0.1))+ 
  lines(summary_df_rm$Error_Test~summary_df_rm$cost, col=rgb(0.8,0.4,0.1,0.7),lwd=2,pch=19,type="b")+
  title("Removed - Train and Test Error with Different cost",adj =0,cex.main=0.9)+
  legend("bottomleft", legend = c("Train Error", "Test Error"), col = c(rgb(0.2,0.4,0.1,0.7), 
                                                                        rgb(0.8,0.4,0.1,0.7)), pch = c(17,19), bty = "n", pt.cex = 1, cex = 0.8, text.col = "black", horiz = F )


p5<-plot(summary_df_flag$Error_Train~summary_df_flag$cost, type="b", bty="l", xlab="Cost",
         ylab="Error",col=rgb(0.2,0.4,0.1,0.7) , lwd=2 , pch=17, ylim=c(0,0.1))+ 
  lines(summary_df_flag$Error_Test~summary_df_flag$cost, col=rgb(0.8,0.4,0.1,0.7),lwd=2,pch=19,type="b")+
  title("Flaged - Train and Test Error with Different cost",adj =0,cex.main=0.9)+
  legend("bottomleft", legend = c("Train Error", "Test Error"), col = c(rgb(0.2,0.4,0.1,0.7), 
                                                                        rgb(0.8,0.4,0.1,0.7)), pch = c(17,19), bty = "n", pt.cex = 1, cex = 0.8, text.col = "black", horiz = F )

p<-grid.arrange(p1,p2,p3,p4,p5, nrow = 2)