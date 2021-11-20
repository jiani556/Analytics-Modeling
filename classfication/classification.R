#+eval=FALSE
# The files credit_card_data.txt (without headers) and credit_card_data-headers.txt (with headers) contain a dataset with 654 data points, 6 continuous and 4 binary predictor variables. It has anonymized credit card applications with a binary response variable (last column) indicating if the application was positive or negative. The dataset is the “Credit Approval Data Set” from the UCI Machine Learning Repository (https://archive.ics.uci.edu/ml/datasets/Credit+Approval) without the categorical variables and without data points that have missing values.
# IP credit_card_data-headers.txt

# Loading and examining data
df<-read.delim("data 2.2/credit_card_data-headers.txt", header = TRUE, sep = "\t")
#### Display Head Lines ####
head(df,2)
#### Show attributes, number of row, and number of col####
attributes(df)
ncol(df)
nrow(df)
#### Show the type, number of unique value, and summary of each variable ####
for (i in attributes(df)$names ){
  print(paste0('Name:',i,'   Class:',class(df[[i]]),'   nlevels:',length(unique(df[[i]]))))
  print(summary(df[[i]]))
  }
#### Event Rate ####
length(df$R1[df$R1=="1"])/nrow(df)
a<-(16807*12345678) %% (2^31-1)
# 1.Using the SVM in the R package kernlab, find a good classifier for this data.
#   Show the equation of your classifier, and how well it classifies the data points in the full data set.
#   (Don’t worry about test/validation data yet; we’ll cover that topic soon.)

#### Library ####
library(kernlab)
#### Create classifier ####
cost<-c(10^(-4:7))
summary_table <- data.frame(row.names=paste0("C",cost))
summary_table$c=c(-4:7)
summary_table7<-summary_table[c(1:12),]
for (c in cost){
  svm <- ksvm(  as.matrix(df[,1:10]),
                as.factor(df[,11]),
                type="C-svc",
                kernel="vanilladot",
                C=c,
                scaled=TRUE  )
  #### calculate a1...am ####
  for (x in 1:10){
      summary_table[paste0("C",c),paste0("a",x)]=
      colSums(svm@xmatrix[[1]] * svm@coef[[1]])[x]
    }
  summary_table[paste0("C",c),"a0"]= svm@b
  summary_table[paste0("C",c),"error"]=as.numeric(svm@error)
  fitted(model)
  summary_table[paste0("C",c),"tn"] = table(df$R1, fitted(svm))[1]
  summary_table[paste0("C",c),"fn"] = table(df$R1, fitted(svm))[2]
  summary_table[paste0("C",c),"fp"] = table(df$R1, fitted(svm))[3]
  summary_table[paste0("C",c),"tp"] = table(df$R1, fitted(svm))[4]
}
summary_table$P=summary_table$tp+summary_table$fp
summary_table$N=summary_table$tn+summary_table$fn
summary_table$TPR=summary_table$tp/(summary_table$tp+summary_table$fn)
summary_table$TNR=summary_table$tn/(summary_table$tn+summary_table$fp)
summary_table$Precision=summary_table$tp/summary_table$P
summary_table$CN=summary_table$tn/summary_table$N
svm
summary_table7$PR=summary_table7$P/(summary_table7$P+summary_table7$N)
# Libraries
library(ggplot2)
library(dplyr)
p1<-ggplot( summary_table7, aes(x=c, y=error)) +
  geom_line( color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=4) +
  labs(title ="SVM Model Performance with Different Cost Value")+ xlab("Cost (10 to the Power of #)") +ylab("Error")+
  scale_x_continuous(breaks = c(-4:7))

p2<-plot(summary_table7$P~summary_table7$c , type="b" , bty="l" , xlab="Cost (10 to the Power of #)" , ylab="# of Prediction" , col=rgb(0.2,0.4,0.1,0.7) , lwd=2 , pch=17, xlim=c(-4,8), ylim=c(0,700))+
  lines(summary_table7$N~summary_table7$c, col=rgb(0.8,0.4,0.1,0.7) , lwd=2 , pch=19 , type="b" )+
  axis(side=1, at=seq(-4, 7, by=1), labels =c(-4:7))+
  title("Number of Positive vs. Negative Prediction (or 1 vs. 0)
with Different Cost Value",adj =0,cex.main=0.9)+
  legend("bottomleft",
       legend = c("Positive", "Negative"),
       col = c(rgb(0.2,0.4,0.1,0.7),
               rgb(0.8,0.4,0.1,0.7)),
       pch = c(17,19),
       bty = "n",
       pt.cex = 1,
       cex = 0.8,
       text.col = "black",
       horiz = F ,
       inset = c(0.15, 0.15))

p2<-plot(summary_table7$TPR~summary_table7$c , type="b" , bty="l" , xlab="Cost (10 to the Power of #)" , ylab="Rate" , col=rgb(0.2,0.4,0.1,0.7) , lwd=2 , pch=17, xlim=c(-4,8),ylim=c(0,1))+
  lines(summary_table7$TNR~summary_table7$c, col=rgb(0.8,0.4,0.1,0.7) , lwd=2 , pch=19 , type="b" )+
  axis(side=1, at=seq(-4, 7, by=1), labels =c(-4:7))+
  title("True Positive Rate (Sensitivity) and True Negative Rate (Specificity)
        with Different Cost Value",adj =0,cex.main=0.9)+
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

# 2. You are welcome, but not required, to try other (nonlinear) kernels as well;
#    we’re not covering them in this course, but they can sometimes be useful and might provide better predictions than vanilladot.
kernel<-c("rbfdot","polydot","vanilladot","tanhdot","laplacedot","besseldot","anovadot","splinedot")
summary_table2 <- data.frame(row.names=kernel)
summary_table2$model=kernel
for (k in kernel){
  svm <- ksvm(  as.matrix(df[,1:10]),
                as.factor(df[,11]),
                type="C-svc",
                kernel=k,
                C=1,
                scaled=TRUE  )
  #### calculate a1...am ####
  for (x in 1:10){
    summary_table2[k,paste0("a",x)]=
      colSums(svm@xmatrix[[1]] * svm@coef[[1]])[x]
  }
  summary_table2[k,"a0"]= svm@b
  summary_table2[k,"error"]=as.numeric(svm@error)
  fitted(model)
  summary_table2[k,"tn"] = table(df$R1, fitted(svm))[1]
  summary_table2[k,"fn"] = table(df$R1, fitted(svm))[2]
  summary_table2[k,"fp"] = table(df$R1, fitted(svm))[3]
  summary_table2[k,"tp"] = table(df$R1, fitted(svm))[4]
}
summary_table2$P=summary_table2$tp+summary_table2$fp
summary_table2$N=summary_table2$tn+summary_table2$fn
summary_table2$TPR=summary_table2$tp/(summary_table2$tp+summary_table2$fn)
summary_table2$TNR=summary_table2$tn/(summary_table2$tn+summary_table2$fp)

ggplot(summary_table2, aes(x=model, y=error)) +
  geom_bar(stat = "identity",color="blue", fill=rgb(0.1,0.4,0.5,0.7)) +
  coord_flip()+xlab("Kernel Functions") +ylab("Error") +
  labs(title ="SVM Model Performance with Different Kernal Function")

plot(summary_table2$TPR~summary_table2$model , type="b" , bty="l" , xlab="Cost (10 to the Power of #)" , ylab="Rate" , col=rgb(0.2,0.4,0.1,0.7) , lwd=2 , pch=17, ylim=c(0,1))+
  lines(summary_table2$TNR~summary_table2$model, col=rgb(0.8,0.4,0.1,0.7) , lwd=2 , pch=19 , type="b" )+
  axis(side=1, at=seq(-4, 7, by=1), labels =c(-4:7))+
  title("True Positive Rate (Sensitivity) and True Negative Rate (Specificity)
        with Different Cost Value",adj =0,cex.main=0.9)+
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

# 3.Using the k-nearest-neighbors classification function kknn contained in the R kknn package,
#   suggest a good value of k, and show how well it classifies that data points in the full data set.
#   Don’t forget to scale the data (scale=TRUE in kknn).
set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data
sample <- sample.int(n = nrow(df), size = floor(.7*nrow(df)), replace = F)
train <- df[sample,c(1:11) ]
test  <- df[-sample,c(1:11) ]

library(kknn)
summary_table3 <- data.frame(row.names= c(1:15),k=c(1:15))
for (x in c(1:15)){
      KNN<-kknn(R1~., train, test, distance =2,scale=TRUE, k=13,
                kernel = "triangular")
summary_table3[x,"tn"] = table(test$R1, round(KNN$fit))[1]
summary_table3[x,"fn"] = table(test$R1, round(KNN$fit))[2]
summary_table3[x,"fp"] = table(test$R1, round(KNN$fit))[3]
summary_table3[x,"tp"] = table(test$R1, round(KNN$fit))[4]
summary_table3[x,"error"]=(summary_table3[x,"fn"] +summary_table3[x,"fp"])/nrow(test)
}
summary(KNN)
ggplot( summary_table3, aes(x=k, y=error)) +
geom_line( color="grey") +
geom_point(shape=21, color="black", fill="#69b3a2", size=4) +
labs(title ="KNN Model Performance with Different K value")+ xlab("Number of Neighbors Considered") +ylab("Error")+
scale_x_continuous(breaks = c(1:15))
