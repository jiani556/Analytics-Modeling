#+eval=FALSE
# IP credit_card_data-headers.txt

# Loading data
df<-read.delim("/data 3.1/credit_card_data-headers.txt", header = TRUE, sep = "\t")

# Question 3.1 Using the same data set (credit_card_data.txt or credit_card_data-headers.txt) as in Question 2.2,
# use the ksvm or kknn function to find a good classifier:
# (a)  using cross-validation (do this for the k-nearest-neighbors model; SVM is optional);
# and (b)  splitting the data into training, validation, and test data sets (pick either KNN or SVM; the other is optional).

#### Library ####
library(kknn)
library(ggplot2)
library(dplyr)
#a-cross-validation#
knn_kernal=c("rectangular","triangular","epanechnikov","biweight","triweight","cos", "inv", "gaussian", "rank", "optimal")
best_knn=train.kknn(R1~ ., df, kmax = 20, ks=10, distance=2, kernel = knn_kernal, scale = TRUE)
summary_table <-melt(best_knn$MEAN.SQU,id=c("row.names") ,na.rm = FALSE, value.name = "MEAN_SQU")
ggplot(summary_table,aes(x=Var2, y=MEAN_SQU))+
  geom_bar(stat = "identity",color="blue", fill=rgb(0.1,0.4,0.5,0.7)) +
  coord_flip()+xlab("Kernel Functions") +ylab("Error") +
  labs(title ="KNN Model Performance CV")

#b#
set.seed(666)
g <- sample(1:3,size=nrow(df),replace=TRUE,prob=c(0.7,0.15,0.15))
train <- df[g==1,]
test <- df[g==2,]
validation <- df[g==3,]

summary_table2 <- data.frame(row.names=knn_kernal,k=knn_kernal)
for (x in knn_kernal){
  KNN<-kknn(R1~., train, test, distance =2,scale=TRUE, k=10,
            kernel = x)
  summary_table2[x,"tn"] = table(test$R1, round(KNN$fit))[1]
  summary_table2[x,"fn"] = table(test$R1, round(KNN$fit))[2]
  summary_table2[x,"fp"] = table(test$R1, round(KNN$fit))[3]
  summary_table2[x,"tp"] = table(test$R1, round(KNN$fit))[4]
  summary_table2[x,"error"]=(summary_table2[x,"fn"] +summary_table2[x,"fp"])/nrow(test)
}
ggplot(summary_table2,aes(x=k, y=error))+
  geom_bar(stat = "identity",color="blue", fill=rgb(0.1,0.4,0.5,0.7)) +
  coord_flip()+xlab("Kernel Functions") +ylab("Error") +
  labs(title ="KNN Model Performance Test Set")
error_test=min(summary_table2$error)
error_test

KNN_inv<-kknn(R1~., train, validation, distance =2,scale=TRUE, k=10,
            kernel = "inv")
error_val<-(table(validation$R1, round(KNN_inv$fit))[2]+table(validation$R1, round(KNN_inv$fit))[3])/nrow(validation)
error_val


# Question 4.2 The iris data set iris.txt contains 150 data points,
# each with four predictor variables and one categorical response.
# The predictors are the width and length of the sepal and petal of flowers and the response is the type of flower.
# The response values are only given to see how well a specific method performed and should not be used to build the model.
# Use the R function kmeans to cluster the points as well as possible.
# Report the best combination of predictors, your suggested value of k, and how well your best clustering predicts flower type.

# Loading and examining data
df2<-read.table("/data 3.1/iris.txt", header = TRUE)
#### Display Head Lines ####
head(df2,2)
#### Show attributes, number of row, and number of col####
attributes(df2)
ncol(df2)
nrow(df2)

#### Show the type, number of unique value, and summary of each variable ####
for (i in attributes(df2)$names ){
  print(paste0('Name:',i,'   Class:',class(df2[[i]]),'   nlevels:',length(unique(df2[[i]]))))
  print(summary(df2[[i]]))
}

df2_v<-scale(df2[,c(1:4)]) # Scaling the data
#clustring differenrt algorithm
for (a in c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen")){
km<-kmeans(df2_v, centers=3, iter.max = 50, nstart = 1,
           algorithm=a, trace=FALSE)
assign(paste('pred', a, sep="_"),km$cluster)
}
pred_Hartigan_wong<-`pred_Hartigan-Wong`
summary_km <-cbind(df2,pred_Hartigan_wong,pred_Lloyd,pred_Forgy,pred_MacQueen)

a<-"MacQueen"
ggplot(summary_km, aes(x=Sepal.Length, y=Sepal.Width, shape=Species, color=factor(pred_MacQueen))) +
             geom_point() +
             labs(title=paste0('K-means: algorithm:', a))

error<-(        nrow(summary_km[summary_km$Species == 'setosa' & summary_km$pred_MacQueen !=2 ,])+
                 nrow(summary_km[summary_km$Species == 'versicolor' & summary_km$pred_MacQueen !=3 ,])+
                 nrow(summary_km[summary_km$Species == 'virginica' & summary_km$pred_MacQueen !=1 ,]))/nrow(summary_km)

#clustring differenrt k
kmean_withinss <- function(k) {
  km <- kmeans(df2_v, centers=k, iter.max = 50, nstart = 1,
               algorithm=a, trace=FALSE)
  return (km$tot.withinss)
}

max_k <-20
withiness <- sapply(2:max_k, kmean_withinss)
elbow <-data.frame(2:max_k, withiness)

  ggplot( elbow, aes(x=X2.max_k, y=withiness)) +
  geom_line( color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=4) +
  labs(title ="Elbow plot: K-Means Performance with Different k Value")+ xlab("K Value") +ylab("Total within-cluster sum of squares,")
