#+eval=FALSE
# IP uscrime.txt

# Loading and examining data
df<-read.delim("/data/uscrime.txt", header = TRUE, sep = "\t")
#### Display Head Lines ####
head(df,2)
#### Show summary, number of row of last column##
summary(df$Crime)
nrow(df)
#Question 5.1  Using crime data from the file uscrime.txt test
#to see whether there are any outliers in the last column (number of crimes per 100,000 people).
#Use the grubbs.test function in the outliers package in R. 

#### Boxplot and Histgram ####

boxplot(df$Crime)
# Layout to split the screen
layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
# Draw the boxplot and the histogram
par(mar=c(0, 3.1, 1.1, 2.1))
boxplot(df$Crime , horizontal=TRUE, xaxt="n",ylim=c(0,2000),col=rgb(0.8,0.8,0,0.5) , frame=F)
par(mar=c(4, 3.1, 1.1, 2.1))
hist(df$Crime,breaks=25,col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" ,xlab="Number of crimes per 100,000 people",xlim=c(0,2000))

#### Library outliers####
library(outliers)

grubbs.test(df$Crime)

crime2<-rm.outlier(df$Crime)
grubbs.test(crime2)

crime3<-rm.outlier(crime2)
grubbs.test(crime3)

crime4<-rm.outlier(crime3)
grubbs.test(crime4)

crime5<-rm.outlier(crime4)
grubbs.test(crime5)

crime6<-rm.outlier(crime5)
grubbs.test(crime6)
