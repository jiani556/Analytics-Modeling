#+eval=FALSE
# IP uscrime.txt

# Loading and examining data
df<-read.delim("data/temps.txt", header = TRUE, sep = "\t")
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

# Question 7.2 Using the 20 years of daily high temperature data for Atlanta (July through October) ,
# build and use an exponential smoothing model to help make a judgment of
# whether the unofficial end of summer has gotten later over the 20 years.
# (Part of the point of this assignment is for you to think about how you might use exponential smoothing to answer this question.
# Feel free to combine it with other models if you’d like to. There’s certainly more than one reasonable approach.)

ts_v = as.vector(unlist(df[,2:21]))                 #create time series
ts  <-ts(ts_v, start=1996, frequency = nrow(df))
plot.ts(ts,xlab="Time",ylab="Temp")

#simple exponential smoothing
single_exp_hw<-HoltWinters(ts, beta = FALSE, gamma = FALSE)
single_exp_hw
plot(single_exp_hw,xlab="Time",ylab="Temp",main="Simple Exponential Smoothing")
single_exp_hw$SSE


single_exp_hw2<-HoltWinters(ts,alpha=0.2, beta = FALSE, gamma = FALSE)
single_exp_hw2
plot(single_exp_hw2,xlab="Time",ylab="Temp",main="Simple Exponential Smoothing alpha0.2")
single_exp_hw2$SSE

#double exponential smoothing
double_exp_hw <- HoltWinters(ts, gamma = FALSE)
double_exp_hw
plot(double_exp_hw,main="Double Exponential Smoothing - Trend")
seasonal_temp = double_exp_hw$fitted
plot( seasonal_temp, main="Double Exponential Smoothing - Trend" )
double_exp_hw$SSE


#double exponential smoothing2
double_exp_hw2 <- HoltWinters(ts, beta = FALSE)
double_exp_hw2
plot(double_exp_hw2)
seasonal_temp2 = double_exp_hw2$fitted
plot( seasonal_temp2,main="Double Exponential Smoothing - seasonal "  )
double_exp_hw2$SSE

#triple exponential smoothing
triple_exp_hw <- HoltWinters(ts,seasonal = 'additive')
triple_exp_hw
plot(triple_exp_hw)
seasonal_temp = triple_exp_hw$fitted
plot( seasonal_temp,main="Triple Exponential Smoothing " )
triple_exp_hw$SSE

#heatmap
seasonal<- matrix(triple_exp_hw$fitted[,4], nrow = 123)
seasonal_est<-as.data.frame(seasonal,col.names=name(c(1995:2005)))
heatmap(seasonal, Rowv = NA, Colv = NA)
