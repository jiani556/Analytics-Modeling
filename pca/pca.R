#+eval=FALSE
# IP uscrime.txt

library(GGally)
library(factoextra)
library(gridExtra)
library(plyr)
# Loading and examining data
df<-read.delim("data/uscrime.txt", header = TRUE, sep = "\t")
ggpairs(df, title="correlogram")  #pair-wise correlation

##scaling 0-100##
fn <- function(x) scale(x,  scale = TRUE)
df_scaled<-as.data.frame(lapply(df[,-16], fn))
df_scaled$Crime<-df$Crime

#spiliting test and train
set.seed(666)
g <- sample(1:2,size=nrow(df_scaled),replace=TRUE,prob=c(0.7,0.3))
train <- df_scaled[g==1,]
test <- df_scaled[g==2,]

# Fit glm model: gaussian model
glm_model1<-glm(Crime~.,family = gaussian,train)
MSE_train1<-mean(glm_model1$residuals^2) #MSE Train
confint(glm_model1) # 95% CI for the coefficients

p1_test<-predict(glm_model1,test,type="response")
p1_residials<-p1_test-test$Crime
MSE_test1<-mean(p1_residials^2) #MSE Train

#PCA
pca<-prcomp(df_scaled[,-16])
summary(pca)

#plot dimensions explained variances
fviz_eig(pca,title="PCA Result - % of Explained Variances")

# Contributions of variables to PC1
g1<-fviz_contrib(pca, choice = "var", axes = 1,title="PCA Result - Contributions of variables to PC1")
g2<-fviz_contrib(pca, choice = "var", axes = 2,title="PCA Result - Contributions of variables to PC2")
grid.arrange(g1, g2, nrow = 2)

#Individuals Distribution at Dim1*Dim2
F1<-fviz_pca_ind(pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title="PCA Result - Individuals Distribution at Dim1*Dim2",label=FALSE
)

#Variables contribution at Dim1*Dim2
F2<-fviz_pca_var(pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title="PCA Result - Variables contribution at Dim1*Dim2",
             repel = TRUE     # Avoid text overlapping
)
grid.arrange(F1, F2, nrow = 1)


summary_table <- data.frame()
for (k in c(1:15)){
  # Fit glm model: gaussian model_pca result
  summary_table[k,'k']<-k
  PCA_df <- as.data.frame(cbind(pca$x[,1:k],'Crime'=df$Crime))
  PCA_train <- PCA_df[g==1,]
  PCA_test <- PCA_df[g==2,]

  glm_model2<-glm(Crime~.,family = gaussian, PCA_train)
  assign(paste0("glm_model_k", k), glm(Crime~.,family = gaussian, PCA_train))
  summary_table[k,'MSE_train']<-mean(glm_model2$residuals^2) #MSE Train

  p2_test<-predict(glm_model2,PCA_test,type="response")
  p2_residials<-p2_test-PCA_test$Crime
  summary_table[k,'MSE_test']<-mean(p2_residials^2) #MSE Train
}

#plot error
p2<-plot(summary_table$MSE_train~summary_table$k, type="b" , bty="l", xlab="First K number of components selected" , ylab="MSE" , col=rgb(0.2,0.4,0.1,0.7) , lwd=2 , pch=17)+
   lines(summary_table$MSE_test~summary_table$k, col=rgb(0.8,0.4,0.1,0.7) , lwd=2 , pch=19 , type="b" )+
   abline(h=c(MSE_train1,MSE_test1), col=c(rgb(0.2,0.4,0.1,0.7),rgb(0.8,0.4,0.1,0.7)), lty=c(2,2), lwd=c(2,2))+
   axis(side=1, at=seq(1, 15, by=1), labels =c(1:15))+
  title("MSE_train and MSE_test with K number of components selected",adj =0,cex.main=0.9)+
  legend("topright",
         legend = c("PCA_GLM_MSE_train", "PCA_GLM_MSE_test", "GLM_MSE_train","GLM_MSE_test"),
         col = c(rgb(0.2,0.4,0.1,0.7),
                 rgb(0.8,0.4,0.1,0.7),
                 rgb(0.2,0.4,0.1,0.7),
                 rgb(0.8,0.4,0.1,0.7)),
         pch = c(17,19,NA,NA),
         lty = c(NA,NA,2,2),
         bty = "n",
         pt.cex = 1,
         cex = 0.8,
         text.col = "black",
         horiz = F
        )


##select first 7 PC calculate variables' coef
pca_glm_coef<-glm_model_k7$coefficients[2:8] # coefficients from pca glm model
Intercept<-glm_model_k7$coefficients[1]      # intercept from pca glm model

eigenvectors<-pca$rotation[,1:7]             # eigenvectors of pc1 to pc7
coeff<-colSums(t(eigenvectors)*pca_glm_coef) # coeff for scaled x

sd_df<-apply(df[,-16], 2, sd)                # df variables sd
mean_df<-apply(df[,-16], 2, mean)            # df variables mean

coeff_unscale<-t(coeff)/sd_df                # unscale coefficients
Intercept_unscale<-Intercept-colSums(t(t(coeff)*mean_df)/sd_df)   # unscale intercept

#new data
new<-c(14.0, 0, 10.0, 12.0, 15.5, 0.640, 94.0, 150, 1.1, 0.120, 3.6, 3200, 20.1, 0.04, 39.0)
new_predict<-sum(t(new)*coeff_unscale)+Intercept_unscale
