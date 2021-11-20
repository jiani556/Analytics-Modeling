# Analytics-Modeling

## Classfication
-  The files credit_card_data.txt (without headers) and credit_card_data-headers.txt (with headers) contain a dataset with 654 data points, 6 continuous and 4 binary predictor variables. It has anonymized credit card applications with a binary response variable (last column) indicating if the application was positive or negative. The dataset is the “Credit Approval Data Set” from the UCI Machine Learning Repository (https://archive.ics.uci.edu/ml/datasets/Credit+Approval) without the categorical variables and without data points that have missing values.
- 1.Using the support vector machine function ksvm contained in the R package kernlab, find a good classifier for this data. S
- 2.try other (nonlinear) kernels
- 3.Using the k-nearest-neighbors classification function kknn contained in the R kknn package, find a good value of k


## Clustering
- The iris data set iris.txt contains 150 data points, each with four predictor variables and one categorical response. The predictors are the width and length of the sepal and petal of flowers and the response is the type of flower. The data is available from the R library datasets and can be accessed with iris once the library is loaded. It is also available at the UCI Machine Learning Repository (https://archive.ics.uci.edu/ml/datasets/Iris ). The response values are only given to see how well a specific method performed and should not be used to build the model.
- Use the R function kmeans to cluster the points as well as possible. Report the best combination of predictors, suggested value of k

## Outliers Detection and CUSUM
- Using crime data from the file uscrime.txt (http://www.statsci.org/data/general/uscrime.txt, description at http://www.statsci.org/data/general/uscrime.html), test to see whether there are any outliers in the last column (number of crimes per 100,000 people). Use the grubbs.test function in the outliers package in R.
- Using July through October daily-high-temperature data for Atlanta for 1996 through 2015, use a CUSUM approach to identify when unofficial summer ends (i.e., when the weather starts cooling off) each year. You can get the data that you need from the file temps.txt or online, for example at http://www.iweathernet.com/atlanta-weather-records or https://www.wunderground.com/history/airport/KFTY/2015/7/1/CustomHistory.html . You can use R if you’d like, but it’s straightforward enough that an Excel spreadsheet can easily do the job too.
- Use a CUSUM approach to make a judgment of whether Atlanta’s summer climate has gotten warmer in that time (and if so, when).

## exponential_smoothing_model
-Using the 20 years of daily high temperature data for Atlanta (July through October), use an exponential smoothing model to help make a judgment of whether the unofficial end of summer has gotten later over the 20 years.

## regression
-Using crime data from http://www.statsci.org/data/general/uscrime.txt (file uscrime.txt, description at http://www.statsci.org/data/general/uscrime.html ), use regression (a useful R function is lm or glm) to predict the observed crime rate in a city

## pca
- Using the same crime data set uscrime.txt apply Principal Component Analysis and then create a regression model using the first few principal components. Specify new model in terms of the original variables (not the principal components), and compare its quality

##random forest
-Using the same crime data set uscrime.txt, find the best model with (a) a regression tree model, and (b) a random forest model.

##feature selection
- Using the crime data set uscrime.txt build a regression model using:
- 1. Stepwise regression
- 2. Lasso
- 3. Elastic net

##fractional factorial design
- Use R’s FrF2 function (in the FrF2 package) to find a fractional factorial design: what set of features should each of the 16 fictitious houses have?

## missing values
- The breast cancer data set breast-cancer-wisconsin.data.txt from http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/ (description at http://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+%28Original%29 ) has missing values.
- 1. Use the mean/mode imputation method to impute values for the missing data.
- 2. Use regression to impute values for the missing data.
- 3. Use regression with perturbation to impute values for the missing data.
- 4. (Optional) Compare the results and quality of classification models (e.g., SVM, KNN) build using (1) the data sets from questions 1,2,3; (2) the data that remains after data points with missing values are removed; and (3) the data set when a binary variable is introduced to indicate missing values.

##optimization
- 1.  Formulate an optimization model (a linear program) to find the cheapest diet that satisfies the maximum and minimum daily nutrition constraints, and solve it using PuLP. (The optimal solution should be a diet of air-popped popcorn, poached eggs, oranges, raw iceberg lettuce, raw celery, and frozen broccoli. UGH!)
