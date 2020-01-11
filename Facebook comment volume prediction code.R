setwd("E:\\PGP BA - greatlearning\\Capstone Project\\Project - Group 1\\4. Facebook Comment Volume Prediction\\Dataset")

# Reading the dataset
mydata = read.csv("Training.csv", header = T)

# Checking the structure of the dataset
str(mydata)

# Structure shows all variables as numerical except Post published weekday & Base DateTime weekday as Factors
# Removing ID, Post Promotion status & CC5 
# As ID is unique page id, Post Promotion status is 0 for all & CC5 is derived out of CC2 & CC3
newdata = mydata[,-c(1,39)]
str(newdata)

# Changing Page category to Factor
newdata$Page.Category=as.factor(newdata$Page.Category)
str(newdata)

# Checking the summary of the dataset
summary(newdata)

attach(newdata)

# Univariate Analysis
# Checking the distribution & outliers for all numerical variables
hist(Target.Variable, xlab = "Number of Comments", col = "sky blue")
boxplot(Target.Variable, horizontal = T,main = "Boxplot", xlab = "No. of Comments")
summary(Target.Variable)

hist(Page.likes, xlab = "Number of Likes", col = "sky blue")
boxplot(Page.likes, horizontal = T,main = "Boxplot", xlab = "No. of Likes")
summary(Page.likes)

hist(Page.Checkins, xlab = "Number of Checkins", col = "sky blue")
boxplot(Page.Checkins, horizontal = T,main = "Boxplot", xlab = "No. of Checkins")
summary(Page.Checkins)

hist(Page.talking.about, xlab = "Number of Revisits", col = "sky blue")
boxplot(Page.talking.about, horizontal = T,main = "Boxplot", xlab = "No. of Revisits")
summary(Page.talking.about)


hist(Base.Time, xlab = "Number of Hours", col = "sky blue")
boxplot(Base.Time, horizontal = T,main = "Boxplot", xlab = "No. of Hours")
summary(Base.Time)

hist(Post.Length, xlab = "Number of Characters", col = "sky blue")
boxplot(Post.Length, horizontal = T,main = "Boxplot", xlab = "No. of Characters")
summary(Post.Length)

hist(Post.Share.Count, xlab = "Number of Shares", col = "sky blue")
boxplot(Post.Share.Count, horizontal = T,main = "Boxplot", xlab = "No. of Shares")
summary(Post.Share.Count)

hist(H.local, xlab = "Number of Hours", col = "sky blue")
boxplot(H.local, horizontal = T,main = "Boxplot", xlab = "No. of Hours")
summary(H.local)

# Checking the frequency for all categorical variables
category = table(Page.Category)
category
barplot(category, main = "Barplot for Page Category", ylab = "No. of Pages",xlab = "Page Categories", col = "sky blue")

post_published= table(Post.published.weekday)
post_published
barplot(post_published, main = "Bar Plot for Post published week day", ylab = "No. of pages published posts", xlab = "Week day", col = "skyblue")


base_time_weekday= table(Base.DateTime.weekday)
base_time_weekday
barplot(base_time_weekday, main = "Bar Plot for Base date time week day", ylab = "No. of pages", xlab = "Week day", col = "skyblue")

# Checking distribution of Feature 5 to 29
library(psych)
multi.hist(newdata[,c(5:29)])


# Bivariate Analysis
library(corrplot)

# Checking relationship between Target variable and all other independent variables
# Using Scatter plot
par(mfrow=c(2,2))
plot(Page.likes,Target.Variable)
plot(Page.Checkins,Target.Variable)
plot(Page.talking.about,Target.Variable)
plot(Post.Share.Count,Target.Variable)

par(mfrow=c(2,2))
plot(Post.Length,Target.Variable)
plot(H.local,Target.Variable)

# Checking Scatter plot for Target variable and Page likes after adjusting outlier
xout=Page.likes>5000000                         # some logical rule for identifying an outlier
plot(Page.likes[!xout],Target.Variable[!xout])  # toss them out

par(mfrow=c(2,2))
plot(CC1,Target.Variable)
plot(CC2,Target.Variable)
plot(CC3,Target.Variable)
plot(CC4,Target.Variable)

plot(CC5,Target.Variable)

# Checking Scatter plot for Target variable and CC2 after adjusting outlier
yout=Target.Variable>500                         # some logical rule for identifying an outlier
plot(CC2[!yout],Target.Variable[!yout])  # toss them out



# Plotting Spearman correlation (non-Parametric measure) without NA & missing values
# At 0.95 confidence level
# Null Hypothesis is relationship is 0
# Alternative Hypothesis is relationship is not 0

cor(newdata[,-c(4:29,39,40)],method = "spearman", use = "na.or.complete")
corrplot(cor(newdata[,-c(4:29,39,40)],method = "spearman", use = "na.or.complete"))

cor(newdata[,-c(4,39,40)],method = "spearman", use = "na.or.complete")
corrplot(cor(newdata[,-c(4,39,40)],method = "spearman", use = "na.or.complete"))


# Conducting Spearmans correlarion test (non-Parametric measure)
cor.test(Target.Variable, Page.likes, method = "spearman", exact = F, data = newdata)
cor.test(Target.Variable, Page.Checkins, method = "spearman", exact = F, data = newdata)
cor.test(Target.Variable, Page.talking.about, method = "spearman", exact = F, data = newdata)
cor.test(Target.Variable, CC1, method = "spearman", exact = F, data = newdata)
cor.test(Target.Variable, CC2, method = "spearman", exact = F, data = newdata)
cor.test(Target.Variable, CC3, method = "spearman", exact = F, data = newdata)
cor.test(Target.Variable, CC4, method = "spearman", exact = F, data = newdata)
cor.test(Target.Variable, CC5, method = "spearman", exact = F, data = newdata)
cor.test(Target.Variable, Base.Time, method = "spearman", exact = F, data = newdata)
cor.test(Target.Variable, Post.Length, method = "spearman", exact = F, data = newdata)
cor.test(Target.Variable, Post.Share.Count, method = "spearman", exact = F, data = newdata)
cor.test(Target.Variable, H.local, method = "spearman", exact = F, data = newdata)


cor.test(Page.likes, Page.talking.about, method = "spearman", exact = F, data = newdata)
cor.test(CC1, Page.talking.about, method = "spearman", exact = F, data = newdata)
cor.test(CC1, CC2, method = "spearman", exact = F, data = newdata)
cor.test(CC1, CC4, method = "spearman", exact = F, data = newdata)
cor.test(CC1, Post.Share.Count, method = "spearman", exact = F, data = newdata)
cor.test(CC4, CC2, method = "spearman", exact = F, data = newdata)
cor.test(CC3, CC5, method = "spearman", exact = F, data = newdata)
cor.test(CC4, Post.Share.Count, method = "spearman", exact = F, data = newdata)


# Removing CC5 as it is derived from CC2 & CC3
filterdata = newdata[,-34]

# Checking for missing values
colSums(is.na(filterdata))
filterdata
library(Amelia)
missmap(filterdata[1:15000,],col = c("steelblue","grey"), y.cex = 0.5,x.cex = 0.5)
missmap(filterdata[15001:32760,],col = c("steelblue","grey"), y.cex = 0.5,x.cex = 0.5)
# This suggests wherever Page talking about is missing, Page checkins is also missing in entire dataset 
# Rest all missing values are random

# Imputing missing values for CC1 with base time 0 to 48
# Imputing missing values for CC4 with base time 0 to 24
filterdata$CC4[which(filterdata$Base.Time<=24 & is.na(filterdata$CC4))] = filterdata$CC1[which(filterdata$Base.Time<=24 & is.na(filterdata$CC4))]
filterdata$CC4[which(filterdata$Base.Time<=24 & is.na(filterdata$CC4))] = filterdata$CC2[which(filterdata$Base.Time<=24 & is.na(filterdata$CC4))]
filterdata$CC1[which(filterdata$Base.Time<=24 & is.na(filterdata$CC1))] = filterdata$CC2[which(filterdata$Base.Time<=24 & is.na(filterdata$CC1))]
filterdata$CC1[which(filterdata$Base.Time<=48 & is.na(filterdata$CC1))] = filterdata$CC2[which(filterdata$Base.Time<=48 & is.na(filterdata$CC1))] + filterdata$CC3[which(filterdata$Base.Time<=48 & is.na(filterdata$CC1))]

# Saving the csv file with CC1 & CC4 imputation
write.csv(filterdata, file = "E:/PGP BA - greatlearning/Capstone Project/Project - Group 1/4. Facebook Comment Volume Prediction/Dataset/imputed_data.csv")

# Applying Knn imputation for rest of the missing values with k=3
imputed_data = read.csv("imputed_data.csv")
imputed_data = imputed_data[-32760,-1]

library(VIM)
knnimputed_all = kNN(imputed_data, k = 3)
write.csv(subset(knnimputed_all, select = Page.likes:Target.Variable), file = "E:/PGP BA - greatlearning/Capstone Project/Project - Group 1/4. Facebook Comment Volume Prediction/Dataset/knnimputed_all.csv")

# Checking the number of NA values
knnimputed_all=read.csv("knnimputed_all.csv")
knnimputed_all = knnimputed_all[,-1]
colSums(is.na(knnimputed_all))

# knnimputed_all.csv is our dataset after filtering variables and imputing missing values

# Finding and treating the noise in the data (13 rows with Extreme Values )
data_noiseless = knnimputed_all[!(knnimputed_all$Page.likes>40000000 | knnimputed_all$Page.talking.about>3000000),]
data_noiseless = data_noiseless[!(data_noiseless$Post.Length>20000),]
write.csv(data_noiseless, file = "E:/PGP BA - greatlearning/Capstone Project/Project - Group 1/4. Facebook Comment Volume Prediction/Dataset/data_noiseless.csv")


# data_noiseless.csv is the dataset without noise in the data
# creating another dataset after removing outliers using Tukey's method

data_noiseless = read.csv("data_noiseless.csv")

# Creating function for outlier treatment
# Tukey's rule says that the outliers are values 
# more than 1.5 times the interquartile range from the quartiles
# either below Q1 - 1.5IQR, or above Q3 + 1.5IQR

# Used the Tukey's method because it is not dependent on distribution of data.
# It will identify the outliers ranged above and below the 1.5*IQR.
# Moreover, the Tukey's method ignores the mean and standard deviation, 
# which are influenced by the extreme values (outliers).

# Added a question (yes/no) to ask whether to keep or remove the outliers in data.
# If the answer is yes then outliers will be replaced with NA.
outlierTukey <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  tot <- sum(!is.na(var_name))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  message("Outliers identified: ", na2 - na1, " from ", tot, " observations")
  message("Proportion (%) of outliers: ", (na2 - na1) / tot*100)
  message("Mean of the outliers: ", mo)
  m2 <- mean(var_name, na.rm = T)
  message("Mean without removing outliers: ", m1)
  message("Mean if we remove outliers: ", m2)
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    message("Outliers successfully removed", "\n")
    return(invisible(dt))
  } else{
    message("Nothing changed", "\n")
    return(invisible(var_name))
  }
}

outlierTukey(data_noiseless,Page.likes)
outlierTukey(data_noiseless,Page.Checkins)
outlierTukey(data_noiseless,Page.talking.about)
outlierTukey(data_noiseless,Post.Share.Count)
outlierTukey(data_noiseless,Post.Length)

# Removing all NA values
outlier_removed = data_noiseless[complete.cases(data_noiseless), ]
write.csv(outlier_removed, file = "E:/PGP BA - greatlearning/Capstone Project/Project - Group 1/4. Facebook Comment Volume Prediction/Dataset/outlier_removed.csv")

# outlier_removed.csv is the dataset without outliers

# Reducing dimensions (Feature 5 to Feature 29) using Factor Analysis(FA)
##Creating a matrix out of the dataframe in order to be used in the Model
X2 = read.csv("outlier_removed.csv")
X2 = X2[,-c(1,2)]
X2 = as.matrix(X2[5:29])

library(nFactors)
ev = eigen(cor(X2)) # get eigenvalues
ev
nS = nScree(x=ev$values)
plotnScree(nS)

Factor = fa(X2,nfactors = 4, fm = "ml", rotate = "varimax")
print(Factor)
fa.diagram(Factor)

Factor_scores = Factor$scores
colnames(Factor_scores)
colnames(Factor_scores)[colnames(Factor_scores)=="ML4"] = "Max.SD"
colnames(Factor_scores)[colnames(Factor_scores)=="ML1"] = "Avg.Median"
colnames(Factor_scores)[colnames(Factor_scores)=="ML2"] = "Min"
colnames(Factor_scores)[colnames(Factor_scores)=="ML3"] = "Min.Avg"
colnames(Factor_scores)

# Creating data set with extracted 4 Factors
outlier_removed = read.csv("outlier_removed.csv")
Factor_nooutlier = data.frame(Factor_scores[,1:4],outlier_removed[,-c(1,2,7:31)])
write.csv(Factor_nooutlier, file = "E:/PGP BA - greatlearning/Capstone Project/Project - Group 1/4. Facebook Comment Volume Prediction/Dataset/Factor_nooutlier.csv")


# Factor_nooutlier.csv is the dataset for preparing Model

# Running Linear Regression to find important variables
Lm_data = read.csv("Factor_nooutlier.csv")
Model1 = lm(Target.Variable~., data = Lm_data[,-1])
summary(Model1)

# R sq is 33% & Important variables are:
# Max.SD
# Avg.Median
# CC2
# CC3
# Base.Time
# Post.Length
# Post.Share.Count
# H.Local

modeldata_noout = Factor_nooutlier[,c(1,2,10,11,13:16,19)]
write.csv(modeldata_noout, file = "E:/PGP BA - greatlearning/Capstone Project/Project - Group 1/4. Facebook Comment Volume Prediction/Dataset/modeldata_noout.csv")

###########################################################################
# Now running Factor analysis for Feature 5 to Feature 29 on data_noiseless.csv
data_noiseless = read.csv("data_noiseless.csv")
X3 = as.matrix(data_noiseless[6:30])

library(nFactors)
ev = eigen(cor(X3)) # get eigenvalues
ev
nS = nScree(x=ev$values)
plotnScree(nS)

Factor3 = fa(X3,nfactors = 4, fm = "ml", rotate = "varimax")
print(Factor3)
fa.diagram(Factor3)

Factor_scores3 = Factor3$scores
colnames(Factor_scores3)
colnames(Factor_scores3)[colnames(Factor_scores3)=="ML4"] = "Max.SD"
colnames(Factor_scores3)[colnames(Factor_scores3)=="ML1"] = "Avg.Median1"
colnames(Factor_scores3)[colnames(Factor_scores3)=="ML2"] = "Min"
colnames(Factor_scores3)[colnames(Factor_scores3)=="ML3"] = "Avg.Median2"
colnames(Factor_scores3)

# Creating data set with extracted 4 Factors
Factor_nonoise = data.frame(Factor_scores3[,1:4],data_noiseless[,-c(1,6:30)])
write.csv(Factor_nonoise, file = "E:/PGP BA - greatlearning/Capstone Project/Project - Group 1/4. Facebook Comment Volume Prediction/Dataset/Factor_nonoise.csv")


# Factor_nooutlier.csv is the dataset for preparing Model

# Running Linear Regression to find important variables
Lm_data2 = read.csv("Factor_nonoise.csv")
Model2 = lm(Target.Variable~., data = Lm_data2[,-1])
summary(Model2)


# R sq is 32% & Important variables are:
# Max.SD
# Avg.Median1
# Min
# Page.likes
# Page.Checkins
# CC2
# CC3
# CC4
# Base.Time
# Post.Share.Count
# H.Local


modeldata_out = Factor_nonoise[,c(1,2,3,5,6,10,11,12,13,15,16,19)]
write.csv(modeldata_out, file = "E:/PGP BA - greatlearning/Capstone Project/Project - Group 1/4. Facebook Comment Volume Prediction/Dataset/modeldata_out.csv")

# We intend to run Random Forest & Neural Network on below Datasets: 
# modeldata_noout (data without outliers, after FA & only important variables)
# modeldata_out (data with outliers, after FA & only important variables)


# Random Forest Modeling
rfdata = read.csv("modeldata_out.csv")
rfdata = rfdata[,-1]

library(mlbench)
library(randomForest)
library(caret)

head(rfdata)
str(rfdata)

rfdata[!complete.cases(rfdata),]

#Let's break the dataset in training and testing dataset in 70:30 proportion

set.seed(123)
id=sample(2,nrow(rfdata),prob = c(0.7,0.3),replace = T)

#to create 2 dataset, fb_train & fb_test

fb_train=rfdata[id==1,]
fb_test=rfdata[id==2,]

#to get the value of mtry the function used is tuneRF from the randomForest library
# considering no. of trees as 501

mtry=tuneRF(fb_train[,-12],fb_train$Target.Variable,ntreeTry = 501,stepFactor = 1.5,improve = 0.01,trace = T,plot=T) 

#for Random Forest no. of trees to be considered is 501 & 
#the no. of variables to be considered at each node for spliting in each tree in the value of mtry which is 3 from tuneRF function

#now we make the Model with name rf & name of function is randomForest

rf=randomForest(Target.Variable~.,data = fb_train,mtry=3,ntree=501,importance=T)
print(rf)

# Variance explained is 61.72%

#to check the importance of IV's
importance(rf)
varImpPlot(rf)

#from the Model the most important variable to predict comments is Base time, followed by CC2.

#to find the accuracy of the Model for the testing dataset 

pred=predict(rf,newdata = fb_test[,-12],type = "response")

# Function that returns Root Mean Squared Error
rmse = function(error)
{
  sqrt(mean(error^2))
}

# Function that returns Mean Absolute Error
mae = function(error)
{
  mean(abs(error))
}

# Actual and Predicted value
actual = fb_test$Target.Variable
predicted = pred

# Calculating error
error = actual - predicted

# Calculating RMSE and MAE
rmse(error)
mae(error)


# Checking on Train data
pred_train=predict(rf,newdata = fb_train[,-12],type = "response")

actual1 = fb_train$Target.Variable
predicted1 = pred_train

# Calculating error
error1 = actual1 - predicted1

# Calculating RMSE and MAE
rmse(error1)
mae(error1)


########################################################################################
# Above code after dividing train & test data set is used for different ntree values
# Same code for Random forest is used for dataset without outlier (modeldata_noout.csv)
########################################################################################

# Artificial Neural Network Modeling
# Dataset with outlier is used

datann=read.csv("modeldata_out.csv")
str(datann)

datann =datann[,-1]
library(neuralnet)

#Normalising Independent Variables's of dataset, to bring numbers between 0 & 1 (x-x min/x max-x min)

datann$Max.SD=(datann$Max.SD-min(datann$Max.SD))/(max(datann$Max.SD)-min(datann$Max.SD))
datann$Avg.Median1=(datann$Avg.Median1-min(datann$Avg.Median1))/(max(datann$Avg.Median1)-min(datann$Avg.Median1))
datann$Min=(datann$Min-min(datann$Min))/(max(datann$Min)-min(datann$Min))
datann$Page.likes=(datann$Page.likes-min(datann$Page.likes))/(max(datann$Page.likes)-min(datann$Page.likes))
datann$Page.Checkins=(datann$Page.Checkins-min(datann$Page.Checkins))/(max(datann$Page.Checkins)-min(datann$Page.Checkins))
datann$CC2=(datann$CC2-min(datann$CC2))/(max(datann$CC2)-min(datann$CC2))
datann$CC3=(datann$CC3-min(datann$CC3))/(max(datann$CC3)-min(datann$CC3))
datann$CC4=(datann$CC4-min(datann$CC3))/(max(datann$CC4)-min(datann$CC4))
datann$Base.Time=(datann$Base.Time-min(datann$Base.Time))/(max(datann$Base.Time)-min(datann$Base.Time))
datann$Post.Share.Count=(datann$Post.Share.Count-min(datann$Post.Share.Count))/(max(datann$Post.Share.Count)-min(datann$Post.Share.Count))
datann$H.local=(datann$H.local-min(datann$H.local))/(max(datann$H.local)-min(datann$H.local))

head(datann)

#creating training & testing data

set.seed(123)
id=sample(2,nrow(datann),prob = c(0.7,0.3),replace=T)
data_train=datann[id==1,]
data_test=datann[id==2,]

#to create a Model with 1 neuron

set.seed(123)
model=neuralnet(Target.Variable~Max.SD+Avg.Median1+Min+Page.likes+Page.Checkins+CC2+CC3+CC4+Base.Time+Post.Share.Count+H.local, data=data_train,hidden = 1,err.fct = "sse",act.fct = "tanh", linear.output = T,  stepmax=1e6) 

plot(model)

output=compute(model,data_test[,-12])

head(output$net.result)

# Function that returns Root Mean Squared Error
rmse <- function(error)
{
  sqrt(mean(error^2))
}

# Function that returns Mean Absolute Error
mae <- function(error)
{
  mean(abs(error))
}

# Actual and Predicted values
actual <- data_test$Target.Variable
predicted <- output$net.result

# Calculating error
error = actual - predicted

# Calculating RMSE and MAE
rmse(error)
mae(error)

# Calculation of R square

rss = sum((predicted - actual) ^ 2)  ## residual sum of squares
tss = sum((actual - mean(actual)) ^ 2)  ## total sum of squares
rsq = 1 - rss/tss
rsq

# Checking on training dataset

output1=compute(model,data_train[,-12])

head(output1$net.result)

# Actual and Predicted values
actual <- data_train$Target.Variable
predicted <- output1$net.result

# Calculating error
error = actual - predicted

# Calculating RMSE and MAE
rmse(error)
mae(error)

# Calculation of R square

rss = sum((predicted - actual) ^ 2)  ## residual sum of squares
tss = sum((actual - mean(actual)) ^ 2)  ## total sum of squares
rsq = 1 - rss/tss
rsq

#to create a Model with 2 neuron

set.seed(123)
model=neuralnet(Target.Variable~Max.SD+Avg.Median1+Min+Page.likes+Page.Checkins+CC2+CC3+CC4+Base.Time+Post.Share.Count+H.local, data=data_train,hidden = 2,err.fct = "sse",act.fct = "tanh", linear.output = T,  stepmax=1e6) 

plot(model)

output=compute(model,data_test[,-12])

head(output$net.result)

# Function that returns Root Mean Squared Error
rmse <- function(error)
{
  sqrt(mean(error^2))
}

# Function that returns Mean Absolute Error
mae <- function(error)
{
  mean(abs(error))
}

# Actual and Predicted values
actual <- data_test$Target.Variable
predicted <- output$net.result

# Calculating error
error = actual - predicted

# Calculating RMSE and MAE
rmse(error)
mae(error)

# Calculation of R square

rss = sum((predicted - actual) ^ 2)  ## residual sum of squares
tss = sum((actual - mean(actual)) ^ 2)  ## total sum of squares
rsq = 1 - rss/tss
rsq

# Checking on training dataset

output1=compute(model,data_train[,-12])

head(output1$net.result)

# Actual and Predicted values
actual <- data_train$Target.Variable
predicted <- output1$net.result

# Calculating error
error = actual - predicted

# Calculating RMSE and MAE
rmse(error)
mae(error)

# Calculation of R square

rss = sum((predicted - actual) ^ 2)  ## residual sum of squares
tss = sum((actual - mean(actual)) ^ 2)  ## total sum of squares
rsq = 1 - rss/tss
rsq


# Dataset without outlier is used

datann=read.csv("modeldata_noout.csv")
str(datann)

datann =datann[,-1]
library(neuralnet)

#Normalising Independent Variable's of dataset, to bring numbers between 0 & 1 (x-x min/x max-x min)

datann$Max.SD=(datann$Max.SD-min(datann$Max.SD))/(max(datann$Max.SD)-min(datann$Max.SD))
datann$Avg.Median=(datann$Avg.Median-min(datann$Avg.Median))/(max(datann$Avg.Median)-min(datann$Avg.Median))
datann$CC2=(datann$CC2-min(datann$CC2))/(max(datann$CC2)-min(datann$CC2))
datann$CC3=(datann$CC3-min(datann$CC3))/(max(datann$CC3)-min(datann$CC3))
datann$Base.Time=(datann$Base.Time-min(datann$Base.Time))/(max(datann$Base.Time)-min(datann$Base.Time))
datann$Post.Length=(datann$Post.Length-min(datann$Post.Length))/(max(datann$Post.Length)-min(datann$Post.Length))
datann$Post.Share.Count=(datann$Post.Share.Count-min(datann$Post.Share.Count))/(max(datann$Post.Share.Count)-min(datann$Post.Share.Count))
datann$H.local=(datann$H.local-min(datann$H.local))/(max(datann$H.local)-min(datann$H.local))

head(datann)

#creating training & testing data

set.seed(123)
id=sample(2,nrow(datann),prob = c(0.7,0.3),replace=T)
data_train=datann[id==1,]
data_test=datann[id==2,]

#to create a Model with 1 neuron

set.seed(123)
model=neuralnet(Target.Variable~Max.SD+Avg.Median+CC2+CC3+Base.Time+Post.Length+Post.Share.Count+H.local, data=data_train,hidden = 1,err.fct = "sse",act.fct = "tanh", linear.output = T) 

plot(model)

output=compute(model,data_test[,-9])

head(output$net.result)

# Actual and Predicted values
actual = data_test$Target.Variable
predicted = output$net.result

# Calculating error
error = actual - predicted

# Example of invocation of functions
rmse(error)
mae(error)

# Calculation of R square

rss = sum((predicted - actual) ^ 2)  ## residual sum of squares
tss = sum((actual - mean(actual)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss
rsq

# Checking on training dataset

output=compute(model,data_train[,-9])

head(output$net.result)

# Actual and Predicted values
actual = data_train$Target.Variable
predicted = output$net.result

# Calculating error
error = actual - predicted

# Example of invocation of functions
rmse(error)
mae(error)

# Calculation of R square

rss = sum((predicted - actual) ^ 2)  ## residual sum of squares
tss = sum((actual - mean(actual)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss
rsq

#to create a Model with 2 neuron

set.seed(123)
model=neuralnet(Target.Variable~Max.SD+Avg.Median+CC2+CC3+Base.Time+Post.Length+Post.Share.Count+H.local, data=data_train,hidden = 2,err.fct = "sse",act.fct = "tanh", linear.output = T, stepmax=1e6) 

plot(model)

output=compute(model,data_test[,-9])

head(output$net.result)

# Actual and Predicted values
actual = data_test$Target.Variable
predicted = output$net.result

# Calculating error
error = actual - predicted

# Example of invocation of functions
rmse(error)
mae(error)

# Calculation of R square

rss = sum((predicted - actual) ^ 2)  ## residual sum of squares
tss = sum((actual - mean(actual)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss
rsq

# Checking on training dataset

output=compute(model,data_train[,-9])

head(output$net.result)

# Actual and Predicted values
actual = data_train$Target.Variable
predicted = output$net.result

# Calculating error
error = actual - predicted

# Example of invocation of functions
rmse(error)
mae(error)

# Calculation of R square

rss = sum((predicted - actual) ^ 2)  ## residual sum of squares
tss = sum((actual - mean(actual)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss
rsq


# Ensemble techinique (Stacked Generalization)

setwd("E:\\PGP BA - greatlearning\\Capstone Project\\Project - Group 1\\4. Facebook Comment Volume Prediction\\Dataset")
mydata = read.csv("modeldata_noout.csv")
mydata = mydata[,-1]

# dividing training and testsing dataset in 70:30 ratio
set.seed(123)
id = sample(2,nrow(mydata),prob = c(0.7,0.3), replace = T)

fb_train = mydata[id==1,]
fb_test = mydata[id==2,]

# Running Random forest in train data
library(mlbench)
library(randomForest)
library(caret)

mtry=tuneRF(fb_train[,-9],fb_train$Target.Variable,ntreeTry = 401,stepFactor = 1.5,improve = 0.01,trace = T,plot=T) 

rf=randomForest(Target.Variable~.,data = fb_train,mtry=4,ntree=401,importance=T)
print(rf)

varImpPlot(rf)

yhat1_RF=predict(rf,newdata = fb_train[,-9],type = "response")

# Function that returns Root Mean Squared Error
rmse = function(error)
{
  sqrt(mean(error^2))
}

# Function that returns Mean Absolute Error
mae = function(error)
{
  mean(abs(error))
}

# Actual and Predicted value
actual = fb_train$Target.Variable
predicted = yhat1_RF

# Calculating error
error = actual - predicted

# Calculating RMSE and MAE
rmse(error)
mae(error)

# Running CART on train data

library(caTools)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(data.table)
library(ROCR)
library(ineq)

r.ctrl=rpart.control(minsplit = 1000, cp=0,xval = 10)
m1=rpart(formula = Target.Variable~Max.SD+Avg.Median+CC2+CC3+Base.Time+Post.Length+Post.Share.Count+H.local, data = fb_train,method = "anova")

yhat2_CART=predict(m1,fb_train,type="matrix")

# Actual and Predicted values
actual = fb_train$Target.Variable
predicted = yhat2_CART

# Calculating error
error = actual - predicted

# Calculating RMSE and MAE
rmse(error)
mae(error)

# Calculation of R square

rss = sum((predicted - actual) ^ 2)  ## residual sum of squares
tss = sum((actual - mean(actual)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss
rsq

# Running Knn on train data
library(class)
# install.packages("FNN")

datann = fb_train

datann$Max.SD=(datann$Max.SD-min(datann$Max.SD))/(max(datann$Max.SD)-min(datann$Max.SD))
datann$Avg.Median=(datann$Avg.Median-min(datann$Avg.Median))/(max(datann$Avg.Median)-min(datann$Avg.Median))
datann$CC2=(datann$CC2-min(datann$CC2))/(max(datann$CC2)-min(datann$CC2))
datann$CC3=(datann$CC3-min(datann$CC3))/(max(datann$CC3)-min(datann$CC3))
datann$Base.Time=(datann$Base.Time-min(datann$Base.Time))/(max(datann$Base.Time)-min(datann$Base.Time))
datann$Post.Length=(datann$Post.Length-min(datann$Post.Length))/(max(datann$Post.Length)-min(datann$Post.Length))
datann$Post.Share.Count=(datann$Post.Share.Count-min(datann$Post.Share.Count))/(max(datann$Post.Share.Count)-min(datann$Post.Share.Count))
datann$H.local=(datann$H.local-min(datann$H.local))/(max(datann$H.local)-min(datann$H.local))

model_knn_train = FNN::knn.reg(train = datann, test = datann, y = datann$Target.Variable, k = 5)
yhat3_KNN = model_knn_train$pred
yhat3_KNN = data.frame(yhat3_KNN)

# Actual and Predicted values
actual = datann$Target.Variable
Predicted_KNN = yhat3_KNN

# Calculating error
error = actual - Predicted_KNN

# Calculating RMSE and MAE
rmse(error)
mae(error)

# Calculation of R square

rss = sum((Predicted_KNN - actual) ^ 2)  ## residual sum of squares
tss = sum((actual - mean(actual)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss
rsq

# Creating new train data using predictions of Random forest, CART & Knn as independent variable
# Dependent variable will be Target variable 

newtraindata = cbind(yhat1_RF,yhat2_CART,yhat3_KNN,fb_train$Target.Variable)
colnames(newtraindata)
colnames(newtraindata)[4] = "Target.Variable"

write.csv(newtraindata, file = "E:/PGP BA - greatlearning/Capstone Project/Project - Group 1/4. Facebook Comment Volume Prediction/Dataset/newtraindata.csv")


# Creating new test dataset

# Running Random Forest model rf on test data

testyhat1_RF=predict(rf,newdata = fb_test[,-9],type = "response")

# Running CART model m1 on test data

testyhat2_CART=predict(m1,fb_test,type="matrix")


# Running Knn on test data
library(class)
# install.packages("FNN")

datann = fb_test

datann$Max.SD=(datann$Max.SD-min(datann$Max.SD))/(max(datann$Max.SD)-min(datann$Max.SD))
datann$Avg.Median=(datann$Avg.Median-min(datann$Avg.Median))/(max(datann$Avg.Median)-min(datann$Avg.Median))
datann$CC2=(datann$CC2-min(datann$CC2))/(max(datann$CC2)-min(datann$CC2))
datann$CC3=(datann$CC3-min(datann$CC3))/(max(datann$CC3)-min(datann$CC3))
datann$Base.Time=(datann$Base.Time-min(datann$Base.Time))/(max(datann$Base.Time)-min(datann$Base.Time))
datann$Post.Length=(datann$Post.Length-min(datann$Post.Length))/(max(datann$Post.Length)-min(datann$Post.Length))
datann$Post.Share.Count=(datann$Post.Share.Count-min(datann$Post.Share.Count))/(max(datann$Post.Share.Count)-min(datann$Post.Share.Count))
datann$H.local=(datann$H.local-min(datann$H.local))/(max(datann$H.local)-min(datann$H.local))

model_knn_test = FNN::knn.reg(train = datann, test = datann, y = datann$Target.Variable, k = 5)
testyhat3_KNN = model_knn_test$pred
testyhat3_KNN = data.frame(testyhat3_KNN)

# Creating new test data using predictions of Random forest, CART & Knn as independent variable
# Dependent variable will be Target variable 

newtestdata = cbind(testyhat1_RF,testyhat2_CART,testyhat3_KNN,fb_test$Target.Variable)
newtestdata = data.frame(newtestdata)
colnames(newtestdata)

# Changing column names as per new train data
colnames(newtestdata)[which(names(newtestdata) == "V4")] = "Target.Variable"
colnames(newtestdata)[which(names(newtestdata) == "testyhat1_RF")] = "yhat1_RF"
colnames(newtestdata)[which(names(newtestdata) == "testyhat2_CART")] = "yhat2_CART"
colnames(newtestdata)[which(names(newtestdata) == "testyhat3_KNN")] = "yhat3_KNN"

write.csv(newtestdata, file = "E:/PGP BA - greatlearning/Capstone Project/Project - Group 1/4. Facebook Comment Volume Prediction/Dataset/newtestdata.csv")

#Applying Random Forest in newtraindata

# Running Random Forest
newtraindata[!complete.cases(newtraindata),]

mtry=tuneRF(newtraindata[,-4],newtraindata$Target.Variable,ntreeTry = 501,stepFactor = 1.5,improve = 0.01,trace = T,plot=T) 

rf_final=randomForest(Target.Variable~.,data = newtraindata,mtry=1,ntree=501,importance=T)
print(rf_final)

pred=predict(rf_final,newdata = newtestdata[,-4],type = "response")

# Actual and Predicted values
actual = newtestdata$Target.Variable
predicted = pred

# Calculating error
error = actual - predicted

# Calculating RMSE and MAE
rmse(error)
mae(error)


# Creating csv file to do manual check of actual vs prediction
predictioncheck = cbind(fb_test$Target.Variable,pred)
write.csv(predictioncheck, file = "E:/PGP BA - greatlearning/Capstone Project/Project - Group 1/4. Facebook Comment Volume Prediction/Dataset/predictioncheck.csv")


# Checking the model performance on train data
pred2=predict(rf,newdata = newtraindata[,-4],type = "response")

# Actual and Predicted values
actual = newtraindata$Target.Variable
predicted = pred2

# Calculating error
error = actual - predicted

# Calculating RMSE and MAE
rmse(error)
mae(error)


