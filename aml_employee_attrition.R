#####################################

# Author: Tan Jun Sheng (Jason)
# Applied Machine Learning (AML)
# Topic: Building a reliable predictive model to predict employee attrition
# Task: Predict employee Attrition
# Model Implemented: Logistic Regression, Decision Tree & Naive Bayes
# Conclusion: Decision Tree has the best performance in terms of predicting employee attrition 

#####################################

#######Import / load dataset into RStudio##########
data <- read.csv(file.choose(),header = T)

#Replace Blank value as NA
data <- mutate_all(data, na_if, "") 


#######Removing irrelevant Features###############
# Remove irrelavant features (DataFrame Operations)
# Variables that are not a useful feature for this analysis will be dropped.
data <- select(data, -c(EmployeeCount,EmployeeNumber,Application.ID,StandardHours,Over18))
names(data)

########Remove wrong rows############
#This two rows are wrongly entered, the data is not consistent with the dataset
data <- data[-c(15656,17028), ]
data[c(15656,17028), ]

####################################

#Data Cleaning & Data Structure Conversion

####################################
#Some variables are wrongly loaded as character
#Features below are loaded and wrongly read as character
data$HourlyRate <- as.integer(data$HourlyRate)
data$MonthlyIncome <- as.integer(data$MonthlyIncome)
data$PercentSalaryHike <- as.integer(data$PercentSalaryHike)

#Some variables are read as "char" when the data was loaded
#Need to convert them into "factors"
#Dplyr package was loaded 
library(dplyr)
data <- data %>% mutate_if(is.character,as.factor)

#Some of the encoded labels are loaded as Integer
data$Education <- as.factor(data$Education)
data$EnvironmentSatisfaction <- as.factor(data$EnvironmentSatisfaction)
data$JobInvolvement <- as.factor(data$JobInvolvement)
data$JobSatisfaction<- as.factor(data$JobSatisfaction)
data$PerformanceRating <- as.factor(data$PerformanceRating)
data$RelationshipSatisfaction <- as.factor(data$RelationshipSatisfaction)
data$WorkLifeBalance<- as.factor(data$WorkLifeBalance)
data$JobLevel<- as.factor(data$JobLevel)
data$StockOptionLevel <- as.factor(data$StockOptionLevel)
str(data)

#Check missing values
colSums(is.na(data))
plot_missing(data) 

# Imputing missing values in continuous variables using mean
# Continuous Variables are involved

data$Age = ifelse(is.na(data$Age),
                    ave(data$Age, FUN = function(x) mean(x, na.rm = TRUE)),
                    data$Age)

data$DailyRate = ifelse(is.na(data$DailyRate),
                          ave(data$DailyRate, FUN = function(x) mean(x, na.rm = TRUE)),
                          data$DailyRate)

data$HourlyRate = ifelse(is.na(data$HourlyRate),
                           ave(data$HourlyRate, FUN = function(x) mean(x, na.rm = TRUE)),
                           data$HourlyRate)

data$MonthlyIncome = ifelse(is.na(data$MonthlyIncome),
                              ave(data$MonthlyIncome, FUN = function(x) mean(x, na.rm = TRUE)),
                              data$MonthlyIncome)

data$MonthlyRate = ifelse(is.na(data$MonthlyRate),
                            ave(data$MonthlyRate, FUN = function(x) mean(x, na.rm = TRUE)),
                            data$MonthlyRate)

data$NumCompaniesWorked = ifelse(is.na(data$NumCompaniesWorked),
                                   ave(data$NumCompaniesWorked, FUN = function(x) mean(x, na.rm = TRUE)),
                                   data$NumCompaniesWorked)

data$PercentSalaryHike= ifelse(is.na(data$PercentSalaryHike),
                                 ave(data$PercentSalaryHike, FUN = function(x) mean(x, na.rm = TRUE)),
                                 data$PercentSalaryHike)

data$YearsAtCompany = ifelse(is.na(data$YearsAtCompany),
                               ave(data$YearsAtCompany, FUN = function(x) mean(x, na.rm = TRUE)),
                               data$YearsAtCompany)

data$YearsInCurrentRole = ifelse(is.na(data$YearsInCurrentRole),
                                   ave(data$YearsInCurrentRole, FUN = function(x) mean(x, na.rm = TRUE)),
                                   data$YearsInCurrentRole)

data$YearsSinceLastPromotion= ifelse(is.na(data$YearsSinceLastPromotion),
                                       ave(data$YearsSinceLastPromotion, FUN = function(x) mean(x, na.rm = TRUE)),
                                       data$YearsSinceLastPromotion)

data$YearsWithCurrManager= ifelse(is.na(data$YearsWithCurrManager),
                                    ave(data$YearsWithCurrManager, FUN = function(x) mean(x, na.rm = TRUE)),
                                    data$YearsWithCurrManager)

data$TrainingTimesLastYear= ifelse(is.na(data$TrainingTimesLastYear),
                                    ave(data$TrainingTimesLastYear, FUN = function(x) mean(x, na.rm = TRUE)),
                                    data$TrainingTimesLastYear)

#After imputation of continuous variables - view Missing Values again
colSums(is.na(data))

#Imputation of categorical variables using mode function

Mode <- function(x) {
  ux <- sort(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}
data['Attrition'] <- lapply(data['Attrition'], function(x)
  replace(x,is.na(x), Mode(x[!is.na(x)])))

Mode <- function(x) {
  ux <- sort(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}
data['BusinessTravel'] <- lapply(data['BusinessTravel'], function(x)
  replace(x,is.na(x), Mode(x[!is.na(x)])))

Mode <- function(x) {
  ux <- sort(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}
data['Department'] <- lapply(data['Department'], function(x)
  replace(x,is.na(x), Mode(x[!is.na(x)])))

Mode <- function(x) {
  ux <- sort(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}
data['DistanceFromHome'] <- lapply(data['DistanceFromHome'], function(x)
  replace(x,is.na(x), Mode(x[!is.na(x)])))

Mode <- function(x) {
  ux <- sort(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}
data['Education'] <- lapply(data['Education'], function(x)
  replace(x,is.na(x), Mode(x[!is.na(x)])))

Mode <- function(x) {
  ux <- sort(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}
data['EducationField'] <- lapply(data['EducationField'], function(x)
  replace(x,is.na(x), Mode(x[!is.na(x)])))

Mode <- function(x) {
  ux <- sort(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}
data['EnvironmentSatisfaction'] <- lapply(data['EnvironmentSatisfaction'], function(x)
  replace(x,is.na(x), Mode(x[!is.na(x)])))

Mode <- function(x) {
  ux <- sort(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}
data['Gender'] <- lapply(data['Gender'], function(x)
  replace(x,is.na(x), Mode(x[!is.na(x)])))

Mode <- function(x) {
  ux <- sort(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}
data['JobInvolvement'] <- lapply(data['JobInvolvement'], function(x)
  replace(x,is.na(x), Mode(x[!is.na(x)])))

Mode <- function(x) {
  ux <- sort(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}
data['JobLevel'] <- lapply(data['JobLevel'], function(x)
  replace(x,is.na(x), Mode(x[!is.na(x)])))

Mode <- function(x) {
  ux <- sort(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}
data['JobRole'] <- lapply(data['JobRole'], function(x)
  replace(x,is.na(x), Mode(x[!is.na(x)])))

Mode <- function(x) {
  ux <- sort(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}
data['JobSatisfaction'] <- lapply(data['JobSatisfaction'], function(x)
  replace(x,is.na(x), Mode(x[!is.na(x)])))

Mode <- function(x) {
  ux <- sort(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}
data['MaritalStatus'] <- lapply(data['MaritalStatus'], function(x)
  replace(x,is.na(x), Mode(x[!is.na(x)])))

Mode <- function(x) {
  ux <- sort(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}
data['OverTime'] <- lapply(data['OverTime'], function(x)
  replace(x,is.na(x), Mode(x[!is.na(x)])))

Mode <- function(x) {
  ux <- sort(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}
data['PerformanceRating'] <- lapply(data['PerformanceRating'], function(x)
  replace(x,is.na(x), Mode(x[!is.na(x)])))

Mode <- function(x) {
  ux <- sort(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}
data['RelationshipSatisfaction'] <- lapply(data['RelationshipSatisfaction'], function(x)
  replace(x,is.na(x), Mode(x[!is.na(x)])))

Mode <- function(x) {
  ux <- sort(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}
data['StockOptionLevel'] <- lapply(data['StockOptionLevel'], function(x)
  replace(x,is.na(x), Mode(x[!is.na(x)])))

Mode <- function(x) {
  ux <- sort(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}
data['TotalWorkingYears'] <- lapply(data['TotalWorkingYears'], function(x)
  replace(x,is.na(x), Mode(x[!is.na(x)])))

Mode <- function(x) {
  ux <- sort(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}
data['WorkLifeBalance'] <- lapply(data['WorkLifeBalance'], function(x)
  replace(x,is.na(x), Mode(x[!is.na(x)])))

Mode <- function(x) {
  ux <- sort(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}
data['Employee.Source'] <- lapply(data['Employee.Source'], function(x)
  replace(x,is.na(x), Mode(x[!is.na(x)])))

#After imputation of categorical variables - view Missing Values again
colSums(is.na(data))
plot_missing(data)

#Feature Selection -Which features are important?
install.packages("Boruta")
library(Boruta)
data_boruta <- Boruta(Attrition ~ ., data = data, doTrace = 2)
print(data_boruta)
plot(data_boruta, las = 2, cex.axis = 0.7)
attStats(data_boruta)

# Checking Class distribution 
table(data$Attrition) 
prop.table(table(data$Attrition)) 
barplot(prop.table(table(data$Attrition)),
        col = rainbow(2),
        ylim = c(0, 0.8),
        main = "class Distribution")

###########Encode Target Variable########### 
#One Hot Encoding
data$Attrition <- factor(data$Attrition, 
                         levels=c("Current employee","Voluntary Resignation"), 
                         labels=c("0", "1"))
levels(data$Attrition)

#######Exploratory Data Analysis (EDA)########
summary(data)
names(data)
head(data)
dim(data)
str(data)

library(ggplot2)
########Age distribution###########
plot_histogram(data$Age, colors(blue)) 
hist_test <- ggplot(data = data, aes(x = Age)) 
hist_test + geom_histogram(binwidth = 5, color = "darkslategray",
                 fill = "darkslategray4", alpha = 0.5) +
            ggtitle("Employee Age Distribution") +
            labs(y = "Number of Employees", x = "Age")

########Monthly Income distribution######
#Univariate Analysis
plot_histogram(data$MonthlyIncome)

library(tidyverse)
#########Bivariate analysis (BusinessTravel and gender)#######
bar <- ggplot(data, aes(x = BusinessTravel, fill = Gender))
bar + geom_bar() + theme_light() +
  labs(y = "Business Travel Count", title = "Employee Business Travel")

#######Bivariate analysis (Marital Status and Attrition)######
bar1 <- ggplot(data, aes(x = Attrition, fill = MaritalStatus))
bar1 + geom_bar() + theme_light() +
  labs(y = "Attrition Count", title = "Employee Attrition by MaritalStatus")

#######Check Correlation between variables######
library(corrplot)
install.packages("corrplot")
corrplot(cor(sapply(data_final,as.integer)),method = "pie")

#################Randomize the data###################
#Dataset has to be randomised as all the data are in orders
#Ex: The first half of the dataset are all male and the second half all female
#ML model would face issue if this is not randomised
set.seed(123)
data_final <- data[sample(1:nrow(data), 23434), ]

#Data Partition 
#Splitting the data
library(caTools)

################################
#EXPERIMENT1 - ORIGINAL DATASET
#Stratified sampling method
# 70% TRAINING SET, 30% TEST SET
################################
set.seed(123)
split = sample.split(data_final$Attrition, SplitRatio = 0.7)
training_set = subset(data_final, split == TRUE)
test_set = subset(data_final, split == FALSE)

#Check class distribution
table(data$Attrition)
prop.table(table(data$Attrition))
prop.table(table(training_set$Attrition))
prop.table(table(test_set$Attrition))

########################################################
#LOGISTIC REGRESSION
#######################################################
# Building classifier ORIGINAL DATASET
# LOGISTIC REGRESSION
classifier = glm(Attrition ~.,
                 training_set,
                 family = binomial)
summary(classifier)

####Predicting the Training set results###############
#TRAIN THE MODEL
pred_prob_training <- predict(classifier, type = 'response', training_set[ ,-2])
pred_prob_training
pred_class_training = ifelse(pred_prob_training > 0.5, 1, 0)
pred_class_training
cbind(pred_prob_training, pred_class_training)
cm_training = table(training_set$Attrition, pred_class_training)
cm_training

# Evaluation metrics using formula
# accuracy = (cm[1,1] + cm[2,2])/ (cm[1,1] + cm [1,2] + cm [2,1] +cm [2,2])
# accuracy rate

accuracy_training <- sum(diag(cm_training))/sum(cm_training)
accuracy_training

# Predicting the Test set results
#TEST THE MODEL 
pred_prob_test <- predict(classifier, type = 'response', test_set[ ,-2] )
pred_prob_test
pred_class_test = ifelse(pred_prob_test > 0.5, 1, 0)
pred_class_test
cm_test = table(test_set$Attrition, pred_class_test)
cbind(pred_prob_training, pred_class_training)
cm_test

# Evaluation metrics using formula
# accuracy = (cm[1,1] + cm[2,2])/ (cm[1,1] + cm [1,2] + cm [2,1] +cm [2,2])
# accuracy

accuracy_test <- sum(diag(cm_test))/sum(cm_test)
accuracy_test 

#CHECK MODEL FIT

#Precision: tp/(tp+fp)
cm_test[1,1] / sum(cm_test[1,1:2])

#Recall: tp/(tp+fn):
cm_test[1,1] / sum(cm_test[1:2,1])

#Formula for F-Score
2 * 0.9825925 * 0.8623554 / (0.9825925 + 0.8623554)

#Finding the sensitivity and specificity rate 
library(caret)
confusionMatrix(factor(pred_class_test), factor(test_set$Attrition), positive = "1")

# ROC curve on test set
# install.packages("ROCR")
library(ROCR)
library(ggplots)

# To draw ROC we need to predict the prob values. 
pred = prediction(pred_prob_test, test_set$Attrition)
perf = performance(pred, "tpr", "fpr")
pred
perf
plot(perf, colorize = T)
plot(perf, colorize=T, 
     main = "ROC curve",
     ylab = "Sensitivity",
     xlab = "1 - Specificity",
     print.cutoffs.at=seq(0,1,0.3),
     text.adj= c(-0.2,1.7))

# Area Under Curve
auc <- as.numeric(performance(pred, "auc")@y.values)
auc <-  round(auc, 3)
auc

#########################################################
#EXPERIMENT2 - Over-Sampling DATASET (23,434 sample)
#LOGISTIC REGRESSION
########################################################
#As original dataset suffer from class imbalanced
#Re-sampling is needed and experimented 

#Install and Load library ROSE (Random Over-Sampling Examples)
install.packages("ROSE")
library(ROSE)
set.seed(123)
data_over <- ovun.sample(Attrition ~., data = training_set, method = 
                           "over", N = 23434)$data
table(data_over$Attrition)
prop.table(table(data_over$Attrition)) 
barplot(prop.table(table(data_over$Attrition)),
        col = rainbow(2),
        ylim = c(0, 0.8),
        main = "class Distribution")

# Stratified sampling method
set.seed(123)
split_over = sample.split(data_over$Attrition, SplitRatio = 0.7)
training_set_over = subset(data_over, split == TRUE)
test_set_over = subset(data_over, split == FALSE)

#Check class distribution for Re-sammpling dataset
table(data_over$Attrition)
prop.table(table(data_over$Attrition))
prop.table(table(training_set_over$Attrition))
prop.table(table(test_set_over$Attrition))

#######################################
# Building classifier Resammpling dataset
# LOGISTIC REGRESSION
######################################
classifier_over = glm(Attrition ~.,
                 training_set_over,
                 family = binomial)
summary(classifier_over)

# Predicting the Training set results
pred_prob_training_over <- predict(classifier, type = 'response', training_set_over[ ,-2])
pred_prob_training_over
pred_class_training_over <- ifelse(pred_prob_training_over > 0.5, 1, 0)
pred_class_training_over
cbind(pred_prob_training_over, pred_class_training_over)
#confusion matrix
cm_training_over = table(training_set_over$Attrition, pred_class_training_over)
cm_training_over

# Evaluation metrics using formula
# accuracy = (cm[1,1] + cm[2,2])/ (cm[1,1] + cm [1,2] + cm [2,1] +cm [2,2])
# accuracy

accuracy_training_over <- sum(diag(cm_training_over))/sum(cm_training_over)
accuracy_training_over

# Predicting the Test set results
pred_prob_test_over <- predict(classifier, type = 'response', test_set_over[ ,-2] )
pred_prob_test_over
pred_class_test_over = ifelse(pred_prob_test_over > 0.5, 1, 0)
pred_class_test_over
cm_test_over = table(test_set_over$Attrition, pred_class_test_over)
cbind(pred_prob_training_over, pred_class_training_over)
cm_test_over

# Evaluation metrics using formula
# accuracy = (cm[1,1] + cm[2,2])/ (cm[1,1] + cm [1,2] + cm [2,1] +cm [2,2])
# accuracy

accuracy_test_over <- sum(diag(cm_test_over))/sum(cm_test_over)
accuracy_test_over 

# To draw ROC we need to predict the prob values. 
pred_over = prediction(pred_prob_test_over, test_set_over$Attrition)
perf_over = performance(pred_over, "tpr", "fpr")
pred_over
perf_over
plot(perf_over, colorize = T)
plot(perf_over, colorize=T, 
     main = "ROC curve",
     ylab = "Sensitivity",
     xlab = "1 - Specificity",
     print.cutoffs.at=seq(0,1,0.3),
     text.adj= c(-0.2,1.7))

# Area Under Curve
auc <- as.numeric(performance(pred_over, "auc")@y.values)
auc <-  round(auc, 3)
auc

#Precision: tp/(tp+fp)
cm_test_over[1,1] / sum(cm_test_over[1,1:2])

#Recall: tp/(tp+fn):
cm_test_over[1,1] / sum(cm_test_over[1:2,1])

#Formula for F-Score: 2 * precision * recall /(precision + recall)
2 * 0.9810034 * 0.7293138 / (0.9810034 + 0.7293138) 

#Finding the sensitivity and specificity rate for the re-sampling data
confusionMatrix(factor(pred_class_test_over), factor(test_set_over$Attrition), positive = "1")

###################################################

#MODEL EXPERIMENTATION 2: DECISION TREE

##################################################

#EXPERIMENT1 - ORIGINAL DATASET
# Stratified sampling method

##################################################
set.seed(123)
split = sample.split(data_final$Attrition, SplitRatio = 0.7)
training_set = subset(data_final, split == TRUE)
test_set = subset(data_final, split == FALSE)

# install.packages('rpart') --> (Recursive Partitioning And Regression Trees) 
# and the R implementation of the CART algorithm
# install.packages("rpart.plot") & load package to implement the model

library(rpart)
library(rpart.plot)
library(party)

tree = rpart(Attrition~ ., data=training_set)
tree
prp(tree) # plot Rpart Model
prp (tree, type = 5, extra = 100)
rpart.plot(tree, extra = 101, nn = TRUE)
plotcp(tree)

# Predict and evaluate the performance of the trained tree model 
tree_predict = predict(tree, test_set, type = "class")
# Now examine the values of Predict. These are the class probabilities
tree_predict


# Producing confusion matrix
tree_confusion_matrix = table(tree_predict, test_set$Attrition)
tree_confusion_matrix

# Calculating the accuracy - confusion matrix (CM)
tree_accuracy = sum(diag(tree_confusion_matrix))/sum(tree_confusion_matrix)
tree_accuracy

# To draw ROC we need to predict the prob values. So we run predict again
# Note that Predict ROC is same as Predict with "type = prob"

tree_predict_ROC = predict(tree, test_set)
tree_predict_ROC 
tree_predict_ROC [,2]

tree_pred = prediction(tree_predict_ROC[,2], test_set$Attrition)
tree_perf = performance(tree_pred, "tpr", "fpr")
tree_pred 
tree_perf 
plot(tree_perf, colorize = T)
plot(tree_perf, colorize=T, 
     main = "ROC curve",
     ylab = "Sensitivity",
     xlab = "1 - Specificity",
     print.cutoffs.at=seq(0,1,0.3),
     text.adj= c(-0.2,1.7))

# Area Under Curve (AUC)
auc = as.numeric(performance(tree_pred, "auc")@y.values)
auc = round(auc, 3)
auc

Precision: tp/(tp+fp)
tree_confusion_matrix[1,1] / sum(tree_confusion_matrix[1,1:2])

#Recall: tp/(tp+fn):
tree_confusion_matrix[1,1] / sum(tree_confusion_matrix[1:2,1])

#F-Score: 2 * precision * recall /(precision + recall)
2 * 0.870987* 0.9858036/ (0.870987+ 0.9858036)

#To find the sensitivity and specificity rate for the re-sampling data
confusionMatrix(factor(tree_predict), factor(test_set$Attrition), positive = "1")

############################################################
#EXPERIMENT2 - OVERSAMPLING DATASET (19,725 Observations)
#NOT DOCUMENTED IN THE AML ASSIGNMENT PROJECT (JUST FOR EXPERIMENT IN R CODE)
############################################################

# Note that this is Startified sampling method
set.seed(123)
split_over = sample.split(data_over$Attrition, SplitRatio = 0.7)
training_set_over = subset(data_over, split == TRUE)
test_set_over = subset(data_over, split == FALSE)

tree = rpart(Attrition~ ., data=training_set_over)
tree
prp(tree) # plot Rpart Model
prp (tree, type = 5, extra = 100)
rpart.plot(tree, extra = 101, nn = TRUE)
plotcp(tree)

# Now we predict and evaluate the performance of the trained tree model 
tree_predict = predict(tree, test_set_over, type = "class")
# Now examine the values of Predict. These are the class probabilities
tree_predict

# Producing confusion matrix
tree_confusion_matrix = table(tree_predict, test_set_over$Attrition)
tree_confusion_matrix

# Calculating the accuracy using the cofusion matrix
tree_accuracy = sum(diag(tree_confusion_matrix))/sum(tree_confusion_matrix)
tree_accuracy

# To draw ROC we need to predict the prob values. So we run predict again
# Note that PredictROC is same as Predict with "type = prob"

tree_predict_ROC = predict(tree, test_set_over)
tree_predict_ROC 
tree_predict_ROC [,2]

tree_pred = prediction(tree_predict_ROC[,2], test_set_over$Attrition)
tree_perf = performance(pred, "tpr", "fpr")
tree_pred 
tree_perf 
plot(tree_perf, colorize = T)
plot(tree_perf, colorize=T, 
     main = "ROC curve",
     ylab = "Sensitivity",
     xlab = "1 - Specificity",
     print.cutoffs.at=seq(0,1,0.3),
     text.adj= c(-0.2,1.7))

# Area Under Curve
auc = as.numeric(performance(tree_pred, "auc")@y.values)
auc = round(auc, 3)
auc

#Precision: tp/(tp+fp)
tree_confusion_matrix[1,1] / sum(tree_confusion_matrix[1,1:2])

#Recall: tp/(tp+fn):
tree_confusion_matrix[1,1] / sum(tree_confusion_matrix[1:2,1])

#F-Score: 2 * precision * recall /(precision + recall)
2 * 0.7835716* 0.9478811/ (0.7835716+ 0.9478811)

#Finding the sensitivity and specificity rate for the re-sampling data
confusionMatrix(factor(tree_predict), factor(test_set_over$Attrition), positive = "1")

##################################################################

#Experimentation 3 (Parameter Tuning on original dataset, 23,000+)
#DOCUMENTED IN AML ASSIGNMENT / PROJECT

##################################################################

accuracy_tuning <- function(tree) {
  tree_pred_tune <- predict(tree, test_set, type = 'class')
  tree_cm <- table(test_set$Attrition, tree_pred_tune)
  accuracy_test_tree <- sum(diag(tree_cm))/ sum(tree_cm)
  accuracy_test_tree
}

control_tree <- rpart.control(misplit = 4,
                              minbucket = round(5 / 3),
                              maxdepth = 3,
                              cp = 0)
tree_tune_fit <- rpart(Attrition~., data = training_set, method = 'class',
                       control = control_tree)
accuracy_tuning(tree_tune_fit)

#####################################################################
#Experimentation 4 (Parameter Tuning on Oversampling dataset, 19,725)
#NOT DOCUMENTED IN THE AML ASSIGNMENT PROJECT (JUST FOR EXPERIMENT IN R CODE)
####################################################################
accuracy_tuning_over <- function(tree) {
  tree_pred_tune_over <- predict(tree, test_set_over, type = 'class')
  tree_cm_over <- table(test_set_over$Attrition, tree_pred_tune_over)
  accuracy_test_tree_over <- sum(diag(tree_cm_over))/ sum(tree_cm_over)
  accuracy_test_tree_over
}

control_tree_over <- rpart.control(misplit = 1,
                              minbucket = 15,
                              maxdepth = 3,
                              cp = -1)
tree_tune_fit_over <- rpart(Attrition~., data = training_set_over, method = 'class',
                       control = control_tree_over)
accuracy_tuning_over(tree_tune_fit_over)

##################################################################

#Experimentation 5 (Parameter on original dataset, 23,000++)
#DOCUMENTED IN AML ASSIGNMENT / PROJECT

##################################################################
# Here we use tree with parameter settings.
# This code generates the tree with training data
tree_with_params = rpart(Attrition ~ ., data=training_set, method="class", minsplit = 1, 
                         minbucket = 10, cp = -1)
prp (tree_with_params)
print(tree_with_params)
summary(tree_with_params)
plot(tree_with_params)
text(tree_with_params)
plotcp(tree_with_params)

# Now we predict and evaluate the performance of the trained tree model 
predict_para_tree = predict(tree_with_params, test_set)
# Now examine the values of Predict. These are the class probabilities
predict_para_tree 
# pred <= predict (mymodel, dataset, type = 'prob')
# To produce classes only, without the probabilities, run the next command.
# By default threshold is set at 0.5 to produce the classes

predict_para_tree = predict(tree_with_params, test_set, type = "class")
predict_para_tree 

# Producing confusion matrix
tree_para_cm = table(predict_para_tree, test_set$Attrition)
tree_para_cm 

# Calculating the accuracy using the cofusion matrix
Accuracy_para = sum(diag(tree_para_cm))/sum(tree_para_cm)
Accuracy_para

# Performance of the DT model
library(caret)
confusionMatrix(predict_para_tree,test_set$Attrition)

tree_predict_para_ROC = predict(tree_with_params, test_set)
tree_predict_para_ROC 
tree_predict_para_ROC [,2]

paratree_pred = prediction(tree_predict_para_ROC[,2], test_set$Attrition)
paratree_perf = performance(paratree_pred, "tpr", "fpr")
paratree_pred 
paratree_perf 
plot(paratree_perf, colorize = T)
plot(paratree_perf, colorize=T, 
     main = "ROC curve",
     ylab = "Sensitivity",
     xlab = "1 - Specificity",
     print.cutoffs.at=seq(0,1,0.3),
     text.adj= c(-0.2,1.7))

# Area Under Curve
auc = as.numeric(performance(paratree_pred, "auc")@y.values)
auc = round(auc, 3)
auc

#Precision: tp/(tp+fp)
tree_para_cm [1,1] / sum(tree_para_cm [1,1:2])

#Recall: tp/(tp+fn):
tree_para_cm [1,1] / sum(tree_para_cm [1:2,1])

#F-Score: 2 * precision * recall /(precision + recall)
2 * 0.9677365* 0.9783674/ (0.9677365+ 0.9783674)

#Finding the sensitivity and specificity rate for the re-sampling data
confusionMatrix(factor(tree_predict), factor(test_set$Attrition), positive = "1")


#########################################################################
#Experimentation 7 (Parameter on oversampling data = BALANCED DATASET)
# Tree with parameter settings.
#DOCUMENTED IN AML ASSIGNMENT / PROJECT
########################################################################

tree_with_params_bal = rpart(Attrition ~ ., data=training_set_bal, method="class", minsplit = 1, 
                             minbucket = 10, cp = -1)
prp (tree_with_params_bal)
print(tree_with_params_bal)
summary(tree_with_params_bal)
plot(tree_with_params_bal)
text(tree_with_params_bal)
plotcp(tree_with_params_bal)

# Predict and evaluate the performance of the trained tree model 
predict_para_tree_bal = predict(tree_with_params_bal, test_set_bal)
# Now examine the values of Predict. These are the class probabilities
predict_para_tree_bal 
# pred <= predict (mymodel, dataset, type = 'prob')
# To produce classes only, without the probabilities, run the next command.
# Threshold is set at 0.5 to produce the classes (BY DEFAULT)

predict_para_tree_bal = predict(tree_with_params_bal, test_set_bal, type = "class")
predict_para_tree_bal 

# Producing confusion matrix (CM)
tree_para_cm = table(predict_para_tree_bal, test_set_bal$Attrition)
tree_para_cm 

# Calculating the accuracy using the confusion matrix
Accuracy_para = sum(diag(tree_para_cm))/sum(tree_para_cm)
Accuracy_para

# Performance of the DT model 
#caret library is read
library(caret)
confusionMatrix(predict_para_tree_bal,test_set_bal$Attrition)

tree_predict_para_ROC = predict(tree_with_params_bal, test_set_bal)
tree_predict_para_ROC 
tree_predict_para_ROC [,2]

paratree_pred = prediction(tree_predict_para_ROC[,2], test_set_bal$Attrition)
paratree_perf = performance(paratree_pred, "tpr", "fpr")
paratree_pred 
paratree_perf 
plot(paratree_perf, colorize = T)
plot(paratree_perf, colorize=T, 
     main = "ROC curve",
     ylab = "Sensitivity",
     xlab = "1 - Specificity",
     print.cutoffs.at=seq(0,1,0.3),
     text.adj= c(-0.2,1.7))

# Area Under Curve (AUC)
auc = as.numeric(performance(paratree_pred, "auc")@y.values)
auc = round(auc, 3)
auc

#Performance evaluation metrics
#Precision: tp/(tp+fp)
tree_para_cm [1,1] / sum(tree_para_cm [1,1:2])

#Recall: tp/(tp+fn):
tree_para_cm [1,1] / sum(tree_para_cm [1:2,1])

#F-Score: 2 * precision * recall /(precision + recall)
2 * 0.9753793* 0.9551875/ (0.9753793+ 0.9551875)

#Finding the sensitivity and specificity rate for the re-sampling data
confusionMatrix(factor(predict_para_tree_bal), factor(test_set_bal$Attrition), positive = "1")

###########################################################
#EXPERIMENT DECISION TREE (CHANGING THE CP VALUE TO 0.0004)
# DOCUMENTED IN AML ASSIGNMENT /PROJECT
###########################################################
tree_with_params_bal = rpart(Attrition ~ ., data=training_set_bal, method="class", minsplit = 1, 
                             minbucket = 10, cp = 0.00041)

#############################################

# THIRD MODEL IMPLEMENTATION
#NAIVE BAYES MODEL

#############################################
#EXPERIMENTATION 1 - PRIGINAL DATASET
#NAIVE BAYES EXPERIMENT 1
#############################################
library(naivebayes)
#basic naive bayes model
Naive_Bayes_basic_hr = naive_bayes(x = training_set[ , -2],
                                   y = training_set$Attrition , laplace = 0 )

# Training set results
y_pred_train_hr = predict (Naive_Bayes_basic_hr, newdata = training_set[ ,-2], type = "prob" )
summary(y_pred_train_hr)
# Predict Class
y_pred_train_class_hr = predict (Naive_Bayes_basic_hr, newdata = training_set[ ,-2], type = "class" )
summary(y_pred_train_class_hr)

# Predicting the Training set results 
cbind(y_pred_train_hr, y_pred_train_class_hr)
cm_training_nb = table(training_set$Attrition, y_pred_train_class_hr)
cm_training_nb

#Conduct accuracy - training set
accuracy_training_nb <- sum(diag(cm_training_nb))/sum(cm_training_nb)
accuracy_training_nb

# Predicting the Test set results (Original Dataset)

y_pred_test_hr = predict(Naive_Bayes_basic_hr, newdata = test_set[ ,-2], type = "prob" )
summary(y_pred_test_hr)
# Predict Class
y_pred_test_class_hr = predict (Naive_Bayes_basic_hr, newdata = test_set[ ,-2], type = "class" )
summary(y_pred_test_class_hr)

cm_test_nb = table(test_set$Attrition, y_pred_test_class_hr)
cbind(y_pred_test_hr, y_pred_test_class_hr)
cm_test_nb

#Accuracy test - test set
accuracy_test_nb <- sum(diag(cm_test_nb))/sum(cm_test_nb)
accuracy_test_nb

#EVALUATE THE MODEL FIT

# ROC curve on test  (ROCR and ggplots needed)
library(ROCR)
library(ggplot2)
pred_nb = prediction(y_pred_test_hr [,2], test_set$Attrition)
perf_nb = performance(pred_nb, "tpr", "fpr")
plot(perf_nb, colorize = T)
plot(perf_nb, colorize=T, 
     main = "ROC curve",
     ylab = "Sensitivity",
     xlab = "1 - Specificity",
     print.cutoffs.at=seq(0,1,0.3),
     text.adj= c(-0.2,1.7))

# Area Under Curve (AUC)
auc <- as.numeric(performance(pred_nb, "auc")@y.values)
auc <-  round(auc, 3)
auc

#Precision: tp/(tp+fp)
cm_test_nb [1,1] / sum(cm_test_nb [1,1:2])

#Recall: tp/(tp+fn):
cm_test_nb[1,1] / sum(cm_test_nb [1:2,1])

#F-Score: 2 * precision * recall /(precision + recall)
2 * 0.8967382* 0.8871426/ (0.8967382+ 0.8871426)

#Finding the sensitivity and specificity rate for the re-sampling data
confusionMatrix(factor(y_pred_test_class_hr), factor(test_set$Attrition), positive = "1")

#################################################################

#NAIVE BAYES EXPERIMENTATION 2 (OVER-SAMPLING DATASET - 19,725)
# NOT DOCUMENTED IN AML PROJECT (JUST FOR EXPEERIMENTING IN RSTUDIO)

#################################################################

#basic naive bayes model
Naive_Bayes_basic_over = naive_bayes(x = training_set_over[ , -2],
                                     y = training_set_over$Attrition , laplace = 0 )

# Training set results
y_pred_train_hr_over = predict (Naive_Bayes_basic_over, newdata = training_set_over[ ,-2], type = "prob" )
summary(y_pred_train_hr_over)
# Predict Class
y_pred_train_class_hr_over = predict (Naive_Bayes_basic_over, newdata = training_set_over[ ,-2], type = "class" )
summary(y_pred_train_class_hr_over)

# Predicting the Training set results 
cbind(y_pred_train_hr_over, y_pred_train_class_hr_over)
cm_training_nb = table(training_set_over$Attrition, y_pred_train_class_hr_over)
cm_training_nb

#Conduct accuracy - training set
accuracy_training_nb <- sum(diag(cm_training_nb))/sum(cm_training_nb)
accuracy_training_nb

# Predicting the Test set results (OVERSAMPLING DATASET)

y_pred_test_hr = predict(Naive_Bayes_basic_over, newdata = test_set_over[ ,-2], type = "prob" )
summary(y_pred_test_hr)
# Predict Class
y_pred_test_class_hr = predict (Naive_Bayes_basic_over, newdata = test_set_over[ ,-2], type = "class" )
summary(y_pred_test_class_hr)

cm_test_nb = table(test_set_over$Attrition, y_pred_test_class_hr)
cbind(y_pred_test_hr, y_pred_test_class_hr)
cm_test_nb

#Accuracy test - test set
accuracy_test_nb <- sum(diag(cm_test_nb))/sum(cm_test_nb)
accuracy_test_nb

# ROC curve on test  (ROCR and ggplots needed)
library(ROCR)
library(ggplot2)
pred_NB = prediction(y_pred_test_hr [,2], test_set_over$Attrition)
perf_NB = performance(pred_NB, "tpr", "fpr")
plot(perf_NB, colorize = T)
plot(perf_NB, colorize=T, 
     main = "ROC curve",
     ylab = "Sensitivity",
     xlab = "1 - Specificity",
     print.cutoffs.at=seq(0,1,0.3),
     text.adj= c(-0.2,1.7))

# Area Under Curve
auc <- as.numeric(performance(pred_NB, "auc")@y.values)
auc <-  round(auc, 3)
auc

#Precision: tp/(tp+fp)
cm_test_nb [1,1] / sum(cm_test_nb [1,1:2])

#Recall: tp/(tp+fn):
cm_test_nb [1,1] / sum(cm_test_nb [1:2,1])

#F-Score: 2 * precision * recall /(precision + recall)
2 * 0.7696055* 0.807152/ (0.7696055+ 0.807152)

#Finding the sensitivity and specificity rate for the re-sampling data
confusionMatrix(factor(y_pred_test_class_hr), factor(test_set_over$Attrition), positive = "1")

#########################################################################
#NAIVE BAYES
#EXPERIMENT3 - Over-Sampling DATASET (23,434 sample - BALANCED DATASET)
#WELL DOCUMENTED AT AML PROJECT
########################################################################

#Install and Load library ROSE (Random Over-Sampling Examples)
install.packages("ROSE")
library(ROSE)
set.seed(123)
data_bal <- ovun.sample(Attrition ~., data = training_set, method = 
                          "over", N = 23434)$data
prop.table(table(data_bal$Attrition)) 
barplot(prop.table(table(data_bal$Attrition)),
        col = rainbow(2),
        ylim = c(0, 0.8),
        main = "class Distribution")

# Note that this is Stratified sampling method
set.seed(123)
split_bal = sample.split(data_bal$Attrition, SplitRatio = 0.7)
training_set_bal = subset(data_bal, split == TRUE)
test_set_bal = subset(data_bal, split == FALSE)

#Check class distribution for Re-sammpling dataset
table(data_over$Attrition)
prop.table(table(data_bal$Attrition))
prop.table(table(training_set_bal$Attrition))
prop.table(table(test_set_bal$Attrition))

##basic naive bayes model (3rd Experimentation - Oversampling 23,436 - Balanced Dataset)
Naive_Bayes_basic_bal = naive_bayes(x = training_set_bal[ , -2],
                                    y = training_set_bal$Attrition , laplace = 0 )

# Training set results
y_pred_train_hr_bal = predict (Naive_Bayes_basic_bal, newdata = training_set_bal[ ,-2], type = "prob" )
summary(y_pred_train_hr_bal)
# Predict Class
y_pred_train_class_hr_bal = predict (Naive_Bayes_basic_bal, newdata = training_set_bal[ ,-2], type = "class" )
summary(y_pred_train_class_hr_bal)

# Predicting the Training set results 
cbind(y_pred_train_hr_bal, y_pred_train_class_hr_bal)
nb_training_cm = table(training_set_bal$Attrition, y_pred_train_class_hr_bal)
nb_training_cm

#Conduct accuracy - training set
accuracy_training_nb <- sum(diag(nb_training_cm))/sum(nb_training_cm)
accuracy_training_nb

# Predicting the Test set results (OVERSAMPLING DATASET - BALANCED DATASET)

y_pred_test_hr_bal = predict(Naive_Bayes_basic_bal, newdata = test_set_bal[ ,-2], type = "prob" )
summary(y_pred_test_hr_bal)
# Predict Class
y_pred_test_class_hr_bal = predict (Naive_Bayes_basic_bal, newdata = test_set_bal[ ,-2], type = "class" )
summary(y_pred_test_class_hr_bal)

nb_cm_test = table(test_set_bal$Attrition, y_pred_test_class_hr_bal)
cbind(y_pred_test_hr_bal, y_pred_test_class_hr_bal)
nb_cm_test

#Accuracy test - test set
accuracy_test_nb <- sum(diag(nb_cm_test))/sum(nb_cm_test)
accuracy_test_nb

# ROC curve on test  (ROCR and ggplots needed)
library(ROCR)
library(ggplot2)
pred_NB = prediction(y_pred_test_hr_bal [,2], test_set_bal$Attrition)
perf_NB = performance(pred_NB, "tpr", "fpr")
plot(perf_NB, colorize = T)
plot(perf_NB, colorize=T, 
     main = "ROC curve",
     ylab = "Sensitivity",
     xlab = "1 - Specificity",
     print.cutoffs.at=seq(0,1,0.3),
     text.adj= c(-0.2,1.7))

#PERFORMANCE EVALUATION METRICS
# Area Under Curve
auc <- as.numeric(performance(pred_NB, "auc")@y.values)
auc <-  round(auc, 3)
auc

#Precision: tp/(tp+fp)
nb_cm_test [1,1] / sum(nb_cm_test [1,1:2])

#Recall: tp/(tp+fn):
nb_cm_test [1,1] / sum(nb_cm_test [1:2,1])

#F-Score: 2 * precision * recall /(precision + recall)
2 * 0.674866* 0.7307489/ (0.674866+ 0.7307489)

#Finding the sensitivity and specificity rate for the re-sampling data
confusionMatrix(factor(y_pred_test_class_hr_bal), factor(test_set_bal$Attrition), positive = "1")




