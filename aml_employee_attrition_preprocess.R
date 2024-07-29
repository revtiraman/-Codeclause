
#####################################

# Author: Tan Jun Sheng (Jason)
# Applied Machine Learning (AML)
# Topic: Building a reliable predictive model to predict employee attrition
# Task: Data analysis &  Predict employee Attrition

#####################################

#Import / load dataset into RStudio
data <- read.csv(file.choose(),header = T)

#View the dataset structure
names(data)
head(data)
dim(data)
str(data)

#Create a copy of the dataset as a backup
data2 <- read.csv(file.choose(),header = T)

# Remove irrelavant features (DataFrame Operations)
#Variables that are not a useful feature for this analysis will be dropped.
#Have to run code by code as it is not concantenated
data <- data[ , -9] # Remove the variable "EmployeeCount"
data <- data[ , -10] # Remove the variable "EmployeeNumber"
data <- data[ , -11] # Remove the variable "Application.ID"
data <- data[ , -28] # Remove the variable "StandardHours"
data <- data[ , -20] # Remove the variable "Over18"
names(data)
names(df_test)
#Data Cleaning
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
str(data)
data$StockOptionLevel <- as.factor(data$StockOptionLevel)

#Check missing values
data_1 <- mutate_all(data, na_if, "") #Replace blank value with NA
colSums(is.na(data_1))
plot_missing(data_1) 

colSums(is.na(data))


#Data Imputation with MICE
colSums(is.na(data_1))
imputed_data <- mice(data, m=3) 
levels(data$Attrition)

# Imputing missing values in continuous variables using mean
# Continuous Variables are involved 
# Age, DailyRate, HourlyRate, MonthlyIncome, MonthlyRate, NumCompaniesWorked, PercentSalaryHike
# StandardHours, TotalWorkingYears, YearsAtCompany, YearsInCurrentRole
# YearsSinceLastPromotion, YearsWithCurrManager

data_1$Age = ifelse(is.na(data_1$Age),
                  ave(data_1$Age, FUN = function(x) mean(x, na.rm = TRUE)),
                  data_1$Age)

data_1$DailyRate = ifelse(is.na(data_1$DailyRate),
                  ave(data_1$DailyRate, FUN = function(x) mean(x, na.rm = TRUE)),
                  data_1$DailyRate)

data_1$HourlyRate = ifelse(is.na(data_1$HourlyRate),
                  ave(data_1$HourlyRate, FUN = function(x) mean(x, na.rm = TRUE)),
                  data_1$HourlyRate)

data_1$MonthlyIncome = ifelse(is.na(data_1$MonthlyIncome),
                  ave(data_1$MonthlyIncome, FUN = function(x) mean(x, na.rm = TRUE)),
                  data_1$MonthlyIncome)

data_1$MonthlyRate = ifelse(is.na(data_1$MonthlyRate),
                           ave(data_1$MonthlyRate, FUN = function(x) mean(x, na.rm = TRUE)),
                           data_1$MonthlyRate)

data_1$NumCompaniesWorked = ifelse(is.na(data_1$NumCompaniesWorked),
                            ave(data_1$NumCompaniesWorked, FUN = function(x) mean(x, na.rm = TRUE)),
                            data_1$NumCompaniesWorked)

data_1$PercentSalaryHike= ifelse(is.na(data_1$PercentSalaryHike),
                              ave(data_1$PercentSalaryHike, FUN = function(x) mean(x, na.rm = TRUE)),
                              data_1$PercentSalaryHike)

data_1$YearsAtCompany = ifelse(is.na(data_1$YearsAtCompany),
                                   ave(data_1$YearsAtCompany, FUN = function(x) mean(x, na.rm = TRUE)),
                                   data_1$YearsAtCompany)

data_1$YearsInCurrentRole = ifelse(is.na(data_1$YearsInCurrentRole),
                                   ave(data_1$YearsInCurrentRole, FUN = function(x) mean(x, na.rm = TRUE)),
                                   data_1$YearsInCurrentRole)

data_1$YearsSinceLastPromotion= ifelse(is.na(data_1$YearsSinceLastPromotion),
                                   ave(data_1$YearsSinceLastPromotion, FUN = function(x) mean(x, na.rm = TRUE)),
                                   data_1$YearsSinceLastPromotion)

data_1$YearsWithCurrManager= ifelse(is.na(data_1$YearsWithCurrManager),
                                       ave(data_1$YearsWithCurrManager, FUN = function(x) mean(x, na.rm = TRUE)),
                                       data_1$YearsWithCurrManager)

#After imputation of continuous variables - view Missing Values again
colSums(is.na(data_1))

#Imputation of categorical variables using mode function

Mode <- function(x) {
  ux <- sort(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}
data_1['Attrition'] <- lapply(data_1['Attrition'], function(x)
  replace(x,is.na(x), Mode(x[!is.na(x)])))

Mode <- function(x) {
  ux <- sort(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}
data_1['BusinessTravel'] <- lapply(data_1['BusinessTravel'], function(x)
  replace(x,is.na(x), Mode(x[!is.na(x)])))

Mode <- function(x) {
  ux <- sort(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}
data_1['Department'] <- lapply(data_1['Department'], function(x)
  replace(x,is.na(x), Mode(x[!is.na(x)])))

Mode <- function(x) {
  ux <- sort(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}
data_1['DistanceFromHome'] <- lapply(data_1['DistanceFromHome'], function(x)
  replace(x,is.na(x), Mode(x[!is.na(x)])))

Mode <- function(x) {
  ux <- sort(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}
data_1['Education'] <- lapply(data_1['Education'], function(x)
  replace(x,is.na(x), Mode(x[!is.na(x)])))

Mode <- function(x) {
  ux <- sort(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}
data_1['EducationField'] <- lapply(data_1['EducationField'], function(x)
  replace(x,is.na(x), Mode(x[!is.na(x)])))

Mode <- function(x) {
  ux <- sort(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}
data_1['EnvironmentSatisfaction'] <- lapply(data_1['EnvironmentSatisfaction'], function(x)
  replace(x,is.na(x), Mode(x[!is.na(x)])))

Mode <- function(x) {
  ux <- sort(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}
data_1['JobInvolvement'] <- lapply(data_1['JobInvolvement'], function(x)
  replace(x,is.na(x), Mode(x[!is.na(x)])))

Mode <- function(x) {
  ux <- sort(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}
data_1['JobLevel'] <- lapply(data_1['JobLevel'], function(x)
  replace(x,is.na(x), Mode(x[!is.na(x)])))

Mode <- function(x) {
  ux <- sort(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}
data_1['JobRole'] <- lapply(data_1['JobRole'], function(x)
  replace(x,is.na(x), Mode(x[!is.na(x)])))

Mode <- function(x) {
  ux <- sort(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}
data_1['JobSatisfaction'] <- lapply(data_1['JobSatisfaction'], function(x)
  replace(x,is.na(x), Mode(x[!is.na(x)])))

Mode <- function(x) {
  ux <- sort(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}
data_1['MaritalStatus'] <- lapply(data_1['MaritalStatus'], function(x)
  replace(x,is.na(x), Mode(x[!is.na(x)])))

Mode <- function(x) {
  ux <- sort(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}
data_1['OverTime'] <- lapply(data_1['OverTime'], function(x)
  replace(x,is.na(x), Mode(x[!is.na(x)])))

Mode <- function(x) {
  ux <- sort(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}
data_1['PerformanceRating'] <- lapply(data_1['PerformanceRating'], function(x)
  replace(x,is.na(x), Mode(x[!is.na(x)])))

Mode <- function(x) {
  ux <- sort(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}
data_1['RelationshipSatisfaction'] <- lapply(data_1['RelationshipSatisfaction'], function(x)
  replace(x,is.na(x), Mode(x[!is.na(x)])))

Mode <- function(x) {
  ux <- sort(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}
data_1['StockOptionLevel'] <- lapply(data_1['StockOptionLevel'], function(x)
  replace(x,is.na(x), Mode(x[!is.na(x)])))

Mode <- function(x) {
  ux <- sort(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}
data_1['TotalWorkingYears'] <- lapply(data_1['TotalWorkingYears'], function(x)
  replace(x,is.na(x), Mode(x[!is.na(x)])))

Mode <- function(x) {
  ux <- sort(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}
data_1['WorkLifeBalance'] <- lapply(data_1['WorkLifeBalance'], function(x)
  replace(x,is.na(x), Mode(x[!is.na(x)])))

Mode <- function(x) {
  ux <- sort(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}
data_1['Employee.Source'] <- lapply(data_1['Employee.Source'], function(x)
  replace(x,is.na(x), Mode(x[!is.na(x)])))

#After imputation of catogerical variables - view Missing Values again
colSums(is.na(data_1))


###Load mlbench and caret package for feature selection process
library(mlbench)
library(caret)
library(ggplot2)
library(lattice)

#Feature Selection -Which features are important?
install.packages("Boruta")
library(Boruta)
boruta <- Boruta(Attrition ~ ., data = data_1, doTrace = 2)
print(boruta)
plot(boruta, las = 2, cex.axis = 0.7)
attStats(boruta)





