# machine-learning-employee-attrition 🧑‍💼

**Topic:**
Building a Reliable Predictive Model to Predict Employee Attrition

*R-Programming will be the coding language used in this project*

No | Dataset | Information
--- | --- | --- 
1 | URL | https://www.kaggle.com/rushikeshghate/capstone-projectibm-employee-attrition-prediction?select=IBM+HR+Data+new.csv
2 | Dataset Name | IBM Employee Attrition
3 | File Type | csv file
4 | Observation | 23,436
5 | Features | 37
6 | Data label |Staying employee as “Current Employee” while resigned employee as “Voluntary Resignation”.

# Introduction
## Why is employee attrition an issue?
- High turnover rates / employee attrition are becoming a huge issue for organisations
- The Great Attrition --> [Click here for more information about The Great Attrition](https://www.mckinsey.com/capabilities/people-and-organizational-performance/our-insights/the-great-attrition-is-making-hiring-harder-are-you-searching-the-right-talent-pools)
- High cost of hiring + training + time-taken for recruitment & onboarding

## What is the problem statement of the project?
- Challenges for organisations to retain their current workforce
- Lack of reliable machine learning tools to predict employee attrition

## Aims & Objectives (What do I aim to achive?) 🌟
**The Aims**
- The overall aim of this project is to build a predictive machine learning model that would play a vital part in HRM by accurately predicting employee attrition

**The Objectives**
- To obtain employee attrition dataset with more observations as compared with previous work
- To identify the key features that could affect prediction of employee attrition
- To build a model (and evaluate model performance) that could accurately predict employee attrition

# Data Preparation
- Dataset exploration 🔍
- Data selection & cleaning 🧹
  - Removing irrelavant features
  - Convert data structure to the suitable ones (*cases where categorical variables are loaded as character instead of factor in RStudio*)
  - Missing values & imputation (*Let's replace the missing values!*)
  - Feature selection (*Does some variables really significantly contribute to the prediction of employee attrition?*)
  - Class distribution of the target variable (*It seems there might be a huge class imbalance issue*) --> Data Re-sampling needed!
![git_8_summary_aml](https://user-images.githubusercontent.com/116934441/218005111-feaf57c7-2695-4eff-bd4b-37d2800127c1.png)
  - Click here to know more about oversampling --> [What is oversampling or undersampling in machine learning?](https://machinelearningmastery.com/random-oversampling-and-undersampling-for-imbalanced-classification/)

- Exploratory Data Analysis (EDA) 🧮
  - Univariate analysis
  - Bivariate analysis
- One-Hot Encoding / Label Encoding 🖥️

# Model Implementation
- Logistic Regression (LR)
- Decision Tree (DT)
- Naive Bayes (NB)

# Performance Evaluation
![git_9_model_per_aml](https://user-images.githubusercontent.com/116934441/218007346-110bec9d-3647-46ea-aa86-014603ce3f0d.png)
- DT performed the best as compared with other models experimented
- Model perofmrance for DT is consistent
- The dataset used was the oversampling dataset without the issue of class imbalance

# Recommendations for Future Work
- Dataset with more observations 
- Build predictive model with real HR dataset (*Current dataset is a fictional dataset provided by IBM*)
- Include more variables (*Such as work from home, hybrid or full physical*) 
- Different feature selection algorithm (*Boruta algorithm was used in this project*)

# Further Information About the Topic 
**Employee Attrition Prediction with Python**

- [Employee Attrition Rate Prediction Using Machine Learning](https://www.enjoyalgorithms.com/blog/attrition-rate-prediction-using-ml)

- [Employee Attrition Prediction - A Guide](https://www.analyticsvidhya.com/blog/2021/11/employee-attrition-prediction-a-comprehensive-guide/)






