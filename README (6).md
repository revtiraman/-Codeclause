# Parkinsons-Disease

Parkinson’s disease (PD) is one of the most prevalent neurodegenerative diseases, affecting over 6 million patients worldwide. PD is characterized by both motor and non-motor symptoms , with the former mainly consisting of resting tremor, muscle rigidity, and akinesia. Such disabling symptoms in advanced Parkinson’s disease can be partially alleviated by the use of deep brain stimulation technology , through delivering a constant high-frequency stimulation (~130Hz) to either subthalamic nucleus (STN) or internal globus pallidus (GPi).

## Using XGBoost on a real-life problem: diagnosing Parkinson’s Disease

XGBoost is a popular technique and a neat alternative to traditional regression/neural nets. It stands for EXtreme Gradient Boosting, and basically builds something of a decision tree to compute gradients. 
Parkinson’s detection: we have several metrics that we can analyze and ultimately we need to diagnose Parkinson’s (classification!). This is a perfect problem for XGBoost.

#### I have trained a full XGBoosting classifier for Parkinson’s disease. You can find the full source code and the data as well.

---
## Data info:
This dataset is downloaded from UCI Machine Learning Repository. It is also available in this repository ([here](https://github.com/Ravjot03/Parkinsons-Disease/blob/master/parkinsons.data)).

---
## Contents:
1. Importing the required libraries.
2. Importing and Reading the dataset.
3. Data Modeling
    - Separating the data into features and target variable.
    - Splitting the data into training and test sets.
    - Training the model.
    - Predicting the model.
    - Calculating the model accuracy.
 
