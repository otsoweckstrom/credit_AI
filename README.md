# Credit Card Classification Algorithm

## Overview
This project implements machine learning models to predict credit card category (Gold/Platinum vs. Blue/Silver) based on customer data. Using a combination of linear regression and random forest techniques, the algorithm analyzes various customer attributes to determine the appropriate credit card tier for each customer.

## Dataset
The project uses the "BankChurners.csv" dataset which contains:
- Customer demographics (age, gender, education level, marital status, income)
- Account information (months on book, relationship count, inactive months)
- Credit metrics (credit limit, revolving balance, average open to buy)
- Transaction data (transaction amounts and counts, quarterly changes)
- Utilization ratios and other calculated metrics

## Data Preprocessing
- Removal of records with unknown values in Education Level, Marital Status, and Income Category
- Exclusion of attrited customers to focus on active accounts only
- Conversion of categorical variables to numeric:
  - Gender: F=0, M=1
  - Education Level: Uneducated=0 to Doctorate=5
  - Marital Status: Single=0, Married=1
  - Card Category: Blue/Silver=0, Gold/Platinum=1
  - Income Category: <$40K=0 to $120K+=4

## Models Implemented
1. **Linear Regression**
   - Multivariate model using all features
   - Individual univariate models for each feature
   - Performance evaluation using k-fold cross-validation (5 folds)
   - AUROC metrics for model comparison

2. **Decision Tree**
   - Visualization of decision rules 
   - Prediction evaluation on test data

3. **Random Forest**
   - 500 trees in the forest
   - Variable importance analysis
   - Cross-validation for robust performance evaluation
   - Confusion matrix and error rate calculation

## Key Results
- The Random Forest model achieved an error rate of approximately 6.13%
- Feature importance analysis identified the most predictive customer attributes
- Model performance was validated using 5-fold cross-validation

## Requirements
The following R packages are required:
- broom
- dplyr
- tidyr
- astsa
- anytime
- corrr
- ggcorrplot
- randomForest
- tree
- pROC
- caret

## Usage
1. Set your working directory to the location containing the "BankChurners.csv" file
2. Run the script to:
   - Clean and preprocess the data
   - Train the linear regression models
   - Build the decision tree
   - Implement the random forest classifier
   - Evaluate model performance

## Future Improvements
- Hyperparameter tuning for the random forest model
- Feature engineering to create more predictive variables
- Exploration of other algorithms (SVM, Neural Networks, etc.)
- Deployment framework for real-time predictions
