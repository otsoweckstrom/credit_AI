library ("broom")
library("dplyr")    
library("tidyr") 
library("astsa")
library("anytime")
library("corrr")
library("ggcorrplot")
library(randomForest)
library(tree)
library(pROC)
library(caret)

getwd()
setwd("/Users/otso/Desktop/Seoultech/Machne Learning in Finance")

#------------------------Data Cleaning------------------------
data1 <-read.csv("BankChurners.csv", sep = ",")


#Get rid of unknown data points
data1 <- dplyr::filter(data1, Education_Level != "Unknown")
data1 <- dplyr::filter(data1, Marital_Status != "Unknown")
data1 <- dplyr::filter(data1, Income_Category != "Unknown")
data1 <- dplyr::filter(data1, Marital_Status != "Divorced")
data1 <- dplyr::filter(data1, Attrition_Flag != "Attrited Customer")

#Shorten unnecessarily long variable names for easier usage
col_index <- which(names(data1) == "Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2")
names(data1)[col_index] <- "Naive_Bayes_mon_2"
col_index <- which(names(data1) == "Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1")
names(data1)[col_index] <- "Naive_Bayes_mon_1"


#Turn all datapoints into numeric
data1 <- data1 %>%
  mutate(Gender = ifelse(Gender == "F", 0, ifelse(Gender == "M", 1, Gender)))

data1$Gender<- as.numeric(data1$Gender)


data1 <- data1 %>%
  mutate(Education_Level = case_when(
    Education_Level == "Uneducated"    ~ 0,
    Education_Level == "High School"    ~ 1,
    Education_Level == "Graduate"       ~ 2,
    Education_Level == "College"        ~ 3,
    Education_Level == "Post-Graduate"  ~ 4,
    Education_Level == "Doctorate"      ~ 5
  ))

data1$Education_Level<- as.numeric(data1$Education_Level)

data1 <- data1 %>%
  mutate(Marital_Status = ifelse(Marital_Status == "Single", 0, ifelse(Marital_Status == "Married", 1, Marital_Status)))

data1$Marital_Status<- as.numeric(data1$Marital_Status)

data1 <- data1 %>%
  mutate(Card_Category = case_when(
    Card_Category == "Blue"    ~ 0,
    Card_Category == "Silver"    ~ 0,
    Card_Category == "Gold"       ~ 1,
    Card_Category == "Platinum"   ~ 1
  ))

data1$Card_Category<- as.numeric(data1$Card_Category)

data1 <- data1 %>%
  mutate(Income_Category = case_when(
    Income_Category == "Less than $40K"    ~ 0,
    Income_Category == "$40K - $60K"    ~ 1,
    Income_Category == "$60K - $80K"       ~ 2,
    Income_Category == "$80K - $120K"   ~ 3,
    Income_Category == "$120K +" ~ 4,
  ))

data1$Income_Category<- as.numeric(data1$Income_Category)


#--------------------------Linear Regression models----------------------
# Set seed for reproducibility
set.seed(123)
#Set number of folds for k-fold cross-validation
num_folds <- 5

#Split data into training and testing datasets and randomize dividing the data


# Create an index vector for random sampling
index <- sample(1:nrow(data1))

# Specify the number of rows for training and testing
n_train <- 3843
n_test <- 5491 - n_train

# Create training dataset
datatrain <- data1[index[1:n_train], ]

# Create testing dataset
datatest <- data1[index[(n_train + 1):(n_train + n_test)], ]


data2<- datatrain[ ,-1:-2]
datanorm <- scale(data2[ ,-14:-21])
cor_mat <- cor(datanorm)
ggcorrplot(cor_mat)

lm_multi.fit <- lm(Card_Category ~ Customer_Age + Gender + Education_Level + Marital_Status + 
               Income_Category + Months_on_book + Total_Relationship_Count + 
               Months_Inactive_12_mon + Credit_Limit+ Total_Revolving_Bal + Avg_Open_To_Buy + Total_Amt_Chng_Q4_Q1 + 
               Total_Trans_Amt + Total_Trans_Ct + Total_Ct_Chng_Q4_Q1 + Avg_Utilization_Ratio + 
               Naive_Bayes_mon_1 + 
               Naive_Bayes_mon_2
             , data = datatrain)

# Linear regression models with one predictor variable each
lm_uni1 <- lm(Card_Category ~ Customer_Age, data = datatrain)
lm_uni2 <- lm(Card_Category ~ Gender, data = datatrain)
lm_uni3 <- lm(Card_Category ~ Education_Level, data = datatrain)
lm_uni4 <- lm(Card_Category ~ Marital_Status, data = datatrain)
lm_uni5 <- lm(Card_Category ~ Income_Category, data = datatrain)
lm_uni6 <- lm(Card_Category ~ Months_on_book, data = datatrain)
lm_uni7 <- lm(Card_Category ~ Total_Relationship_Count, data = datatrain)
lm_uni8 <- lm(Card_Category ~ Months_Inactive_12_mon, data = datatrain)
lm_uni9 <- lm(Card_Category ~ Credit_Limit, data = datatrain)
lm_uni10 <- lm(Card_Category ~ Total_Revolving_Bal, data = datatrain)
lm_uni11 <- lm(Card_Category ~ Avg_Open_To_Buy, data = datatrain)
lm_uni12 <- lm(Card_Category ~ Total_Amt_Chng_Q4_Q1, data = datatrain)
lm_uni13 <- lm(Card_Category ~ Total_Trans_Amt, data = datatrain)
lm_uni14 <- lm(Card_Category ~ Total_Trans_Ct, data = datatrain)
lm_uni15 <- lm(Card_Category ~ Total_Ct_Chng_Q4_Q1, data = datatrain)
lm_uni16 <- lm(Card_Category ~ Avg_Utilization_Ratio, data = datatrain)
lm_uni17 <- lm(Card_Category ~ Naive_Bayes_mon_1, data = datatrain)
lm_uni18 <- lm(Card_Category ~ Naive_Bayes_mon_2, data = datatrain)

cat("Summary for lm_multi", i, ":\n")
summary(lm_multi.fit)
# List of univariate linear regression models
uni_models <- list(
  lm_uni1, lm_uni2, lm_uni3, lm_uni4, lm_uni5,
  lm_uni6, lm_uni7, lm_uni8, lm_uni9, lm_uni10,
  lm_uni11, lm_uni12, lm_uni13, lm_uni14, lm_uni15,
  lm_uni16, lm_uni17, lm_uni18
)

# Use lapply to summarize each model
summaries <- lapply(uni_models, summary)

# Print the summaries
for (i in seq_along(summaries)) {
  cat("Summary for lm_uni", i, ":\n")
  print(summaries[[i]])
  cat("\n")
}


p1 <- predict(lm.fit, datatest, interval = "confidence")
plot.ts(p1)

# K-fold cross-validation with 5 folds for the linear regression models
k <- 5
folds <- cut(seq(1, nrow(datatrain)), breaks = k, labels = FALSE)

# Vector to store AUROC values for each fold
auroc_values_multi <- numeric(k)
auroc_values_uni <- matrix(numeric(k * length(uni_models)), nrow = k)

# Loop over folds
for (i in 1:k) {
  # Create training and testing datasets for this fold
  train_indices <- which(folds != i)
  test_indices <- which(folds == i)
  train_data <- datatrain[train_indices, ]
  test_data <- datatrain[test_indices, ]
  
  # Fit the multiple linear regression model
  lm_multi.fit <- lm(Card_Category ~ Customer_Age + Gender + Education_Level + Marital_Status + 
                       Income_Category + Months_on_book + Total_Relationship_Count + 
                       Months_Inactive_12_mon + Credit_Limit + Total_Revolving_Bal + Avg_Open_To_Buy + Total_Amt_Chng_Q4_Q1 + 
                       Total_Trans_Amt + Total_Trans_Ct + Total_Ct_Chng_Q4_Q1 + Avg_Utilization_Ratio + 
                       Naive_Bayes_mon_1 + Naive_Bayes_mon_2, data = train_data)
  
  # AUROC for the multiple linear regression model on the test set
  roc_multi <- roc(test_data$Card_Category, predict(lm_multi.fit, test_data))
  auc_multi <- auc(roc_multi)
  auroc_values_multi[i] <- auc_multi
  
  # Loop over univariate linear regression models
  for (j in seq_along(uni_models)) {
    # Fit the univariate linear regression model
    uni_fit <- uni_models[[j]]
    
    # AUROC for the univariate linear regression model on the test set
    roc_uni <- roc(test_data$Card_Category, predict(uni_fit, test_data))
    auc_uni <- auc(roc_uni)
    auroc_values_uni[i, j] <- auc_uni
  }
}

# Print AUROC values for the multiple linear regression model
cat("AUROC values for multiple linear regression model (each fold):\n")
for (i in 1:k) {
  cat("Fold", i, ":", auroc_values_multi[i], "\n")
}
cat("Mean AUROC for multiple linear regression model across all folds:", mean(auroc_values_multi), "\n")

# Print mean AUROC values for each univariate linear regression model
cat("mean AUROC values for univariate linear regression models (each fold):\n")
for (j in seq_along(uni_models)) {
  cat("Variable", j, ":", mean(auroc_values_uni[, j]), "\n")
}


mse1 <- mean((p1[,"fit"] - datatest$Card_Category)^2)

# AUROC for the multiple linear regression model
roc_multi <- roc(datatrain$Card_Category, predict(lm_multi.fit, datatrain), levels = c(0, 1))
auc_multi <- auc(roc_multi)

# AUROC for each univariate linear regression model
auc_uni1 <- auc(roc(datatrain$Card_Category, predict(lm_uni1, datatrain), levels = c(0, 1)))
auc_uni2 <- auc(roc(datatrain$Card_Category, predict(lm_uni2, datatrain), levels = c(0, 1)))
auc_uni3 <- auc(roc(datatrain$Card_Category, predict(lm_uni3, datatrain), levels = c(0, 1)))
auc_uni4 <- auc(roc(datatrain$Card_Category, predict(lm_uni4, datatrain), levels = c(0, 1)))
auc_uni5 <- auc(roc(datatrain$Card_Category, predict(lm_uni5, datatrain), levels = c(0, 1)))
auc_uni6 <- auc(roc(datatrain$Card_Category, predict(lm_uni6, datatrain), levels = c(0, 1)))
auc_uni7 <- auc(roc(datatrain$Card_Category, predict(lm_uni7, datatrain), levels = c(0, 1)))
auc_uni8 <- auc(roc(datatrain$Card_Category, predict(lm_uni8, datatrain), levels = c(0, 1)))
auc_uni9 <- auc(roc(datatrain$Card_Category, predict(lm_uni9, datatrain), levels = c(0, 1)))
auc_uni10 <- auc(roc(datatrain$Card_Category, predict(lm_uni10, datatrain), levels = c(0, 1)))
auc_uni11 <- auc(roc(datatrain$Card_Category, predict(lm_uni11, datatrain), levels = c(0, 1)))
auc_uni12 <- auc(roc(datatrain$Card_Category, predict(lm_uni12, datatrain), levels = c(0, 1)))
auc_uni13 <- auc(roc(datatrain$Card_Category, predict(lm_uni13, datatrain), levels = c(0, 1)))
auc_uni14 <- auc(roc(datatrain$Card_Category, predict(lm_uni14, datatrain), levels = c(0, 1)))
auc_uni15 <- auc(roc(datatrain$Card_Category, predict(lm_uni15, datatrain), levels = c(0, 1)))
auc_uni16 <- auc(roc(datatrain$Card_Category, predict(lm_uni16, datatrain), levels = c(0, 1)))
auc_uni17 <- auc(roc(datatrain$Card_Category, predict(lm_uni17, datatrain), levels = c(0, 1)))
auc_uni18 <- auc(roc(datatrain$Card_Category, predict(lm_uni18, datatrain), levels = c(0, 1)))

# Print AUROC values
cat("AUROC - Customer_Age:", auc(roc(datatrain$Card_Category, predict(lm_uni1, datatrain), levels = c(0, 1))), "\n")
cat("AUROC - Gender:", auc(roc(datatrain$Card_Category, predict(lm_uni2, datatrain), levels = c(0, 1))), "\n")
cat("AUROC - Education_Level:", auc(roc(datatrain$Card_Category, predict(lm_uni3, datatrain), levels = c(0, 1))), "\n")
cat("AUROC - Marital_Status:", auc(roc(datatrain$Card_Category, predict(lm_uni4, datatrain), levels = c(0, 1))), "\n")
cat("AUROC - Income_Category:", auc(roc(datatrain$Card_Category, predict(lm_uni5, datatrain), levels = c(0, 1))), "\n")
cat("AUROC - Months_on_book:", auc(roc(datatrain$Card_Category, predict(lm_uni6, datatrain), levels = c(0, 1))), "\n")
cat("AUROC - Total_Relationship_Count:", auc(roc(datatrain$Card_Category, predict(lm_uni7, datatrain), levels = c(0, 1))), "\n")
cat("AUROC - Months_Inactive_12_mon:", auc(roc(datatrain$Card_Category, predict(lm_uni8, datatrain), levels = c(0, 1))), "\n")
cat("AUROC - Credit_Limit:", auc(roc(datatrain$Card_Category, predict(lm_uni9, datatrain), levels = c(0, 1))), "\n")
cat("AUROC - Total_Revolving_Bal:", auc(roc(datatrain$Card_Category, predict(lm_uni10, datatrain), levels = c(0, 1))), "\n")
cat("AUROC - Avg_Open_To_Buy:", auc(roc(datatrain$Card_Category, predict(lm_uni11, datatrain), levels = c(0, 1))), "\n")
cat("AUROC - Total_Amt_Chng_Q4_Q1:", auc(roc(datatrain$Card_Category, predict(lm_uni12, datatrain), levels = c(0, 1))), "\n")
cat("AUROC - Total_Trans_Amt:", auc(roc(datatrain$Card_Category, predict(lm_uni13, datatrain), levels = c(0, 1))), "\n")
cat("AUROC - Total_Trans_Ct:", auc(roc(datatrain$Card_Category, predict(lm_uni14, datatrain), levels = c(0, 1))), "\n")
cat("AUROC - Total_Ct_Chng_Q4_Q1:", auc(roc(datatrain$Card_Category, predict(lm_uni15, datatrain), levels = c(0, 1))), "\n")
cat("AUROC - Avg_Utilization_Ratio:", auc(roc(datatrain$Card_Category, predict(lm_uni16, datatrain), levels = c(0, 1))), "\n")
cat("AUROC - Naive_Bayes_mon_1:", auc(roc(datatrain$Card_Category, predict(lm_uni17, datatrain), levels = c(0, 1))), "\n")
cat("AUROC - Naive_Bayes_mon_2:", auc(roc(datatrain$Card_Category, predict(lm_uni18, datatrain), levels = c(0, 1))), "\n")


#---------------------------------ML model------------------



tree.credit <- tree(Card_Category ~ Customer_Age + Gender + Education_Level + Marital_Status + 
                      Income_Category + Months_on_book + Total_Relationship_Count + 
                      Months_Inactive_12_mon + Credit_Limit+ Total_Revolving_Bal + Avg_Open_To_Buy + Total_Amt_Chng_Q4_Q1 + 
                      Total_Trans_Amt + Total_Trans_Ct + Total_Ct_Chng_Q4_Q1 + Avg_Utilization_Ratio + 
                      Naive_Bayes_mon_1 + 
                      Naive_Bayes_mon_2, datatrain)
###
summary(tree.credit)
###
plot(tree.credit)
text(tree.credit, pretty = 0)



# Convert 'Card_Category' to a factor
datatrain$Card_Category <- as.factor(datatrain$Card_Category)


# Build a random forest model
rf_model <- randomForest(
  Card_Category ~ Customer_Age + Gender + Education_Level + Marital_Status + 
    Income_Category + Months_on_book + Total_Relationship_Count + 
    Months_Inactive_12_mon + Credit_Limit+ Total_Revolving_Bal + Avg_Open_To_Buy + Total_Amt_Chng_Q4_Q1 + 
    Total_Trans_Amt + Total_Trans_Ct + Total_Ct_Chng_Q4_Q1 + Avg_Utilization_Ratio + 
    Naive_Bayes_mon_1 + 
    Naive_Bayes_mon_2,
  data = datatrain,
  ntree = 500,      # Number of trees in the forest
  mtry = sqrt(ncol(datatrain) - 1)  # Number of variables randomly sampled at each split
)
levels(datatrain$Card_Category) <- c("Level0", "Level1")


# Print a summary of the random forest model
print(rf_model)
# Print summary of the random forest model with k-fold cross-validation
print(rf_model_kvalidate)

# Print AUROC values for both models
cat("AUROC:", rf_model$results$ROC, "\n")
cat("AUROC:", rf_model_kvalidate$results$ROC, "\n")


#tree prediction
yhat.cr <- predict(tree.credit, newdata = datatest)
plot(yhat.cr, datatest$Card_Category)
abline(0, 1)
mean((yhat.cr - datatest$Card_Category)^2) 

#random forest prediction
yhat.rf <- predict(rf_model, newdata = datatest, type = "class")
plot(yhat.rf, datatest$Card_Category)
abline(0, 1)
table(yhat.rf, datatest$Card_Category) #Confusion matrix
error.rf <- 1 - (1444+91+12)/1648 #Error rate
error.rf #0.06128641


plot(rf_model)

# Variable importance plot for random forest model
varImpPlot(rf_model)


# Create a training control object for k-fold cross-validation
train_control <- trainControl(
  method = "cv",                # Cross-validation method
  number = num_folds,            # Number of folds
  verboseIter = TRUE,            # Print progress
  summaryFunction = twoClassSummary,  # Function for summarizing performance
  classProbs = TRUE              # Enable class probabilities for AUROC
)


# Build a random forest model using k-fold cross-validation
rf_model_kvalidate <- train(
  Card_Category ~ Customer_Age + Gender + Education_Level + Marital_Status + 
    Income_Category + Months_on_book + Total_Relationship_Count + 
    Months_Inactive_12_mon + Credit_Limit + Total_Revolving_Bal + Avg_Open_To_Buy + 
    Total_Amt_Chng_Q4_Q1 + Total_Trans_Amt + Total_Trans_Ct + Total_Ct_Chng_Q4_Q1 + 
    Avg_Utilization_Ratio + Naive_Bayes_mon_1 + Naive_Bayes_mon_2,
  data = datatrain,
  method = "rf",                 # Random forest method
  trControl = train_control
)

plot(rf_model_kvalidate)



