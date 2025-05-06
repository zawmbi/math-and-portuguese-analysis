# Analysis of Final Grades for Math and Portuguese Students

This repository contains all code, data, and the written report for our STAT 385 final project. We explore three modeling approaches—classification trees, logistic regression, and Lasso regression—to predict whether a student passes both Math and Portuguese in the third trimester.


- **data/**  
  Raw data downloaded from the UCI repository.  
- **code/FinalProject.R**  
  A single R script that:
  1. Merges and cleans both datasets  
  2. Performs exploratory analysis (correlation heatmap, scatterplot of final grades)  
  3. Fits classification trees (full, pruned, bagged, boosted, random forest)  
  4. Fits a full logistic regression, then a stepwise‐selected logistic model  
  5. Fits a Lasso regression with cross‐validation  
  6. Generates all figures and prints model summaries and confusion matrices  
- **Math and Portuguese Analysis Report.pdf**  
  A polished write‐up of our methods, results, and conclusions, including all numbered figures and tables.

## Prerequisites

- R (≥ 4.0)  
- The following R packages:
  ```r
  install.packages(c(
    "ggplot2", "reshape2", "caret", "broom",
    "rpart", "rpart.plot", "ipred", "adabag",
    "randomForest", "glmnet"
  ))
