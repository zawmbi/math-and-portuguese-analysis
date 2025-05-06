###############################################################################
# Final Project STAT 385
# Group: Linda Mansour, Jing Meng, Kieran Simenson
# Due Date: 05/05/2025
###############################################################################

math <- read.csv("math.csv", stringsAsFactors = TRUE)
portuguese <- read.csv("portuguese.csv", stringsAsFactors = TRUE)
set.seed(05052025) # the due date 
merged <- merge(math, portuguese, by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
str(merged) # shows the data type for each variable
################################################################################
# Cleaning: This involves checking for duplicates, making sure NAs are 
# consistent among the dataset, and identifying missing data if there is any.
################################################################################

merged[duplicated(merged)] # This confirmed that there actually is no duplicates

all_nas <- colSums(is.na(merged))
all_nas # This confirms that there are no NA values in our dataset

# just for clarity: .x is now .math and .y is now .portuguese 
colnames(merged) <- gsub("\\.x$", ".math", colnames(merged)) # substitutes .x for .math
colnames(merged) <- gsub("\\.y$", ".portuguese", colnames(merged)) # substitutes .y for .portuguese

# Choose which dataset's data you'd rather keep for discrepancy columns like
# guardian, alc, etc. depending on the data we want to use. I combined just
# these for now, but if we come across problems, we can edit it. 

# EDITABLE: 
merged$guardian <- merged$guardian.math
merged$guardian.math <- NULL
merged$guardian.portuguese <- NULL


unique(merged) # This helps us to check that our discrepancy column fixes were successful

# Dropping columns we definitely will not use, this is our officially cleaned dataframe.
# EDITABLE: 
cleaned <- subset(merged, select = -c(Walc.math, Walc.portuguese, Dalc.math, Dalc.portuguese, goout.math, goout.portuguese, romantic.math, romantic.portuguese, freetime.math, freetime.portuguese, famrel.math, famrel.portuguese, health.math, health.portuguese))
length(merged) # 53 columns

nrow(merged) # 382 rows

length(cleaned) # 38 columns
nrow(cleaned) # 382 rows (successful)

################################################################################
# Factor creation: This involves converting our variables of interest (G1, G2, and G3
# for portuguese and math) into factors with 4 labels: Pass both, Pass portuguese,
# Pass math, and Fail both (where a failing grade is < 10 )
################################################################################

clean_factor <- cleaned

# create new NA columns for factored data
clean_factor$pass_fail_1 <- NA
clean_factor$pass_fail_2 <- NA
clean_factor$pass_fail_3 <- NA

# Loop to designate values with pass/fail categories for each trimester
# first trimester
for (i in 1:dim(clean_factor)[1]) {
  if ( (clean_factor[i, "G1.math"] >= 10) & (clean_factor[i, "G1.portuguese"] >= 10) ) {
    clean_factor[i,"pass_fail_1"] <- "Pass both"
  }
  else if ( (clean_factor[i, "G1.math"] < 10) & (clean_factor[i, "G1.portuguese"] >= 10) ) {
    clean_factor[i, "pass_fail_1"] <- "Pass portuguese"
  }
  else if ( (clean_factor[i, "G1.math"] >= 10) & (clean_factor[i, "G1.portuguese"] < 10) ) {
    clean_factor[i, "pass_fail_1"] <- "Pass math"
  }
  else {
    clean_factor[i, "pass_fail_1"] <- "Fail both"
  }
}
# convert to factor
clean_factor$pass_fail_1 <- factor(clean_factor$pass_fail_1, levels=c("Fail both", "Pass math", "Pass portuguese", "Pass both"), labels=c("Fail both", "Pass math", "Pass portuguese", "Pass both"))

# second trimester
for (i in 1:dim(clean_factor)[1]) {
  if ( (clean_factor[i, "G2.math"] >= 10) & (clean_factor[i, "G2.portuguese"] >= 10) ) {
    clean_factor[i,"pass_fail_2"] <- "Pass both"
  }
  else if ( (clean_factor[i, "G2.math"] < 10) & (clean_factor[i, "G2.portuguese"] >= 10) ) {
    clean_factor[i, "pass_fail_2"] <- "Pass portuguese"
  }
  else if ( (clean_factor[i, "G2.math"] >= 10) & (clean_factor[i, "G2.portuguese"] < 10) ) {
    clean_factor[i, "pass_fail_2"] <- "Pass math"
  }
  else {
    clean_factor[i, "pass_fail_2"] <- "Fail both"
  }
}
clean_factor$pass_fail_2 <- factor(clean_factor$pass_fail_2, levels=c("Fail both", "Pass math", "Pass portuguese", "Pass both"), labels=c("Fail both", "Pass math", "Pass portuguese", "Pass both"))

# third trimester
for (i in 1:dim(clean_factor)[1]) {
  if ( (clean_factor[i, "G3.math"] >= 10) & (clean_factor[i, "G3.portuguese"] >= 10) ) {
    clean_factor[i,"pass_fail_3"] <- "Pass both"
  }
  else if ( (clean_factor[i, "G3.math"] < 10) & (clean_factor[i, "G3.portuguese"] >= 10) ) {
    clean_factor[i, "pass_fail_3"] <- "Pass portuguese"
  }
  else if ( (clean_factor[i, "G3.math"] >= 10) & (clean_factor[i, "G3.portuguese"] < 10) ) {
    clean_factor[i, "pass_fail_3"] <- "Pass math"
  }
  else {
    clean_factor[i, "pass_fail_3"] <- "Fail both"
  }
}
clean_factor$pass_fail_3 <- factor(clean_factor$pass_fail_3, levels=c("Fail both", "Pass math", "Pass portuguese", "Pass both"), labels=c("Fail both", "Pass math", "Pass portuguese", "Pass both"))

# remove G1-3 for math and portuguese, as those now contain collinear data
clean_factor <- subset(clean_factor, select = -c(G1.math, G2.math, G3.math, G1.portuguese, G2.portuguese, G3.portuguese))

################################################################################
# Create binary outcome variable BEFORE splitting the data
# This ensures both training and test sets have the same target variable.
# The binary outcome will be used in classification models:
# - 1 = student passed both subjects in the 3rd trimester
# - 0 = student failed one or both subjects
################################################################################

clean_factor$pass_binary <- ifelse(clean_factor$pass_fail_3 == "Pass both", 1, 0)
clean_factor$pass_binary <- as.factor(clean_factor$pass_binary)

################################################################################
# Splitting data into training and test sets
# We test two splits: 70/30 and 80/20 to evaluate which offers the best balance.
# 
# - 70/30 → gives more test data for reliable performance evaluation (recommended)
# - 80/20 → gives more training data which can slightly improve model learning
################################################################################

# --- 70/30 split ---
# ~70% of data used for training, 30% for testing
split_index_1 <- sample(1:nrow(clean_factor), 0.70 * nrow(clean_factor))
train_set_1 <- clean_factor[split_index_1, ]   # training set with 70% of the data
test_set_1 <- clean_factor[-split_index_1, ]   # test set with the remaining 30%

# --- 80/20 split ---
# ~80% of data used for training, 20% for testing
split_index_2 <- sample(1:nrow(clean_factor), 0.80 * nrow(clean_factor))
train_set_2 <- clean_factor[split_index_2, ]   # training set with 80% of the data
test_set_2 <- clean_factor[-split_index_2, ]   # test set with the remaining 20%

#######################################################################
# Choose which split to use for final model training and evaluation
# Based on accuracy and balance, we will continue using the 70/30 split
# Final training set: train_set_1
# Final test set: test_set_1
#############################################################################


################################################################################
# Classification Trees: Create 5 trees (full, pruned, bagged, boosted, and random
# forest) to predict the 3rd trimester (pass_fail_3) without the 1st + 2nd
################################################################################
library(rpart)
library(rpart.plot)
library(mlbench)

## Remove unnecessary variables (G1 pass/fail, G2 pass/fail, and G3 pass/fail binary)
clean_tree <- subset(clean_factor, select = -c(pass_fail_1, pass_fail_2, pass_binary))
train_set <- subset(train_set_1, select = -c(pass_fail_1, pass_fail_2, pass_binary))
test_set <- subset(test_set_1, select = -c(pass_fail_1, pass_fail_2, pass_binary))
truth <- test_set$pass_fail_3

# build entire tree
pf.tree.full <- rpart(pass_fail_3~., data=train_set, control=rpart.control(minsplit=2, minbucket=1, cp=0))
full.pred <- predict(pf.tree.full, test_set, type="class")
full.tab <- table(truth, full.pred)
#             	full.pred
# truth         	Fail both Pass math Pass portuguese Pass both
#   Fail both           	1     	0           	0     	2
#   Pass math           	0     	0           	1     	0
#   Pass portuguese     	1     	3           	9    	22
#   Pass both           	1     	1          	14    	60
full.err <- (sum(full.tab)-sum(diag(full.tab)))/sum(full.tab)
# error: 0.3913043
# prune tree
bestcp <- pf.tree.full$cptable[which.min(pf.tree.full$cptable[,"xerror"]),"CP"]
# cp = 0.04123711
pruned_cat <- prune(pf.tree.full, bestcp)
prp(pruned_cat, type=2, extra=1)
################################################################################
# Only one value of "Pass math" in test set. No outcome of "Pass math" in
# pruned tree. Unlikely to get accurate predictions that take into account
# all four outcomes. Switch to pass fail binary.
################################################################################

################################################################################
# Classification Trees: Create 5 trees (full, pruned, bagged, boosted, and
# random forest) to predict the 3rd trimester as a binary
################################################################################

## Remove unnecessary variables (G1 pass/fail, G2 pass/fail, and G3 pass/fail)
clean_tree <- subset(clean_factor, select = -c(pass_fail_1, pass_fail_2, pass_fail_3))
train_set <- subset(train_set_1, select = -c(pass_fail_1, pass_fail_2, pass_fail_3))
test_set <- subset(test_set_1, select = -c(pass_fail_1, pass_fail_2, pass_fail_3))
truth <- test_set$pass_binary
## Add labels for pass_binary such that 0 = Fail (1-2 classes), 1 = Pass (both)
test_set$pass_binary <- factor(test_set$pass_binary, labels = c("Fail", "Pass"))
train_set$pass_binary <- factor(train_set$pass_binary, labels = c("Fail", "Pass"))

# build entire tree
pf.tree.full <- rpart(pass_binary~., data=train_set, control=rpart.control(minsplit=2, minbucket=1, cp=0))
full.pred <- predict(pf.tree.full, test_set, type="class")
full.tab <- table(truth, full.pred)
#  	full.pred
# truth Fail Pass
# 	0   16   23
# 	1   24   52
full.err <- (sum(full.tab)-sum(diag(full.tab)))/sum(full.tab)
# error: 0.4086957

# build pruned tree
bestcp <- pf.tree.full$cptable[which.min(pf.tree.full$cptable[,"xerror"]),"CP"]
# cp = 0.02061856
pf.pruned <- prune(pf.tree.full, cp= bestcp)
prune.pred <- predict(pf.pruned, test_set, type="class")
prune.tab <- table(truth, prune.pred)
#  	prune.pred
# truth Fail Pass
# 	0   14   25
# 	1   11   65
prune.err <- (sum(prune.tab)-sum(diag(prune.tab)))/sum(prune.tab)
# error: 0.3130435
prp(pf.pruned, type=2, extra=1)

# build bagged tree
library(ipred)
library(tree)
pf.bag <- bagging(pass_binary~., data=train_set, coob=TRUE)
bag.full.pred <- predict(pf.bag, test_set)
bag.full.tab <- table(truth, bag.full.pred$class)
# truth Fail Pass
# 	0   14   25
# 	1	9   67
bag.full.err <- (sum(bag.full.tab)-sum(diag(bag.full.tab)))/sum(bag.full.tab)
# error: 0.2956522

# build boosted tree
library(adabag)
mfinal <- c(10:50)
maxdepth <- c(3:10)
errmatx <- matrix(0, length(mfinal), length(maxdepth))
for (i in 1:length(mfinal)) {
  for (j in 1:length(maxdepth)) {
    pf.boost <- boosting(pass_binary~., data=train_set, mfinal=mfinal[i], coeflearn="Zhu", control=rpart.control(maxdepth = maxdepth[j]))
    boost.pred <- predict(pf.boost, test_set)
    errmatx[i,j] <- boost.pred$error
  }
}
hist(errmatx)
which.min(errmatx)
errmatx[which.min(errmatx)]
# min error of 0.2782609 is found at errmatx[8,3]
mfinal <- mfinal[8]
maxdepth <- maxdepth[3]

pf.boost <- boosting(pass_binary~., data=train_set, mfinal=mfinal, coeflearn="Zhu", control=rpart.control(maxdepth = maxdepth))
boost.pred <- predict(pf.boost, test_set)
boost.tab <- boost.pred$confusion
#            	Observed Class
# Predicted Class Fail Pass
#        	Fail   12   20
#        	Pass   27   56
boost.err <- (sum(boost.tab)-sum(diag(boost.tab)))/sum(boost.tab)
# error: 0.4086957

# build random forest with m = sqrt(p), m= p/2, m= p
library(randomForest)
num.var <- ncol(train_set) - 1   # 32

rf.mod.sqrt <- randomForest(pass_binary ~ ., data = train_set,
                            mtry = floor(sqrt(num.var)), # only difference from bagging is here
                            ntree = 300,
                            proximity = TRUE,
                            importance = TRUE)
rf.mod.half <- randomForest(pass_binary ~ ., data = train_set,
                            mtry = floor((num.var)/2), # only difference from bagging is here
                            ntree = 300,
                            proximity = TRUE,
                            importance = TRUE)
rf.mod.full <- randomForest(pass_binary ~ ., data = train_set,
                            mtry = num.var, # bagging
                            ntree = 300,
                            proximity = TRUE,
                            importance = TRUE)
plot(rf.mod.sqrt$err.rate[,1], type = "l", lwd = 3, lty=1, col = "blue",
     main = "Random forest: OOB estimate of error rate",
     xlab = "Number of Trees", ylab = "OOB error rate", ylim=c(0.22,.4))
lines(rf.mod.half$err.rate[,1],lwd=3, lty=2, col="red")
lines(rf.mod.full$err.rate[,1],lwd=3, lty=3, col="green")
legend("topright",c("m=sqrt(p)","m=p/2","m=p(bagging)"),col=c("blue","red","green"), lty=c(1,2,3))
# m=sqrt(p) seems to be the best fit, as it stabilizes ~150 trees

tuneRF(subset(train_set, select= -pass_binary), train_set$pass_binary, ntreeTry = 150)
title("Random forest: Tuning the hyperparameter m")
# m = 10

rf.mod.10 <- randomForest(pass_binary ~ ., data = train_set,
                          mtry = 10, # only difference from bagging is here
                          ntree = 200,
                          proximity = TRUE,
                          importance = TRUE)
rf.pred <- predict(rf.mod.10, subset(test_set, select= -pass_binary), type="class")
rf.tab <- table(truth, rf.pred)
#  	rf.pred
# truth Fail Pass
# 	0   16   23
# 	1   14   62
rf.err <- (sum(rf.tab)-sum(diag(rf.tab)))/sum(rf.tab)
# error: 0.3217391

## Determine best tree
err.mtx <- matrix(c("Full", full.err, "Pruned", prune.err, "Bagged", bag.full.err,
                    "Boosted", boost.err, "Random Forest", rf.err),
                  byrow=TRUE, ncol=2)
# "Full"      	"0.408695652173913"
# "Pruned"    	"0.31304347826087"
# "Bagged"    	"0.295652173913043"
# "Boosted"   	"0.408695652173913"
# "Random Forest" "0.321739130434783"

## All models performed better than the full tree except boosted, which performed equally
## Best model: Bagged
sort(pf.bag$importance, decreasing = TRUE)[1:5]
# failures.math    	absences.math  absences.portuguese           	Mjob	schoolsup.portuguese
# 32.584834        	11.944222            	11.591415       	7.831451            	5.549673
plot(pf.bag$importance, main="Important Variables (Bagging)", xlab="Variable Index", ylab="Importance")



################################################################################
# Logistic Regression 
# ################################################################################
# 
num_cols <- names(clean_factor)[sapply(clean_factor, is.numeric)] # checks which columns are numbers (logistic)
num_cols
correlation_matrix <- cor(clean_factor[, num_cols])
high_corr_vars <- findCorrelation(correlation_matrix, cutoff = 0.80) # 80% correlation cutoff

logistic_cleaned <- clean_factor[, -which(names(clean_factor) %in% num_cols[high_corr_vars])]
length(logistic_cleaned)


logistic_cleaned <- subset(logistic_cleaned, select = -c(pass_fail_1, pass_fail_2, pass_fail_3, activities.math, activities.portuguese, higher.math, higher.portuguese, schoolsup.math))
length(logistic_cleaned)

# ^^^^ this is important for reducing multicollinearity and improving variance compared to using the whole data set

# pass both/fail at least 1
# define pass/fail groups from the original merged data
fail_group <- merged[ clean_factor$pass_binary == "0", ]
pass_group <- merged[ clean_factor$pass_binary == "1", ]

# scatterplot for math grades vs portugese grades (pass/fail)
plot(fail_group$G3.math,      fail_group$G3.portuguese,
     col = "red", pch = 1,
     xlab = "Final Grade – Math",
     ylab = "Final Grade – Portuguese",
     main = "Pass Both vs Fail At Least One")

points(pass_group$G3.math,    pass_group$G3.portuguese,
       col = "blue", pch = 3)

legend("bottomright",
       legend = c("Fail One or Both", "Pass Both"),
       col    = c("red","blue"),
       pch    = c(1,3))
abline(0, 1, lty = 2)


# new training and test sets using 70/30 split
log_split_index_1 <- sample(1:nrow(logistic_cleaned), 0.70 * nrow(logistic_cleaned))
log_train_set_1 <- logistic_cleaned[log_split_index_1, ]
log_test_set_1 <- logistic_cleaned[-log_split_index_1, ]

# full logistic regression model
logitmod_full <- glm(pass_binary ~ ., data = log_train_set_1, family = binomial, control = glm.control(maxit = 50)) # 50 iterations necessary for the model to actually fully converge
summary(logitmod_full)


pred_probs_full <- predict(logitmod_full, newdata = log_test_set_1, type = "response")
pred_classes_full <- as.numeric(pred_probs_full > 0.5)  # the passing/failing threshold 50%
confusionMatrix(as.factor(pred_classes_full), log_test_set_1$pass_binary)


#  histogram comparing predicted probabilities by actual test outcome
ggplot(data.frame(prob = pred_probs_full, actual = log_test_set_1$pass_binary),
       aes(x = prob, fill = actual)) +
  geom_histogram(position = "identity", bins = 20, alpha = 0.6) +
  scale_fill_manual(values = c("red", "blue"), labels = c("Fail", "Pass")) +
  labs(title = "Predicted Probabilities by Outcome (Full Model)",
       x = "Predicted Probability", y = "Count", fill = "Actual Outcome") +
  theme_minimal() +
  scale_x_continuous(limits = c(0, 1))

# stepwise logistic regression model
step_logit <- step(logitmod_full, direction = "both", trace = FALSE)
summary(step_logit)

pred_probs_step <- predict(step_logit, newdata = log_test_set_1, type = "response")
pred_classes_step <- as.numeric(pred_probs_step > 0.5) # the passing/failing threshold 50%
confusionMatrix(as.factor(pred_classes_step), log_test_set_1$pass_binary)

# stepwise model predicted probability histogram
ggplot(data.frame(prob = pred_probs_step, actual = log_test_set_1$pass_binary),
       aes(x = prob, fill = actual)) +
  geom_histogram(position = "identity", bins = 20, alpha = 0.6) +
  scale_fill_manual(values = c("red", "blue"), labels = c("Fail", "Pass")) +
  labs(title = "Predicted Probabilities by Outcome (Stepwise Model)",
       x = "Predicted Probability", y = "Count", fill = "Actual Outcome") +
  theme_minimal() +
  scale_x_continuous(limits = c(0, 1))




ggplot(clean_factor, aes(x = pass_binary)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Pass Outcome Histogram",
       x = "Pass Both (1 = Yes, 0 = No)",
       y = "Student Count") +
  theme_minimal()



# After fitting step_logit
step_summary <- tidy(step_logit)  # broom::tidy extracts estimates & p-values
step_summary_sig <- subset(step_summary, p.value < 0.05 & term != "(Intercept)")

ggplot(step_summary_sig, aes(x = reorder(term, estimate), y = estimate)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Significant Predictors (Stepwise Model)",
       x = "Variable",
       y = "Log-Odds Estimate") +
  theme_minimal()


# Plotting decision boundary using two predictors from stepwise model
# We'll use failures.math and absences.portuguese (both in step_logit)

# Generate grid of values for plotting
x_seq <- seq(min(logistic_cleaned$failures.math), max(logistic_cleaned$failures.math), length.out = 100)
y_seq <- seq(min(logistic_cleaned$absences.portuguese), max(logistic_cleaned$absences.portuguese), length.out = 100)
grid_vals <- expand.grid(failures.math = x_seq, absences.portuguese = y_seq)

# Add predicted probabilities using the final stepwise model
# grid_vals$prob <- predict(step_logit, newdata = grid_vals, type = "response")
fail_group_g3 <- merged[clean_factor$pass_binary == 0, ]
pass_group_g3 <- merged[clean_factor$pass_binary == 1, ]


step2d <- glm(pass_binary ~ failures.math + absences.portuguese,
              data = logistic_cleaned, family = binomial)

# uses the simple failures.math + absences.portuguese model
grid_vals$prob <- predict(step2d, newdata = grid_vals, type = "response")


################################################################################
# Lasso Regression using glmnet (with 70/30 split)
# This model performs automatic variable selection by shrinking less important
# coefficients to zero, helping us identify key predictors of pass/fail status
################################################################################

# Load required package
library(glmnet)

# Prepare data for glmnet:
################################################################################
# Drop pass_fail_1, pass_fail_2, and pass_fail_3 before modeling
# These variables are derived from the same G1-G3 grade data as our target variable
# pass_binary. Including them as predictors would cause data leakage, since they 
# directly encode information about the outcome we are trying to predict.
#
# Keeping them in the model would artificially boost performance and make results
# unreliable. To ensure a valid and interpretable model, we exclude them here.
################################################################################
lasso_train_set_1 <- train_set_1 # rename the train_set_1 to lasso_train_set_1 for not impact other models 
lasso_test_set_1 <- test_set_1 # rename the test_set_1 to lasso_test_set_1 for not impact other models 

lasso_train_set_1 <- subset(lasso_train_set_1, select = -c(pass_fail_1, pass_fail_2, pass_fail_3))
lasso_test_set_1 <- subset(lasso_test_set_1, select = -c(pass_fail_1, pass_fail_2, pass_fail_3))

# - model.matrix() creates a numeric matrix from predictors (excluding the intercept)
# - response variable must be numeric (factor levels "0" and "1")
x_train <- model.matrix(pass_binary ~ ., data = lasso_train_set_1)[,-1]  # predictors
y_train <- lasso_train_set_1$pass_binary                                 # response (factor)

x_test <- model.matrix(pass_binary ~ ., data = lasso_test_set_1)[,-1]
y_test <- lasso_test_set_1$pass_binary

# Run Lasso with cross-validation to choose the best lambda

cvfit <- cv.glmnet(x_train, y_train, alpha = 1, family = "binomial")

# Plot cross-validation error vs. lambda
plot(cvfit)
title("Lasso CV: Choosing Optimal Lambda", line = 2.5)

# Extract the best lambda (regularization strength)
best_lambda <- cvfit$lambda.min
best_lambda  # Print optimal lambda value
#  0.03803874

# Fit final Lasso model using best lambda
lasso_model <- glmnet(x_train, y_train, alpha = 1, lambda = best_lambda, family = "binomial")

# Show selected (non-zero) coefficients from the model
lasso_coef <- coef(lasso_model)
selected_vars <- rownames(lasso_coef)[lasso_coef[,1] != 0]
selected_vars <- selected_vars[-1]  # Remove intercept
print(selected_vars) 
###############################################################################################################
#[1] "age"                     "Medu"                    "Fedu"                    "Mjobservices"           
#[5] "reasonreputation"        "failures.math"           "higher.mathyes"          "schoolsup.portugueseyes"
#[9] "higher.portugueseyes"    "absences.portuguese"     "guardianother" 
###############################################################################################################

################################################################################
# Evaluate the Lasso model on the test set
################################################################################

# Predict probabilities on test set
lasso_probs <- predict(lasso_model, s = best_lambda, newx = x_test, type = "response")

# Classify as "1" if predicted prob > 0.5, otherwise "0"
lasso_pred <- ifelse(lasso_probs > 0.5, 1, 0)

# Calculate accuracy on test data
lasso_accuracy <- mean(lasso_pred == as.numeric(as.character(y_test)))
print(paste("Lasso Test Accuracy:", round(lasso_accuracy, 4)))
# Lasso Test Accuracy: 0.7217

# Confusion Matrix
actual <- factor(y_test, levels = c(0, 1))
predicted <- factor(lasso_pred, levels = c(0, 1))
conf_matrix <- table(Predicted = predicted, Actual = actual)
print("Confusion Matrix:")
print(conf_matrix)

####################
#  Actual
#Predicted  0  1
#        0 10  3
#        1 29 73
###################

# Coefficient Plot
# Extract non-zero coefficients (excluding intercept)
lasso_coef <- coef(lasso_model)
coef_matrix <- as.matrix(lasso_coef)

# Convert to data frame and keep non-zero coefficients
coef_df <- as.data.frame(coef_matrix)
coef_df <- coef_df[coef_df[,1] != 0, , drop = FALSE]  # keep non-zero
coef_df <- coef_df[-1,, drop = FALSE]  # drop intercept

# Rename column properly
names(coef_df)[1] <- "Coefficient"  # change column name to 'Coefficient'

# Add variable names as a new column
coef_df$Variable <- rownames(coef_df)
rownames(coef_df) <- NULL  # clean rownames

# Load ggplot2 and plot
library(ggplot2)

ggplot(coef_df, aes(x = reorder(Variable, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Important Predictors Selected by Lasso",
       x = "Variable",
       y = "Coefficient") +
  theme_minimal()


