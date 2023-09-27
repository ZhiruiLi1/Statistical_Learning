# Libraries
library(tidyverse)
library(caret)
library(glmnet)
library(MASS)
library(InformationValue)

# Load data
tb_df <- read.csv("tb_train.csv")
ext_df <- read.csv("tb_ext.csv") # external data set will use to compare

# Remove missing data
tb_df <- na.omit(tb_df)
ext_df <- na.omit(ext_df)

# Change outcome to factor
tb_df$xpert <- as.factor(tb_df$xpert) 

# Create folds for k-fold CV
folds <- createFolds(tb_df$xpert, k=10)


############## LASSO ####################

# Potential lambda values
lambda_seq <- exp(seq(-7,-1,0.1))

# Lasso using cross-validation through the caret package
# glmnet - alpha=1 for lasso, alpha=0 for ridge
trainCtrl <- trainControl(method = "cv", index = folds)
lasso_mod_cv <- train(xpert~.,  # can add interactions/transformations in formula
             data = tb_df,
             preProc = c("scale"), # scale data
             method = 'glmnet', 
             trControl = trainCtrl,
             tuneGrid = expand.grid(alpha = 1, lambda = lambda_seq),
                      # tuning parameters for CV
             metric =  "Accuracy" # can change to RMSE for regression
)
ggplot(lasso_mod_cv)

# Fit the best lasso with lambda on whole data
lasso_mod_best <- lasso_mod_cv$finalModel$lambdaOpt
coef(lasso_mod_cv$finalModel, lasso_mod_cv$finalModel$lambdaOpt)

# Training accuracy and AUC
lasso_probs <- predict(lasso_mod_cv, tb_df, type="prob")
lasso_pred <- ifelse(lasso_probs[,2]>0.5, 1, 0)
sum(lasso_pred == tb_df$xpert)/nrow(tb_df)
plotROC(tb_df$xpert,lasso_probs[,2],Show.labels=F) 

# Test accuracy, calibration, and AUC
lasso_probs_test <- predict(lasso_mod_cv, ext_df, type="prob")
lasso_pred_test <- ifelse(lasso_probs_test[,2]>0.5, 1, 0)
sum(lasso_pred_test == ext_df$xpert)/nrow(ext_df)
plotROC(ext_df$xpert,lasso_probs_test[,2],Show.labels=F) 

############## RIDGE ####################

# Ridge using cross-validation through the caret package
# glmnet - alpha=1 for lasso, alpha=0 for ridge
trainCtrl <- trainControl(method = "cv", index = folds)
ridge_mod_cv <- train(xpert~.,  # can add interactions/transformations in formula
                      data = tb_df,
                      preProc = c("scale"), # scale data
                      method = 'glmnet', 
                      trControl = trainCtrl,
                      tuneGrid = expand.grid(alpha = 0, lambda = lambda_seq),
                      # tuning parameters for CV
                      metric =  "Accuracy" # can change to RMSE for regression
)
ggplot(ridge_mod_cv)

# Fit the best ridge with lambda on whole data
ridge_mod_best <- ridge_mod_cv$finalModel$lambdaOpt
coef(ridge_mod_cv$finalModel, ridge_mod_cv$finalModel$lambdaOpt) 

# Training accuracy and AUC
ridge_probs <- predict(ridge_mod_cv, tb_df, type="prob")
ridge_pred <- ifelse(ridge_probs[,2]>0.5, 1, 0)
sum(ridge_pred == tb_df$xpert)/nrow(tb_df)
plotROC(tb_df$xpert,ridge_probs[,2],Show.labels=F) 

# Test accuracy, calibration, and AUC
ridge_probs_test <- predict(ridge_mod_cv, ext_df, type="prob")
ridge_pred_test <- ifelse(ridge_probs_test[,2]>0.5, 1, 0)
sum(ridge_pred_test == ext_df$xpert)/nrow(ext_df)
plotROC(ext_df$xpert,ridge_probs_test[,2],Show.labels=F) 

############## Best-Subset ####################

# Without CV
glm1 <- glm(xpert~1, data=tb_df, family="binomial")
glm2 <- glm(xpert~., data=tb_df, family="binomial")
step_best <- stepAIC(glm1,
                     direction="forward",
                     scope=list(upper=glm2,lower=glm1),
                     k=2) #2 = AIC, log(n) = BIC, try changing this!
coef(step_best)

# Find the best subset model using forward stepwise selection
best_cv <- rep(0,12)
# All possible number of steps
for (j in 1:12){
  # Iterate over folds
  for (i in 1:10){
    
    # Run selection up to j steps with k = 0 (no penalty)
    glm1 <- glm(xpert~1, data=tb_df[-folds[[i]],], family="binomial")
    glm2 <- glm(xpert~., data=tb_df[-folds[[i]],], family="binomial")
    step_best <- stepAIC(glm1,
                         direction="forward",
                         scope=list(upper=glm2,lower=glm1),
                         k=0,
                         trace=0,
                         steps = j)
    
    # Get accuracy on withheld test set
    pred_probs <- predict(step_best, tb_df[folds[[i]],], type="response")
    pred_vals <- ifelse(pred_probs>0.5, 1, 0)
    best_cv[j] <- best_cv[j] + sum(pred_vals == tb_df$xpert[folds[[i]]])
  }
}
best_num_steps <- which.max(best_cv/nrow(tb_df))

# Refit
glm1 <- glm(xpert~1, data=tb_df, family="binomial")
glm2 <- glm(xpert~., data=tb_df, family="binomial")
step_best <- stepAIC(glm1,
                     direction="forward",
                     scope=list(upper=glm2,lower=glm1),
                     k=0,
                     steps = best_num_steps) 
coef(step_best)

# Training accuracy and AUC
best_probs <- predict(step_best, tb_df, type="response")
best_pred <- ifelse(best_probs>0.5, 1, 0)
sum(best_pred == tb_df$xpert)/nrow(tb_df)
plotROC(tb_df$xpert,best_probs,Show.labels=F) 

# Test accuracy and AUC
best_probs_test <- predict(step_best, ext_df, type="response")
best_pred_test <- ifelse(best_probs_test>0.5, 1, 0)
sum(best_pred_test == ext_df$xpert)/nrow(ext_df)
plotROC(ext_df$xpert,best_probs_test,Show.labels=F)

# Compare coefficients
coef_df <- data.frame(lasso = as.data.frame.matrix(coef(lasso_mod_cv$finalModel, 
                                                   lasso_mod_cv$finalModel$lambdaOpt)),
           ridge = as.data.frame.matrix(coef(ridge_mod_cv$finalModel, 
                                             ridge_mod_cv$finalModel$lambdaOpt))) %>%   
  rename(lasso = s1, ridge = s1.1)
coef_df[abs(coef_df) < 0.00001] <- NA
coef_df$best <- NA
coef_df[names(coef(step_best)),"best"] <- coef(step_best)
coef_df
# What do you notice?

