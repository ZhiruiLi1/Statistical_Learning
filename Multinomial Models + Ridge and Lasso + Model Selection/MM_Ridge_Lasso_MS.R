library(tidyverse)
library(caret)
library(glmnet)
library(MASS)
library(InformationValue)

election <- read_csv("/Users/justinli/Desktop/Election12_Large.csv")
election <- na.omit(election)
election$abortion <- as.factor(election$abortion)
election$congapp <- as.factor(election$congapp)
election$deathpen <- as.factor(election$deathpen)
election$age <- as.factor(election$age)
election$educ <- as.factor(election$educ)
election$income <- as.factor(election$income)
election$econ_past <- as.factor(election$econ_past)
election$unemp_past <- as.factor(election$unemp_past)
election$religion <- as.factor(election$religion)
election$gun_control <- as.factor(election$gun_control)
election$aca_app <- as.factor(election$aca_app)
election$immig_citizen <- as.factor(election$immig_citizen)
election$immig_jobs <- as.factor(election$immig_jobs)
election$govwaste <- as.factor(election$govwaste)
election$trust_social <- as.factor(election$trust_social)
election$govcorrpt <- as.factor(election$govcorrpt)
election$black <- as.factor(election$black)
election$hispanic <- as.factor(election$hispanic)
election$veteran <- as.factor(election$veteran)
election$envir_gwarm <- as.factor(election$envir_gwarm)
election$gay_marry <- as.factor(election$gay_marry)
election$gay_adopt <- as.factor(election$gay_adopt)
election$gender <- as.factor(election$gender)
election$married <-as.factor(election$married)
election$owngun <- as.factor(election$owngun)
election$usworld_stay <- as.factor(election$usworld_stay)
election$vote12 <- ifelse(election$vote12 == "Obama", 0, 1)
election$vote12 <- as.factor(election$vote12)

set.seed(123)

split = sample(c(rep(0, 0.8*nrow(election)), rep(1, 0.2*nrow(election))))
train = election[split == 0, ]
test = election[split == 1, ]
dim(train)
dim(test)
folds <- createFolds(train$vote12, k=10)
lambda_seq <- exp(seq(-7,-1,0.1))
lambda_seq
trainCtrl <- trainControl(method = "cv", index = folds)
lasso_mod_cv <- train(vote12~.,
                      data = train,
                      method = 'glmnet', # alpha=1 for lasso
                      preProc = c("scale"), # scale data # standardize the dataset trControl = trainCtrl,
                      tuneGrid = expand.grid(alpha = 1, lambda = lambda_seq),
                      metric = "Accuracy" # RMSE for regression
                      # Accuracy for classification
)
ggplot(lasso_mod_cv)
lasso_mod_best <- lasso_mod_cv$finalModel$lambdaOpt
lasso_mod_best
coef(lasso_mod_cv$finalModel, lasso_mod_cv$finalModel$lambdaOpt)
lasso_probs <- predict(lasso_mod_cv, train, type="prob")
lasso_pred <- ifelse(lasso_probs[,2]>0.5, 1, 0) # 1 means vote for Romney sum(lasso_pred == train$vote12)/nrow(train)
plotROC(train$vote12, lasso_probs[,2], Show.labels=F)
# (data, outcome)
lasso_probs_test <- predict(lasso_mod_cv, test, type="prob")
lasso_pred_test <- ifelse(lasso_probs_test[,2]>0.5, 1, 0) # 1 means vote for Romney sum(lasso_pred_test == test$vote12)/nrow(test)
plotROC(test$vote12, lasso_probs_test[,2], Show.labels=F)
# (data, outcome)
trainCtrl <- trainControl(method = "cv", index = folds)
ridge_mod_cv <- train(vote12~., # can add interactions/transformations in formula
                      data = train,
                      preProc = c("scale"), # scale data
                      method = 'glmnet',
                      trControl = trainCtrl,
                      tuneGrid = expand.grid(alpha = 0, lambda = lambda_seq), # ridge metric = "Accuracy" # can change to RMSE for regression
)
ggplot(ridge_mod_cv)
ridge_mod_best <- ridge_mod_cv$finalModel$lambdaOpt
coef(ridge_mod_cv$finalModel, ridge_mod_cv$finalModel$lambdaOpt)
ridge_probs <- predict(ridge_mod_cv, train, type="prob")
ridge_pred <- ifelse(ridge_probs[,2]>0.5, 1, 0)
sum(ridge_pred == train$vote12)/nrow(train)
plotROC(train$vote12, ridge_probs[,2], Show.labels=F)
ridge_probs_test <- predict(ridge_mod_cv, test, type="prob")
ridge_pred_test <- ifelse(ridge_probs_test[,2]>0.5, 1, 0)
sum(ridge_pred_test == test$vote12)/nrow(test)
plotROC(test$vote12, ridge_probs_test[,2], Show.labels=F)
# logistic regression with just the intercept
glm1 <- glm(vote12~1, data=train, family="binomial")
glm2 <- glm(vote12~., data=train, family="binomial")
step_best <- stepAIC(glm1,
                     direction="forward", scope=list(upper=glm2, lower=glm1), k=2) #k = 2:AIC, k = log(n):BIC
coef(step_best)
best_probs <- predict(step_best, train, type="response")
best_pred <- ifelse(best_probs>0.5, 1, 0) # 1 means vote for Romney sum(best_pred == train$vote12)/nrow(train)
plotROC(train$vote12, best_probs, Show.labels=F)
best_probs_test <- predict(step_best, test, type="response")
best_pred_test <- ifelse(best_probs_test>0.5, 1, 0)
sum(best_pred_test == test$vote12)/nrow(test)
plotROC(test$vote12, best_probs_test, Show.labels=F)
coef_df <- data.frame(lasso = as.data.frame.matrix(coef(lasso_mod_cv$finalModel,
                                                        lasso_mod_cv$finalModel$lambdaOpt)),
                      ridge = as.data.frame.matrix(coef(ridge_mod_cv$finalModel,
                                                        ridge_mod_cv$finalModel$lambdaOpt))) %> rename(lasso = s1, ridge = s1.1) # rename columns
coef_df
coef_df[abs(coef_df) < 0.00001] <- NA
coef_df$best <- NA
coef_df[names(coef(step_best)), "best"] <- coef(step_best) coef_df
max_steps = dim(train)[2]
best_cv <- rep(0, max_steps)
# All possible number of steps
for (j in 1:max_steps){
  # Iterate over folds
  for (i in 1:10){
    # Run selection up to j steps with k = 0 (no penalty)
    glm1 <- glm(vote12~1, data=train[-folds[[i]],], family="binomial")
    glm2 <- glm(vote12~., data=train[-folds[[i]],], family="binomial")
    step_best_cv <- stepAIC(glm1,
                            direction="forward", # forward stepwise selection scope=list(upper=glm2,lower=glm1),
                            k=0, # no penalty for number of predictors
                            trace = 0, # no printed information
                            steps = j # the maximum number of steps to be considered) # Get accuracy on withheld test set
                            pred_probs <- predict(step_best_cv, train[folds[[i]],], type="response")
                            pred_vals <- ifelse(pred_probs>0.5, 1, 0)
                            best_cv[j] <- best_cv[j] + sum(pred_vals == train$vote12[folds[[i]]]) # folds[[i]] accurately return indexes
                            # count total accurate prediction for each step
  } }
best_cv/nrow(train)
# Above values show the accuracy on training data for each step. For example, the first va
best_num_steps <- which.max(best_cv/nrow(train)) # CV on best number of steps best_num_steps = 16
glm1 <- glm(vote12~1, data = election, family="binomial")
glm2 <- glm(vote12~., data = election, family="binomial")
step_best_final <- stepAIC(glm1,
                           direction="forward",
                           scope=list(upper=glm2,lower=glm1),
                           k=0,
                           trace = 0,
                           steps = best_num_steps)
coef(step_best_final)
length(coef(step_best_final))
length(coef(step_best))
best_probs_noreg_test <- predict(step_best_final, test, type="response") best_pred_noreg_test <- ifelse(best_probs_noreg_test>0.5, 1, 0) # 1 means vote for Romne sum(best_pred_noreg_test == test$vote12)/nrow(test)
plotROC(test$vote12, best_probs_noreg_test, Show.labels=F)
#(data, outcome)
coef_df[names(coef(step_best_final)), "best_cv"] <- coef(step_best_final) coef_df
                            

