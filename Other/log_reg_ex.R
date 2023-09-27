# Logistic Regression Example - 2012 Election Data
# Data source: This example uses the data set Election12.csv, which can be found in the Data 
# folder on Canvas. This data is a subset of data from the American National Election Survey 
# from 2012 (https://electionstudies.org/project/2012-time-series-study/).

# Libraries
library(tidyverse)
library(lmtest) # for comparing nested models
library(InformationValue) # for ROC curves

# Load in data
elec_df <- read.csv("Election12.csv")
elec_df <- elec_df %>% select(-c(X)) # remove id column
head(elec_df)

# Missing quite a few rows for outcome - will omit missing data
sum(complete.cases(elec_df$vote12))/nrow(elec_df)
elec_df <- na.omit(elec_df)
dim(elec_df)

# Transform response to 0/1 with Obama=1
elec_df$vote12 <- ifelse(elec_df$vote12=='Obama',1,0)
elec_df$vote12 <- as.factor(elec_df$vote12)

# Model with all variables
glm.1 <- glm(vote12~.,data=elec_df,family="binomial")
summary(glm.1)

# Consider an interaction term
glm.2 <- glm(vote12~.+black*religion,data=elec_df,family="binomial")
summary(glm.2)
lrtest(glm.1, glm.2)

# Consider an interaction term
glm.3 <- glm(vote12~.+black*religion+hispanic*religion,data=elec_df,family="binomial")
summary(glm.3)
lrtest(glm.2, glm.3) # not significant

# Outliers
hist(resid(glm.2, type="pearson")) # change to deviance
abs_resid <- abs(resid(glm.2))
elec_df[order(abs_resid,decreasing=TRUE)[1:10],]

# Accuracy, sensitivity, specificity
prob <- predict(glm.2, newdata = elec_df, type = "response")
pred_vote <- ifelse(prob>0.5, 1, 0)
table(pred_vote,elec_df$vote12)

# ROC
plotROC(elec_df$vote12,prob,Show.labels=F)  

# Calibration
elec_df$prob <- prob
elec_df$prob_grp <- ceiling(order(prob)/374) 
calibration_df <- elec_df %>% 
  group_by(prob_grp) %>%
  summarize(avg_vote = mean(vote12==1), avg_prob = mean(prob))

ggplot(calibration_df)+geom_point(aes(x=avg_prob,y=avg_vote))+
  geom_abline(aes(slope=1,intercept=0),col="red")





