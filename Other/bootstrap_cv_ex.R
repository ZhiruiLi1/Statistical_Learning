library(tidyverse)
library(mgcv)
library(splines)

# Same data as for spline example
df <- read.csv("baseseg.csv")
df <- df %>% dplyr::select(c(gfr, sbase, dbase, baseu, bascre, AGE, SEX, black))
df$SEX <- as.factor(df$SEX)
df$black <- as.factor(df$black)
df <- na.omit(df)

hist(df$gfr)
summary(df$gfr)
quants <- quantile(df$gfr,c(0.05,0.95))

bootstrap_quants <- function(df){
  df_samp <- df[sample(1:nrow(df), nrow(df), replace=TRUE),]
  return(quantile(df_samp$gfr, c(0.05, 0.95)))
}

results <- replicate(1000,bootstrap_quants(df))
dim(results)
sd1 <- sd(results[1,])
sd2 <- sd(results[2,])

ggplot() +
  geom_histogram(aes(df$gfr)) +
  labs(x="GFR", y="Count") +
  geom_segment(aes(x=quants[1], xend=quants[1], y = 0, yend=150), color="red")+
  geom_segment(aes(x=quants[2], xend=quants[2], y = 0, yend=150), color="red")+
  geom_segment(aes(x=quants[1]-1.95*sd1, xend=quants[1]+1.95*sd1, y=75, yend=75), color="red")+
  geom_segment(aes(x=quants[2]-1.95*sd2, xend=quants[2]+1.95*sd2, y=75, yend=75), color="red")+
  theme_minimal()

# Spline - choose knots

ids <- sample(1:nrow(df), nrow(df), replace=FALSE)
folds <- sapply(1:nrow(df), function(i) i %% 10 +1)


errors <- c(0,0,0)
for (i in 1:10){
  df_train <- df[ids[folds!=i],]
  df_test <- df[ids[folds==i],]
  
  knots1 <- quantile(df_train$bascre, c(0.1, 0.9))
  mod1 <- lm(gfr~ns(bascre,knots=knots1), df_train)
  pred1 <- predict(mod1, df_test)
  errors[1] <- errors[1] + mean((df_test$gfr-pred1)^2)*nrow(df_test)/nrow(df)
  
  knots2 <- quantile(df_train$bascre, c(0.1, 0.5, 0.9))
  mod2 <- lm(gfr~ns(bascre,knots=knots2), df_train)
  pred2 <- predict(mod2, df_test)
  errors[2] <- errors[2] + mean((df_test$gfr-pred2)^2)*nrow(df_test)/nrow(df)
  
  knots3 <- quantile(df_train$bascre, c(0.1, 0.3, 0.5, 0.7, 0.9))
  mod3 <- lm(gfr~ns(bascre,knots=knots3), df_train)
  pred3 <- predict(mod3, df_test)
  errors[3] <- errors[3] + mean((df_test$gfr-pred3)^2)*nrow(df_test)/nrow(df)
}
errors

knots3 <- quantile(df$bascre, c(0.1, 0.3, 0.5, 0.7, 0.9))
mod3 <- lm(gfr~ns(bascre,knots=knots1), df)
df_test <- data.frame(bascre = seq(min(df$bascre), max(df$bascre), 0.01))
pred3 <- predict(mod3, df_test)
ggplot()+
  geom_point(aes(x=df$bascre,y=df$gfr)) +
  geom_line(aes(x=df_test$bascre, y=pred3), color="red") +
  labs(x="Serum Creatinine", y="GFR") +
  theme_minimal()
  
