# Load packages
library(nlme)
library(splines)
library(tidyverse)
library(lme4)

# Load data
pressure_df <- read.csv("pressure_train.csv")

# Change to factor for R, C, u_out
pressure_df$R = as.factor(pressure_df$R)
pressure_df$C = as.factor(pressure_df$C)
pressure_df$u_out = as.factor(pressure_df$u_out)

# Model from last time - ignores correlation within a breath
knots.time <- quantile(pressure_df$time_step, c(0.1, 0.3, 0.5, 0.7, 0.9))
knots.u_in <- quantile(pressure_df$u_in, c(0.1, 0.3, 0.5, 0.7, 0.9))
mod <- lm(log(pressure+3) ~ ns(time_step, knots=knots.time)*u_out+
             ns(u_in, knots=knots.u_in)*R*C*u_out, 
           data = pressure_df)
plot(mod)

# Model with random intercept
mee_mod <- lmer(log(pressure+3) ~ ns(time_step, knots=knots.time)*u_out+
       ns(u_in, knots=knots.u_in)*R*C*u_out+(1|breath_id), data=pressure_df)
summary(mee_mod)
qqplot(residuals(mee_mod), qnorm(ppoints(nrow(pressure_df))))
qqplot(residuals(mee_mod), qnorm(ppoints(nrow(pressure_df))))

# Random and fixed effects
ranef(mee_mod)
fixef(mee_mod)

# MAE - only slight improvement
mean(abs(predict(mod)-pressure_df$pressure))
mean(abs(predict(mee_mod)-pressure_df$pressure))

# Other models - sample
breath_ids <- unique(pressure_df$breath_id)
ids <- sample(breath_ids, 100, replace=FALSE)
pressure_samp <- pressure_df[pressure_df$breath_id %in% ids, ]
pressure_samp <- pressure_samp[pressure_samp$u_out == 1, ]

mod_1 <- lmer(log(pressure+3) ~ poly(time_step,degree=2)+(1|breath_id), 
                     data=pressure_samp)
mod_2 <- lmer(log(pressure+3) ~ (1+poly(time_step,degree=2)|breath_id), 
                data=pressure_samp)
summary(mod_1)
anova(mod_1, mod_2)
mean(abs(predict(mod_2)-pressure_samp$pressure))
