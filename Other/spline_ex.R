# Spline Example
library(tidyverse)
library(GGally)
library(splines)

# Data source: globular filtration rate for patients collected from multiple studies
df <- read.csv("baseseg.csv")
apply(df, 2, function(x) sum(complete.cases(x))/nrow(df))

# 1) Serum Creatinine (bascre); 
# 2) Systolic blood pressure (sbase); 
# 3) Diastolic blood pressure (dbase); 
# 4) Urine protein (baseu); 
# 5) Age; 
# 6) Gender (Sex = 1 if male; = 0 if female); 
# 7) African-American (black) 
df <- df %>% dplyr::select(c(gfr, sbase, dbase, baseu, bascre, AGE, SEX, black))
df$SEX <- as.factor(df$SEX)
df$black <- as.factor(df$black)
df <- na.omit(df)

# Plot shows nonlinear relationship
ggpairs(df)
plot(df$bascre, df$gfr)
plot(df$baseu, df$gfr)
hist(log(df$baseu+1))

mod1 <- lm(gfr ~., df)
summary(mod1) 
plot(mod1)

# Diagnostics show a poor fit
plot(df$AGE, residuals(mod1))
plot(df$baseu, residuals(mod1))
plot(df$bascre, residuals(mod1))
plot(df$dbase, residuals(mod1))
plot(df$sbase, residuals(mod1))

# First transform GFR
mod2 <- lm(sqrt(gfr) ~., df)
summary(mod2)
plot(mod2)

plot(df$AGE, residuals(mod2))
plot(df$baseu, residuals(mod2))
plot(df$bascre, residuals(mod2))
plot(df$dbase, residuals(mod2))
plot(df$sbase, residuals(mod2))

# Consider adding non-linearity
knots_baseu <- quantile(df$baseu, c(0.1, 0.5, 0.9))
knots_bascre <- quantile(df$bascre, c(0.1, 0.5, 0.9))
mod3 <- lm(sqrt(gfr)~ns(baseu, knots = knots_baseu)+
             ns(bascre, knots=knots_bascre)+dbase+sbase+AGE+SEX, data=df)
summary(mod3)
plot(mod3)

# Should we transform baseu or age
mod4 <- lm(sqrt(gfr)~log(baseu)+ns(bascre, knots=knots_bascre)+AGE+SEX, data=df)
summary(mod4)
plot(df$AGE, residuals(mod4))

mod5 <- lm(sqrt(gfr)~log(baseu)+ns(bascre, knots=knots_bascre)+poly(AGE, degree=2)+SEX, data=df)
summary(mod5)

# Library gam
library(mgcv)
mod5 <- gam(sqrt(gfr)~log(baseu)+ns(bascre, knots=knots_bascre)+poly(AGE, degree=2)+SEX, data=df)
summary(mod5)
plot.gam(mod5, all.terms=TRUE) # shows nonlinear relationships

# Look at diagnostics
plot(df$AGE, residuals(mod5))
plot(df$baseu, residuals(mod5))
plot(df$bascre, residuals(mod5))

# Could we have achieved similar with simpler transformations?
mod6 <- lm(gfr~I(1/baseu)+I(1/bascre)+poly(AGE, degree=2)+SEX, data=df)
summary(mod6)
plot(mod6)

# Plot spline for bascre 
bascre_grid <- seq(min(df$bascre), max(df$bascre), 0.1)
df_new <- data.frame(bascre=bascre_grid)
df_new$AGE <- mean(df$AGE)
df_new$SEX <- as.factor(0)
df_new$baseu <- mean(df$baseu)
y <- predict(mod5, df_new)
plot(bascre_grid, y)
