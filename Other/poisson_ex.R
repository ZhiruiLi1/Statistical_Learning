# Poisson Regression Example

# Based on the Open Case Study:
# Stephens, Alexandra and Jager, Leah and Taub, Margaret and Hicks, Stephanie. (2019, February 14). 
# opencasestudies/ocs-police-shootings-firearm-legislation: 
# Firearm Legislation and Fatal Police Shootings in the United States (Version v1.0.0). Zenodo. 
# http://doi.org/10.5281/zenodo.2565249
# Corresponding Paper: https://ajph.aphapublications.org/doi/full/10.2105/AJPH.2017.303770

library(tidyverse)
library(MASS)
library(usdata)

p_df <- read.csv("firearm_data.csv")
head(p_df)
dim(p_df)

# Test Poisson distribution over all US - does not look to be the case
expected <- mean(p_df$gunshot_tally)
chi.squared <- (p_df$gunshot_tally - expected)^2/expected
chi.squared.statistic <- sum(chi.squared)
pchisq(chi.squared.statistic, 50-2, lower.tail =F)

# Look at predicting rate of gunshot tallies   
mv <- glm(gunshot_tally ~  brady_scores + 
            ownership + 
            age + 
            violent_crime +
            offset(log(total_pop)), 
          family="poisson", data = p_df)
summary(mv)
hist(residuals(mv,type="pearson"))

# We can use a chi-squared test to see if distributions looks similar
chi.squared.stat.mv <- sum(residuals(mv, type = "pearson")^2)
p.value.mv <- pchisq(chi.squared.stat.mv, 50-2, lower.tail =F)

# Another type of model would be a negative binomial model
mv_nb <- glm.nb(gunshot_tally ~  brady_scores + 
                  age + 
                  violent_crime +
                  ownership +
                  offset(log(total_pop)),
                data = p_df)
summary(mv_nb)

chi.squared.stat.mv.nb <- sum(residuals(mv_nb, type = "pearson")^2)
pchisq(chi.squared.stat.mv.nb, 50-2, lower.tail =F)

p_df$pred <- predict(mv_nb, p_df, "response")

plt<- ggplot(p_df, aes(x = brady_scores)) + 
  geom_text(aes(y = gunshot_tally/total_pop*1000000/2,label=state2abbr(NAME), color="data")) +
  geom_point(aes(y = pred/total_pop*1000000/2, color = "fitted")) + 
  ggtitle("Negative Binomial Regression") + 
  ylab("Annualized Rate of Fatal Police Shootings per 1 000 000") + 
  xlab("Firearm Legislative Strength Score") + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5 ))
plt
