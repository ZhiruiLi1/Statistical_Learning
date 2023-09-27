################## Example 1: Simulated Data

# Simple line with noise
x <- runif(1000)
y <- x + rnorm(1000, 0, 0.01)
plot(x,y)
sim_df <- data.frame(x=x, y=y)

# Run PCA
pca_sim = prcomp(sim_df, scale = TRUE, retx = TRUE)
names(pca_sim)

# Let's look at the principal components and standard deviation for each component
pca_sim$rotation
pca_sim$sdev

# Let's look at the projection
new_pts <- as.matrix(sim_df) %*% pca_sim$rotation # X not centered or scaled
new_pts <- pca_sim$x # this gives the right projection
plot(new_pts[,1], new_pts[,2], ylim=c(-1,1))

################## Example 2: Eigenfaces
# Data Info: https://cam-orl.co.uk/facedatabase.html
# Accessed from: https://cs.nyu.edu/~roweis/data.html

df <- read.csv("faces.csv")
dim(df) # 64 x 64 images plus a label for 40 individuals
df$y 

# Test/train data - take two samples for each person out
train_ind <- (1:400 %% 10 <= 8) & (1:400 %% 10 > 0)
df_train <- df[train_ind,]
df_test <- df[!train_ind,]

# View a face
ex_face <- (matrix(as.numeric(df[1, -4097]), nrow=64))
image(ex_face, col=grey(seq(0, 1, length=256)))

# Run PCA
pca_res = prcomp(df_train[,-4097], retx = TRUE)

# The eigenvectors are stored in the rotation output
dim(pca_res$rotation)
pca_res$rotation[,1]  # not that useful to look at in this case

# Can look at percent variance explained
pca_var = pca_res$sdev^2
per_var = cumsum(pca_var)/sum(pca_var)
plot(per_var, xlab="Principal Component ", 
     ylab = " Cumulative Proportion of Variance Explained ", 
     ylim=c(0,1), type = "b")

# Let's try scaling
pca_res_scale = prcomp(df_train[,-4097], scale = TRUE, retx = TRUE)
pca_var_scale = pca_res_scale$sdev ^2
per_var_scale = cumsum(pca_var_scale)/sum(pca_var_scale)
plot(per_var_scale, xlab="Principal Component ", 
     ylab = " Cumulative Proportion of Variance Explained ", 
     ylim=c(0,1), type = "b")

# Maybe a slight improvement but in this case might make sense to stick to unscaled version
per_var[10]
per_var_scale[10]

# Reconstruction with 100 eigenvectors
img_recon <- (as.matrix(df_train[1,-4097]) %*% pca_res$rotation[, 1:25]) %*% t(pca_res$rotation[, 1:25])
image(matrix(as.numeric(img_recon), nrow=64), col=grey(seq(0, 1, length=256)))


################## Example 3: Factor Analysis

# Data using - results from difference tests
?ability.cov

# Covariance matrix
ability.cov$cov

# Factor analysis
fa1 <- factanal(covmat= ability.cov, factors=1, rotation="none")
fa1 
fa1$loadings # loadings
fa1$uniquenesses # remaining variance for each variable
apply(fa1$loadings^2,1,sum) # communalities

# Factor analysis with two factors
fa2 <- factanal(covmat= ability.cov, factors=2, rotation="none")
fa2
fa2$loadings

# Factor analysis with two factors and rotation
fa3 <- factanal(covmat= ability.cov, factors=2, rotation="varimax")
fa3
fa3$loadings

library(tidyverse)
fact_df <- as.data.frame(fa3$loadings[, 1:2])
fact_df$var <- rownames(fact_df)
fact_df <- fact_df %>% gather("factor","loading",c(Factor1, Factor2))

# Heat map
ggplot(fact_df)+geom_tile(aes(x=factor, y=var, fill=loading))
