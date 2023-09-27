# Clustering examples

################## Example 1: Simulated Data

# Three components
library(MASS)
df <- matrix(0, ncol=2, nrow = 90)
df[1:30,] <- mvrnorm(30, mu = c(0,0), Sigma = 0.5*diag(2))
df[31:60,] <- mvrnorm(30, mu = c(2,2), Sigma = 0.5*diag(2))
df[61:90,] <- mvrnorm(30, mu = c(0,5), Sigma = 0.5*diag(2))
labels <- c(rep(1,30), rep(2,30), rep(3,30))
plot(df[,1], df[,2], col = labels)

# Run k-means
km_res <- kmeans(df, 3, nstart=20)
names(km_res)

# How do these compare to how data was generated?
km_res$centers
km_res$size
km_res$cluster

# Comparing to two clusters
km_res_2 <- kmeans(df, 2, nstart=20)
km_res_2$tot.withinss/km_res_2$totss
km_res$tot.withinss/km_res$totss

# Add an outlier
df[90,] <- c(100,100)
km_res <- kmeans(df, 3, nstart=20)
km_res$centers
km_res$size
km_res$cluster


################## Example 2: k-Means for Faces Data

# Faces data again but with only first 10 faces
df <- read.csv("faces.csv")
df <- df[1:100,]

# Run k-means for multiple cluster sizes and plot number of clusters
within_ss <- sapply(1:70, function(k){kmeans(x=df[,-4097], centers = k)$tot.withinss})
plot(1:70, within_ss/within_ss[1], type = "b")

# Look at center of one cluster and compare to points
km_res <- kmeans(x=df[,-4097], centers = 10)
km_res$cluster
image(matrix(as.numeric(km_res$centers[2,]), nrow=64), col=grey(seq(0, 1, length=256)))
image(matrix(as.numeric(df[1,-4097]), nrow=64), col=grey(seq(0, 1, length=256)))
image(matrix(as.numeric(df[5,-4097]), nrow=64), col=grey(seq(0, 1, length=256)))
image(matrix(as.numeric(df[7,-4097]), nrow=64), col=grey(seq(0, 1, length=256)))

################## Example 3: Hierarchical Clustering for Colleges

# College data set (a subset from American College Survey)
library(ISLR)
df <- College
names(df)

# Look at colleges with low acceptance and scale
df$Acc_Rate <- df$Accept/df$Apps
df <- df[df$Acc_Rate < 0.5, ]
df$Private <- ifelse(df$Private == "Yes", 1, 0)
scale_df <- scale(df)

# Run hierarchical clustering
df_dist <- dist(scale_df)
hc_ave <- hclust(df_dist, method="average")

# Look at Brown's cluster
plot(hc_ave, labels = rownames(scale_df), main="Average Linkage", xlab="", sub="",ylab="")

# Choices of linkage
?hclust
hc_comp <- hclust(df_dist, method="complete")
plot(hc_comp, labels = rownames(scale_df), main="Complete Linkage", xlab="", sub="",ylab="")

# Look at clusters for given height
hc_clusters <- cutree(hc_comp, h=5)
hc_clusters[hc_clusters == 1]
hc_clusters[hc_clusters == 3]

