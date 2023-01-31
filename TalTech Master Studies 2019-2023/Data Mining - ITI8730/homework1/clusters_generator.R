# Author: Urmas Pitsi
# Date:   19.sep.2019
# Version: 2
# This script generates random clusters. Feel free to copy, share and change anything you want.
# Happy experimenting!

rm(list=ls()) # Clear environment.
library(mvtnorm) # Import statistics library
set.seed(123) # Seed is necessary to make it reproducible.

num_datapoints <- 50 #sample(50:100, 1) # Number of datapoints in one cluster.
num_clusters <- 2 #sample(3:10, 1) # Number of clusters. 
cluster_colors <- rainbow(num_clusters) # Colors for each cluster.
x <- matrix(0, num_clusters * num_datapoints, 3) # Initialize output matrix with zeros. Fill in later.
colors <- matrix(cluster_colors[1], nrow(x), 1) # Initialize colors vector.

mean_range <- 100:300 # Range from which each cluster's mean is randomly drawn.
variance_range <- 50:100 # Range from which each cluster's variance is randomly drawn.

# For each cluster create some random data.
for (j in 1:num_clusters){
  sigma1 <- sample(variance_range, 1)
  # Simple hack to produce positive definite sigma matrix. Add some random shift to sigma1.
  sigma <- matrix(c(sigma1 + sample(1:50, 1), sigma1, sigma1, sigma1 + sample(1:50, 1)), ncol=2)
  mean <- c(sample(mean_range, 1), sample(mean_range, 1))
  xx <- rmvnorm(n=num_datapoints, mean=mean, sigma=sigma) # Multivariate normal distribution.
  start_idx <- ((j - 1) * num_datapoints + 1)
  end_idx <- (j * num_datapoints)
  x[start_idx:end_idx, 1:2] <- xx[,] # Copy current cluster into final result.
  x[start_idx:end_idx, 3] <- j # Store cluster index in the 3rd column.
  colors[start_idx:end_idx] <- cluster_colors[j] # Store cluster color in colors vector.
}

# Plot results.
plot(x[, 1], x[, 2], col=colors, type="p",
     xlim=c(floor(min(x[, 1])), ceiling(max(x[, 1]))), ylim=c(floor(min(x[, 2])), ceiling(max(x[, 2]))))

# Save results into RData file.
#x <- x[, 1:2] # Take first 2 columns. 3rd column has true cluster idx, don't save that.
save(x, file="C:/Users/urmas/Documents/TalTech/Andmekaeve/assignment2/input/kdata_hw2_outliers.RData") # Specify output location.




# Add some addtitional Outliers
x_outlier <- sample(mean_range, 10)
y_outlier <- x_outlier + sample(50:60, 10)

plot(x[, 1], x[, 2], col=colors, type="p",
     xlim=c(floor(min(x[, 1])), ceiling(max(x[, 1]))), ylim=c(floor(min(x[, 2])), ceiling(max(x[, 2]))))
par(new=TRUE)
plot(x_outlier, y_outlier, col="black", type="p",
     xlim=c(floor(min(x[, 1])), ceiling(max(x[, 1]))), ylim=c(floor(min(x[, 2])), ceiling(max(x[, 2]))))

outliers <- matrix(0, length(x_outlier), 3)
outliers[,1] <- x_outlier
outliers[,2] <- y_outlier
outliers[,3] <- 3

x_new <- matrix(0, nrow(x) + nrow(outliers), 3)
x_new[1:nrow(x),] <- x[,]
x_new[(nrow(x)+1):nrow(x_new),] <- outliers[,]

x <- x_new

