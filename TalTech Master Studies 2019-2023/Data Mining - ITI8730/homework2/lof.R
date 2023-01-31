library(matrixStats)
source("C:/Users/urmas/Documents/TalTech/Andmekaeve/assignment2/cluster_analysis.R")
set.seed(0)

#---------------------------------------------------------------------------------
# Run K-Means algorithm and plot results
#---------------------------------------------------------------------------------
# 2D example
load(file="C:/Users/urmas/Documents/TalTech/Andmekaeve/assignment2/input/kdata_hw2_outliers.RData")
y <- x[,3]
x <- x[,1:2]
plot(x, col="grey", main="Raw input")

metric <- "euclidean"
num_clusters <- 3

# Runs kmeans 10 times and returns best result.
kmeans_result <- kmeans_clusters_best(x, num_clusters=num_clusters, ini_method=1, metric=metric)
centers <- kmeans_result[[1]]
labels <- kmeans_result[[2]]
plot_results(x, centers, rainbow(num_clusters)[labels])

##################################################################################
# K-Nearest Neighbors.
##################################################################################

plot_results(x, centers, rainbow(num_clusters)[y], title="Clusters")
labels <- y

##################################################################################
# Reachability: reach-dist(k) of (A,B) = max[k-dist(B), dist(A,B)].
##################################################################################

# Reachability dist of A from B is dist A->B, but at least the k-distance of B.
# 1. select two points: A and B.
# 2. find k-neighbor indices of A.
# 3. For each (a) calc reachability to B.
# 4. Local Density: lrd(k)(A) = 1 / (Sum of (a->B) / k).
# 5. LOF = sum(lrd(B)/k/lrd(A), B is set of k-Neighbors of A.

reachability_dist <- function(idx_a, idx_b, x, kdist_of_b, metric="euclidean"){
  d <- distance(to_matrix(x[idx_a,]), to_matrix(x[idx_b,]), name=metric)
  if (d < kdist_of_b){ return(kdist_of_b) }
  else { return(d) }
}

lrd <- function(idx_a, x, k, kneib_data){
  neighbors_of_a <- kneib_data[idx_a, 4:(4+k-1)]
  res <- 0.0
  for(i in 1:length(neighbors_of_a)){
    n <- neighbors_of_a[i]
    kdist <- kneib_data[n, 2]         # kn-distance / max distance of neighbor.
    d <- kneib_data[idx_a, (3 + k + i)]   # distance from neighbor -> a
    if (d < kdist) { res <- res + kdist }
    else { res <- res + d }
  }
  res <- res / k
  return(1 / res)
}

local_reach_densities <- function(x, k, kneib_data){
  len <- nrow(x)
  res <- 1:len
  for(i in 1:len){
    res[i] <- lrd(i, x, k, kneib_data)
  }
  return(res)
}

lof <- function(idx_a, x, k, kneib_data, densities){
  neighbors_of_a <- kneib_data[idx_a, 4:(4+k-1)]
  lrd_b <- 0.0
  for(i in neighbors_of_a){
    lrd_b <- lrd_b + densities[i]
  }
  lrd_b <- lrd_b / k
  return(lrd_b / densities[idx_a])
}

local_outlier_factors <- function(x, k, kneib_data){
  len <- nrow(x)
  res <- 1:len
  densities <- local_reach_densities(x, k, kneib_data)
  for(i in 1:len){
    res[i] <- lof(i, x, k, kneib_data, densities)
  }
  return(res)
}

LOF_report <- function(x, k){
  # x: input data, k: number of k in k-nearest neighbors.
  # Returns 2 column matrix:
  # 1:Local Density, 2:Local Outlier Factor.
  kneighbors_data <- knneighbors(x, NULL, y, k=k)
  loc_reach_dens <- local_reach_densities(x, k, kneighbors_data)
  loc_outlier_fac <- local_outlier_factors(x, k, kneighbors_data)
  
  res <- matrix(0, nrow=nrow(x), ncol=2)
  res[,1] <- loc_reach_dens
  res[,2] <- loc_outlier_fac
  return(res)
}

plot1 <- function(data, label_colors, title="clusters", x_label="", y_label="", point_size=0.8,
                  label_size=1.0, axis_size=0.7, title_size=0.8, subtitle_size=0.8){
  xlim <- c(floor(min(data[,1])), ceiling(max(data[,1])))
  ylim <- c(floor(min(data[,2])), ceiling(max(data[,2])))
  
  plot(data, col=label_colors, xlim=xlim, ylim=ylim, main=title, xlab=x_label, ylab=y_label, cex=point_size,
       cex.lab=label_size, cex.axis=axis_size, cex.main=title_size, cex.sub=subtitle_size)
}

# LOF Result: (n,2) matrix. 1.Col: Local Density, 2.Col: LOF
lof_result <- LOF_report(x, 7)

LOF_values <- lof_result[,2]

# Average LOF by classes/clusters.
avg_lof <- mean(LOF_values[y==2])

# Hide some LOF labels: too many for showing properly.
lof_labels <- round(LOF_values,1)
lof_labels[ ((1:length(y) %% 6 == 0) + (y==3)) < 1] <- ""

# Plot LOF with threshhold value.
lof_threshold <- 2.5
thresh_labels <- (LOF_values < lof_threshold) * 1 + 1

colors <- rainbow(3)[1:2]
plot1(x, colors[thresh_labels], point_size=0.8, title="Local Outlier Factors, k=7, threshold=2.5")
text(x, labels=lof_labels, cex=0.8, pos=1)
legend("bottomright", legend=c("LOF > 2.5", "LOF <= 2.5"), col=colors,
       pch=c(17,19), bty="n", pt.cex=0.8, cex=0.8, text.col="black", inset=c(0.02, 0.02)) #horiz=F

# Plot LOFs.
#plot1(x, rainbow(num_clusters)[y], point_size=0.5, title="Local Outlier Factors, k=7")
#text(x, labels=lof_labels, cex=0.8, pos=1)

#terrain.colors(2)
#contour(x,col=terrain.colors(20),x=x,y=y, xlab="x",ylab="y");
#lines(alignment$index1,alignment$index2,col="red",lwd=2);


