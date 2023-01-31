library(matrixStats)
#library(cluster)
source("C:/Users/urmas/Documents/TalTech/Andmekaeve/assignment2/distances.R")

# https://en.wikipedia.org/wiki/Cluster_analysis

#---------------------------------------------------------------------------------
# General functions. 
#---------------------------------------------------------------------------------
accuracy_score <- function(truth, prediction) {
  return(sum(truth==prediction) / length(truth))
}

confusion_matrix <- function(truth, prediction) {
  labels <- unique(truth)
  num_labels <- length(labels)
  res <- matrix(0, nrow=num_labels, ncol=num_labels)
  for (i in 1:length(truth)) {
    p <- prediction[i]
    t <- truth[i]
    res[t,p] <- res[t,p] + 1
  }
  return(res)
}

counter_by <- function(data, decreasing=FALSE) {
  V <- unique(data)
  res <- matrix(NA, nrow=length(V), ncol=2)
  for (i in 1:length(V)) {
    res[i,1] <- V[i]
    res[i,2] <- count(data, value=V[i])
  }
  sorted <- sort(res[,2], index.return=TRUE, decreasing=decreasing)
  res[,1] <- res[,1][sorted[[2]]]
  res[,2] <- res[,2][sorted[[2]]]
  return(res)
}

#---------------------------------------------------------------------------------
# K-Means algorithm. 
#---------------------------------------------------------------------------------
# Initialize centers.
initialize_centers <- function(data, num_clusters, method=1){
  if (method==1){
    # Choose random points from existing data.
    return(data[sample(1:nrow(data), num_clusters),])
  }
  else if (method==2){
    # Choose random points in the bounds of data.
    x_range = min(data[,1]):max(data[,1])
    y_range = min(data[,2]):max(data[,2])
    return(matrix(c(sample(x_range, num_clusters), sample(y_range, num_clusters)), ncol=ncol(data)))
  }
  else {
    return(data[sample(1:nrow(data), num_clusters),])
  }
}

# Assign each point to a cluster.
find_closest_centers <- function(data, centers, metric="euclidean"){
  n <- nrow(centers)
  res <- matrix(999999999.9, nrow(data), ncol=n)
  for (i in 1:n) {
    b <- matrix(c(centers[i,]), nrow=nrow(data), ncol=ncol(data), byrow=TRUE)
    res[, i] <- distance(data, b, metric)
  }
  return(max.col(-res))
}

# Calculate new center for each cluster.
calculate_new_centers <- function(data, closest_centers, old_centers, metric="euclidean"){
  n <- nrow(old_centers)
  res <- old_centers # matrix(0.00001, nrow=n, ncol=ncol(data))
  #eps <- 0.001
  for (i in 1:n) {
    a <- data[closest_centers==i,]
    if (nrow(a) > 0) {
      res[i,] <- colMeans(a) # Arithmetic average of coordinates.
    }
  }
  return(res)
}

check_if_solution <- function(new_centers, old_centers){
  return(sum(abs(new_centers - old_centers)) < 1.0)
}

# Iterate to find clusters.
kmeans_clusters <- function(data, num_clusters=3, num_iterations=20, ini_method=1, metric="euclidean", verbose=FALSE){
  # Start iterating until solution.
  for (i in 1:num_iterations){
    # 1. Initialize random center points for clusters or store new centers.
    if (i == 1) { centers <- initialize_centers(data, num_clusters=num_clusters, method=ini_method) }
    else { centers <- new_centers }
    
    # 2. Assign each point to a cluster.
    closest_centers <- find_closest_centers(data, centers, metric=metric)

    # 3. Calculate center of each cluster.
    new_centers <- calculate_new_centers(data, closest_centers, centers, metric=metric)

    # 4. Check if solution.
    found_solution <- (check_if_solution(new_centers, centers))
    if (found_solution){
      if (verbose) { print(paste("Found solution at iteration:", i)) }
      break
    }
  }
  return(list(new_centers, closest_centers))
}

kmeans_clusters_best <- function(data, num_clusters=3, num_iterations=20, ini_method=1, metric="euclidean") {
  # Runs kmeans_clusters algorithm n-times and returns best result based on criteria (minimum).
  n <- 10
  res <- NULL
  criteria_value <- 999999999999.9 # Initially very large value, we optimize for smaller is better.
  #criteria_value <- silhouette_score(data, labels, metric)
  for (i in 1:n) {
    res_new <- kmeans_clusters(data, num_clusters, num_iterations, ini_method, metric)
    centers <- res_new[[1]]
    labels <- res_new[[2]]
    criteria_value_new <- cluster_inertia(data, labels, centers, metric)
    if (criteria_value_new < criteria_value) {
      res <- res_new
      criteria_value <- criteria_value_new
    }
  }
  return(res)
}

kmeans_k_finder <- function(data, num_clusters_range=(2:5), metric="euclidean") {
  # Returns score value for each k, where k is number of clusters.
  res <- matrix(NA, nrow=length(num_clusters_range), ncol=2)
  #score <- silhouette_score(data, labels, metric)
  for (i in 1:length(num_clusters_range)) {
    k <- num_clusters_range[i]
    kmeans_result <- kmeans_clusters_best(data, k, metric=metric)
    centers <- kmeans_result[[1]]
    labels <- kmeans_result[[2]]
    score <- cluster_inertia(data, labels, centers, metric)
    res[i,1] <- k
    res[i,2] <- score
  }
  return(res)
}

#---------------------------------------------------------------------------------
# Cluster analytics: try to interpret the results with Silhouette etc. 
#---------------------------------------------------------------------------------

distances_2_clusters <- function(cluster1, cluster2=NULL, metric="euclidean", knneighbors=0){
  # If cluster2=NULL then calculate inside cluster1 otherwise between cluster1 and cluster2.
  # Result colummns: 1:Sum, 2:Average, 3:Min, 4:Max, 5:Sum of squared distances,
  #   6:K-nearest neighbors indexes if k is specified + k-distances.
  #   E.g. if knneighbors=4 then 8 columns will be added.
  # knneighbors: adds 2 * k columns with indexes and distances of k-nearest neighbors.
  is_same_cluster = is.null(cluster2)
  if (is_same_cluster) { cluster2 <- cluster1 }
  res <- matrix(0.0, nrow=nrow(cluster1), ncol=(5 + 2 * knneighbors))
  c <- ncol(cluster1)
  n <- nrow(cluster1)
  n2 <- nrow(cluster2)
  for (i in 1:n){
    a_one <- c(cluster1[i,])                          # Take row-by-row.
    a <- matrix(a_one, nrow=n2, ncol=c, byrow=TRUE)   # Expand to match dimensions.
    d <- distance(a, cluster2, metric)                # Distances from point to all points in cluster2.
    s <- sum(d)                                       # Sum distances to other points.
    if (is_same_cluster) { n <- n - 1 }               # Div by n - 1: exclude itself
    res[i, 1] <- s                                    # Sum
    res[i, 2] <- if (n > 1) {s / n } else { s }       # Average
    res[i, 3] <- if (is_same_cluster) { min(d[1:n!=i]) } else { min(d) } # Min distance to a point in cluster2. Eclude itself if same cluster.
    res[i, 4] <- max(d)                               # Max distance to a point in cluster2.
    res[i, 5] <- sum(d * d)                           # Sum of squared distances.
    if (knneighbors > 0) {
      skip <- if (is_same_cluster) { 1 } else { 0 }   # If same cluster, then first element is itself, start from 2nd.
      sorted_dist <- sort(d, index.return=TRUE)
      res[i, 6:(6+knneighbors-1)] <- sorted_dist[[2]][(1+skip):(knneighbors+skip)] # k-nearest neighbors indexes.
      res[i, (6+knneighbors):(6+2*knneighbors-1)] <- sorted_dist[[1]][(1+skip):(knneighbors+skip)] # k-nearest neighbors distances.
    }
  }
  return(res)
}

cluster_analytics <- function(data, labels, metric="euclidean"){
  # Result colummns:
  # 1.Label.
  # 2.Original row nr.
  # 3.Sum of distances in same cluster.
  # 4.Average distance in same cluster.
  # 5.Minimum average distance to an outside cluster.
  # 6.Silhouette coefficient: https://en.wikipedia.org/wiki/Silhouette_(clustering)
  L <- unique(labels)
  res <- matrix(0.0, nrow=nrow(data), ncol=6)
  c <- ncol(data)
  r <- 1
  for (l in L) {
    idx_same_cluster <- which((labels==l))
    data_same_cluster <- data[idx_same_cluster,]
    n <- nrow(data_same_cluster)
    r2 <- r + n - 1
    inside_analytics <- distances_2_clusters(data_same_cluster, NULL, metric)
    res[r:r2, 1] <- l
    res[r:r2, 2] <- matrix(idx_same_cluster, ncol=1)[,1]
    res[r:r2, 3] <- inside_analytics[,1] # Sum of distances inside cluster.
    res[r:r2, 4] <- inside_analytics[,2] # Average distance inside cluster.
    
    min_avg_out <- matrix(99999999999.9, nrow=nrow(inside_analytics), ncol=1)
    for (k in L[L!=l]) {
      data_not_cluster <- data[(labels==k),]
      outside_analytics <- distances_2_clusters(data_same_cluster, data_not_cluster, metric)
      min_avg_out <- rowMins(matrix(c(min_avg_out, outside_analytics[,2]), ncol=2))
    }
    res[r:r2, 5] <- min_avg_out # Minimum average distance to an outside cluster.
    r <- r2 + 1
  }
  res[,6] <- (res[,5] - res[,4]) / rowMaxs(matrix(c(res[,5], res[,4]), ncol=2))
  return(res)
}

intra_cluster_distances <- function(data, labels, metric="euclidean"){
  L <- unique(labels)
  res <- matrix(0.0, nrow=length(L), ncol=3)
  for (l in L) {
    inside_analytics <- distances_2_clusters(data[(labels==l),], NULL, metric)
    res[l, 1] <- sum(inside_analytics[,1]) / 2 # Sum of distances inside cluster. Each is calculated twice.
    res[l, 2] <- mean(inside_analytics[,2]) # Average distance inside cluster.
    res[l, 3] <- sum(inside_analytics[,5]) / 2 # Sum of squared distances inside cluster.
  }
  return(res)
}

inter_cluster_distances <- function(data, labels, metric="euclidean"){
  L <- unique(labels)
  res <- matrix(0.0, nrow=length(L), ncol=3)
  for (l in L) {
    inside_analytics <- distances_2_clusters(data[(labels==l),], data[(labels!=l),], metric)
    res[l, 1] <- sum(inside_analytics[,1]) / 2 # Sum of distances inside cluster. Each is calculated twice.
    res[l, 2] <- mean(inside_analytics[,2]) # Average distance inside cluster.
    res[l, 3] <- sum(inside_analytics[,5]) / 2 # Sum of squared distances inside cluster.
  }
  return(res)
}

intra_to_inter_ratio <- function(data, labels, metric="euclidean") {
  return (intra_cluster_distances(x, labels, metric) / inter_cluster_distances(x, labels, metric))
}

silhouette_coefficient <- function(data, labels, metric="euclidean") {
  # https://en.wikipedia.org/wiki/Silhouette_(clustering)
  analytics <- cluster_analytics(x, labels, metric)
  coeff <- analytics[,6]
  labels <- analytics[,1]
  return (matrix(c(labels, coeff), ncol=2))
}

silhouette_score <- function(data, labels, metric="euclidean") {
  # Returns mean silhouette coefficient.
  analytics <- cluster_analytics(x, labels, metric)
  coeff <- analytics[,6]
  return (mean(coeff))
}

cluster_inertia <- function(data, labels, centers, metric="euclidean") {
  # Returns the mean squared distance between each data point and its closest centroid.
  L <- unique(labels)
  num_labels <- length(L)
  dist_to_center <- matrix(NA, nrow=num_labels, ncol=1)
  for (i in 1:num_labels) {
    l <- L[i]
    d <- distances_2_clusters(matrix(centers[l,], nrow=1), data[(labels==l),], metric) # Returns one row: distances to selected center.
    dist_to_center[i,1] <- d[1,5] / sum(labels==l) # Mean sum of squared distances.
  }
  return(mean(dist_to_center[,1]))
}

#---------------------------------------------------------------------------------
# K-Nearest neighbors algorithm. 
#---------------------------------------------------------------------------------
knneighbors <- function(data_to_classify, data, labels, k=1, metric="euclidean") {
  # Returns labels based on K-nearest neighbors algorithm.
  # Returns matrix with 3 + k Columns:
  # 1:Most popular label, 2:k-th neighbor distance (max distance),
  # 3:Average distance of k-nearest neighbors.
  # 4:4+k: indices of nearest neighbors
  # 4+k:4+2k: distances to neighbors
  res <- matrix(0.0, nrow=nrow(data_to_classify), ncol=(3+2*k))
  
  # For each datapoint calculate distances to other datapoints.
  d <- distances_2_clusters(data_to_classify, data, metric, knneighbors=k)
  
  for (i in 1:nrow(data_to_classify)) {
    k_idx <- d[i, 6:(6+k-1)] # Indexes
    k_labels <- labels[k_idx]
    most_popular_label <- counter_by(k_labels, decreasing=TRUE)[1,1]
    max_distance <- max(d[i, (6+k):(6+2*k-1)]) # Distances
    avg_distance <- sum(d[i, (6+k):(6+2*k-1)]) / k # Distances
    #k_dist <- d[i, (6+k):(6+2*k-1)] # Distances
    res[i,1] <- most_popular_label
    res[i,2] <- max_distance
    res[i,3] <- avg_distance
    res[i,4:(4+2*k-1)] <- d[i, 6:(6+2*k-1)] # Indexes and distances
  }
  return(res)
}

knneighbors_k_finder <- function(data_to_classify, true_labels, data, labels, k_range=(2:5), metric="euclidean") {
  # Returns score value for each k, where k is number of nearest neighbors.
  res <- matrix(NA, nrow=length(k_range), ncol=2)
  for (i in 1:length(k_range)) {
    k <- k_range[i]
    knn_result <- knneighbors(data_to_classify, data, labels, k, metric=metric)
    score <- accuracy_score(true_labels, knn_result)
    res[i,1] <- k
    res[i,2] <- score
  }
  return(res)
}

#---------------------------------------------------------------------------------
# Plot and experiment.
#---------------------------------------------------------------------------------
# Plot results.
plot_results <- function(data, centers, label_colors, centers_color="black",
                        title="clusters", x_label="", y_label="", point_size=0.5){
  xlim <- c(floor(min(data[,1])), ceiling(max(data[,1])))
  ylim <- c(floor(min(data[,2])), ceiling(max(data[,2])))
  plot(data, col=label_colors, xlim=xlim, ylim=ylim, main=title, xlab="", ylab="", cex=point_size)
  par(new=TRUE)
  plot(centers, col=centers_color, xlim=xlim, ylim=ylim, xlab=x_label, ylab=y_label)
}

add_centers_to_plot <- function(data, centers, color="black"){
  xlim <- c(floor(min(data[,1])), ceiling(max(data[,1])))
  ylim <- c(floor(min(data[,2])), ceiling(max(data[,2])))
  par(new=TRUE)
  plot(centers, col=color, xlim=xlim, ylim=ylim, xlab="", ylab="")
}

# Experiment and plot.
run_experiment <- function(data, num_clusters=3, ini_method=1, metric="euclidean", plot_output=TRUE){
  cluster_colors <- rainbow(num_clusters)
  kmeans_result <- kmeans_clusters(data, num_clusters=num_clusters, ini_method=ini_method, metric=metric)
  centers <- kmeans_result[[1]]
  labels <- kmeans_result[[2]]
  if (plot_output){
    plot_results(data, centers, cluster_colors[labels])
  }
  return(kmeans_result)
}
