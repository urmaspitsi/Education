library(matrixStats)
source("C:/Users/urmas/Documents/TalTech/Andmekaeve/assignment1/cluster_analysis.R")

#---------------------------------------------------------------------------------
# Run K-Means algorithm and plot results
#---------------------------------------------------------------------------------
# 3D example
# library(rgl)
# load(file="C:/Users/urmas/Documents/TalTech/Andmekaeve/assignment1/input/Data3D.RData")
# x <- t
# y <- x[,4]
# x <- x[,1:3]
# plot3d(x[,1], x[,2], x[,3])      # Plot raw data.
# plot3d(x[,1], x[,2], x[,3], c=y) # Plot with ground truth labels.

# 2D example
#load(file="C:/Users/urmas/Documents/TalTech/Andmekaeve/assignment1/input/kdata_hw1_1.RData")
load(file="C:/Users/urmas/Documents/TalTech/Andmekaeve/assignment1/input/kData.RData")
y <- x[,3]
x <- x[,1:2]
plot(x, col="grey", main="Raw input")

metric <- "euclidean"
#metric <- "canberra"
num_clusters <- 3

# Runs kmeans 1 time and returns and plots results.
kmeans_result <- run_experiment(x, num_clusters=num_clusters, ini_method=1, metric=metric, plot_output=TRUE)

# Runs kmeans 10 times and returns best result.
kmeans_result <- kmeans_clusters_best(x, num_clusters=num_clusters, ini_method=1, metric=metric)

centers <- kmeans_result[[1]]
labels <- kmeans_result[[2]]
plot_results(x, centers, rainbow(num_clusters)[labels])
plot3d(x[,1], x[,2], x[,3], c=labels)
inertia <- cluster_inertia(x, labels, centers)
inertia
silhouette_score(x, labels)

#---------------------------------------------------------------------------------
# k-finder: elbow rule. 
#---------------------------------------------------------------------------------

k_finder <- kmeans_k_finder(x, num_clusters_range=(2:6))
plot(k_finder, type="b", xlab="k", ylab="inertia", main="k finder: inertia vs k")

#---------------------------------------------------------------------------------
# Compare with built-in k-means. 
#---------------------------------------------------------------------------------
kmeans_builtin <- kmeans(x, num_clusters)
labels_builtin <- kmeans_builtin[["cluster"]]
centers_builtin <- kmeans_builtin[["centers"]]
plot_results(x, centers_builtin, rainbow(num_clusters)[labels_builtin])

inertia_builtin <- cluster_inertia(x, labels_builtin, centers_builtin)
inertia_builtin

#---------------------------------------------------------------------------------
# Switch plots back-and-forth to see the result. 
#---------------------------------------------------------------------------------
plot(x, col="grey")
plot_results(x, centers, rainbow(num_clusters)[labels] )

# Add built-in plot on top.
add_centers_to_plot(x, centers_builtin, color="red")

# View original data with both centers.
plot(x, col="grey")
add_centers_to_plot(x, centers, color="black")
add_centers_to_plot(x, centers_builtin, color="red")

# Calculate difference between my centers and built-in k-means.
library(data.table)
centers_sorted <- data.table(centers, key="V1")
centers_builtin_sorted <- data.table(centers_builtin, key="V1")

differences <- (centers_sorted - centers_builtin_sorted) / centers_builtin_sorted
total_abs_diff <- sum(abs(differences))
avg_abs_diff <- total_abs_diff / num_clusters
print(paste("avg_abs_diff = ", round(100 * avg_abs_diff, 2), "%", sep=""))

#---------------------------------------------------------------------------------

save(kmeans_result, file="C:/Users/urmas/Documents/TalTech/Andmekaeve/assignment1/output/kmeans_result_hw1_5.RData")

#---------------------------------------------------------------------------------
# K-Nearest Neighbors.
#---------------------------------------------------------------------------------

idx_to_classify <- sample(1:nrow(x), 100)
idx_valid <- sample(1:nrow(x), 1000)

x_train <- x[idx_to_classify,]
y_truth <- labels[idx_to_classify]
x_valid <- x[idx_valid,]
y_valid <- labels[idx_valid]

y_pred <- knneighbors(x_train, x_valid, y_valid, k=3)
accuracy_score(y_truth, y_pred)
confusion_matrix(y_truth, y_pred)

knneighbors_k_finder(x_train, y_truth, x_valid, y_valid, k_range=2*(2:5))

xlim <- c(floor(min(x[,1])), ceiling(max(x[,1])))
ylim <- c(floor(min(x[,2])), ceiling(max(x[,2])))
pred_colors <- rainbow(5)[y_pred]
plot(x_train, col=pred_colors, xlim=xlim, ylim=ylim)
par(new=TRUE)
plot(x_valid, col="grey", xlim=xlim, ylim=ylim)


#---------------------------------------------------------------------------------
# Compare KNN with an external library solution.
#---------------------------------------------------------------------------------

library(class)
knn_res <- (knn(x_valid, x_train, y_valid, k = 5, l=4, prob=TRUE))
predicted_classes <- as.numeric(knn_res)
predicted_classes
t(a)
sum((t(a) == predicted_classes) * 1 - 1) == 0

#---------------------------------------------------------------------------------

metric <- "euclidean"
inside_distance <- intra_cluster_distances(x, labels, metric)
inside_distance_builtin <- intra_cluster_distances(x, labels_builtin, metric)
sum(inside_distance[,1]) - sum(inside_distance_builtin[,1])

intra_distance <- intra_cluster_distances(x, labels, metric)
inter_distance <- inter_cluster_distances(x, labels, metric)
ratio_of_in_out_dist <- intra_to_inter_ratio(x, labels, metric)
intra_distance
inter_distance

analytics <- cluster_analytics(x, labels, metric)

silhouette_data <- analytics[,6]
silhouette_labels <- analytics[,1]

plot(silhouette_data, col=rainbow(num_clusters)[silhouette_labels], type="p")
plot(silhouette_data, col=rainbow(num_clusters)[silhouette_labels], type="h")

#puuduvad tulemused
