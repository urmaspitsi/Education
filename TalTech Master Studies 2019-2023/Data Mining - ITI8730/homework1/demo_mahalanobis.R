library(matrixStats)
source("C:/Users/urmas/Documents/TalTech/Andmekaeve/assignment1/cluster_analysis.R")

load(file="C:/Users/urmas/Documents/TalTech/Andmekaeve/assignment1/input/kdata_hw1_1.RData")
y <- x[,3]
x <- x[,1:2]
#plot(x, col="grey", main="Raw input")

a <- x[x[,1]>260,] # Take some data
xlim <- c(floor(min(a[,1])), ceiling(max(a[,1])))
ylim <- c(floor(min(a[,2])), ceiling(max(a[,2])))

# Plot data.
plot(a, col="grey", main="Raw input", xlim=xlim, ylim=ylim)

# Find center point of the data.
c <- matrix(c(mean(a[,1]), mean(a[,2])), ncol=2)

# Calculate 4 different equidistant points from center: diagonally up/down and right/left.
c1 <- c - 10
c2 <- c + 10
c3 <- c + c(-10, 10)
c4 <- c + c(10, -10)
points <- t(matrix(c(c1, c2, c3, c4), nrow=2))

# Builtin Mahalanobis distance.
mah <- sqrt(mahalanobis(points, c, cov=cov(a, a)))
mah <- round(mah, 1)

# My implementation of Mahalanobis distance.
mah2 <- sqrt(mahalan(points, c, solve(cov(a, a))))
mah2 <- round(mah2, 1)

sum(mah != mah2) == 0

c_to_c1 <- t(matrix(c(c, c1), nrow=2))
c_to_c2 <- t(matrix(c(c, c2), nrow=2))
c_to_c3 <- t(matrix(c(c, c3), nrow=2))
c_to_c4 <- t(matrix(c(c, c4), nrow=2))

# Plot data with center point and selected.
points_color <- "grey"
center_color <- "black"
shifted_points_color <- "red"
line_color <- "black"

plot(a, col=points_color, main="Mahalanobis distance: 4 euclidean-equidistant points from center.", xlim=xlim, ylim=ylim)
par(new=TRUE)
plot(c, type="p", col=center_color, xlim=xlim, ylim=ylim, xlab="", ylab="")
text(c, labels=c("Center"), pos=4)
par(new=TRUE)
plot(points, col=shifted_points_color, xlim=xlim, ylim=ylim, xlab="", ylab="")

par(new=TRUE)
plot(c_to_c1, type="l", col=line_color, xlim=xlim, ylim=ylim, xlab="", ylab="")
text(c_to_c1, labels=c("", mah[1]), pos=4)
par(new=TRUE)
plot(c_to_c2, type="l", col=line_color, xlim=xlim, ylim=ylim, xlab="", ylab="")
text(c_to_c2, labels=c("", mah[2]), pos=4)
par(new=TRUE)
plot(c_to_c3, type="l", col=line_color, xlim=xlim, ylim=ylim, xlab="", ylab="")
text(c_to_c3, labels=c("", mah[3]), pos=4)
par(new=TRUE)
plot(c_to_c4, type="l", col=line_color, xlim=xlim, ylim=ylim, xlab="", ylab="")
text(c_to_c4, labels=c("", mah[4]), pos=4)




