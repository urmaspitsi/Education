# my distance functions

chebyshev <- function(x1, x2){ return(rowMaxs(abs(x1 - x2))) }

manhattan <- function(x1, x2){ return(rowSums(abs(x1 - x2))) }

canberra <- function(x1, x2){
  # https://en.wikipedia.org/wiki/Canberra_distance
  # https://docs.scipy.org/doc/scipy/reference/generated/scipy.spatial.distance.canberra.html
  # should be applied for values > 0, to avoid div/0 and instability
  return(rowSums(abs(x1 - x2) / (abs(x1) + abs(x2)), na.rm=TRUE ) )
}

unit_test_canberra <- function(){
  # source: https://docs.scipy.org/doc/scipy/reference/generated/scipy.spatial.distance.canberra.html
  x1 <- matrix(c(1,0,0), nrow=1)
  x2 <- matrix(c(0,1,0), nrow=1)
  test1 <- round(canberra(x1, x2), 4) == 2.0
  print(paste("test1", test1))

  x1 <- matrix(c(1,1,0), nrow=1)
  x2 <- matrix(c(0,1,0), nrow=1)
  test2 <- round(canberra(x1, x2), 4) == 1.0
  print(paste("test2", test2))

    # source: http://www.code10.info/index.php?option=com_content&view=article&id=49:article_canberra-distance&catid=38:cat_coding_algorithms_data-similarity&Itemid=57
  x1 <- matrix(c(1,1,1), nrow=1)
  x2 <- matrix(c(1,1,0), nrow=1)
  x3 <- matrix(c(2,2,2), nrow=1)
  x4 <- matrix(c(10,10,10), nrow=1)
  x5 <- matrix(c(11,11,11), nrow=1)
  x6 <- matrix(c(10,5,0), nrow=1)
  from_x1 <- round(c(canberra(x1, x1), canberra(x1, x2), canberra(x1, x3), canberra(x1, x4), canberra(x1, x5), canberra(x1, x6)), 3)
  print(paste("test3", sum(from_x1 - c(0.0, 1.0, 1.0, 2.455, 2.5, 2.485)) == 0.0 ))
  from_x2 <- round(c(canberra(x2, x2), canberra(x2, x3), canberra(x2, x4), canberra(x2, x5), canberra(x2, x6)), 3)
  print(paste("test4", sum(from_x2 - c(0.000, 1.667, 2.636, 2.667, 1.485)) == 0.0 ))
  from_x3 <- round(c(canberra(x3, x3), canberra(x3, x4), canberra(x3, x5), canberra(x3, x5)), 3)
  print(paste("test5", sum(from_x3 - c(0.000, 2.000, 2.077, 2.077)) == 0.0 ))
  from_x4 <- round(c(canberra(x4, x4), canberra(x4, x5), canberra(x4, x6)), 3)
  print(paste("test6", sum(from_x4 - c(0.000, 0.143, 1.333)) == 0.0 ))
  from_x5 <- round(c(canberra(x5, x5), canberra(x5, x6)), 3)
  print(paste("test7", sum(from_x5 - c(0.000, 1.423)) == 0.0 ))
  return(c(from_x1, from_x2, from_x3, from_x4, from_x5))
}
#unit_test_canberra()

euclidean <- function(x1, x2){ return(sqrt(rowSums((x1 - x2) ^ 2))) }

minkowski <- function(x1, x2, p=3){
  # Generalized Lp-Norm with weights W: w(i).
  # Here implementation is Lp norm, without weights.
  if (p <= 1) {
    if (p <= 0){
      print(paste("Warning! Parameter p in minkowski must be >= 1. Calculating with p = 1 instead. Value given:", p))
    } 
    return(manhattan(x1, x2))
  }
  else if (p == 2) {
    return(euclidean(x1, x2))
  }
  else {
    return(rowSums((abs(x1 - x2)) ^ p) ^ (1 / p))
  }
}

mahalan <- function(x, x_mean, covmatrix_inverse){
  # Returns squared Mahalanobis distance as in the standard library.
  # Assumes row vectors are stacked: different dimensions of x are in columns of x.

  # https://docs.scipy.org/doc/scipy/reference/generated/scipy.spatial.distance.mahalanobis.html
  # https://stat.ethz.ch/R-manual/R-devel/library/stats/html/mahalanobis.html
  # https://en.wikipedia.org/wiki/Mahalanobis_distance

  # If the covariance matrix is the identity matrix,
  # the Mahalanobis distance reduces to the Euclidean distance.

  # Applications:
  # Mahalanobis's definition was prompted by the problem of identifying the similarities
  # of skulls based on measurements in 1927.
  # Mahalanobis distance is widely used in cluster analysis and classification techniques.
  # It is closely related to Hotelling's T-square distribution used for multivariate statistical
  # testing and Fisher's Linear Discriminant Analysis that is used for supervised classification.

  # In order to use the Mahalanobis distance to classify a test point as belonging to one of N classes,
  # one first estimates the covariance matrix of each class, usually based on samples 
  # known to belong to each class. Then, given a test sample, one computes the Mahalanobis distance
  # to each class, and classifies the test point as belonging to that class for which 
  # the Mahalanobis distance is minimal.
  
  # Mahalanobis distance and leverage are often used to detect outliers, 
  # especially in the development of linear regression models.
  # A point that has a greater Mahalanobis distance from the rest of the sample population of points
  # is said to have higher leverage since it has a greater influence on the slope or coefficients
  # of the regression equation.
  # Mahalanobis distance is also used to determine multivariate outliers.
  if (nrow(x_mean) == 1 && nrow(x_mean) < nrow(x)){
    x_mean <- matrix(x_mean, nrow=nrow(x), ncol=ncol(x), byrow=TRUE)
  }
  d <- x - x_mean
  return(diag(d %*% covmatrix_inverse  %*% t(d))) # Extract diagonal: result dim = n*columns.
  # return(sqrt(t(d) %*% solve(covmatrix)  %*% d))
  # return(sqrt(d %*% covmatrix_inverse  %*% t(d))) # Assumes inverse of covariance matrix.
}

unit_test_mahalan <- function(){
  # https://docs.scipy.org/doc/scipy/reference/generated/scipy.spatial.distance.mahalanobis.html
  iv <- matrix(c(c(1,0.5,0.5), c(0.5,1,0.5), c(0.5,0.5,1)), nrow=3)
  print(mahalan(matrix(c(1,0,0), nrow=1), matrix(c(0,1,0), nrow=1), iv)[1,1] == 1)
  print(mahalan(matrix(c(0,2,0), nrow=1), matrix(c(0,1,0), nrow=1), iv)[1,1] == 1)
  print(round(mahalan(matrix(c(2,0,0), nrow=1), matrix(c(0,1,0), nrow=1), iv), 5)[1,1] == 1.73205)
}

distance <- function(x1, x2, name, arg1=NULL){
  if (name == "euclidean"){
    return(euclidean(x1, x2))
  }
  else if (name == "manhattan"){
    return(manhattan(x1, x2))
  }
  else if (name == "minkowski"){
    if (is.null(arg1)) {
      return(minkowski(x1, x2))
    }
    else {
      return(minkowski(x1, x2, arg1))
    }
  }
  else if (name == "mahalan"){
    return(mahalan(x1, x2, arg1))
  }
  else if (name == "chebyshev"){
    return(chebyshev(x1, x2))
  }
  else if (name == "canberra"){
    return(canberra(x1, x2))
  }
  else {
    print(paste("Warning! Distance function", name, "doesn't exist. Using Euclidean distance instead."))
    return(euclidean(x1, x2))
  }
}

to_matrix <- function(x){ return(matrix(x, nrow=1, ncol=length(x), byrow=TRUE)) }

