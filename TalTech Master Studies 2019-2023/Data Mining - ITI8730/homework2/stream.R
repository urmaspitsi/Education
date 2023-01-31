library(gplots)
set.seed(0)

#----------------------------------------------------------------------------------------
# Data stream function.
#----------------------------------------------------------------------------------------
fetch_new_element <- function(x){ return(x + 1) }

#----------------------------------------------------------------------------------------
# Selection functions: whether to include incoming datapoint into reservoir.
#----------------------------------------------------------------------------------------
select_with_probability_k_div_n <- function(k, n){
  # Returns TRUE with probability k / n, where k: reservoir size, n: nth element from stream.
  return((k / n) > runif(1))
}

select_with_probability_logk_div_logn <- function(k, n){
  # Returns TRUE with probability k / n, where k: reservoir size, n: nth element from stream.
  return((log(k) / log(n)) > runif(1))
}

select_always <- function(k, n){
  # Returns TRUE.
  return(TRUE)
}

#----------------------------------------------------------------------------------------
# Replacement functions.
#----------------------------------------------------------------------------------------
replace_randomly <- function(new_element, current_reservoir){
  res <- current_reservoir
  res[sample(1:length(current_reservoir), 1)] <- new_element
  return(res)
}

replace_oldest <- function(new_element, current_reservoir){
  res <- current_reservoir
  res[which.min(current_reservoir)] <- new_element
  return(res)
}

#----------------------------------------------------------------------------------------
# Accumulate data stream.
#----------------------------------------------------------------------------------------
accumulate_stream <- function(initial_reservoir, num_steps, selection_func, replacement_func, fetch_func=fetch_new_element){
  # Returns matrix(num_steps, length(initial_reservoir)) containing accumulation of reservoirs at consecutive timesteps.
  reservoir_size <- length(initial_reservoir)
  next_element <- max(initial_reservoir)
  res <- matrix(0, nrow=(num_steps+1), ncol=reservoir_size)
  res[1,] <- initial_reservoir
  for(i in 2:(num_steps+1)){
    next_element <- fetch_func(next_element)
    previous_reservoir <- res[(i-1),]
    if (selection_func(reservoir_size, i)) {
      next_reservoir <- replacement_func(next_element, previous_reservoir)
      res[i,] <- sort(next_reservoir)
    }
    else {
      res[i,] <- previous_reservoir
    }
  }
  return(res)
}

#----------------------------------------------------------------------------------------
# Experiment with streams and plot results.
#----------------------------------------------------------------------------------------
reservoir_size <- 10
num_timesteps <- 101

idx <- (1:num_timesteps %% 25) == 0
idx[1] <- TRUE

acc <- accumulate_stream(1:reservoir_size, num_timesteps, select_with_probability_k_div_n, replace_randomly)
heatmap.2(acc[idx,], xlab="Reservoir", ylab="Timesteps at every 25th step", main="Select: k/n, Replace: random", dendrogram="none", Rowv=FALSE, Colv=FALSE, trace="none")

acc <- accumulate_stream(1:reservoir_size, num_timesteps, select_with_probability_k_div_n, replace_oldest)
heatmap.2(acc[idx,], xlab="Reservoir", ylab="Timesteps at every 25th step", main="Select: k/n, Replace: oldest", dendrogram="none", Rowv=FALSE, Colv=FALSE, trace="none")

acc <- accumulate_stream(1:reservoir_size, num_timesteps, select_with_probability_logk_div_logn, replace_randomly)
heatmap.2(acc[idx,], xlab="Reservoir", ylab="Timesteps at every 25th step", main="Select: log(k)/log(n), Replace: random", dendrogram="none", Rowv=FALSE, Colv=FALSE, trace="none")

acc <- accumulate_stream(1:reservoir_size, num_timesteps, select_with_probability_logk_div_logn, replace_oldest)
heatmap.2(acc[idx,], xlab="Reservoir", ylab="Timesteps at every 25th step", main="Select: log(k)/log(n), Replace: oldest", dendrogram="none", Rowv=FALSE, Colv=FALSE, trace="none")






