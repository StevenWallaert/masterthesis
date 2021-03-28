# function to simulate data:
# 
# 
# n - number of samples
# p - number of features
# sig_p - number of significant features, predictive of response
# d - difference between 2 groups in the significant features
# no correlation structure (for now)
# all gaussian

create_data <- function(n, p, sig_p, d){
  require(tidyverse)
  n_target <- n %/% 2 # as of now: equal n target-control
  n_control <- n - n_target
  
  # idea is to randomize the order of the target and controls
  # not sure if this is needed though
  in_order <- c(rep(1, n_target), rep(0, n_control))
  Y <- sample(in_order, size = n, replace = FALSE)
  
  # keep track of the significant features
  significant_features <- seq_len(sig_p) # seq_len: covers the case where sig_p = 0

  # create X following X_i ~ N(Y_i*d, sigma = 1)
  X <- sapply(rep(0, p), function(x) rnorm(n, x, 1))
  X[, significant_features] <- X[, significant_features] + Y*d

  # output in several formats
  # XY <- as_tibble(X, .name_repair = "unique") %>%
  #   add_column(Y)
  # YX <- tibble(Y, as_tibble(X, .name_repair = "unique"))
  
  list(X=X, Y=Y, sig_features = significant_features)
}


