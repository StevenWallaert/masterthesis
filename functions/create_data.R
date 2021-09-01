# function to simulate data:
# 
# 
# n - number of samples
# p - number of features
# sig_p - number of significant features, predictive of response
# d - difference between 2 groups in the significant features
# no correlation structure (for now)
# all gaussian
library("tidyverse")
source("functions/simcor.r")
create_data <- function(n, p, sig_p, d, correlated){
  
  n_target <- n %/% 2 # as of now: equal n target-control
  n_control <- n - n_target
  
  # idea is to randomize the order of the target and controls
  # not sure if this is needed though
  in_order <- c(rep(1, n_target), rep(0, n_control))
  Y <- sample(in_order, size = n, replace = FALSE)
  
  # keep track of the significant features
  significant_features <- seq_len(sig_p) # seq_len: covers the case where sig_p = 0

  # create X following X_i ~ N(Y_i*d, sigma = 1)
  if (correlated == "no"){
    X <- sapply(rep(0, p), function(x) rnorm(n, x, 1))
  } else if (correlated == "block") {
    # block correlation structure
    # 6 blocks of 10 correlated features, rest uncorrelates (cf. https://link.springer.com/article/10.1186/s12920-020-00826-6)
    intracor <- 0.9
    
    Sigma <- simcor(k = 7, size = c(10, 10, 10, 10, 10, 10, p-60), 
                    rho = c(intracor, intracor, intracor, intracor, intracor, intracor, 0), 
                    delta = 0)
    X <- mvtnorm::rmvnorm(n, sigma = Sigma)
  } else {
    stop("Invalid value given for argument 'correlated', must be either 'no' or 'block'")
  }
  
  X[, significant_features] <- X[, significant_features] + Y*d

  # output in several formats
  # XY <- as_tibble(X, .name_repair = "unique") %>%
  #   add_column(Y)
  # YX <- tibble(Y, as_tibble(X, .name_repair = "unique"))
  
  list(X=X, Y=Y, sig_features = significant_features)
}


