# following function comes directly from https://rdrr.io/github/MarkJBrewer/ICsims/man/simcor.html


#######################################
##  Simulating different correlation ##
##  structures as in Hardin et al.   ##
#######################################


################################
##  Simulating the Constant Correlation
################################

# this function simulates a block correlation matrix
# The size and base correlation for each block is user specified
# There is an additional delta parameter for the off diagonal correlations


# k is the number of groups
# size is a vector of length k specifying the size of each group 
# rho is a vector of length k specifying base correlation values
# epsilon <- 0.99 - max(rho)
# eidim is the space from which the noise is generated, the smaller the more noise
# delta is the correlation of the off diagonal blocks

simcor = function (k = 6, size = c(10, 5, 8, 2, 15, 50), rho = c(0.7, 
                                                                 0.7, 0.5, 0.9, 0.85, 0.4), delta = 0.39, epsilon = 0.99 - 
                     max(rho), eidim = 2) 
{
  ndim <- sum(size)
  bigcor <- matrix(rep(delta, ndim * ndim), ncol = ndim)
  for (i in 1:k) {
    cor <- matrix(rep(rho[i], size[i] * size[i]), ncol = size[i])
    if (i == 1) {bigcor[1:size[1], 1:size[1]] <- cor}
    if (i != 1) {bigcor[(sum(size[1:(i - 1)]) + 1):sum(size[1:i]), 
                        (sum(size[1:(i - 1)]) + 1):sum(size[1:i])] <- cor}
  }
  diag(bigcor) <- 1 - epsilon
  
  eivect <- c()
  for (i in 1:ndim) {
    ei <- runif(eidim, -1, 1)
    eivect <- cbind(eivect, sqrt(epsilon) * ei/sqrt(sum(ei^2)))
  }
  bigE <- t(eivect) %*% eivect
  cor.nz <- bigcor + bigE
  cor.nz
}

