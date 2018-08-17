source('utils.R')

# Distance Calculations
scaled_square_dist <- function(X, X2, lenscale) {
  # L2-norm = |(X - X2ᵀ)/lenscale |
  #         = sqrt(((X - X2ᵀ) / lenscale)^2)
  
  X <- X / lenscale
  X2 <- X2 / lenscale
  Xs <- rowSums(X ^ 2)
  X2s <- rowSums(X2 ^ 2)
  
  N <- length(Xs)
  M <- length(X2s)
  D <- -2*(X  %*% t(X2))
  return(D + rep.row(X2s, N) + rep.col(Xs, M))
  # return(D + rep.row(Xs, nrow(Xs)) + rep.col(X2s, nrow(X2s)))
}

scaled_euclid_dist <- function(X, X2, lenscale) {
  r2 <- scaled_square_dist(X, X2, lenscale)
  return(sqrt(r2 + 1e-12))
}

square <- power_fn(2.)


# Kernels
matern32 <- function(lenscale, variance) {
  function(X1, X2 = X1) {
    R = scaled_euclid_dist(X1, X2, lenscale)
    variance * (1. + sqrt(3.) * R) * exp(-sqrt(3.) * R)
  }
}

matern52 <- function(lenscale, variance) {
  function(X1, X2 = X1) {
    R = scaled_euclid_dist(X1, X2, lenscale)
    variance * (1.0 + sqrt(5.) * R + 5. / 3. * square(R)) * exp(-sqrt(5.) * R)
  }
}

rbf <- function(lenscale, variance) {
  function(X1, X2 = X1) {
    R = scaled_square_dist(X1, X2, lenscale)
    variance * exp(-R / 2)
  }
}

periodic <- function(lenscale, variance, period) {
  function(X1, X2 = X1) {
    # TODO...
  }
}

