source('kernels.R')


gp <- function(kernel_fn) {
  structure(list(
    
    predict_f = function(X, y, Xnew) {
      K <- kernel_fn(X, X)
      Kinv <- solve(jitter(K, 1e-6))
      Ks <- kernel_fn(X, Xnew)
      Kss <- kernel_fn(Xnew, Xnew)
      
      f_mean <- t(Ks) %*% Kinv %*% y
      f_var <- Kss - t(Ks) %*% Kinv %*% Ks
      
      return(list(
        mean = f_mean,
        var = f_var
      ))
    },
    
    draw_priors = function(X, n_priors) {
      N <- nrow(X)
      
      K <- kernel_fn(X, X)
      L <- chol(K + diag(1e-10, N, N)) # jitted
      
      f_prior <- L %*% rand_iid(rnorm, N = N, M = n_priors)
      return(f_prior)
    }
  ), class = 'gp')
}

