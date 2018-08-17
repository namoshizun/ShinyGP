rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}

rep.col<-function(x,n){
  matrix(rep(x,each=n), ncol=n, byrow=TRUE)
}

power_fn <- function(exponent) {
  function(x) {
    x ^ exponent
  }
}

rand_iid <- function(rand_generator, N, M = 1, ...) {
  matrix(rand_generator(N*M, ...), N, M) 
}
