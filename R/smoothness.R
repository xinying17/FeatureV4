
smoothness <- function(Lap,f,s){
  # f is the function vector
  # s is parameter on Laplacian function
  f <- as.matrix(f)
  require('expm', quietly = TRUE)
  require('matrixcalc',quietly = TRUE)

  r <- eigen(Lap)
  V <- r$vectors
  lam <- r$values
  lam[lam<0] = 0
  Lmbd = diag(lam ** abs(s))
  if(s<0){
    Lmbd = ginv(Lmbd)
  }
  newL = V %*% Lmbd %*% solve(V)
  lap_fun <- function(x) {x %*% newL %*% x}

  smooth_value <- apply(f, 1, lap_fun)
  return(smooth_value)
}
