trunc <- function(x, ..., prec = 0){
  base::trunc(x * 10^prec, ...) / 10^prec
}

m_avg <- function(x,n=5){
  stats::filter(x, rep(1/n,n), sides=2)
}