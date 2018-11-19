trunc <- function(x, ..., prec = 0){
  base::trunc(x * 10^prec, ...) / 10^prec
}
