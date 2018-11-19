trunc <- function(x, ..., prec = 0){
  base::trunc(x * 10^prec, ...) / 10^prec
}

m_avg <- function(x,n=5){
  stats::filter(x, rep(1/n,n), sides=2)
}

boot_ci_propdiff <- function(data, ntimes = 1000, prob = NULL, bounds = c(.025, .975)) {
  boot_dist <- rerun(ntimes, sample(data, replace = TRUE, prob = prob)) %>%
    map(., mean) %>% flatten_dbl
  quantile(boot_dist, bounds)
}