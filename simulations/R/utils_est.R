################################################################################
# NORMS
################################################################################

# Frobenius norm
frobeniusNorm <- function(est, pop_cov) {
  return(matrixStats::sum2((est - pop_cov)^2))
}
frobeniusNorm_safe <- purrr::possibly(frobeniusNorm, otherwise = NA_real_)

# Operator norm
operatorNorm <- function(est, pop_cov) {
  return(Matrix::norm(est - pop_cov, type = "2"))
}
operatorNorm_safe <- purrr::possibly(operatorNorm, otherwise = NA_real_)
