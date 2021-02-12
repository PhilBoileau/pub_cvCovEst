# helper/wrapper to generate input data based on differing mechanisms
sim_cov_data <- function(n_obs = 100,
                         p_dim = 100,
                         sim_type = c("1", "2", "3", "4", "5", "6", "7", "8")) {
  # set default
  sim_type <- match.arg(sim_type)

  #############################################################################
  ## HELPER FUNCTIONS TO SPECIFY DATA-GENERATING MECHANISMS
  #############################################################################

  # dense covariance matrix (1)
  cov1_sim <- function(p, val) {
    covmat <- matrix(val, nrow = p, ncol = p) + diag(val, nrow = p)
    return(covmat)
  }

  # AR(1) autocorrelation matrix (2)
  cov2_sim <- function(p, rho) {
    times <- seq_len(p)
    H <- abs(outer(times, times, "-"))
    covmat <- rho^H
    covmat[cbind(times, times)] <- covmat[cbind(times, times)]
    return(covmat)
  }

  # MA(1) autocorrelation matrix (3)
  cov3_sim <- function(p, rho) {
    return(as.matrix(Matrix::band(cov1_sim(p, rho), k1 = -1, k2 = 1)))
  }

  # MA(2) autocorrelation matrix (4)
  cov4_sim <- function(p, rho_1, rho_2) {
    # loop through all entries in the matrix
    dat <- lapply(
      seq_len(p),
      function(i) {
        lapply(
          seq_len(p),
          function(j) {
            if (abs(i - j) == 0)
              1
            else if (abs(i - j) == 1)
              rho_1
            else if (abs(i - j) == 2)
              rho_2
            else
              0
          }
        )
      }
    )

    # fill the autocorellation matrix
    covmat <- matrix(data = unlist(dat), nrow = p)
    return(covmat)
  }

  # covariance matrix for thresholding estimator (5)
  cov5_sim <- function(p) {
    U_dat <- runif(p^2)
    U_dat <- sapply(
      U_dat,
      function(u) {
        if (u < 1/4)
          1
        else if (u < 1/2)
          -1
        else
          0
      }
    )
    U <- matrix(U_dat, nrow = p)
    covmat <- diag(p)  + t(U) %*% U
    return(cov2cor(covmat))
  }

  # general toeplitz matrix for tapering estimator (6)
  cov6_sim <- function(p, rho, alpha) {
    times <- seq_len(p)
    H <- abs(outer(times, times, "-")) + diag(p)
    H <- H^-(1 + alpha) * rho
    covmat <- H + diag(p)*(1-rho)
    return(covmat)
  }

  # general toeplitz matrix with alternating signs (7)
  cov7_sim <- function(p, rho, alpha) {
    times <- seq_len(p)
    H <- abs(outer(times, times, "-")) + diag(p)
    H <- H^-(1 + alpha) * rho
    covmat <- H + diag(p)*(1 - rho)

    sign_mat <- sapply(
      times,
      function(i) {
        sapply(
          times,
          function(j) {
            (-1)^(abs(i - j))
          }
        )
      }
    )
    return(covmat * sign_mat)
  }

  # POET estimator (8)
  # l: the number of latent factors
  cov8_sim <- function(p, l) {
    B <- MASS::mvrnorm(n = p, mu = rep(0, l), Sigma = diag(l))
    cov_mat <- tcrossprod(B) + diag(p)
    return(cov_mat)
  }

  #############################################################################

  # generate covariance matrix from appropriate mechanism
  if (sim_type == "1") {
    # first set of cov mats
    sim_covmat <-  cov1_sim(p = p_dim, val = 0.5)
  } else if (sim_type == "2") {
    # second set of cov mats
    sim_covmat <- cov2_sim(p = p_dim, rho = 0.7)
  } else if (sim_type == "3") {
    # third set of cov mats
    sim_covmat <- cov3_sim(p = p_dim, rho = 0.7)
  } else if (sim_type == "4") {
    # fourth set of cov mats
    sim_covmat <- cov4_sim(p = p_dim, rho_1 = 0.6, rho_2 = 0.3)
  } else if (sim_type == "5") {
    # fifth set of cov mats
    sim_covmat <- cov5_sim(p = p_dim)
  } else if (sim_type == "6") {
    # sixth set of cov mats
    sim_covmat <- cov6_sim(p = p_dim, rho = 0.6, alpha = 0.3)
  } else if (sim_type == "7") {
    # seventh set of cov mats
    sim_covmat <- cov7_sim(p = p_dim, rho = 0.6, alpha = 0.3)
  } else if (sim_type == "8") {
    # eight set of cov mats
    sim_covmat <- cov8_sim(p = p_dim, l = 3)
  }

  # generate dataset
  sim_dat <-  MASS::mvrnorm(n = n_obs,
                            mu = rep(0, p_dim),
                            Sigma = sim_covmat)

  # organize output (covariance matrix and data)
  out <- list(covmat = sim_covmat,
              dataset = sim_dat)
  return(out)
}
