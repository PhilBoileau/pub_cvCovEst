# use custom package library
.libPaths("/global/scratch/nhejazi/R")
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")

# read in command line arguments
args <- R.utils::commandArgs(trailingOnly = TRUE, asValues = TRUE,
                             defaults = list(n_sim = 200,
                                             n_obs = c(50, 100, 200, 500),
                                             pn_ratio = c(0.3, 0.5, 1, 2, 5),
                                             sim_type = "1",
                                             sim_parallel = TRUE,
                                             cvcov_parallel = FALSE,
                                             future_workers = 25L))
# NOTE: scenarios considered in the literature
# 1) Bickel & Levina: n_obs = 100, p_dim = (10, 100, 200, 1000), n_sim = 100
#    (p_dim = 1000 is left unreported in the main paper)
# 2) Rothman et al.: n_obs = 100, p_dim = (30, 100, 200, 500), n_sim = 50

# print for logging reference
print(args)
assertthat::assert_that(args$sim_parallel != args$cvcov_parallel)

# packages
library(here)
library(tidyverse)
library(purrr)
library(foreach)
library(future)
library(doFuture)
library(doRNG)
#library(cvCovEst)
devtools::load_all(here::here("..", "..", "cvCovEst"))

# fixing parallelization
library(openblasctl)
openblas_set_num_threads(1)
library(OpenMPController)
omp_set_num_threads(1)

# load helper scripts, parallelization, PRNG
source(here("R", "utils_est.R"))
source(here("R", "01_setup_data.R"))
source(here("R", "02_fit_estimators.R"))

# parallelization options
registerDoFuture()
if (args$sim_parallel) {
  plan(multiprocess, workers = as.integer(args$future_workers))
  options(future.globals.maxSize = 10^32)
} else {
  plan(transparent)
}
set.seed(621364)

# define regimes by generating dimensionality based on ratio with sample size
dim_all <- expand.grid(args$n_obs, args$pn_ratio)
colnames(dim_all) <- c("n_obs", "pn_ratio")
## hack way to exclude only the 500 x 2500 scenario
if (nrow(dim_all) > 1) {
  bigpn_idx <- which(dim_all$n_obs == 500 & dim_all$pn_ratio == 5)
  dim_all <- dim_all[-bigpn_idx, ]
}

# perform simulation across sample size x dimension
lapply(seq_len(nrow(dim_all)), function(idx) {
  # set dimensionality and sample size
  p_dim <- round(dim_all$n_obs[idx] * dim_all$pn_ratio[idx])
  n_dim <- dim_all$n_obs[idx]

  # parallelized loop over simulation repititions
  results <- foreach(z = seq_len(args$n_sim),
                     .options.multicore = list(preschedule = FALSE),
                     .errorhandling = "remove") %dorng% {
    # generate data
    dgp_and_data <- sim_cov_data(
      n_obs = n_dim,
      p_dim = p_dim,
      sim_type = args$sim_type
    )

    # fit estimators
    sim_est_out <- fit_estimators(
      data_in = dgp_and_data$dataset,
      cov_mat = dgp_and_data$covmat,
      cv_scheme = "v_fold",
      v_folds = 5L,
      parallel = args$cvcov_parallel
    )

    # compare to oracle by combining risk difference ratios into a table
    oracle_tbl <- list(
      "n_dim" = n_dim,
      "p_dim" = p_dim,
      "cv_cv_riskdiff" = sim_est_out$cv_cv_riskdiff,
      "oracle_cv_riskdiff" = sim_est_out$oracle_cv_riskdiff,
      "cv_oracle_riskdiff_ratio" = sim_est_out$cv_oracle_riskdiff_ratio,
      "full_oracle_riskdiff_ratio" = sim_est_out$full_oracle_riskdiff_ratio
    ) %>%
    as_tibble()

    # include p_dim and n_dim in norms table
    norms_tbl <- sim_est_out$norms_tbl %>%
      mutate(
        p_dim = p_dim,
        n_dim = n_dim
      )

    # remove temporary data structures
    rm(dgp_and_data, sim_est_out); gc()

    # create the list of dataframes to return
    sim_est_output <- list(
      oracle = oracle_tbl,
      norms = norms_tbl
    )
    return(sim_est_output)
  }

  # concatenate results across repititions before moving to next regime
  oracle_tbl <- lapply(seq_len(args$n_sim),
                       function(x) results[[x]]$oracle) %>% bind_rows
  norms_tbl <- lapply(seq_len(args$n_sim),
                      function(x) results[[x]]$norms) %>% bind_rows

  # combine into a single object
  out <- list(
    oracle_tbl = oracle_tbl,
    norms_tbl = norms_tbl
  )

  # save results to file
  time_stamp <- str_replace_all(Sys.time(), " ", "_")
  file_name <- paste0("v", packageVersion("cvCovEst"),  "_",
                      "n", n_dim, "_", "p", p_dim, "_",
                      time_stamp, ".rds")
  saveRDS(object = out, file = here("data", paste0("dgp", args$sim_type),
                                    file_name))
})
