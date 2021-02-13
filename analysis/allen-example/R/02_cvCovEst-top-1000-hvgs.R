################################################################################
# Run cvCovEst on top 1000 features ############################################
################################################################################

# load the libraries
library(here)
library(SingleCellExperiment)
library(future)
library(future.apply)
library(cvCovEst)

# fixing parallelization
library(openblasctl)
openblas_set_num_threads(1)
library(OpenMPController)
omp_set_num_threads(1)

# load the data ################################################################

processed_data_ls <- readRDS(
  file = here("allen-example", "results", "preprocess_data.rds")
)
sce_allen <- processed_data_ls[[1]]
top_hvgs <- processed_data_ls[[2]]

# format data
data_in <- t(logcounts(sce_allen)[which(rownames(sce_allen) %in% top_hvgs), ])

# run cvCovEst #################################################################
set.seed(5234523)
plan(multiprocess, workers = 5L)
cv_cov_results <- cvCovEst(
  dat = data_in,
  estimators = c(linearShrinkLWEst, thresholdingEst, sampleCovEst,
                 scadEst, poetEst, adaptiveLassoEst,denseLinearShrinkEst),
  estimator_params = list(
    thresholdingEst = list(gamma = seq(0.05, 0.3, by = 0.05)),
    scadEst = list(lambda = seq(0.05, 0.5, by = 0.05)),
    poetEst = list(k = seq(5L, 10L), lambda = seq(0.05, 0.3, by = 0.05)),
    adaptiveLassoEst = list(lambda = seq(0.1, 0.5, by = 0.1),
                            n = seq(0.1, 0.5, by = 0.1))
  ),
  cv_loss = cvMatrixFrobeniusLoss,
  cv_scheme = "v_fold",
  v_folds = 5L,
  center = TRUE,
  scale = TRUE,
  parallel = TRUE
)

# save the results
saveRDS(cv_cov_results,
        file = here("allen-example", "results", "p1000_cv_cov_est_results.rds")
)
