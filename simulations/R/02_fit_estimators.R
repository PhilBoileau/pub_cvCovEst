# Given a data a dataset and a covariance matrix, this functions estimates
# the Frobenius and Operator losses of the provided estimators, as well as of the
# cross-validation procedure.
fit_estimators <- function(
  data_in,
  cov_mat,
  estimators = c(linearShrinkLWEst, thresholdingEst, sampleCovEst,
                 bandingEst, taperingEst, denseLinearShrinkEst,
                 nlShrinkLWEst, scadEst, poetEst, adaptiveLassoEst),
  estimator_params = list(
    thresholdingEst = list(gamma = seq(0.1, 1, by = 0.1)),
    bandingEst = list(k = seq(1L, 5L)),
    taperingEst = list(k = seq(2L, 10L, by = 2L)),
    scadEst = list(lambda = seq(0.1, 1, by = 0.1)),
    poetEst = list(k = seq(1L, 5L), lambda = seq(0.1, 0.3, by = 0.1)),
    adaptiveLassoEst = list(lambda = seq(0.1, 0.5, by = 0.1),
                            n = seq(0.1, 0.5, by = 0.2))
  ),
  cv_scheme = "v_fold",
  v_folds = 5L,
  mc_split = 0.5,
  parallel = FALSE
) {

  # run cross-validation selector
  cv_cov_est_out <- cvCovEst(
    dat = data_in,
    estimators = !!enexpr(estimators),
    estimator_params = estimator_params,
    cv_scheme = cv_scheme,
    v_folds = v_folds,
    mc_split = mc_split,
    center = TRUE,
    scale = FALSE,
    parallel = parallel,
    true_cov_mat = cov_mat
  )

  # linear shrinkage
  linear_shrink_est <- linearShrinkLWEst(data_in)

  # dense linear shrinkage
  dense_linear_shrink_est <- denseLinearShrinkEst(data_in)

  # nonlinear shrinkage
  nonlinear_shrink_est <- nlShrinkLWEst(data_in)

  # hard thresholding
  hard_thresh_est <- cvCovEst(
    dat = data_in,
    estimators = c(thresholdingEst),
    estimator_params = list(
      thresholdingEst = list(gamma = seq(0.05, 1, by = 0.05))
    ),
    cv_scheme = cv_scheme,
    v_folds = v_folds,
    mc_split = mc_split,
    center = TRUE,
    scale = FALSE,
    parallel = parallel
  )$estimate

  # sample covariance
  sample_cov_est <- coop::covar(data_in)

  # banding
  banding_est <- cvCovEst(
    dat = data_in,
    estimators = c(bandingEst),
    estimator_params = list(
      bandingEst = list(k = seq(1L, 10L))
    ),
    cv_scheme = cv_scheme,
    v_folds = v_folds,
    mc_split = mc_split,
    center = TRUE,
    scale = FALSE,
    parallel = parallel
  )$estimate

  # tapering
  tapering_est <- cvCovEst(
    dat = data_in,
    estimators = c(taperingEst),
    estimator_params = list(
      taperingEst = list(k = seq(2L, 10L, by = 2L))
    ),
    cv_scheme = cv_scheme,
    v_folds = v_folds,
    mc_split = mc_split,
    center = TRUE,
    scale = FALSE,
    parallel = parallel
  )$estimate

  # SCAD thresholding
  SCAD_est <- cvCovEst(
    dat = data_in,
    estimators = c(scadEst),
    estimator_params = list(
      scadEst = list(lambda = seq(0.05, 1, by = 0.05))
    ),
    cv_scheme = cv_scheme,
    v_folds = v_folds,
    mc_split = mc_split,
    center = TRUE,
    scale = FALSE,
    parallel = parallel
  )$estimate

  # POET
  POET_est <- cvCovEst(
    dat = data_in,
    estimators = c(poetEst),
    estimator_params = list(
      poetEst = list(k = seq(1L, 10L), lambda = seq(0.1, 1, by = 0.1))
    ),
    cv_scheme = cv_scheme,
    v_folds = v_folds,
    mc_split = mc_split,
    center = TRUE,
    scale = FALSE,
    parallel = parallel
  )$estimate

  # adaptive LASSO thresholding
  adapt_lasso_est <- cvCovEst(
    dat = data_in,
    estimators = c(adaptiveLassoEst),
    estimator_params = list(
      adaptiveLassoEst = list(
        lambda = seq(0.1, 0.5, by = 0.1),
        n = seq(0.1, 0.5, by = 0.1))
    ),
    cv_scheme = cv_scheme,
    v_folds = v_folds,
    mc_split = mc_split,
    center = TRUE,
    scale = FALSE,
    parallel = parallel
  )$estimate

  # concatenate estimates into list for easier evaluation
  estimator_names <- c(
    "cvCovEst",
    "linearShrinkLWEst",
    "denseLinearShrinkEst",
    "thresholdingEst",
    "sampleCovMat",
    "bandingEst",
    "nlShrinkLWEst",
    "taperingEst",
    "scadEst",
    "poetEst",
    "adaptiveLassoEst"
  )
  frob_norms <- c(
    cv_cov_est_out$estimate %>% frobeniusNorm_safe(cov_mat),
    linear_shrink_est %>% frobeniusNorm_safe(cov_mat),
    dense_linear_shrink_est %>% frobeniusNorm_safe(cov_mat),
    hard_thresh_est %>% frobeniusNorm_safe(cov_mat),
    sample_cov_est %>% frobeniusNorm_safe(cov_mat),
    banding_est %>% frobeniusNorm_safe(cov_mat),
    nonlinear_shrink_est %>% frobeniusNorm_safe(cov_mat),
    tapering_est %>% frobeniusNorm_safe(cov_mat),
    SCAD_est %>% frobeniusNorm_safe(cov_mat),
    POET_est %>% frobeniusNorm_safe(cov_mat),
    adapt_lasso_est %>% frobeniusNorm_safe(cov_mat)
  )
  oper_norms <- c(
    cv_cov_est_out$estimate %>% operatorNorm_safe(cov_mat),
    linear_shrink_est %>% operatorNorm_safe(cov_mat),
    dense_linear_shrink_est %>% operatorNorm_safe(cov_mat),
    hard_thresh_est %>% operatorNorm_safe(cov_mat),
    sample_cov_est %>% operatorNorm_safe(cov_mat),
    banding_est %>% operatorNorm_safe(cov_mat),
    nonlinear_shrink_est %>% operatorNorm_safe(cov_mat),
    tapering_est %>% operatorNorm_safe(cov_mat),
    SCAD_est %>% operatorNorm_safe(cov_mat),
    POET_est %>% operatorNorm_safe(cov_mat),
    adapt_lasso_est %>% operatorNorm_safe(cov_mat)
  )

  # create the table of outputs
  norms_tbl <- tibble(
    estimator = estimator_names,
    frobenius_norm = frob_norms,
    operator_norm = oper_norms,
    .name_repair = "minimal"
  )

  # return list of norm table and conditional risk difference ratio
  return(
    list(
      cv_oracle_riskdiff_ratio = cv_cov_est_out$cv_oracle_riskdiff_ratio,
      cv_cv_riskdiff = cv_cov_est_out$cv_cv_riskdiff,
      oracle_cv_riskdiff = cv_cov_est_out$oracle_cv_riskdiff,
      full_oracle_riskdiff_ratio = cv_cov_est_out$full_oracle_riskdiff_ratio,
      norms_tbl = norms_tbl
    )
  )
}
