# this file contains plotting functions for the results aggregates in script 04

# Mean risk difference ratio plot (Eq 18 in manuscript)
# Input: The combined (across n, p, and dgp) table of risk diff ratios
# Output: A tiled plot of results
meanRiskDiffRatioPlot <- function(dgp_ratio_tbl) {
  dgp_ratio_tbl %>%
    mutate(
      pn_ratio = p_dim / n_dim,
      pn_ratio = factor(pn_ratio,
                        levels = c("0.3", "0.5", "1", "2", "5"),
                        labels = c("J/n = 0.3", "J/n = 0.5",
                                   "J/n = 1", "J/n = 2", "J/n = 5")),
      n_dim = factor(n_dim, levels = c("50", "100", "200", "500"))
    ) %>%
    group_by(
      n_dim, pn_ratio, data_gen_pro
    ) %>%
    summarise(
      mean_cv_cv_risk_diff = mean(cv_cv_riskdiff),
      mean_oracle_cv_risk_diff = mean(oracle_cv_riskdiff),
      .groups = "drop"
    ) %>%
    mutate(
      exp_ratio = mean_cv_cv_risk_diff / mean_oracle_cv_risk_diff
    ) %>%
    ggplot(aes(x = n_dim, y = exp_ratio)) +
      facet_grid(rows = vars(data_gen_pro), cols = vars(pn_ratio),
                 scales = "free_y") +
      geom_hline(yintercept = 1, linetype = "dotted",
                 colour = "red", alpha = 0.5) +
      geom_point() +
      scale_y_continuous(breaks = waiver(), n.breaks = 3) +
      xlab("Number of observations") +
      ylab(TeX(
        paste0("$\\frac{E_{P_0}\\left[\\tilde{\\theta}_{p_n,n}",
               "(\\hat{k}_{p_n,n}) - \\theta_0\\right]}",
               "{E_{P_0}\\left[\\tilde{\\theta}_{p_n,n}",
               "(\\tilde{k}_{p_n, n}) - \\theta_0\\right]}$"))) +
      ggtitle("Mean cross-validated conditional risk difference ratios") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
}


# Risk difference ratio plot (Eq 19 in manuscript)
# Input: The combined (across n, p, and dgp) table of risk diff ratios
# Output: A tiled plot of results
riskDiffRatioPlot <- function(dgp_ratio_tbl) {
  dgp_ratio_tbl %>%
    mutate(
      pn_ratio = p_dim / n_dim,
      pn_ratio = factor(pn_ratio,
                        levels = c("0.3", "0.5", "1", "2", "5"),
                        labels = c("J/n = 0.3", "J/n = 0.5",
                                   "J/n = 1", "J/n = 2", "J/n = 5")),
      n_dim = factor(n_dim, levels = c("50", "100", "200", "500"))
    ) %>%
    ggplot(aes(x = n_dim, y = cv_oracle_riskdiff_ratio)) +
      facet_grid(rows = vars(data_gen_pro), cols = vars(pn_ratio),
                 scales = "free_y") +
      geom_hline(yintercept = 1, linetype = "dotted",
                colour = "red", alpha = 0.5) +
      geom_jitter(height = 0, width = 0.2, size = 0.3, alpha = 0.3) +
      xlab("Number of observations") +
      ylab(TeX(
        paste0(
          "$\\frac{\\tilde{\\theta}_{p_n,n}(\\hat{k}_{p_n,n}) - \\theta_0}",
          "{\\tilde{\\theta}_{p_n,n}(\\tilde{k}_{p_n,n}) - \\theta_0}$"))
      ) +
      ggtitle("Cross-validated conditional risk difference ratios") +
      scale_y_continuous(breaks = waiver(), n.breaks = 3) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
}


# Full risk difference ratio plot (Eq 21 in manuscript)
# Input: The combined (across n, p, and dgp) table of risk diff ratios
# Output: A tiled plot of results
fullRiskDiffRatioPlot <- function(dgp_ratio_tbl) {
  dgp_ratio_tbl %>%
    mutate(
      pn_ratio = p_dim / n_dim,
      pn_ratio = factor(pn_ratio,
                        levels = c("0.3", "0.5", "1", "2", "5"),
                        labels = c("J/n = 0.3", "J/n = 0.5",
                                   "J/n = 1", "J/n = 2", "J/n = 5")),
      n_dim = factor(n_dim, levels = c("50", "100", "200", "500"))
    ) %>%
    ggplot(aes(x = n_dim, y = full_oracle_riskdiff_ratio)) +
      facet_grid(rows = vars(data_gen_pro), cols = vars(pn_ratio),
                 scales = "free_y") +
      geom_hline(yintercept = 1, linetype = "dotted",
                colour = "red", alpha = 0.5) +
      geom_jitter(height = 0, width = 0.2, size = 0.3, alpha = 0.3) +
      xlab("Number of observations") +
      ylab(TeX(
        paste0(
          "$\\frac{\\tilde{\\theta}_{n}(\\hat{k}_{p_n,n}) - \\theta_0}",
          "{\\tilde{\\theta}_{n}(\\tilde{k}_{n}) - \\theta_0}$"))
      ) +
      ggtitle("Full-dataset conditional risk difference ratios") +
      scale_y_continuous(breaks = waiver(), n.breaks = 3) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
}

# Comparison of estimators' mean Frobenius norms
# Input: The combined (across n, p, and dgp) table of Frobenius norms for all
# estimators considered.
# Output: A tiled plot of the results
frobeniusNormPlot <- function(dgp_norms_tbl) {
  pd <- ggplot2::position_dodge(0.5)
  dgp_norms_tbl %>%
    mutate(
      pn_ratio = p_dim / n_dim,
      pn_ratio = factor(pn_ratio,
                        levels = c("0.3", "0.5", "1", "2", "5"),
                        labels = c("J/n = 0.3", "J/n = 0.5",
                                   "J/n = 1", "J/n = 2", "J/n = 5")),
      n_dim = factor(n_dim, levels = c("50", "100", "200", "500"))
    ) %>%
    group_by(
      n_dim, pn_ratio, data_gen_pro, estimator
    ) %>%
    summarise(mf = mean(frobenius_norm), .groups = "drop") %>%
    ggplot(aes(x = n_dim, y = log(mf),
               fill = estimator, shape = estimator, alpha = estimator)) +
      facet_grid(rows = vars(data_gen_pro), cols = vars(pn_ratio),
                 scales = "free_y") +
      geom_point(position = pd) +
      geom_line(aes(group = estimator), alpha = 0.025, position = pd) +
      xlab("Number of observations") +
      ylab("Log mean Frobenius norm") +
      scale_fill_brewer(name = "Estimators", palette = "Set3",
                        labels = c("Adaptive LASSO", "Banding",
                                   "cvCovEst", "Dense linear shrinkage",
                                   "Linear shrinkage", "Nonlinear shrinkage",
                                   "POET", "Sample covariance matrix",
                                   "SCAD thresholding", "Tapering",
                                   "Hard thresholding")) +
      scale_shape_manual(name = "Estimators",
                         labels = c("Adaptive LASSO", "Banding",
                                    "cvCovEst", "Dense linear shrinkage",
                                    "Linear shrinkage", "Nonlinear shrinkage",
                                    "POET", "Sample covariance matrix",
                                    "SCAD thresholding", "Tapering",
                                    "Hard thresholding"),
                         values = c(21, 22, 8, 23, 23, 23, 24,
                                    4, 21, 22, 21)) +
      scale_alpha_manual(name = NULL,
                         labels = c("Adaptive LASSO", "Banding",
                                    "cvCovEst", "Dense linear shrinkage",
                                    "Linear shrinkage", "Nonlinear shrinkage",
                                    "POET", "Sample covariance matrix",
                                    "SCAD thresholding", "Tapering",
                                    "Hard thresholding"),
                         values = c(0.2, 0.2, 1, 0.2, 0.2, 0.2, 0.2,
                                    0.2, 0.2, 0.2, 0.2)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 4)) +
      guides(alpha = FALSE) +
      ggtitle("Mean Frobenius norm comparison") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
            legend.position = "bottom")
}

# Comparison of estimators' mean spectral norms
# Input: The combined (across n, p, and dgp) table of spectral norms for all
# estimators considered.
# Output: A tiled plot of the results
spectralNormPlot <- function(dgp_norms_tbl) {
  pd <- ggplot2::position_dodge(0.5)
  dgp_norms_tbl %>%
    mutate(
      pn_ratio = p_dim / n_dim,
      pn_ratio = factor(pn_ratio,
                        levels = c("0.3", "0.5", "1", "2", "5"),
                        labels = c("J/n = 0.3", "J/n = 0.5",
                                   "J/n = 1", "J/n = 2", "J/n = 5")),
      n_dim = factor(n_dim, levels = c("50", "100", "200", "500"))
    ) %>%
    group_by(
      n_dim, pn_ratio, data_gen_pro, estimator
    ) %>%
    summarise(mo = mean(operator_norm), .groups = "drop") %>%
    ggplot(aes(x = n_dim, y = log(mo),
               fill = estimator, shape = estimator, alpha = estimator)) +
      facet_grid(rows = vars(data_gen_pro), cols = vars(pn_ratio),
                 scales = "free_y") +
      geom_point(position = pd) +
      geom_line(aes(group = estimator), alpha = 0.025, position = pd) +
      xlab("Number of observations") +
      ylab("Log mean spectral norm") +
      scale_fill_brewer(name = "Estimators", palette = "Set3",
                        labels = c("Adaptive LASSO", "Banding",
                                   "cvCovEst", "Dense linear shrinkage",
                                   "Linear shrinkage", "Nonlinear shrinkage",
                                   "POET", "Sample covariance matrix",
                                   "SCAD thresholding", "Tapering",
                                   "Hard thresholding")) +
      scale_shape_manual(name = "Estimators",
                         labels = c("Adaptive LASSO", "Banding",
                                    "cvCovEst", "Dense linear shrinkage",
                                    "Linear shrinkage", "Nonlinear shrinkage",
                                    "POET", "Sample covariance matrix",
                                    "SCAD thresholding", "Tapering",
                                    "Hard thresholding"),
                         values = c(21, 22, 8, 23, 23, 23, 24,
                                    4, 21, 22, 21)) +
      scale_alpha_manual(name = NULL,
                         labels = c("Adaptive LASSO", "Banding",
                                    "cvCovEst", "Dense linear shrinkage",
                                    "Linear shrinkage", "Nonlinear shrinkage",
                                    "POET", "Sample covariance matrix",
                                    "SCAD thresholding", "Tapering",
                                    "Hard thresholding"),
                         values = c(0.2, 0.2, 1, 0.2, 0.2, 0.2, 0.2,
                                    0.2, 0.2, 0.2, 0.2)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 3)) +
      guides(alpha = FALSE) +
      ggtitle("Mean spectral norm comparison") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
            legend.position = "bottom")
}
