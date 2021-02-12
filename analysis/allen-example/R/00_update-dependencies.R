# set user-specific package library
if (grepl("savio2", Sys.info()["nodename"])) {
  .libPaths("/global/scratch/users/philippe_boileau/R")
  Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS = "true")
  lib_loc <- "/global/scratch/users/philippe_boileau/R"
}

# set CRAN mirror
options(repos = structure(c(CRAN = "https://cran.rstudio.com/")))

# lie to pkgbuild, as per Jeremy
install.packages("fansi", lib = lib_loc)
pkgbuild:::cache_set("has_compiler", TRUE)

# from CRAN
install.packages(c("here", "tidyverse", "remotes", "future", "future.apply",
                   "doFuture", "foreach", "doRNG", "OpenMPController", "rlang",
                   "purrr", "R.utils", "matrixStats", "MASS", "coop",
                   "origami", "rmarkdown", "BiocManager", "openblasctl",
                   "Rcpp", "RSpectra", "xml2"),
                 lib = lib_loc)
install.packages("openblasctl", repos = "https://hpcran.org", lib = lib_loc)

# from Bioc
BiocManager::install(c("BiocParallel", "scRNAseq", "scater", "orgMm.eg.db",
                       "scran"), lib = lib_loc)

# install private GitHub package via local git repo
remotes::install_local(here::here("..", "..", "..", "cvCovEst"), lib = lib_loc,
                       force = TRUE)

# update all packages
update.packages(ask = FALSE, lib.loc = lib_loc)
