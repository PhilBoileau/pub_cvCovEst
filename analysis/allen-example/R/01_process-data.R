###############################################################################
# Process Zeisel Data ##########################################################
################################################################################

# Load pkgs
library(here)
library(scRNAseq)
library(scater)
library(org.Mm.eg.db)
library(scran)

# load Allen data #############################################################

# load the dataset
sce_allen <- ReprocessedAllenData()

# filter out the cells that did not pass the quality checks
sce_allen <- sce_allen[, (sce_allen$passes_qc_checks_s == "Y" &
                          !is.na(sce_allen$Core.Type))]
sce_allen <- sce_allen[, sce_allen$Core.Type != "Intermediate"]

# normalization ################################################################

set.seed(412321)
counts(sce_allen) <- assay(sce_allen, "rsem_counts")
clusters <- quickCluster(sce_allen)
sce_allen <- computeSumFactors(sce_allen, clusters = clusters)
sce_allen <- logNormCounts(sce_allen)

# variance modelling ###########################################################

dec_allen <- modelGeneVarWithSpikes(sce_allen, "ERCC",
                                    assay.type = "rsem_counts")
top_hvgs <- getTopHVGs(dec_allen, n = 1000)

# save processed data and most variable genes ##################################

# remove some assays to make it smaller
assay(sce_allen, "tophat_counts") <- NULL
assay(sce_allen, "cufflinks_fpkm") <- NULL
assay(sce_allen, "rsem_tpm") <- NULL

saveRDS(
  list(sce_allen, top_hvgs),
  file = here("allen-example", "results", "preprocess_data.rds")
)
