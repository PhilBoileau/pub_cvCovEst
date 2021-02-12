################################################################################
# Process Zeisel Data ##########################################################
################################################################################

# Load pkgs
library(here)
library(scRNAseq)
library(scater)
library(org.Mm.eg.db)
library(scran)

# load zeisel data #############################################################

# load the dataset
sce_zeisel <- ZeiselBrainData()

# aggregate some gene's expressions
sce_zeisel <- aggregateAcrossFeatures(
  sce_zeisel,
  sub("_loc[0-9]+$", "", rownames(sce_zeisel))
)

# annotate the genes
rowData(sce_zeisel)$Ensembl <- mapIds(
  org.Mm.eg.db, keys = rownames(sce_zeisel),
  keytype = "SYMBOL", column = "ENSEMBL"
)

# quality control ##############################################################

# compute quality control metrics
stats <- perCellQCMetrics(
  sce_zeisel,
  subsets = list(
    Mt = rowData(sce_zeisel)$featureType == "mito"
  )
)
qc <- quickPerCellQC(
  stats,
  percent_subsets = c("altexps_ERCC_percent", "subsets_Mt_percent")
)

# discard poor quality cells
sce_zeisel <- sce_zeisel[, !qc$discard]

# normalization ################################################################

set.seed(412321)
clusters <- quickCluster(sce_zeisel)
sce_zeisel <- computeSumFactors(sce_zeisel, clusters = clusters)
sce_zeisel <- logNormCounts(sce_zeisel)

# variance modelling ###########################################################

dec_zeisel <- modelGeneVarWithSpikes(sce_zeisel, "ERCC")
top_hvgs <- getTopHVGs(dec_zeisel, n = 1000)

# save processed data and most variable genes ##################################
saveRDS(
  list(sce_zeisel, top_hvgs),
  file = here("results", "preprocess_data.rds")
)
