# Contains plotting functions for scRNA-seq analyses

# The following function compares the UMAP representations using PCA and
# cvPCA as initializations. The sce object input to the function should contain
# "UMAP" and "cvUMAP" reduced dims, and should have a class label variable
# dictated by the class argument as a string.
plot_reduced_dims <- function(sce, class, point_size = 0.3) {
  # compute the silhouette widths of each reduced dimensional representation
  umap_sil <- cluster::silhouette(
    x = as.integer(as.factor(colData(sce)[, class])),
    dist(reducedDim(sce, "UMAP"))
  )
  cvumap_sil <- cluster::silhouette(
    x = as.integer(as.factor(colData(sce)[, class])),
    dist(reducedDim(sce, "cvUMAP"))
  )

  # plot the results
  umap_pca_p <- plotReducedDim(sce, dimred = "UMAP",
                               colour_by = class,
                               point_size = point_size) +
                  ggtitle(paste0("PCA → UMAP",
                                 " (Ave. Sil. Width: ",
                                 format(round(mean(umap_sil[, 3]), 2),
                                        nsmall = 2),
                                 ")")) +
                  scale_color_brewer(name = "Cell type", palette = "Dark2")
  umap_cvpca_p <- plotReducedDim(sce, dimred = "cvUMAP",
                                 colour_by = class,
                                 point_size = point_size) +
                    ggtitle(paste0("cvCovEst → PCA → UMAP",
                                   " (Ave. Sil. Width: ",
                                   format(round(mean(cvumap_sil[, 3]), 2),
                                          nsmall = 2),
                                   ")")) +
                    xlab("UMAP 1") +
                    ylab("UMAP 2") +
                    scale_color_brewer(name = "Cell type", palette = "Dark2")
  ggpubr::ggarrange(
    umap_pca_p, umap_cvpca_p, ncol = 2,
    common.legend = TRUE,
    legend = "bottom"
  )
}
