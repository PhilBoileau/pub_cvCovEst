library(here)
library(tidyverse)
library(ggpubr)

# load the Zeisel and Allen analyses plots
zeisel_p <- readRDS(file = here("combined", "graphs", "zeisel_1000hvgs.rds"))
zeisel_sum <- readRDS(
  file = here("combined", "graphs", "zeisel_cvcovest_summary.rds")
)
zeisel_eig <- readRDS(file = here("combined", "graphs", "zeisel_eig_comp.rds"))
allen_p <- readRDS(file = here("combined", "graphs", "allen_1000hvgs.rds"))
allen_sum <- readRDS(
  file = here("combined", "graphs", "allen_cvcovest_summary.rds")
)
allen_eig <- readRDS(file = here("combined", "graphs", "allen_eig_comp.rds"))

# UMAP plots ##################################################################

# create whitespace as a buffer between both sets of plots
whitespace_p <- ggparagraph(text = " ", color = "black", size = "12",
                            face = "plain", family = "times", lineheight = 1)

# combine the UMAP plots
combined_p <- ggarrange(
  allen_p,
  whitespace_p,
  zeisel_p,
  nrow = 3,
  ncol = 1,
  labels = c("A", " ", "B"),
  align = "hv",
  heights = c(1, 0.05, 1),
  widths = c(1, 1, 1)
)

# save the plot
ggsave(
  plot = combined_p,
  filename = "combined_plot.png",
  path = here("combined", "graphs"),
  dpi = 320,
  device = "png",
  width = 7,
  height = 5,
  units = "in",
  scale = 1.7
)

# Zeisel et al diagnostic plots ###############################################

zeisel_diag_p <- ggarrange(
  zeisel_sum,
  zeisel_eig,
  nrow = 2,
  ncol = 1,
  labels = c("A", "B"),
  heights = c(3, 1),
  widths = c(1, 1)
)
ggsave(
  plot = zeisel_diag_p,
  filename = "zeisel_diag_plot.png",
  path = here("combined", "graphs"),
  dpi = 300,
  device = "png",
  width = 7,
  height = 8,
  units = "in",
  scale = 1.7
)


# Allen et al diagnostic plots ###############################################

allen_diag_p <- ggarrange(
  allen_sum,
  allen_eig,
  nrow = 2,
  ncol = 1,
  labels = c("A", "B"),
  heights = c(3, 1),
  widths = c(1, 1)
)
ggsave(
  plot = allen_diag_p,
  filename = "allen_diag_plot.png",
  path = here("combined", "graphs"),
  dpi = 300,
  device = "png",
  width = 7,
  height = 8,
  units = "in",
  scale = 1.7
)
