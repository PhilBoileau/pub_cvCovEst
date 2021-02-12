library(here)
library(tidyverse)
library(ggpubr)

# load the Zeisel and Allen analyses plots
zeisel_p <- readRDS(file = here("combined", "graphs", "zeisel_1000hvgs.rds"))
allen_p <- readRDS(file = here("combined", "graphs", "allen_1000hvgs.rds"))

# create whitespace as a buffer between both sets of plots
whitespace_p <- ggparagraph(text = " ", color = "black", size = "12",
                            face = "plain", family = "times", lineheight = 1)

# combine the plots
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
  path = here("analysis", "combined", "graphs"),
  dpi = 320,
  device = "png",
  width = 7,
  height = 5,
  units = "in",
  scale = 1.7
)
