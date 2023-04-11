suppressPackageStartupMessages(library(ppcor))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggpubr))
suppressPackageStartupMessages(library(extrafont))
suppressPackageStartupMessages(library(tikzDevice))
suppressPackageStartupMessages(library(tinytex))
suppressPackageStartupMessages(library(ggrepel))
suppressPackageStartupMessages(library(reshape2))
suppressPackageStartupMessages(library(ggbreak))

set.seed(42)

theme_pubr <- function(legend = c("top", "bottom", "left", "right", "none")) {
  base_size <- 10
  base_family <- 'LM Roman 10'
  half_line <- base_size / 2
  if (!is.numeric(legend)) legend <- match.arg(legend)
  panel.border <- element_blank()
  axis.line <- element_line(colour = "black", size = 0.5)
  plot.margin <- margin(0, half_line, half_line, half_line)
  .theme <- theme_bw(base_size = base_size, base_family = base_family) %+replace% theme(panel.border = panel.border, panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = axis.line, axis.text = element_text(color = "black"), legend.key = element_blank(), strip.background = element_rect(colour = "black"), plot.margin = plot.margin, legend.position = legend, complete = TRUE)
  .theme <- .theme + theme(axis.title.x = element_text(margin = margin(t = half_line)), axis.title.y = element_text(margin = margin(r = half_line)))
  .theme
}

scor <- function(x, y, type, p) {
  if (cor.test(x, y, method = type, use = "complete.obs", exact = FALSE)$p.value >= p) NaN
  else cor(x, y, method = type, use = "complete.obs")
}

# From: https://stackoverflow.com/a/45614547/4475789
GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin,
                           draw_group = function(self, data, ..., draw_quantiles = NULL) {
                             data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                             grp <- data[1, "group"]
                             newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                             newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                             newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])

                             if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                               stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                                         1))
                               quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
                               aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                               aesthetics$alpha <- rep(1, nrow(quantiles))
                               both <- cbind(quantiles, aesthetics)
                               quantile_grob <- GeomPath$draw_panel(both, ...)
                               ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                             }
                             else {
                               ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                             }
                           })
geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ...,
                              draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE,
                              show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}

num_apa <- function(value, places) {
  if (value < 0) sprintf('\\shortminus %s', substring(sprintf('%.10f', round(-value, places)), 2, 2 + places))
  else substring(sprintf('%.10f', round(value, places)), 2, 2 + places)
}

spearman_val <- function(s, p, p_level = 0.01, bold_level = 0.005, places = 3) {
  if (p > p_level) '--'
  else if (s == 1.00) ''
  else if (abs(p) < bold_level) sprintf('$\\bf{%s}$', num_apa(s, places))
  else sprintf('$%s$', num_apa(s, places))
}