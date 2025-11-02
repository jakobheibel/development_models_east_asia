#' Create Custom Rectangle Dendrogram
#'
#' Helper function for custom dendrogram visualization
#' @param dend Dendrogram object
#' @param k Number of clusters
#' @param k_colors Colors for clusters
#' @param additional parameters for customization
#' @return ggplot2 layer
#' @export
custom_rect_dendrogram <- function(dend, k = NULL, h = NULL, k_colors = NULL,
                                   palette = NULL, rect_fill = FALSE, rect_lty = 2,
                                   lower_rect = -1.5, rect_width_offset = 3.5, ...) {
  
  if (missing(k_colors) & !is.null(palette))
    k_colors <- palette
  
  prop_k_height <- 0.5
  if (!dendextend::is.dendrogram(dend))
    stop("x is not a dendrogram object.")
  
  k <- if (!is.null(h)) {
    length(unique(dendextend::cutree(dend, h = h)))
  } else {
    k
  }
  
  tree_heights <- dendextend::heights_per_k.dendrogram(dend)[-1]
  tree_order <- stats::order.dendrogram(dend)
  
  if (is.null(k))
    stop("specify k")
  if (k < 2) {
    stop(gettextf("k must be between 2 and %d", length(tree_heights)),
         domain = NA)
  }
  
  cluster <- dendextend::cutree(dend, k = k)
  clustab <- table(cluster)[unique(cluster[tree_order])]
  m <- c(0, cumsum(clustab))
  which <- 1L:k
  
  # Create rectangle coordinates
  df <- map_df(seq_along(which), function(n) {
    next_k_height <- tree_heights[names(tree_heights) == k + 1]
    if (length(next_k_height) == 0) {
      next_k_height <- 0
      prop_k_height <- 1
    }
    
    tibble(
      xmin = m[which[n]] + 0.66,
      ymin = lower_rect - rect_width_offset,
      xmax = m[which[n] + 1] + 0.33,
      ymax = tree_heights[names(tree_heights) == k] * prop_k_height +
        next_k_height * (1 - prop_k_height)
    )
  })
  
  # Handle colors
  color <- k_colors
  if (length(color) == 1 && color == "cluster")
    color <- "default"
  if (ggpubr:::.is_col_palette(color))
    color <- ggpubr:::.get_pal(color, k = k)
  else if (length(color) > 1 & length(color) < k) {
    color <- rep(color, k)[1:k]
  }
  
  if (rect_fill) {
    fill <- color
    alpha <- 0.2
  } else {
    fill <- "transparent"
    alpha <- 0
  }
  
  df$color <- color
  df$cluster <- as.factor(paste0("c", 1:k))
  
  ggplot2::geom_rect(
    data = df,
    aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax),
    fill = fill,
    color = color,
    linetype = rect_lty,
    alpha = alpha,
    ...
  )
}