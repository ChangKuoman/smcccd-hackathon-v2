boxplot_gv <- function(data, x_col, y_col, title = NULL, x_title = NULL, y_title = NULL, y_lim = NULL, color_palette = "Blues") {
  x_sym <- sym(x_col)
  y_sym <- sym(y_col)
  
  # Get unique levels for fill colors
  levels_count <- length(unique(data[[x_col]]))
  colors <- scales::brewer_pal(palette = color_palette)(levels_count)
  
  # Format title
  plot_title <- str_wrap(
    if (!is.null(title)) title else paste("Distribución de", y_col, "según", x_col),
    width = 60
  )
  
  x_axis_title <- ifelse(is.null(x_title), x_col, x_title)
  y_axis_title <- ifelse(is.null(y_title), y_col, y_title)
  
  # Default Y limits
  y_range <- range(data[[y_col]], na.rm = TRUE)
  if (is.null(y_lim)) {
    y_lim <- c(floor(y_range[1]), ceiling(y_range[2]))
  }
  
  # Build the plot
  #p <- 
    ggplot(data, aes(x = !!x_sym, y = !!y_sym)) +
    geom_boxplot(
      fill = colors,
      lwd = 0.3,
      outlier.colour = "red"
    ) +
    scale_x_discrete(name = str_wrap(x_axis_title, width = 40)) +
    scale_y_continuous(name = y_axis_title, limits = y_lim) +
    ggtitle(plot_title) +
    theme_minimal(base_size = 13)
  
  
  #ggsave("images/g2.png", plot = p, width = 10, height = 6)
}
