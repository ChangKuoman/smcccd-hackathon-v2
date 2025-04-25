boxplot_g <- function(data, var, name) {

  if (!var %in% names(data)) {
    stop(paste("Column", var, "not found in data"))
  }
  
  x <- data[[var]]
  
  #png("images/g1.png", width = 600, height = 600)  # Set dimensions for the image
  
  # Crear boxplot
  boxplot(
    x,
    col = "royalblue",
    ylim = c(min(x), max(x)),
    main = paste("Graph:", name),
    cex.main = 1,
    ylab = name,
    outcol = "red"
  )
  #dev.off()
}
