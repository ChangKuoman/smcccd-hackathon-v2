boxplot_g <- function(data, var, name) {

  if (!var %in% names(data)) {
    stop(paste("Column", var, "not found in data"))
  }
  
  x <- data[[var]]
  
  # Crear boxplot
  boxplot(
    x,
    col = "royalblue",
    ylim = c(min(x), max(x)),
    main = paste("Graph:", name, "Boxplot"),
    cex.main = 1,
    ylab = name,
    outcol = "red"
  )
  
  # Calcular y mostrar coeficiente de variación
  cv <- sd(x) / mean(x)
  cat("El coeficiente de variación es de", round(cv * 100, 2), "%.\n")
}
