regresion_lineal <- function(data, x_col, y_col, titulo = "Gráfico de Regresión Lineal", y_lim = NULL) {
  # Convertir a símbolos para uso en ggplot
  x_sym <- rlang::sym(x_col)
  y_sym <- rlang::sym(y_col)
  
  # Crear el gráfico
  p <- ggplot(data, aes(x = !!x_sym, y = !!y_sym)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, color = "red", alpha = 0.3, linetype = 1) +
    labs(title = titulo,
         x = x_col,
         y = y_col) +
    theme_minimal()
  
  if (!is.null(y_lim)) {
    p <- p + ylim(y_lim)
  }
  
  print(p)
  
  # Calcular y mostrar la correlación
  cor_val <- cor(data[[x_col]], data[[y_col]], use = "complete.obs")
  cat("La correlación entre", y_col, "y", x_col, "es de", round(cor_val, 3), "\n")
  
  formula_text <- as.formula(paste(y_col, "~", x_col))
  modelo <- lm(formula_text, data = data)
  
  # Calcular R²
  r2 <- summary(modelo)$r.squared
  cat("El coeficiente de determinación (R²) es de", round(r2, 3), "\n")
  
  
  return(cor_val)
}

