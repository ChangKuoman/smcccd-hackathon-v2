regresion_lineal <- function(data, x_col, y_col, 
                            title = "Linear Regression Plot", 
                            x_title = NULL, 
                            y_title = NULL, 
                            y_lim = NULL) {
  # Convert to symbols for use in ggplot
  x_sym <- rlang::sym(x_col)
  y_sym <- rlang::sym(y_col)
  
  # Default to column names if no custom titles are provided
  if (is.null(x_title)) x_title <- x_col
  if (is.null(y_title)) y_title <- y_col
  
  # Create the plot
  p <- ggplot(data, aes(x = !!x_sym, y = !!y_sym)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, color = "red", alpha = 0.3, linetype = 1) +
    labs(title = title,
         x = x_title,
         y = y_title) +
    theme_minimal()
  
  if (!is.null(y_lim)) {
    p <- p + ylim(y_lim)
  }
  
  print(p)
  
  # Calculate and print the correlation
  cor_val <- cor(data[[x_col]], data[[y_col]], use = "complete.obs")
  cat("The correlation between", y_col, "and", x_col, "is", round(cor_val, 3), "\n")
  
  # Fit linear model and get formula
  formula_text <- as.formula(paste(y_col, "~", x_col))
  model <- lm(formula_text, data = data)
  
  # Calculate R²
  r2 <- summary(model)$r.squared
  cat("The coefficient of determination (R²) is", round(r2, 3), "\n")
  
  # Print regression formula
  coef <- coef(model)
  cat("The regression formula is:\n")
  cat(paste(y_col, "=", round(coef[1], 3), "+", round(coef[2], 3), "*", x_col), "\n")
  
  return(cor_val)
}
