barplot_g <- function(data, column_name, name, highlight_values = NULL, highlight_color = "indianred2", default_color = "gray30") {
  # Get the actual column from the dataframe
  column_data <- data[[column_name]]
  
  # Create frequency table
  freq_table <- table(column_data)
  prop_table <- prop.table(freq_table) * 100
  p_df <- data.frame(Categoria = names(prop_table), Porcentaje = as.numeric(prop_table))
  
  # Prepare color vector
  color_vector <- if (!is.null(highlight_values)) {
    ifelse(names(freq_table) %in% highlight_values, highlight_color, default_color)
  } else {
    rep(default_color, length(freq_table))
  }
  
  # Plot
  plot <- ggplot(data, aes_string(y = column_name)) +
    geom_bar(fill = color_vector) +
    xlab(paste("Number of:", name)) +
    ylab(name) +
    ggtitle(paste("Graph:", name, "Distribution"))
  
  print(plot)
  
  return(p_df)
}