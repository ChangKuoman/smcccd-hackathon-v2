exponencial <- function(data, col_name, titulo = "Gráfico de Densidad", 
                                         xlabel = "Valor", ylabel = "Densidad", 
                                         ylim = c(0, 0.1), from = 0, to = 40, step = 0.1) {
  # Verifica que la columna exista
  if (!col_name %in% names(data)) {
    stop(paste("La columna", col_name, "no existe en los datos."))
  }
  
  valores <- data[[col_name]]
  valores <- valores[!is.na(valores)]  # Eliminar NAs
  
  # Graficar densidad observada
  plot(density(valores, from = from), 
       col = "red", 
       lwd = 3, 
       main = titulo,
       xlab = xlabel,
       ylab = ylabel,
       ylim = ylim)
  
  # Secuencia para la distribución teórica
  x <- seq(from, to, step)
  
  # Agregar curva teórica exponencial con lambda estimado como 1/media
  lines(x, dexp(x, rate = 1 / mean(valores)), col = "blue", lty = 1, lwd = 2)
  
  # Leyenda
  legend("topright", 
         legend = c("Curva Observada", "Curva Teórica"),
         lty = 1, 
         lwd = 3, 
         col = c("red", "blue"), 
         bty = "n",
         cex = 0.8)
  
  estadisticas = gofstat(fitdist(valores, "exp"))
  cat("Valor p del test Chi-cuadrado:", estadisticas$chisqpvalue, "\n")
  cat("Valor p del test Kolmogorov-Smirnov:", estadisticas$kstest, "\n")
  
  
  ajuste_exp <- fitdist(valores, "exp")
  print(ajuste_exp)
}