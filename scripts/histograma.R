histograma <- function(data, var, nombre) {

  if (!var %in% names(data)) {
    stop(paste("Column", var, "not found in data"))
  }
  
  x <- data[[var]]
  n <- length(x)
  
  # Rango y Sturges
  valor_min <- min(x)
  valor_max <- max(x)
  rango <- valor_max - valor_min
  k <- ceiling(1 + log2(n)) # Regla de Sturges
  A <- rango / k # Amplitud
  
  # Límites
  limites <- seq(valor_min, valor_max, by = A)
  if (tail(limites, 1) < valor_max) {
    limites <- c(limites, valor_max) # por si se queda corto
  }
  
  # Tabla de frecuencias
  datos_agrupados <- cut(x, breaks = limites, right = TRUE, include.lowest = TRUE)
  tabla <- table(datos_agrupados)
  frecuencias <- as.numeric(tabla)
  
  # Bordes de intervalos
  L <- limites[-length(limites)]     # Límite inferior de cada clase
  U <- limites[-1]                   # Límite superior de cada clase
  puntos_medios <- (L + U) / 2       # Marca de clase
  
  # Media agrupada
  media <- sum(puntos_medios * frecuencias) / n
  
  # Moda agrupada
  moda_index <- which.max(frecuencias)
  fi <- frecuencias[moda_index]
  fi_1 <- ifelse(moda_index == 1, 0, frecuencias[moda_index - 1])
  fi1 <- ifelse(moda_index == length(frecuencias), 0, frecuencias[moda_index + 1])
  Li_moda <- L[moda_index]
  moda <- Li_moda + ((fi - fi_1) / ((fi - fi_1) + (fi - fi1))) * A
  
  # Mediana agrupada
  F_acum <- cumsum(frecuencias)
  mediana_index <- which(F_acum >= n / 2)[1]
  Fi_1 <- ifelse(mediana_index == 1, 0, F_acum[mediana_index - 1])
  Li_mediana <- L[mediana_index]
  f_mediana <- frecuencias[mediana_index]
  mediana <- Li_mediana + ((n / 2 - Fi_1) / f_mediana) * A
  
  # Histograma
  hist(x, 
       breaks = limites,
       xlim = c(valor_min - A, valor_max + A),
       ylim = c(0, max(frecuencias) + 5),
       xlab = nombre,
       ylab = "Absolute Frequency",
       main = paste("Graph:", nombre, "Histogram"),
       col = "darkgrey",
       border = "white")
  
  abline(v = moda, col = "blue", lwd = 3, lty = 1)
  abline(v = media, col = "green", lwd = 3, lty = 1)
  abline(v = mediana, col = "red", lwd = 3, lty = 1)
  
  legend("topright", 
         legend = c("Mode", "Mean", "Median"), 
         fill = c("blue", "green", "red"),
         cex = 0.7,
         title = "Central Tendency")
  
  # Print results
  cat("📊 Grouped statistics for", nombre, ":\n")
  cat("   - Minimum value:", valor_min, "\n")
  cat("   - Maximum value:", valor_max, "\n")
  cat("   - Range:", rango, "\n")
  cat("   - Records:", n, "\n")
  cat("   - Intervals (Sturges):", k, "\n")
  cat("   - Width:", round(A, 2), "\n")
  cat("   - Mean:", round(media, 2), "\n")
  cat("   - Mode:", round(moda, 2), "\n")
  cat("   - Median:", round(mediana, 2), "\n")
}
