norm <- function(data, var, name) {
  
  if (!var %in% names(data)) {
    stop(paste("Column", var, "not found in data"))
  }
  
  y <- data[[var]]
  
  # Density plot with theoretical normal
  plot(density(y, na.rm=TRUE),
       col="red",
       lwd=3,
       main=paste("Graph:", name, "Norm"),
       xlab=name,
       ylab="Density",
       ylim=c(0, 0.25))
  
  x = seq(min(y, na.rm = TRUE), max(y, na.rm = TRUE), 0.1)
  curve(dnorm(x, mean(y,na.rm=TRUE), sd(y,na.rm=TRUE)), 
        lwd = 2, 
        col = "blue", 
        add = T)
  
  legend("topleft", 
         c("Observed Curve", "Theoretical Curve"),
         lty = 1, 
         lwd = 3, 
         col = c("red", "blue"), 
         bty = "n",
         cex = 0.8)
  
  # Fit and describe distribution
  fit <- fitdist(y, "norm")
  print(plot(fit))
  
  print(descdist(y, boot = 100))
  
  gof <- gofstat(fit)
  print(gof$kstest)
  
  return(fit)
}
