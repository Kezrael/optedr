# Auxiliar function for algorithms --------------------------------------
# Funciones auxiliares para el algoritmo

# Busca el máximo de una función a través de una rejilla
findmax <- function(sens, min, max, grid.length) {
  if(min <= max){
    grid <- seq(min, max, length.out = grid.length)
  }
  else {
    grid <- seq(max, min, length.out = grid.length)
  }
  xmax <- grid[which.max(map(grid, sens))]
  return(xmax)
}



# Función para actualizar los pesos de un diseño para el algoritmo WF D opt
updateWeights <- function(design, sens, k, delta) {
  weights <- design$Weight*(map_dbl(design$Point, sens)/k)^delta
  return(weights/sum(weights))
}

# Función para actualizar los pesos de un diseño para el algoritmo WF Ds opt
updateWeightsDS <- function(design, sens, s, delta) {
  weights <- design$Weight*(map_dbl(design$Point, sens)/s)^delta
  # print(length(design$Weight))
  # print(length((map_dbl(des$Point, sens)/k)^delta))
  return(weights)
}

# Función para actualizar los pesos de un diseño para el algoritmo WF I opt
updateWeightsI <- function(design, sens, crit, delta) {
  weights <- design$Weight*(map_dbl(design$Point, sens)/crit)^delta
  # print(length(design$Weight))
  # print(length((map_dbl(des$Point, sens)/k)^delta))
  return(weights)
}


# Función para actualizar un diseño añadiendo un nuevo punto (o peso al mismo si está cerca de otro ya en el diseño)
updateDesign <- function(design, xmax, delta){
  absdiff <- abs(design$Point - xmax) < delta
  if (any(absdiff))
  {
    pos <- min(which(absdiff == TRUE))
    design$Point[[pos]] <- (design$Point[[pos]] + xmax)/2
  }
  else
  {
    design[nrow(design) + 1,] <- c(xmax, 1/(nrow(design) + 1))
  }
  design$Weight <- rep(1/nrow(design), nrow(design))
  return(design)
}

# Actualizar un diseño sin añadir puntos
updateDesignTotal <- function(design, delta){
  updated <- FALSE
  finished <- FALSE
  while(!finished) {
    for(i in 1:(length(design$Point)-1)) {
      absdiff <- abs(design$Point[-seq(1, i)] - design$Point[i]) < delta
      if (any(absdiff)){
        updated <- TRUE
        design <- updateDesign(design[-i,], design$Point[i], delta)
        break
      }
    }
    finished <- !updated
    updated <- FALSE
  }
  return(design)
}

# Función para eliminar los puntos con menos de un determinado peso, delta
deletePoints <- function(design, delta) {
  updatedDesign <- design[design$Weight > delta, ]
  updatedDesign$Weight <- updatedDesign$Weight/sum(updatedDesign$Weight)
  return(updatedDesign)
}

# General auxiliar functions --------------------------
# Cálculo de la traza de una matriz
tr <- function(m) {
  return(sum(diag(m)))
}

# Dibuja la función de sensibilidad (y el diseño?)
plot_sens <- function(min, max, sens_function, criterion_value){
  grid <- seq(212, 422, length.out = 10000)
  sens_grid <- map_dbl(grid, sens_function)

  sensibility <- ggplot(data = data.frame(x = grid, y = sens_grid), mapping = aes(x = x)) +
    theme_bw() +
    geom_line(mapping = aes(x = x, y = y), color = "steelblue3") +
    stat_function(fun = function(x) criterion_value, col = "goldenrod3") +
    xlim(min, max) +
    labs(x = "X", y = "Y")
}

