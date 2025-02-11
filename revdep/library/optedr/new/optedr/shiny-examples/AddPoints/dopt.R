# General Antoine

# Cálculo de la traza de una matriz
tr <- function(m) {
  return(sum(diag(m)))
}

# Calcular matrix de un diseño para la ecuación de Antoine
dmatrixAntoine <- function(a, b, c, design) {
  a <- a*log(10)
  b <- b*log(10)

  m11 <- 0
  m12 <- 0
  m13 <- 0
  m21 <- 0
  m22 <- 0
  m23 <- 0
  m31 <- 0
  m32 <- 0
  m33 <- 0

  for(i in seq_along(design$Weight)){
    m11 <- m11 + exp(a - b/(c + design$Point[[i]]))*exp(a - b/(c + design$Point[[i]]))*design$Weight[[i]]
    m12 <- m12 + exp(a - b/(c + design$Point[[i]]))*(-(exp(a - b/(c + design$Point[[i]]))/(c + design$Point[[i]])))*design$Weight[[i]]
    m13 <- m13 + exp(a - b/(c + design$Point[[i]]))*(b*exp(a - b/(c + design$Point[[i]])))/(c + design$Point[[i]])^2*design$Weight[[i]]
    m21 <- m12
    m22 <- m22 + (-(exp(a - b/(c + design$Point[[i]]))/(c + design$Point[[i]])))*(-(exp(a - b/(c + design$Point[[i]]))/(c + design$Point[[i]])))*design$Weight[[i]]
    m23 <- m23 + (-(exp(a - b/(c + design$Point[[i]]))/(c + design$Point[[i]])))*(b*exp(a - b/(c + design$Point[[i]])))/(c + design$Point[[i]])^2*design$Weight[[i]]
    m31 <- m13
    m32 <- m23
    m33 <- m33 + (b*exp(a - b/(c + design$Point[[i]])))/(c + design$Point[[i]])^2*(b*exp(a - b/(c + design$Point[[i]])))/(c + design$Point[[i]])^2*design$Weight[[i]]
  }

  return(matrix(c(m11, m12, m13, m21, m22, m23, m31, m32, m33), nrow = 3, ncol = 3, byrow = TRUE))
}



# design <- data.frame(points = c(1, 50.5, 100), weights = c(1/3, 1/3, 1/3))

# (design2 <- doptAntoine(8.07131, 730.63, 233.426, 1, 100))
# matInit2 <- dmatrixAntoine(a = 8.07131, b = 730.63, c = 233.426, design = design2)

# solve(matInit2)

# matInit <- dmatrixAntoine(a = 8.07131, b = 1730.63, c = 233.426, design = design)


# Función para encontrar el x donde se alcanza el máximo de una función en una rejilla dada, evaluándolo en la misma
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

# findmax(sens, 1, 100, 1000)


# Función para encontrar el mínimo de una función en una rejilla dada, evaluándolo en la misma
findminval <- function(sens, min, max, grid.length) {
  if(min <= max){
    grid <- seq(min, max, length.out = grid.length)
  }
  else {
    grid <- seq(max, min, length.out = grid.length)
  }
  minval <- min(map_dbl(grid, sens))
  return(minval)
}


# Función para encontrar el mínimo de una función en una rejilla dada, evaluándolo en la misma
findmaxval <- function(sens, min, max, grid.length) {
  if(min <= max){
    grid <- seq(min, max, length.out = grid.length)
  }
  else {
    grid <- seq(max, min, length.out = grid.length)
  }
  maxval <- max(map_dbl(grid, sens))
  return(maxval)
}


# findmax(sens, 1, 100, 1000)



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

# des <- data.frame(Point = c(1, 50.5, 100), Weight = c(1/4, 1/4, 1/2))

# updateDesign(des, 50, 0.6)

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

# des <- data.frame(Point = c(1, 50.5, 100, 5, 30, 80), Weight = c(1/4, 1/4, 1/4, 1/200, 1/100, 1/500))

# deletePoints(des, 0.005)






# --------------------------- D-Optimalidad ----------------------------------

# Función de sensibilidad para D-Optimalidad de la ecuación de Antoine
dsensAntoine <- function(a, b, c, mat) {
  a <- a*log(10)
  b <- b*log(10)
  invMat <- solve(mat)
  sensX <- function(x) {
    # return(exp(a - b/(c + x))*exp(a - b/(c + x))*desMat[[1,1]]+exp(a - b/(c + x))*(-(exp(a - b/(c + x))/(c + x)))*desMat[[1,2]]+exp(a - b/(c + x))*(b*exp(a - b/(c + x)))/(c + x)^2*desMat[[1,3]]+
    #          exp(a - b/(c + x))*(-(exp(a - b/(c + x))/(c + x)))*desMat[[2,1]]+(-(exp(a - b/(c + x))/(c + x)))*(-(exp(a - b/(c + x))/(c + x)))*desMat[[2,2]]+(-(exp(a - b/(c + x))/(c + x)))*(b*exp(a - b/(c + x)))/(c + x)^2*desMat[[2,3]]+
    #          (b*exp(a - b/(c + x)))/(c + x)^2*exp(a - b/(c + x))*desMat[[3,1]]+(b*exp(a - b/(c + x)))/(c + x)^2*(-(exp(a - b/(c + x))/(c + x)))*desMat[[3,2]]+(b*exp(a - b/(c + x)))/(c + x)^2*(b*exp(a - b/(c + x)))/(c + x)^2*desMat[[3,3]])
    return(exp(a - b/(c + x))*(invMat[[1,1]]*exp(a - b/(c + x)) + (invMat[[3,1]]*b*exp(a - b/(c + x)))/(c + x)^2 - (invMat[[2,1]]*exp(a - b/(c + x)))/(c + x)) -
             (exp(a - b/(c + x))*(invMat[[1,2]]*exp(a - b/(c + x)) + (invMat[[3,2]]*b*exp(a - b/(c + x)))/(c + x)^2 - (invMat[[2,2]]*exp(a - b/(c + x)))/(c + x)))/(c + x) +
             (b*exp(a - b/(c + x))*(invMat[[1,3]]*exp(a - b/(c + x)) + (invMat[[3,3]]*b*exp(a - b/(c + x)))/(c + x)^2 - (invMat[[2,3]]*exp(a - b/(c + x)))/(c + x)))/(c + x)^2)
  }
  return(sensX)
}


# optdes <- data.frame(Point = c(1, 50.5, 100), Weight = c(1/3, 1/3, 1/3))

# desMat <- dmatrixAntoine(8.07131, 1730.63, 233.426, optdes)

# sens <- dsensAntoine(8.07131, 1730.63, 233.426, desMat)

# sens(100)

# Cálculo Analítico del diseño D-Óptimo para la ecuación de Antoine
doptAntoine <- function(A, B, C, min, max){
  A <- log(10)*A
  B <- log(10)*B

  if(round(min - max, 3) == 0){
    opdes <- data.frame(Point = c(min), Weight = c(1))
    return(opdes)
  }

  T1 <- (-3*C^3+2*(B)^2*max-6*C^2*max-3*B*(C^2-max^2)-3*C*max^2-sqrt(3)*sqrt((B)^2*(C+max)^4))/(2*(B)^2+6*B*(C+max)+3*(C+max)^2)

  if(min <= T1)
  {
    T2 <- (-3*C^3+2*(B)^2*max-6*C^2*max-3*B*(C^2-max^2)-3*C*max^2+sqrt(3)*sqrt((B)^2*(C+max)^4))/(2*(B)^2+6*B*(C+max)+3*(C+max)^2)
    opdes <- data.frame(Point = c(T1, T2, max), Weight = c(1/3, 1/3, 1/3))
  }
  else
  {
    T2 <- (1/(2*(-B - 2*C - max - min)))*(2*C^2 - B*max - B*min - 2*max*min - sqrt((-2*C^2 + B*max + B*min + 2*max*min)^2 - 4*(-B - 2*C - max - min)*(C^2*max + C^2*min - B*max*min + 2*C*max*min)))
    opdes <- data.frame(Point = c(min, T2, max), Weight = c(1/3, 1/3, 1/3))
  }

  return(opdes)
}
# A <- 18.58487808693377
# B <- 1682.3377464942398
# C <- 233.426
#
# min <- 1
# max <- 100
# (1/(2*(-B - 2*C - max - min)))*(2*C^2 - B*max - B*min - 2*max*min - sqrt((-2*C^2 + B*max + B*min + 2*max*min)^2 - 4*(-B - 2*C - max - min)*(C^2*max + C^2*min - B*max*min + 2*C*max*min)))


# typeof(doptAntoine(A = 8.07131, B = 1730.63, C = 233.426, 99, 374))

# class(doptAntoine(A = 8.07131, B = 1730.63, C = 233.426, 99, 374))
# doptAntoine(A = 8.07131, B = 1730.63, C = 233.426, 1, 40)

# doptAntoine(A = 8.14019, B = 1810.94, C = 244.485, 1, 40)



# Cálculo de D-Eficiencia de la matriz 1 respecto a la matriz 2
deff <- function(mat1, mat2, k) {
  return((det(mat1)/det(mat2))^(1/k))
}

# dmatrixAntoine(input$a_par, input$b_par, input$c_par, ing_df$design)
#
# 5	0.375
# 35	0.3125
# 70	0.3125
#
# deff(dmatrixAntoine(input$a_par, input$b_par, input$c_par, ing_df$design),
#      dmatrixAntoine(input$a_par, input$b_par, input$c_par, ing_df$opt_design), 3)

# Valor del criterio de D-Optimalidad para la matriz
dcrit <- function(mat, k) {
  return((1/det(mat))^(1/k))
}

# dcrit(matInit, 3)


# Función para actualizar los pesos de un diseño para el algoritmo WF
updateWeights <- function(design, sens, k, delta) {
  weights <- design$Weight*(map_dbl(design$Point, sens)/k)^delta
  # print(length(design$Weight))
  # print(length((map_dbl(des$Point, sens)/k)^delta))
  return(weights/sum(weights))
}

# des <- data.frame(Point = c(1, 50.5, 100), Weight = c(1/4, 1/3, 1/2))

# desMat <- dmatrixAntoine(8.07131, 1730.63, 233.426, des)

# sens <- dsensAntoine(8.07131, 1730.63, 233.426, desMat)

# (des$Weight <- updateWeights(des, sens, 3, 1/2))




# ______________________________________________________

# López-Ríos

# Calculate boundaries for given delta
delta_bound <- function(delta, k, sens_min, sens_max = Inf){
  if(identical(sens_max, Inf)) {
    sens_max <- k
  }
  min <- -(-1 + delta)*((-1 + delta - sens_min*delta)/(-1 + delta))^(1/k)
  max <- -(-1 + delta)*((-1 + delta - sens_max*delta)/(-1 + delta))^(1/k)
  return(c(min, max))
}

# delta_bound(1/4, 3, -2.50965)


# Calculate delta for given efficiency


# NO SE PUEDE "FÁCIL"
# Calculate boundary for given efficiency
# efficiency_bound <- function(deff, k, sens_min){
#   min <- -(-1 + delta)*((-1 + delta - sens_min*delta)/(-1 + delta))^(1/k)
#   max <- -(-1 + delta)*((-1 + delta - k*delta)/(-1 + delta))^(1/k)
#   return(c(min, max))
# }



# Calculate sens value for given delta and efficiency
sens_val_to_add <- function(deff, delta, k){
  return((1 - delta)/delta*((deff/(1 - delta))^k - 1))
}

# sens_val_to_add(0.9, 1/4, 3)


# Elimina puntos duplicados
update_sequence <- function(points, tol){
  i <- 1
  imax <- (length(points)-1)
  while(i < imax) {
    absdiff <- c(rep(T, i), abs(points[-seq(1, i)] - points[i]) > tol)
    points <- points[absdiff]
    i <- i+1
    imax <- (length(points)-1)
  }
  return(points)
}




# Calculate crosspoints
crosspoints <- function(deff, delta, k, sens, gridlength, tol, xmin, xmax){
  val <- sens_val_to_add(deff, delta, k)

  sensfix <- function(x){
    return(sens(x) - val)
  }

  sols <- seq(xmin, xmax, length.out = gridlength) %>%
    map(nleqslv, fn = sensfix) %>%
    map(function(x) x$x) %>%
    unlist

  # Eliminar duplicados
  sols_upd <- update_sequence(sols, tol)

  # Quedarnos con puntos dentro del espacio de diseño
  sols_upd <- sols_upd[sols_upd >= xmin & sols_upd <= xmax]

  # Eliminar los que no son soluciones
  sols_upd <- sols_upd[abs(map_dbl(sols_upd, sens) - val) < tol]


  return(sort(sols_upd))
}

# optdes <- doptAntoine(A = 8.07131, B = 1730.63, C = 233.426, 1, 100)

# desMat <- dmatrixAntoine(8.07131, 1730.63, 233.426, optdes)

# sens <- dsensAntoine(8.07131, 1730.63, 233.426, desMat)

# sols_new <- crosspoints(0.9, 1/4, 3, sens, 1000, 10^(-3), 1, 100)



# Update design given crosspoints and delta
add_points <- function(points, delta, design){
  new_points <- data.frame("Point" = points, "Weight" = rep(delta/length(points), times = length(points)))
  design["Weight"] <- design["Weight"]*(1-delta)
  return(rbind(design, new_points))
}

# des <- data.frame(Point = c(1, 50.5, 100), Weight = c(1/3, 1/3, 1/3))
# add_points(c(20, 70, 90), 1/4, des)


# Uniform design
# uniform_design <- function(n, xmin, xmax){
#   unif_des <- data.frame("Point" = seq(xmin,xmax,length.out = n), "Weight" = rep(1/n, times = n))
#   return(unif_des)
# }

# Arithmetic design
# arithmetic_design <- function(n, xmin, xmax, dopt_mat, model, a = 0, b = 0, c = 0){
#   if(identical(model, "Cuadratic")){
#     arithdes_opt <- function(r){
#       arith_des <- data.frame("Point" = seq(r,xmax,length.out = n), "Weight" = rep(1/n, times = n))
#       arith_mat <- dmatrixCuad(arith_des)
#       return(deff(dopt_mat,arith_mat, 3))
#     }
#   } else if(identical(model, "Antoine Equation")){
#     arithdes_opt <- function(r){
#       arith_des <- data.frame("Point" = seq(r,xmax,length.out = n), "Weight" = rep(1/n, times = n))
#       arith_mat <- dmatrixAntoine(a, b, c, arith_des)
#       return(deff(dopt_mat,arith_mat, 3))
#     }
#   } else if(identical(model, "Michaelis-Menten")){
#     arithdes_opt <- function(r){
#       arith_des <- data.frame("Point" = seq(r,xmax,length.out = n), "Weight" = rep(1/n, times = n))
#       arith_mat <- dmatrixMM(a, b, arith_des)
#       return(deff(dopt_mat,arith_mat, 2))
#     }
#   }

  # grid <- seq(0, 1, length.out = 101)[1:100]
  #
  # map_dbl(grid, arithdes_opt)

  #if(criterion %in% c("D-Optimality", "Ds-Optimality")){
    # r_val <- round(nloptr::direct(fn = arithdes_opt,lower = xmin, upper = xmax-(xmax-xmin)/10, control=list(xtol_rel=1e-8, maxeval=1000))$par, 3)
  # }
  # else{
  #   seq(xmin, xmax, length.out = 1000)[which.max(map(seq(xmin, xmax-(xmax-xmin)/5, length.out = 1000), arithdes_opt))]
  # }

#   return(data.frame("Point" = seq(r_val,xmax,length.out = n), "Weight" = rep(1/n, times = n)))
# }

# opt_des <- doptCuad(1, 100)
# dopt_mat <- dmatrixCuad(opt_des)
#
# arithmetic_design(5, 1, 100, dopt_mat, "Cuadratic")

# opt_des <- doptMM(227.27, 43.73, 0, 10)
# dopt_mat <- dmatrixMM(227.27, 43.73, opt_des)
#
# arithmetic_design(5, 0, 10, dopt_mat, "Michaelis-Menten", 227.27, 43.73)



# Geometric design
# geometric_design <- function(n, xmin, xmax, dopt_mat, model, a = 0, b = 0, c = 0){
#   if(identical(model, "Cuadratic")){
#     geodes_opt <- function(r){
#       geom_des <- data.frame("Point" = (xmax-xmin)*r^(0:(n-1))+xmin, "Weight" = rep(1/n, times = n))
#       geom_mat <- dmatrixCuad(geom_des)
#       return(deff(dopt_mat,geom_mat, 3))
#     }
#   } else if(identical(model, "Antoine Equation")){
#     geodes_opt <- function(r){
#       geom_des <- data.frame("Point" = (xmax-xmin)*r^(0:(n-1))+xmin, "Weight" = rep(1/n, times = n))
#       geom_mat <- dmatrixAntoine(a, b, c, geom_des)
#       return(deff(dopt_mat,geom_mat, 3))
#     }
#   } else if(identical(model, "Michaelis-Menten")){
#     geodes_opt <- function(r){
#       geom_des <- data.frame("Point" = (xmax-xmin)*r^(0:(n-1))+xmin, "Weight" = rep(1/n, times = n))
#       geom_mat <- dmatrixMM(a, b, geom_des)
#       return(deff(dopt_mat,geom_mat, 2))
#     }
#   }
#
#   r_val <- round(nloptr::direct(fn = geodes_opt,lower = 0, upper = 1, control=list(xtol_rel=1e-8, maxeval=1000))$par, 3)
#
#   return(data.frame("Point" = (xmax-xmin)*r_val^(0:(n-1))+xmin, "Weight" = rep(1/n, times = n)) %>% arrange(Point))
# }

# opt_des <- doptCuad(1, 100)
# dopt_mat <- dmatrixCuad(opt_des)
#
#
# geometric_design(5, 1, 100, dopt_mat, "Cuadratic")




# -----------------------------------------------------------------------------------------------

# MODELO CUADRÁTICO


# Calcular matrix de un diseño
dmatrixCuad <- function(design) {

  m11 <- 0
  m12 <- 0
  m13 <- 0
  m21 <- 0
  m22 <- 0
  m23 <- 0
  m31 <- 0
  m32 <- 0
  m33 <- 0

  for(i in seq_along(design$Weight)){
    m11 <- m11 + design$Weight[[i]]
    m12 <- m12 + design$Point[[i]]*design$Weight[[i]]
    m13 <- m13 + design$Point[[i]]^2*design$Weight[[i]]
    m21 <- m12
    m22 <- m13
    m23 <- m23 + design$Point[[i]]^3*design$Weight[[i]]
    m31 <- m13
    m32 <- m23
    m33 <- m33 +  design$Point[[i]]^4*design$Weight[[i]]
  }

  return(matrix(c(m11, m12, m13, m21, m22, m23, m31, m32, m33), nrow = 3, ncol = 3, byrow = TRUE))
}




# --------------------------- D-Optimalidad ----------------------------------

# Función de sensibilidad para D-Optimalidad
dsensCuad <- function(mat) {
  invMat <- solve(mat)
  sensX <- function(x) {
    return(invMat[[1,1]]+x*(invMat[[2,1]]+invMat[[1,2]])+x^2*(invMat[[1,3]]+invMat[[2,2]]+invMat[[3,1]])+x^3*(invMat[[2,3]]+invMat[[3,2]])+x^4*invMat[[3,3]])
  }
  return(sensX)
}



# Cálculo Analítico del diseño D-Óptimo
doptCuad <- function(min, max){
  opdes <- data.frame(Point = c(min, (min+max)/2, max), Weight = c(1/3, 1/3, 1/3))

  return(opdes)
}

# doptCuad(0,1)


# -----------------------------------------------------------------------------------------------

# Michaelis-Menten


# Calcular matrix de un diseño
dmatrixMM <- function(K, V, design) {

  m11 <- 0
  m12 <- 0
  m21 <- 0
  m22 <- 0


  for(i in seq_along(design$Weight)){
    m11 <- m11 + (design$Point[[i]]^2/(K + design$Point[[i]])^2)*design$Weight[[i]]
    m12 <- m12 + (-((V * design$Point[[i]]^2)/(K + design$Point[[i]])^3) )*design$Weight[[i]]
    m21 <- m12
    m22 <- m22 + ((V^2 * design$Point[[i]]^2)/(K + design$Point[[i]])^4)*design$Weight[[i]]
  }

  return(matrix(c(m11, m12, m21, m22), nrow = 2, ncol = 2, byrow = TRUE))
}

# matMM <- dmatrixMM(227.27, 43.73, optdes)




# --------------------------- D-Optimalidad ----------------------------------


# Función de sensibilidad para D-Optimalidad
dsensMM <- function(K, V, mat) {
  invMat <- solve(mat)
  sensX <- function(x) {
    return((x *(-((invMat[[2, 1]] * V * x) / (K + x) ^ 2 ) + (invMat[[1, 1]] * x) / (K + x))) / (K + x) - (V * x *(-((invMat[[2, 2]] * V * x) / (K + x) ^ 2) + (invMat[[1, 2]] * x) / (K + x))) / (K + x) ^ 2)
  }
  return(sensX)
}

# sensM <- dsensMM(227.27, 43.73, matMM)
#
# sensM(10)

# Cálculo Analítico del diseño D-Óptimo
doptMM <- function(K, V, min, max){
  b <- max/K
  s <- b/(2+b)
  opdes <- data.frame(Point = c(s*K, max), Weight = c(1/2, 1/2))

  return(opdes)
}

# optdes <- doptMM(227.27, 43.73, 0, 10)


# -----------------------------------------------------------------------------------------------

# MODELO CUADRÁTICO HETEROCEDÁSTICO


# Calcular matrix de un diseño
dmatrixCuadHet <- function(order, design) {

  m11 <- 0
  m12 <- 0
  m13 <- 0
  m21 <- 0
  m22 <- 0
  m23 <- 0
  m31 <- 0
  m32 <- 0
  m33 <- 0

  for(i in seq_along(design$Weight)){
    m11 <- m11 + design$Weight[[i]] * exp(-design$Point[[i]]) * design$Point[[i]]^(1+order)
    m12 <- m12 + design$Point[[i]]*design$Weight[[i]] * exp(-design$Point[[i]]) * design$Point[[i]]^(1+order)
    m13 <- m13 + design$Point[[i]]^2*design$Weight[[i]] * exp(-design$Point[[i]]) * design$Point[[i]]^(1+order)
    m21 <- m12
    m22 <- m13
    m23 <- m23 + design$Point[[i]]^3*design$Weight[[i]] * exp(-design$Point[[i]]) * design$Point[[i]]^(1+order)
    m31 <- m13
    m32 <- m23
    m33 <- m33 +  design$Point[[i]]^4*design$Weight[[i]] * exp(-design$Point[[i]]) * design$Point[[i]]^(1+order)
  }

  return(matrix(c(m11, m12, m13, m21, m22, m23, m31, m32, m33), nrow = 3, ncol = 3, byrow = TRUE))
}




# --------------------------- D-Optimalidad ----------------------------------

# Función de sensibilidad para D-Optimalidad
dsensCuadHet <- function(order, mat) {
  invMat <- solve(mat)
  sensX <- function(x) {
    return(exp(-x) * x^(1+order)*(invMat[[1,1]]+x*(invMat[[2,1]]+invMat[[1,2]])+x^2*(invMat[[1,3]]+invMat[[2,2]]+invMat[[3,1]])+x^3*(invMat[[2,3]]+invMat[[3,2]])+x^4*invMat[[3,3]]))
  }
  return(sensX)
}



# Cálculo Analítico del diseño D-Óptimo
doptCuadHet <- function(order){
  opdes <- data.frame(Point = solve(glaguerre.polynomials(3, order, FALSE)[[4]]), Weight = c(1/3, 1/3, 1/3))

  return(opdes)
}

# opt_des <- doptCuadHet(0.5)
# M <- dmatrixCuadHet(0.5, opt_des)
# sens <- dsensCuadHet(0.5, M)
# sens(7.0328990)
