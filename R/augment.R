
#' Augment Design
#'
#' @description
#' D-Augments a design. The user gives an initial design for which he would like to add points
#' and specifies the weight of the new points. Then he is prompted to choose a
#' minimum efficiency. After that, the candidate points region is calculated
#' and the user can choose the points and weights to add.
#'
#' @param init_design A dataframe with "Point" and "Weight" columns that represents the initial design to augment
#' @param alpha Combined weight of the new points
#' @param model Formula that represent the model with x as the unknown
#' @param parameters Character vector with the unknown parameters of the model to estimate
#' @param par_values Numeric vector with the initial values of the unknown parameters
#' @param design_space Numeric vector with the extremes of the space of the design
#' @param calc_optimal_design Boolean parameter, if TRUE, the optimal design is calculated and efficiencies of the initial and augmented design are given
#' @param weight_fun Optional one variable function that represents the square of the structure of variance, in case of heteroscedastic variance of the response
#'
#' @return A dataframe that represents the D-augmented design
#' @export
#'
#' @examples
#' \dontrun{
#' init_des <- data.frame("Point" = c(30, 60, 90), "Weight" = c(1/3, 1/3, 1/3))
#' augment_design(init_des, 0.25, y ~ 10^(a-b/(c+x)), c("a","b","c"),
#' c(8.07131,  1730.63, 233.426), c(1, 100), TRUE)
#' augment_design(init_des, 0.25, y ~ 10^(a-b/(c+x)), c("a","b","c"),
#' c(8.07131,  1730.63, 233.426), c(1, 100), FALSE)
#' }
augment_design <- function(init_design, alpha, model, parameters, par_values, design_space, calc_optimal_design, weight_fun = function(x) 1) {
  oldw <- getOption("warn")
  options(warn = -1)
  x_value <- NULL

  grad <- gradient(model, parameters, par_values, weight_fun)
  inf_mat_1 <- inf_mat(grad, init_design)
  sens_1 <- dsens(grad, inf_mat_1)
  min_sens <- findminval(sens_1, design_space[[1]], design_space[[2]], 10000)
  max_sens <- findmaxval(sens_1, design_space[[1]], design_space[[2]], 10000)
  if(calc_optimal_design){
    optimal_design <- opt_des("D-Optimality", model, parameters, par_values, design_space, weight_fun = weight_fun)
    inf_mat_opt <- inf_mat(grad, optimal_design$optdes)
    eff_1 <- (det(inf_mat_1) / det(inf_mat_opt))^(1 / length(parameters))*100
    message(crayon::blue(cli::symbol$info), " The efficiency of the initial design is ", round(eff_1, digits = 2), "%")
  }
  delta_range <- delta_bound(alpha, length(parameters), min_sens, max_sens)
  delta_val <- -Inf
  while(delta_val < delta_range[[1]] || delta_val > delta_range[[2]]){
    # Incluir gráfico?
    # try/catch, or check NA
    delta_val <- as.numeric(readline(prompt=paste("Choose a value for the minimum relative efficiency between", crayon::magenta(ceiling(delta_range[[1]]*100)/100), "and", crayon::magenta(floor(delta_range[[2]]*100)/100), ": \n")))
    if(delta_val < delta_range[[1]] || delta_val > delta_range[[2]]){
      cat(crayon::red(cli::symbol$cross), "The efficiency must be in the given range")
    }
  }
  # Puntos de corte y valor del corte
  cross <- sort(crosspoints(delta_val, alpha, length(parameters), sens_1, 10000, 10^(-3), design_space[[1]], design_space[[2]]))
  val_to_add <- sens_val_to_add(delta_val, alpha, length(parameters))

  # # Obtener start y par para tener las regiones
  start <- getStart(cross, design_space[[1]], design_space[[2]], val_to_add, sens_1)
  par <- getPar(cross)
  cand_points_reg <- getCross2(cross, design_space[[1]], design_space[[2]], start, par)

  x_val <- seq(design_space[[1]], design_space[[2]], length.out = 10000)
  eff <- function(x){
    return((1 - alpha) * (1 + alpha * sens_1(x)/(1 - alpha))^(1 / length(parameters)))
  }
  y_val <- purrr::map_dbl(x_val, eff)

  p <- ggplot2::ggplot() +
    ggplot2::geom_line(mapping = ggplot2::aes(x = x_val, y = y_val), color = "steelblue3") +
    ggplot2::geom_hline(yintercept =  eff(cross[1]), color = "goldenrod3") +
    ggplot2::xlim(design_space[[1]], design_space[[2]]) +
    ggplot2::labs(x = "x", y = "Efficiency") +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.border = ggplot2::element_blank(), panel.grid.major = ggplot2::element_blank(),
                                                        panel.grid.minor = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"))

  efficiency <- eff(cross[1])
  for(i in 1:(length(cand_points_reg)/2)){
    loop_input = paste("ggplot2::geom_segment(ggplot2::aes(x=",cand_points_reg[2*i-1],",xend=",cand_points_reg[2*i],",y=efficiency,yend=efficiency), size = 1.5, color = 'green3')", sep="")
    p <- p + eval(parse(text=loop_input))
  }

  values <- purrr::map_dbl(cross, eff)
  cutoffpoints <- data.frame("x_value" = cross, "eff" = values)

  p <- p + ggplot2::geom_point(data = cutoffpoints, ggplot2::aes(x = x_value, y = eff), shape = 16, size = 2, color = "firebrick3")

  p <- p + ggplot2::geom_segment(ggplot2::aes(x = design_space[[1]], xend = design_space[[1]], y = delta_range[1], yend = delta_range[2]), col = "mediumpurple2", size = 1.5)

  # Graph of region of candidates points
  plot(p)

  cutoff_text <- ""
  for(i in 1:(length(cand_points_reg)/2)){
    if(i != 1){
      cutoff_text <- paste0(cutoff_text, ", [", ceiling(cand_points_reg[2*i-1]*100)/100, "-", floor(cand_points_reg[2*i]*100)/100, "]")
    }
    else {
      cutoff_text <- paste0(cutoff_text, "[", ceiling(cand_points_reg[2*i-1]*100)/100, "-", floor(cand_points_reg[2*i]*100)/100, "]")
    }

  }

  new_points <- data.frame(Point = double(), Weight = double())

  point_to_add <- -1
  while(!is.na(point_to_add)){
    cat("The region(s) are ", crayon::green(cutoff_text))
    point_to_add <- as.numeric(readline(prompt="Choose a point to add or enter another character to finish: \n"))
    if(!is.na(point_to_add)){
      in_cand_reg <- F
      for(i in 1:(length(cand_points_reg)/2)){
        if(point_to_add >= cand_points_reg[2*i-1] & point_to_add <= cand_points_reg[2*i]){
          in_cand_reg <- T
          break
        }
      }
      if(in_cand_reg){
        weight_ok <- 0
        while(weight_ok == 0){
          weight_to_add <- as.numeric(readline(prompt = "Choose the weight of the point: \n"))
          if(is.na(weight_to_add)){
            cat(crayon::red(cli::symbol$cross), "The weight must be a positive number")
          }
          else if(weight_to_add <= 0){
            cat(crayon::red(cli::symbol$cross), "The weight must be positive")
          }
          else {
            new_points[nrow(new_points) + 1,] = c(point_to_add, weight_to_add)
            weight_ok <- 1
          }
        }
      }
      else{
        cat(crayon::red(cli::symbol$cross), "The point is outside the candidate points region \n")
      }
    }
  }
  if(nrow(new_points > 0)){
    aug_design <- add_design(init_design, new_points, alpha)
  }
  else{
    aug_design <- init_design
  }

  options(warn = oldw)
  if(calc_optimal_design){
    inf_mat_aug <- inf_mat(grad, aug_design)
    eff_2 <- (det(inf_mat_aug) / det(inf_mat_opt))^(1 / length(parameters))*100
    message(crayon::blue(cli::symbol$info), " The efficiency of the augmented design is ", round(eff_2, digits = 2), "%")
  }

  return(aug_design)
}


#' Calculate efficiency bounds
#'
#' @description
#' Given the weight of new points, number of parameters and range of the sensitivity
#' function, calculates the range of possible minimum efficiency for the D-augmented design.
#'
#' @param alpha Combined weight of the new points to add
#' @param k Number of parameters of the model
#' @param sens_min Minimum value of the sensitivity function
#' @param sens_max Maximum value of the sensitivity function
#'
#' @return A numeric vector with two components, minimum and maximum efficiency (over 1)
#'
#' @examples
#' optedr:::delta_bound(0.3, 3, 1.3, 3)
#' optedr:::delta_bound(1/4, 3, -2.50965)
delta_bound <- function(alpha, k, sens_min, sens_max = Inf){
  if(identical(sens_max, Inf)) {
    sens_max <- k
  }
  min <- -(-1 + alpha)*((-1 + alpha - sens_min*alpha)/(-1 + alpha))^(1/k)
  max <- -(-1 + alpha)*((-1 + alpha - sens_max*alpha)/(-1 + alpha))^(1/k)
  return(c(min, max))
}




#' Calculates sensitivity function value for given delta and efficiency
#'
#' @description
#' Uses the formula to calculate the sensitivity function value that delimits
#' which points can be added to the design guaranteing the chosen efficiency.
#'
#' @param deff Minimum efficiency of the resulting design
#' @param alpha Combined weight of the new points to add
#' @param k Number of parameters of the model
#'
#' @return Value of the sensitivity function over. Points with a sensitivity
#' function over that are suitable to be added.
#'
#' @examples
#' optedr:::sens_val_to_add(0.9, 1/4, 3)
sens_val_to_add <- function(deff, alpha, k){
  return((1 - alpha)/alpha*((deff/(1 - alpha))^k - 1))
}



#' Calculate crosspoints
#'
#' @description
#' Given the parameters for D-augmenting a design, this function calculates the
#' crosspoints in the sensitivity function that delimit the candidate points
#' region
#'
#' @param deff Minimum efficiency chosen by the user
#' @param alpha Combined weight of the new points
#' @param k Number of unknown parameters of the model
#' @param sens Sensitivity function of the design for the model
#' @param gridlength Number of points in the grid to find the crosspoints
#' @param tol Tolerance that establishes how close two points close to one another are considered the same
#' @param xmin Minimum of the space of the design
#' @param xmax Maximum of the space of the design
#'
#' @return A numeric vector of crosspoints that define the candidate points region
#'
crosspoints <- function(deff, alpha, k, sens, gridlength, tol, xmin, xmax){
  val <- sens_val_to_add(deff, alpha, k)

  sensfix <- function(x){
    return(sens(x) - val)
  }

  sols <- unlist(purrr::map(purrr::map(seq(xmin, xmax, length.out = gridlength), nleqslv::nleqslv, fn = sensfix), function(x) x$x))

  # Eliminar duplicados
  sols_upd <- update_sequence(sols, tol)

  # Quedarnos con puntos dentro del espacio de diseño
  sols_upd <- sols_upd[sols_upd >= xmin & sols_upd <= xmax]

  # Eliminar los que no son soluciones
  sols_upd <- sols_upd[abs(purrr::map_dbl(sols_upd, sens) - val) < tol]


  return(sort(sols_upd))
}


#' Update design given crosspoints and alpha
#'
#' @description
#' Given a set of points, a weight and the design, the function adds these points
#' to the new design with uniform weight, and combined weight alpha
#'
#' @param points Points to be added to the design
#' @param alpha Combined weight of the new points to be added to the design
#' @param design A design as a dataframe with "Point" and "Weight" columns
#'
#' @return A design as a dataframe with "Point" and "Weight" columns that is the
#' addition of the design and the new points
#'
#' @examples
#' des_1 <- data.frame("Point" = c(1, 2, 3), "Weight" = c(1/4, 1/4, 1/2))
#' optedr:::add_points(c(4, 5), 0.5, des_1)
add_points <- function(points, alpha, design){
  new_points <- data.frame("Point" = points, "Weight" = rep(alpha/length(points), times = length(points)))
  design["Weight"] <- design["Weight"]*(1-alpha)
  return(rbind(design, new_points))
}


#' Add two designs
#'
#' @param design_1 A dataframe with 'Point' and 'Weight' as column that represent the first design to add
#' @param design_2 A dataframe with 'Point' and 'Weight' as column that represent the second design to add
#' @param alpha Weight of the first design
#'
#' @return A design as a dataframe with the weighted addition of the two designs
#'
#' @examples
#' des_1 <- data.frame("Point" = c(1, 2), "Weight" = c(0.5, 0.5))
#' des_2 <- data.frame("Point" = c(3, 4), "Weight" = c(0.5, 0.5))
#' optedr:::add_design(des_1, des_2, 0.3)
add_design <- function(design_1, design_2, alpha){
  design_2[, c("Weight")] <- design_2[, c("Weight")] / sum(design_2[, c("Weight")]) * alpha
  design_1[, c("Weight")] <- design_1[, c("Weight")] * (1 - alpha)
  return(rbind(design_1, design_2))
}
