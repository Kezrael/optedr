
#' Augment Design
#'
#' @description
#' Augments a design. The user gives an initial design for which he would like to add points
#' and specifies the weight of the new points. Then he is prompted to choose a
#' minimum efficiency. After that, the candidate points region is calculated
#' and the user can choose the points and weights to add.
#'
#' @param criterion character variable with the chosen optimality criterion. Can be one of the following:
#'   * 'D-Optimality'
#'   * 'Ds-Optimality'
#'   * 'A-Optimality'
#'   * 'I-Optimality'
#' @param init_design dataframe with "Point" and "Weight" columns that represents the initial design to augment
#' @param alpha combined weight of the new points
#' @param model formula that represents the model with x as the independent variable
#' @param parameters character vector with the unknown parameters of the model to estimate
#' @param par_values numeric vector with the initial values of the unknown parameters
#' @param design_space numeric vector with the limits of the space of the design
#' @param calc_optimal_design boolean parameter, if TRUE, the optimal design is calculated and efficiencies of the initial and augmented design are given
#' @param weight_fun optional one variable function that represents the square of the structure of variance, in case of heteroscedastic variance of the response
#' @param par_int optional numeric vector with the index of the \code{parameters} of interest for Ds-optimality.
#' @param matB optional matrix of dimensions k x k, integral of the information matrix of the model over the
#'   interest region for I-optimality.
#' @param distribution character specifying the probability distribution of the response. Can be one of the following:
#'   * 'Homoscedasticity'
#'   * 'Gamma', which can be used for exponential or normal heteroscedastic with constant relative error
#'   * 'Poisson'
#'   * 'Logistic'
#'   * 'Log-Normal' (work in progress)
#'
#'
#' @return A dataframe that represents the D-augmented design
#' @export
#'
#' @examples
#' init_des <- data.frame("Point" = c(30, 60, 90), "Weight" = c(1/3, 1/3, 1/3))
#' augment_design("D-Optimality", init_des, 0.25, y ~ 10^(a-b/(c+x)), c("a","b","c"),
#'   c(8.07131,  1730.63, 233.426), c(1, 100), TRUE)
#' augment_design("D-Optimality", init_des, 0.25, y ~ 10^(a-b/(c+x)), c("a","b","c"),
#'   c(8.07131,  1730.63, 233.426), c(1, 100), FALSE)
augment_design <- function(criterion, init_design, alpha, model, parameters, par_values, design_space, calc_optimal_design, par_int = NA, matB = NA, distribution = NA, weight_fun = function(x) 1) {
  # oldw <- getOption("warn")
  # options(warn = -1)
  if(interactive()){
    if(!is.na(distribution)){
      weight_fun <- weight_function(model, parameters, par_values, distribution = distribution)

        # dplyr::case_when(criterion == "D-Optimality" ~ daugment_design(init_design, alpha, model, parameters, par_values, design_space, calc_optimal_design, weight_fun),
        #                                    criterion == "A-Optimality" ~ laugment_design(init_design, alpha, model, parameters, par_values, design_space, calc_optimal_design, matB = diag(length(parameters)), weight_fun),
        #                                    criterion == "I-Optimality" ~ laugment_design(init_design, alpha, model, parameters, par_values, design_space, calc_optimal_design, matB, weight_fun),
        #                                    criterion == "Ds-Optimality" ~ dsaugment_design(init_design, alpha, model, parameters, par_values, design_space, calc_optimal_design, par_int, weight_fun),
        #                                    TRUE ~ NA)
    }
    # else {
    #
    #   augmented_design <- dplyr::case_when(criterion == "D-Optimality" ~ daugment_design(init_design, alpha, model, parameters, par_values, design_space, calc_optimal_design, weight_f),
    #                                        criterion == "A-Optimality" ~ laugment_design(init_design, alpha, model, parameters, par_values, design_space, calc_optimal_design, matB = diag(length(parameters)), weight_f),
    #                                        criterion == "I-Optimality" ~ laugment_design(init_design, alpha, model, parameters, par_values, design_space, calc_optimal_design, matB, weight_f),
    #                                        criterion == "Ds-Optimality" ~ dsaugment_design(init_design, alpha, model, parameters, par_values, design_space, calc_optimal_design, par_int, weight_f),
    #                                        TRUE ~ NA)
    # }
    if(criterion == "D-Optimality"){
      augmented_design <- daugment_design(init_design, alpha, model, parameters, par_values, design_space, calc_optimal_design, weight_fun)
    } else if(criterion == "A-Optimality"){
      augmented_design <- laugment_design(init_design, alpha, model, parameters, par_values, design_space, calc_optimal_design, matB = diag(length(parameters)), weight_fun)
    } else if(criterion == "I-Optimality"){
      if(is.na(matB)){
        grad <- gradient(model, parameters, par_values, weight_fun)
        matB <- integrate_reg_int(grad, length(parameters), design_space)
      }
      augmented_design <- laugment_design(init_design, alpha, model, parameters, par_values, design_space, calc_optimal_design, matB, weight_fun)
    } else if(criterion == "Ds-Optimality"){
      augmented_design <- dsaugment_design(init_design, alpha, model, parameters, par_values, par_int, design_space, calc_optimal_design, weight_fun)
    } else return(NA)
    return(augmented_design)
  }
  return(NA)
}



#' Get Augment Regions
#'
#' @description
#' Given a model and criterion, calculates the candidate points region. The user gives an initial design for which
#' he would like to add points and specifies the weight of the new points. Then he is prompted
#' to choose a minimum efficiency. After that, the candidate points region is calculated.
#'
#' @param criterion character with the chosen optimality criterion. Can be one of the following:
#'   * 'D-Optimality'
#'   * 'Ds-Optimality'
#'   * 'A-Optimality'
#'   * 'I-Optimality'
#' @param init_design dataframe with "Point" and "Weight" columns that represents the initial design to augment
#' @param alpha combined weight of the new points
#' @param model formula that represent the model with x as the independent variable
#' @param parameters character vector with the unknown parameters of the model to estimate
#' @param par_values numeric vector with the initial values of the unknown parameters
#' @param design_space numeric vector with the limits of the space of the design
#' @param calc_optimal_design boolean parameter, if TRUE, the optimal design is calculated and efficiencies of the initial and augmented design are given
#' @param weight_fun optional one variable function that represents the square of the structure of variance, in case of heteroscedastic variance of the response
#' @param par_int optional numeric vector with the index of the \code{parameters} of interest for Ds-optimality.
#' @param matB optional matrix of dimensions k x k, integral of the information matrix of the model over the
#'   interest region for I-optimality.
#' @param distribution character specifying the probability distribution of the response. Can be one of the following:
#'   * 'Homoscedasticity'
#'   * 'Gamma', which can be used for exponential or normal heteroscedastic with constant relative error
#'   * 'Poisson'
#'   * 'Logistic'
#'   * 'Log-Normal' (work in progress)
#'
#'
#' @return A vector of the points limiting the candidate points region
#' @export
#'
#' @examples
#' init_des <- data.frame("Point" = c(30, 60, 90), "Weight" = c(1/3, 1/3, 1/3))
#' get_augment_region("D-Optimality", init_des, 0.25, y ~ 10^(a-b/(c+x)), c("a","b","c"),
#'   c(8.07131,  1730.63, 233.426), c(1, 100), TRUE)
#' get_augment_region("D-Optimality", init_des, 0.25, y ~ 10^(a-b/(c+x)), c("a","b","c"),
#'   c(8.07131,  1730.63, 233.426), c(1, 100), FALSE)
get_augment_region <- function(criterion, init_design, alpha, model, parameters, par_values, design_space, calc_optimal_design, par_int = NA, matB = NA, distribution = NA, weight_fun = function(x) 1) {
  if(interactive()){
    if(!is.na(distribution)){
      weight_fun <- weight_function(model, parameters, par_values, distribution = distribution)
      # augment_region <- dplyr::case_when(criterion == "D-Optimality" ~ get_daugment_region(init_design, alpha, model, parameters, par_values, design_space, calc_optimal_design, weight_fun),
      #                                      criterion == "A-Optimality" ~ get_laugment_region(init_design, alpha, model, parameters, par_values, design_space, calc_optimal_design, matB = diag(length(parameters)), weight_fun),
      #                                      criterion == "I-Optimality" ~ get_laugment_region(init_design, alpha, model, parameters, par_values, design_space, calc_optimal_design, matB, weight_fun),
      #                                      criterion == "Ds-Optimality" ~ get_dsaugment_region(init_design, alpha, model, parameters, par_values, design_space, calc_optimal_design, par_int, weight_fun))
    }
    # else {
    #   weight_f <- weight_function(model, char_vars, values, distribution = distribution)
    #   augment_region <- dplyr::case_when(criterion == "D-Optimality" ~ get_daugment_region(init_design, alpha, model, parameters, par_values, design_space, calc_optimal_design, weight_f),
    #                                        criterion == "A-Optimality" ~ get_laugment_region(init_design, alpha, model, parameters, par_values, design_space, calc_optimal_design, matB = diag(length(parameters)), weight_f),
    #                                        criterion == "I-Optimality" ~ get_laugment_region(init_design, alpha, model, parameters, par_values, design_space, calc_optimal_design, matB, weight_f),
    #                                        criterion == "Ds-Optimality" ~ get_dsaugment_region(init_design, alpha, model, parameters, par_values, design_space, calc_optimal_design, par_int, weight_f))
    # }
    if(criterion == "D-Optimality"){
      augment_region <- get_daugment_region(init_design, alpha, model, parameters, par_values, design_space, calc_optimal_design, weight_fun)
    } else if(criterion == "A-Optimality"){
      augment_region <- get_laugment_region(init_design, alpha, model, parameters, par_values, design_space, calc_optimal_design, matB = diag(length(parameters)), weight_fun)
    } else if(criterion == "I-Optimality"){
      if(is.na(matB)){
        grad <- gradient(model, parameters, par_values, weight_fun)
        matB <- integrate_reg_int(grad, length(parameters), design_space)
      }
      augment_region <- get_laugment_region(init_design, alpha, model, parameters, par_values, design_space, calc_optimal_design, matB, weight_fun)
    } else if(criterion == "Ds-Optimality"){
      augment_region <- get_dsaugment_region(init_design, alpha, model, parameters, par_values, par_int, design_space, calc_optimal_design, weight_fun)
    } else return(NA)
    return(augment_region)
  }
  return(NA)
}






#' D-Augment Design
#'
#' @description
#' D-Augments a design. The user gives an initial design for which he would like to add points
#' and specifies the weight of the new points. Then he is prompted to choose a
#' minimum efficiency. After that, the candidate points region is calculated
#' and the user can choose the points and weights to add.
#'
#' @inherit augment_design return params examples
#'
#' @family augment designs
#'
daugment_design <- function(init_design, alpha, model, parameters, par_values, design_space, calc_optimal_design, weight_fun = function(x) 1) {
  x_value <- NULL
  grad <- gradient(model, parameters, par_values, weight_fun)
  inf_mat_1 <- inf_mat(grad, init_design)
  sens_1 <- dsens(grad, inf_mat_1)
  min_sens <- findminval(sens_1, design_space[[1]], design_space[[2]], 10000)
  max_sens <- findmaxval(sens_1, design_space[[1]], design_space[[2]], 10000)
  min_eff <- -(-1 + alpha)*((-1 + alpha - min_sens*alpha)/(-1 + alpha))^(1/length(parameters))
  max_eff <- -(-1 + alpha)*((-1 + alpha - max_sens*alpha)/(-1 + alpha))^(1/length(parameters))
  if(calc_optimal_design){
    optimal_design <- opt_des("D-Optimality", model, parameters, par_values, design_space, weight_fun = weight_fun)
    inf_mat_opt <- inf_mat(grad, optimal_design$optdes)
    eff_1 <- (det(inf_mat_1) / det(inf_mat_opt))^(1 / length(parameters))*100
    message(crayon::blue(cli::symbol$info), " The efficiency of the initial design is ", round(eff_1, digits = 2), "%")
  }
  delta_range <- c(min_eff, max_eff)
  delta_val <- -Inf
  eval <- 0
  while(delta_val < delta_range[[1]] || delta_val > delta_range[[2]]){
    # Incluir gráfico?
    # try/catch, or check NA
    delta_val <- suppressWarnings(as.numeric(readline(prompt=paste("Choose a value for the minimum relative efficiency between", crayon::magenta(ceiling(delta_range[[1]]*100)/100), "and", crayon::magenta(floor(delta_range[[2]]*100)/100), ": \n"))))
    if(is.na(delta_val)){
      cat(crayon::red(cli::symbol$cross), "The efficiency must be a number")
      delta_val <- -Inf
    } else if(delta_val < delta_range[[1]] || delta_val > delta_range[[2]]){
      cat(crayon::red(cli::symbol$cross), "The efficiency must be in the given range")
    }
    eval <- eval + 1
    if(eval > 1000){
      return(NULL)
    }
  }
  # Puntos de corte y valor del corte
  val_to_add <- ((1 - alpha)/alpha*((delta_val/(1 - alpha))^length(parameters) - 1))
  cross <- sort(crosspoints(val_to_add, sens_1, 10000, 10^(-3), design_space[[1]], design_space[[2]]))


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
    point_to_add <- suppressWarnings(as.numeric(readline(prompt="Choose a point to add or enter another character to finish: \n")))
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
          weight_to_add <- suppressWarnings(as.numeric(readline(prompt = "Choose the weight of the point: \n")))
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

  # options(warn = oldw)
  if(calc_optimal_design){
    inf_mat_aug <- inf_mat(grad, aug_design)
    eff_2 <- (det(inf_mat_aug) / det(inf_mat_opt))^(1 / length(parameters))*100
    message(crayon::blue(cli::symbol$info), " The efficiency of the augmented design is ", round(eff_2, digits = 2), "%")
  }

  return(aug_design)
}



#' L-Augment Design
#'
#' @description
#' L-Augments a design. The user gives an initial design for which he would like to add points
#' and specifies the weight of the new points. Then he is prompted to choose a
#' minimum efficiency. After that, the candidate points region is calculated
#' and the user can choose the points and weights to add.
#'
#' @inherit augment_design params
#'
#' @return A dataframe that represents the L-augmented design
#'
#' @examples
#' init_des <- data.frame("Point" = c(30, 60, 90), "Weight" = c(1/3, 1/3, 1/3))
#' augment_design("I-Optimality", init_des, 0.25, y ~ 10^(a-b/(c+x)), c("a","b","c"),
#'   c(8.07131,  1730.63, 233.426), c(1, 100), TRUE)
#' augment_design("I-Optimality", init_des, 0.25, y ~ 10^(a-b/(c+x)), c("a","b","c"),
#'   c(8.07131,  1730.63, 233.426), c(1, 100), FALSE)
#'
#' @family augment designs
#'
laugment_design <- function(init_design, alpha, model, parameters, par_values, design_space, calc_optimal_design, matB, weight_fun = function(x) 1) {
  x_value <- NULL
  grad <- gradient(model, parameters, par_values, weight_fun)
  inf_mat_1 <- inf_mat(grad, init_design)
  dsens_1 <- dsens(grad, inf_mat_1)
  isens_1 <- isens(grad, inf_mat_1, matB)
  crit_1 <- icrit(inf_mat_1, matB)
  eff_fun <- function(x) (1-alpha)*(crit_1/(crit_1 - alpha*isens_1(x)/ (1-alpha+alpha*dsens_1(x))))
  min_eff <- findminval(eff_fun, design_space[[1]], design_space[[2]], 10000)
  max_eff <- findmaxval(eff_fun, design_space[[1]], design_space[[2]], 10000)
  if(calc_optimal_design){
    optimal_design <- opt_des("I-Optimality", model, parameters, par_values, design_space, matB = matB, weight_fun = weight_fun)
    inf_mat_opt <- inf_mat(grad, optimal_design$optdes)
    eff_1 <- (tr(matB %*% solve(inf_mat_opt)) / tr(matB %*% solve(inf_mat_1))) * 100
    message(crayon::blue(cli::symbol$info), " The efficiency of the initial design is ", round(eff_1, digits = 2), "%")
  }
  delta_range <- c(min_eff, max_eff)
  delta_val <- -Inf
  eval <- 0
  while(delta_val < min_eff || delta_val > max_eff){
    # Incluir gráfico?
    # try/catch, or check NA
    delta_val <- suppressWarnings(as.numeric(readline(prompt=paste("Choose a value for the minimum relative efficiency between", crayon::magenta(ceiling(delta_range[[1]]*100)/100), "and", crayon::magenta(floor(delta_range[[2]]*100)/100), ": \n"))))
    if(is.na(delta_val)){
      cat(crayon::red(cli::symbol$cross), "The efficiency must be a number")
      delta_val <- -Inf
    } else if(delta_val < delta_range[[1]] || delta_val > delta_range[[2]]){
      cat(crayon::red(cli::symbol$cross), "The efficiency must be in the given range")
    }
    eval <- eval + 1
    if(eval > 1000){
      return(NULL)
    }
  }
  # Puntos de corte y valor del corte
  cross <- sort(crosspoints(delta_val, eff_fun, 10000, 10^(-3), design_space[[1]], design_space[[2]]))

  # # Obtener start y par para tener las regiones
  start <- getStart(cross, design_space[[1]], design_space[[2]], delta_val, eff_fun)
  par <- getPar(cross)
  cand_points_reg <- getCross2(cross, design_space[[1]], design_space[[2]], start, par)

  x_val <- seq(design_space[[1]], design_space[[2]], length.out = 10000)
  y_val <- purrr::map_dbl(x_val, eff_fun)

  p <- ggplot2::ggplot() +
    ggplot2::geom_line(mapping = ggplot2::aes(x = x_val, y = y_val), color = "steelblue3") +
    ggplot2::geom_hline(yintercept =  eff_fun(cross[1]), color = "goldenrod3") +
    ggplot2::xlim(design_space[[1]], design_space[[2]]) +
    ggplot2::labs(x = "x", y = "Efficiency") +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.border = ggplot2::element_blank(), panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"))

  efficiency <- eff_fun(cross[1])
  for(i in 1:(length(cand_points_reg)/2)){
    loop_input = paste("ggplot2::geom_segment(ggplot2::aes(x=",cand_points_reg[2*i-1],",xend=",cand_points_reg[2*i],",y=efficiency,yend=efficiency), size = 1.5, color = 'green3')", sep="")
    p <- p + eval(parse(text=loop_input))
  }

  values <- purrr::map_dbl(cross, eff_fun)
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
    point_to_add <- suppressWarnings(as.numeric(readline(prompt="Choose a point to add or enter another character to finish: \n")))
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
          weight_to_add <- suppressWarnings(as.numeric(readline(prompt = "Choose the weight of the point: \n")))
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

  # options(warn = oldw)
  if(calc_optimal_design){
    inf_mat_aug <- inf_mat(grad, aug_design)
    eff_2 <- (tr(matB %*% solve(inf_mat_opt)) / tr(matB %*% solve(inf_mat_aug))) * 100
    message(crayon::blue(cli::symbol$info), " The efficiency of the augmented design is ", round(eff_2, digits = 2), "%")
  }

  return(aug_design)
}


#' Ds-Augment Design
#'
#' @description
#' Ds-Augments a design. The user gives an initial design for which he would like to add points
#' and specifies the weight of the new points. Then he is prompted to choose a
#' minimum efficiency. After that, the candidate points region is calculated
#' and the user can choose the points and weights to add.
#'
#' @inherit augment_design params
#'
#' @return A dataframe that represents the Ds-augmented design
#'
#' @examples
#' init_des <- data.frame("Point" = c(30, 60, 90), "Weight" = c(1/3, 1/3, 1/3))
#' augment_design("Ds-Optimality", init_des, 0.25, y ~ 10^(a-b/(c+x)), c("a","b","c"),
#'   c(8.07131,  1730.63, 233.426), c(1, 100), par_int = c(1), TRUE)
#' augment_design("Ds-Optimality", init_des, 0.25, y ~ 10^(a-b/(c+x)), c("a","b","c"),
#'   c(8.07131,  1730.63, 233.426), c(1, 100), par_int = c(1), FALSE)
#'
#' @family augment designs
#'
dsaugment_design <- function(init_design, alpha, model, parameters, par_values, par_int, design_space, calc_optimal_design, weight_fun = function(x) 1) {
  x_value <- NULL
  grad <- gradient(model, parameters, par_values, weight_fun)
  grad22 <- gradient22(model, parameters, par_values, par_int, weight_fun)
  inf_mat_1 <- inf_mat(grad, init_design)
  inf_mat_22 <- inf_mat(grad22, init_design)
  sens_1 <- dsens(grad, inf_mat_1)
  sens_22 <- dsens(grad22, inf_mat_22)
  # dssens_1 <- dssens(grad, inf_mat_1, par_int)
  dseff <- function(x) (1 - alpha) * ((1 + alpha * sens_1(x) / (1 - alpha)) / (1 + alpha * sens_22(x) / (1 - alpha)))^(1/length(par_int))
  min_sens <- findminval(dseff, design_space[[1]], design_space[[2]], 10000)
  max_sens <- findmaxval(dseff, design_space[[1]], design_space[[2]], 10000)
  if(calc_optimal_design){
    optimal_design <- opt_des("Ds-Optimality", model, parameters, par_values, design_space, par_int = par_int, weight_fun = weight_fun)
    inf_mat_opt <- inf_mat(grad, optimal_design$optdes)
    if (length(inf_mat_1[-par_int, -par_int]) == 1) {
      eff_1 <- (inf_mat_opt[-par_int, -par_int] / det(inf_mat_opt) / (inf_mat_1[-par_int, -par_int] / det(inf_mat_1)))^(1 / length(par_int))*100
    } else {
      eff_1 <- (det(inf_mat_opt[-par_int, -par_int]) / det(inf_mat_opt) / (det(inf_mat_1[-par_int, -par_int]) / det(inf_mat_1)))^(1 / length(par_int))*100
    }
    message(crayon::blue(cli::symbol$info), " The efficiency of the initial design is ", round(eff_1, digits = 2), "%")
  }
  delta_range <- c(min_sens, max_sens)
  delta_val <- -Inf
  eval <- 0
  while(delta_val < delta_range[[1]] || delta_val > delta_range[[2]]){
    # Incluir gráfico?
    # try/catch, or check NA
    delta_val <- suppressWarnings(as.numeric(readline(prompt=paste("Choose a value for the minimum relative efficiency between", crayon::magenta(ceiling(delta_range[[1]]*100)/100), "and", crayon::magenta(floor(delta_range[[2]]*100)/100), ": \n"))))
    if(is.na(delta_val)){
      cat(crayon::red(cli::symbol$cross), "The efficiency must be a number")
      delta_val <- -Inf
    } else if(delta_val < delta_range[[1]] || delta_val > delta_range[[2]]){
      cat(crayon::red(cli::symbol$cross), "The efficiency must be in the given range")
    }
    eval <- eval + 1
    if(eval > 1000){
      return(NULL)
    }
  }
  # Puntos de corte y valor del corte
  cross <- sort(crosspoints(delta_val, dseff, 10000, 10^(-3), design_space[[1]], design_space[[2]]))
  # val_to_add <- sens_val_to_add(delta_val, alpha, length(parameters))

  # # Obtener start y par para tener las regiones
  start <- getStart(cross, design_space[[1]], design_space[[2]], delta_val, dseff)
  par <- getPar(cross)
  cand_points_reg <- getCross2(cross, design_space[[1]], design_space[[2]], start, par)

  x_val <- seq(design_space[[1]], design_space[[2]], length.out = 10000)
  # eff <- function(x){
  #   return((1 - alpha) * (1 + alpha * sens_1(x)/(1 - alpha))^(1 / length(parameters)))
  # }
  y_val <- purrr::map_dbl(x_val, dseff)

  p <- ggplot2::ggplot() +
    ggplot2::geom_line(mapping = ggplot2::aes(x = x_val, y = y_val), color = "steelblue3") +
    ggplot2::geom_hline(yintercept =  dseff(cross[1]), color = "goldenrod3") +
    ggplot2::xlim(design_space[[1]], design_space[[2]]) +
    ggplot2::labs(x = "x", y = "Efficiency") +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.border = ggplot2::element_blank(), panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"))

  efficiency <- dseff(cross[1])
  for(i in 1:(length(cand_points_reg)/2)){
    loop_input = paste("ggplot2::geom_segment(ggplot2::aes(x=",cand_points_reg[2*i-1],",xend=",cand_points_reg[2*i],",y=efficiency,yend=efficiency), size = 1.5, color = 'green3')", sep="")
    p <- p + eval(parse(text=loop_input))
  }

  values <- purrr::map_dbl(cross, dseff)
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
    point_to_add <- suppressWarnings(as.numeric(readline(prompt="Choose a point to add or enter another character to finish: \n")))
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
          weight_to_add <- suppressWarnings(as.numeric(readline(prompt = "Choose the weight of the point: \n")))
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

  # options(warn = oldw)
  if(calc_optimal_design){
    inf_mat_aug <- inf_mat(grad, aug_design)
    if (length(inf_mat_aug[-par_int, -par_int]) == 1) {
      eff_2 <- (inf_mat_opt[-par_int, -par_int] / det(inf_mat_opt) / (inf_mat_aug[-par_int, -par_int] / det(inf_mat_aug)))^(1 / length(par_int))*100
    } else {
      eff_2 <- (det(inf_mat_opt[-par_int, -par_int]) / det(inf_mat_opt) / (det(inf_mat_aug[-par_int, -par_int]) / det(inf_mat_aug)))^(1 / length(par_int))*100
    }
    message(crayon::blue(cli::symbol$info), " The efficiency of the augmented design is ", round(eff_2, digits = 2), "%")
  }

  return(aug_design)
}


#' Get D-augment region
#'
#' @description
#' Given a model, calculates the candidate points region for D-Optimality. The user gives an initial
#' design for which he would like to add points and specifies the weight of the new points. Then he
#' is prompted to choose a minimum efficiency. After that, the candidate points region is calculated.
#'
#' @inherit get_augment_region return params examples
#'
#' @family augment regions
#'
get_daugment_region <- function(init_design, alpha, model, parameters, par_values, design_space, calc_optimal_design, weight_fun = function(x) 1) {
  x_value <- NULL
  grad <- gradient(model, parameters, par_values, weight_fun)
  inf_mat_1 <- inf_mat(grad, init_design)
  sens_1 <- dsens(grad, inf_mat_1)
  min_sens <- findminval(sens_1, design_space[[1]], design_space[[2]], 10000)
  max_sens <- findmaxval(sens_1, design_space[[1]], design_space[[2]], 10000)
  min_eff <- -(-1 + alpha)*((-1 + alpha - min_sens*alpha)/(-1 + alpha))^(1/length(parameters))
  max_eff <- -(-1 + alpha)*((-1 + alpha - max_sens*alpha)/(-1 + alpha))^(1/length(parameters))
  if(calc_optimal_design){
    optimal_design <- opt_des("D-Optimality", model, parameters, par_values, design_space, weight_fun = weight_fun)
    inf_mat_opt <- inf_mat(grad, optimal_design$optdes)
    eff_1 <- (det(inf_mat_1) / det(inf_mat_opt))^(1 / length(parameters))*100
    message(crayon::blue(cli::symbol$info), " The efficiency of the initial design is ", round(eff_1, digits = 2), "%")
  }
  delta_range <- c(min_eff, max_eff)
  delta_val <- -Inf
  eval <- 0
  while(delta_val < delta_range[[1]] || delta_val > delta_range[[2]]){
    # Incluir gráfico?
    # try/catch, or check NA
    delta_val <- suppressWarnings(as.numeric(readline(prompt=paste("Choose a value for the minimum relative efficiency between", crayon::magenta(ceiling(delta_range[[1]]*100)/100), "and", crayon::magenta(floor(delta_range[[2]]*100)/100), ": \n"))))
    if(is.na(delta_val)){
      cat(crayon::red(cli::symbol$cross), "The efficiency must be a number")
      delta_val <- -Inf
    } else if(delta_val < delta_range[[1]] || delta_val > delta_range[[2]]){
      cat(crayon::red(cli::symbol$cross), "The efficiency must be in the given range")
    }
    eval <- eval + 1
    if(eval > 1000){
      return(NULL)
    }
  }
  # Puntos de corte y valor del corte
  val_to_add <- ((1 - alpha)/alpha*((delta_val/(1 - alpha))^length(parameters) - 1))
  cross <- sort(crosspoints(val_to_add, sens_1, 10000, 10^(-3), design_space[[1]], design_space[[2]]))


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

  return(cand_points_reg)
}



#' Get L-augment region
#'
#' @description
#' Given a model, calculates the candidate points region for L-Optimality. The user gives an initial
#' design for which he would like to add points and specifies the weight of the new points. Then he
#' is prompted to choose a minimum efficiency. After that, the candidate points region is calculated.
#'
#' @inherit get_augment_region return params examples
#'
#' @family augment region
#'
get_laugment_region <- function(init_design, alpha, model, parameters, par_values, design_space, calc_optimal_design, matB, weight_fun = function(x) 1) {
  x_value <- NULL
  grad <- gradient(model, parameters, par_values, weight_fun)
  inf_mat_1 <- inf_mat(grad, init_design)
  dsens_1 <- dsens(grad, inf_mat_1)
  isens_1 <- isens(grad, inf_mat_1, matB)
  crit_1 <- icrit(inf_mat_1, matB)
  eff_fun <- function(x) (1-alpha)*(crit_1/(crit_1 - alpha*isens_1(x)/ (1-alpha+alpha*dsens_1(x))))
  min_eff <- findminval(eff_fun, design_space[[1]], design_space[[2]], 10000)
  max_eff <- findmaxval(eff_fun, design_space[[1]], design_space[[2]], 10000)
  if(calc_optimal_design){
    optimal_design <- opt_des("I-Optimality", model, parameters, par_values, design_space, matB = matB, weight_fun = weight_fun)
    inf_mat_opt <- inf_mat(grad, optimal_design$optdes)
    eff_1 <- (tr(matB %*% solve(inf_mat_opt)) / tr(matB %*% solve(inf_mat_1))) * 100
    message(crayon::blue(cli::symbol$info), " The efficiency of the initial design is ", round(eff_1, digits = 2), "%")
  }
  delta_range <- c(min_eff, max_eff)
  delta_val <- -Inf
  eval <- 0
  while(delta_val < min_eff || delta_val > max_eff){
    # Incluir gráfico?
    # try/catch, or check NA
    delta_val <- suppressWarnings(as.numeric(readline(prompt=paste("Choose a value for the minimum relative efficiency between", crayon::magenta(ceiling(delta_range[[1]]*100)/100), "and", crayon::magenta(floor(delta_range[[2]]*100)/100), ": \n"))))
    if(is.na(delta_val)){
      cat(crayon::red(cli::symbol$cross), "The efficiency must be a number")
      delta_val <- -Inf
    } else if(delta_val < delta_range[[1]] || delta_val > delta_range[[2]]){
      cat(crayon::red(cli::symbol$cross), "The efficiency must be in the given range")
    }
    eval <- eval + 1
    if(eval > 1000){
      return(NULL)
    }
  }
  # Puntos de corte y valor del corte
  cross <- sort(crosspoints(delta_val, eff_fun, 10000, 10^(-3), design_space[[1]], design_space[[2]]))

  # # Obtener start y par para tener las regiones
  start <- getStart(cross, design_space[[1]], design_space[[2]], delta_val, eff_fun)
  par <- getPar(cross)
  cand_points_reg <- getCross2(cross, design_space[[1]], design_space[[2]], start, par)

  x_val <- seq(design_space[[1]], design_space[[2]], length.out = 10000)
  y_val <- purrr::map_dbl(x_val, eff_fun)

  p <- ggplot2::ggplot() +
    ggplot2::geom_line(mapping = ggplot2::aes(x = x_val, y = y_val), color = "steelblue3") +
    ggplot2::geom_hline(yintercept =  eff_fun(cross[1]), color = "goldenrod3") +
    ggplot2::xlim(design_space[[1]], design_space[[2]]) +
    ggplot2::labs(x = "x", y = "Efficiency") +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.border = ggplot2::element_blank(), panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"))

  efficiency <- eff_fun(cross[1])
  for(i in 1:(length(cand_points_reg)/2)){
    loop_input = paste("ggplot2::geom_segment(ggplot2::aes(x=",cand_points_reg[2*i-1],",xend=",cand_points_reg[2*i],",y=efficiency,yend=efficiency), size = 1.5, color = 'green3')", sep="")
    p <- p + eval(parse(text=loop_input))
  }

  values <- purrr::map_dbl(cross, eff_fun)
  cutoffpoints <- data.frame("x_value" = cross, "eff" = values)

  p <- p + ggplot2::geom_point(data = cutoffpoints, ggplot2::aes(x = x_value, y = eff), shape = 16, size = 2, color = "firebrick3")

  p <- p + ggplot2::geom_segment(ggplot2::aes(x = design_space[[1]], xend = design_space[[1]], y = delta_range[1], yend = delta_range[2]), col = "mediumpurple2", size = 1.5)

  # Graph of region of candidates points
  plot(p)

  return(cand_points_reg)
}


#' Get Ds-augment region
#'
#' @description
#' Given a model, calculates the candidate points region for Ds-Optimality. The user gives an initial
#' design for which he would like to add points and specifies the weight of the new points. Then he
#' is prompted to choose a minimum efficiency. After that, the candidate points region is calculated.
#'
#' @inherit get_augment_region return params examples
#'
#' @family augment region
#'
get_dsaugment_region <- function(init_design, alpha, model, parameters, par_values, par_int, design_space, calc_optimal_design, weight_fun = function(x) 1) {
  x_value <- NULL
  grad <- gradient(model, parameters, par_values, weight_fun)
  grad22 <- gradient22(model, parameters, par_values, par_int, weight_fun)
  inf_mat_1 <- inf_mat(grad, init_design)
  inf_mat_22 <- inf_mat(grad22, init_design)
  sens_1 <- dsens(grad, inf_mat_1)
  sens_22 <- dsens(grad22, inf_mat_22)
  # dssens_1 <- dssens(grad, inf_mat_1, par_int)
  dseff <- function(x) (1 - alpha) * ((1 + alpha * sens_1(x) / (1 - alpha)) / (1 + alpha * sens_22(x) / (1 - alpha)))^(1/length(par_int))
  min_sens <- findminval(dseff, design_space[[1]], design_space[[2]], 10000)
  max_sens <- findmaxval(dseff, design_space[[1]], design_space[[2]], 10000)
  if(calc_optimal_design){
    optimal_design <- opt_des("Ds-Optimality", model, parameters, par_values, design_space, par_int = par_int, weight_fun = weight_fun)
    inf_mat_opt <- inf_mat(grad, optimal_design$optdes)
    if (length(inf_mat_1[-par_int, -par_int]) == 1) {
      eff_1 <- (inf_mat_opt[-par_int, -par_int] / det(inf_mat_opt) / (inf_mat_1[-par_int, -par_int] / det(inf_mat_1)))^(1 / length(par_int))*100
    } else {
      eff_1 <- (det(inf_mat_opt[-par_int, -par_int]) / det(inf_mat_opt) / (det(inf_mat_1[-par_int, -par_int]) / det(inf_mat_1)))^(1 / length(par_int))*100
    }
    message(crayon::blue(cli::symbol$info), " The efficiency of the initial design is ", round(eff_1, digits = 2), "%")
  }
  delta_range <- c(min_sens, max_sens)
  delta_val <- -Inf
  eval <- 0
  while(delta_val < delta_range[[1]] || delta_val > delta_range[[2]]){
    # Incluir gráfico?
    # try/catch, or check NA
    delta_val <- suppressWarnings(as.numeric(readline(prompt=paste("Choose a value for the minimum relative efficiency between", crayon::magenta(ceiling(delta_range[[1]]*100)/100), "and", crayon::magenta(floor(delta_range[[2]]*100)/100), ": \n"))))
    if(is.na(delta_val)){
      cat(crayon::red(cli::symbol$cross), "The efficiency must be a number")
      delta_val <- -Inf
    } else if(delta_val < delta_range[[1]] || delta_val > delta_range[[2]]){
      cat(crayon::red(cli::symbol$cross), "The efficiency must be in the given range")
    }
    eval <- eval + 1
    if(eval > 1000){
      return(NULL)
    }
  }
  # Puntos de corte y valor del corte
  cross <- sort(crosspoints(delta_val, dseff, 10000, 10^(-3), design_space[[1]], design_space[[2]]))
  # val_to_add <- sens_val_to_add(delta_val, alpha, length(parameters))

  # # Obtener start y par para tener las regiones
  start <- getStart(cross, design_space[[1]], design_space[[2]], delta_val, dseff)
  par <- getPar(cross)
  cand_points_reg <- getCross2(cross, design_space[[1]], design_space[[2]], start, par)

  x_val <- seq(design_space[[1]], design_space[[2]], length.out = 10000)
  # eff <- function(x){
  #   return((1 - alpha) * (1 + alpha * sens_1(x)/(1 - alpha))^(1 / length(parameters)))
  # }
  y_val <- purrr::map_dbl(x_val, dseff)

  p <- ggplot2::ggplot() +
    ggplot2::geom_line(mapping = ggplot2::aes(x = x_val, y = y_val), color = "steelblue3") +
    ggplot2::geom_hline(yintercept =  dseff(cross[1]), color = "goldenrod3") +
    ggplot2::xlim(design_space[[1]], design_space[[2]]) +
    ggplot2::labs(x = "x", y = "Efficiency") +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.border = ggplot2::element_blank(), panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"))

  efficiency <- dseff(cross[1])
  for(i in 1:(length(cand_points_reg)/2)){
    loop_input = paste("ggplot2::geom_segment(ggplot2::aes(x=",cand_points_reg[2*i-1],",xend=",cand_points_reg[2*i],",y=efficiency,yend=efficiency), size = 1.5, color = 'green3')", sep="")
    p <- p + eval(parse(text=loop_input))
  }

  values <- purrr::map_dbl(cross, dseff)
  cutoffpoints <- data.frame("x_value" = cross, "eff" = values)

  p <- p + ggplot2::geom_point(data = cutoffpoints, ggplot2::aes(x = x_value, y = eff), shape = 16, size = 2, color = "firebrick3")

  p <- p + ggplot2::geom_segment(ggplot2::aes(x = design_space[[1]], xend = design_space[[1]], y = delta_range[1], yend = delta_range[2]), col = "mediumpurple2", size = 1.5)

  # Graph of region of candidates points
  plot(p)

  return(cand_points_reg)
}






#' Gradient function for a subset of variables
#'
#' @description
#' Calculates the gradient function of a \code{model} with respect to a subset of the parameters given in
#' \code{par_int}, \code{char_vars}, evaluates it at the provided \code{values} and returns the result as
#' a function of the variable \code{x}.
#'
#' @param model formula describing the model, which must contain only \code{x}, the parameters defined in
#'   \code{char_vars} and the numerical operators.
#' @param char_vars character vector of the parameters of the model.
#' @param values numeric vector with the nominal values of the parameters in \code{char_vars}.
#' @param par_int vector of indexes indicating the subset of variables to omit in the calculation of the gradient.
#' @param weight_fun optional one variable function that represents the square of the structure of variance, in case of heteroscedastic variance of the response
#'
#' @return A function depending on \code{x} that's the gradient of the \code{model} with respect to \code{char_vars}
gradient22 <- function(model, char_vars, values, par_int, weight_fun = function(x) 1) {
  # vars <- as.list(match.call())[-(1:2)]
  # char_vars <- sapply(vars, as.character)
  ext_char_vars <- c(char_vars, "x")
  arglist <- lapply(ext_char_vars, function(x) NULL)
  f <- as.function(append(stats::setNames(arglist, ext_char_vars), quote({})))
  f1 <- stats::deriv(model, char_vars[-par_int], f)
  f2 <- function(x_val) {
    attr(do.call(f1, as.list(c(values, x_val))), "gradient")
  }
  f3 <- function(x){
    return(f2(x)*weight_fun(x))
  }
  return(f3)
}




#' Calculate crosspoints
#'
#' @description
#' Given the parameters for augmenting a design, this function calculates the
#' crosspoints in the efficiency function that delimit the candidate points
#' region
#'
#' @param val Efficiency value to solve in the curve relationing the space of the design and efficiency of new design
#' @param sens Sensitivity function of the design for the model
#' @param gridlength Number of points in the grid to find the crosspoints
#' @param tol Tolerance that establishes how close two points close to one another are considered the same
#' @param xmin Minimum of the space of the design
#' @param xmax Maximum of the space of the design
#'
#' @return A numeric vector of crosspoints that define the candidate points region
#'
crosspoints <- function(val, sens, gridlength, tol, xmin, xmax){

  sensfix <- function(x){
    return(sens(x) - val)
  }

  sols <- vector(mode = "numeric", length = gridlength)
  cli::cli_progress_bar("Calculating regions", total = gridlength)
  startsx <- seq(design_space[[1]], design_space[[2]], length.out = gridlength)
  for(i in 1:gridlength){
    sols[i] <- nleqslv::nleqslv(startsx[i], fn = sensfix)$x
    cli::cli_progress_update()
  }

  # sols <- unlist(purrr::map(purrr::map(cli::cli_progress_along(seq(xmin, xmax, length.out = gridlength), name = "Calculating regions"), nleqslv::nleqslv, fn = sensfix), function(x) x$x))

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
add_points <- function(points, alpha, design){
  new_points <- data.frame("Point" = points, "Weight" = rep(alpha/length(points), times = length(points)))
  design["Weight"] <- design["Weight"]*(1-alpha)
  return(rbind(design, new_points))
}


#' Add two designs
#'
#' @param design_1 A dataframe with 'Point' and 'Weight' as columns that represent the first design to add
#' @param design_2 A dataframe with 'Point' and 'Weight' as columns that represent the second design to add
#' @param alpha Weight of the first design
#'
#' @return A design as a dataframe with the weighted addition of the two designs
add_design <- function(design_1, design_2, alpha){
  design_2[, c("Weight")] <- design_2[, c("Weight")] / sum(design_2[, c("Weight")]) * alpha
  design_1[, c("Weight")] <- design_1[, c("Weight")] * (1 - alpha)
  return(rbind(design_1, design_2))
}
