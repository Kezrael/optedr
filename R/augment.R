
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
#'   * 'L-Optimality'
#' @param init_design dataframe with "Point" and "Weight" columns that represents the initial design to augment
#' @param alpha combined weight of the new points
#' @param model formula that represents the model with x as the independent variable
#' @param parameters character vector with the unknown parameters of the model to estimate
#' @param par_values numeric vector with the initial values of the unknown parameters
#' @param design_space numeric vector with the limits of the space of the design
#' @param calc_optimal_design boolean parameter, if TRUE, the optimal design is calculated and efficiencies of the initial and augmented design are given
#' @param weight_fun optional one variable function that represents the square of the structure of variance, in case of heteroscedastic variance of the response
#' @param par_int optional numeric vector with the index of the \code{parameters} of interest for Ds-optimality.
#' @param matB optional matrix of dimensions k x k, for L-optimality.
#' @param distribution character specifying the probability distribution of the response. Can be one of the following:
#'   * 'Homoscedasticity'
#'   * 'Gamma', which can be used for exponential or normal heteroscedastic with constant relative error
#'   * 'Poisson'
#'   * 'Logistic'
#'   * 'Log-Normal' (work in progress)
#' @param delta_val optional numeric value for the minimum relative efficiency. If \code{NULL} (default),
#'   the user is prompted interactively. Providing this value enables non-interactive use.
#' @param new_points optional dataframe with \code{Point} and \code{Weight} columns specifying the points
#'   to add. All points must lie within the candidate region determined by \code{delta_val}. If \code{NULL}
#'   (default), the user is prompted interactively.
#'
#' @return A dataframe that represents the augmented design
#' @export
#'
#' @examples
#' \donttest{
#' init_des <- data.frame("Point" = c(30, 60, 90), "Weight" = c(1/3, 1/3, 1/3))
#' region <- get_augment_region("D-Optimality", init_des, 0.25,
#'   y ~ 10^(a - b/(c + x)), c("a", "b", "c"),
#'   c(8.07131, 1730.63, 233.426), c(1, 100), FALSE, delta_val = 0.85)
#' new_pts <- data.frame(Point = mean(region$region[1:2]), Weight = 1)
#' augment_design("D-Optimality", init_des, 0.25, y ~ 10^(a - b/(c + x)),
#'   c("a", "b", "c"), c(8.07131, 1730.63, 233.426), c(1, 100), FALSE,
#'   delta_val = 0.85, new_points = new_pts)
#' }
augment_design <- function(criterion, init_design, alpha, model, parameters, par_values,
                           design_space, calc_optimal_design,
                           par_int = NA, matB = NULL, distribution = NA,
                           weight_fun = function(x) 1,
                           delta_val = NULL, new_points = NULL) {
  design_vars <- detect_design_vars(model, parameters)
  if (is_multifactor(design_vars)) {
    design_space <- canonicalise_design_space(design_space, design_vars)
    if (!is.na(distribution))
      weight_fun <- weight_function(model, parameters, par_values, distribution = distribution)
    grad <- gradient(model, parameters, par_values, weight_fun)
    k    <- length(parameters)
    if (criterion == "A-Optimality") matB <- diag(k)
    if (criterion == "I-Optimality") matB <- integrate_reg_int(grad, k, design_space)
    if (criterion == "Ds-Optimality") {
      grad22     <- gradient22(model, parameters, par_values, par_int, weight_fun)
      inf_mat_1  <- inf_mat(grad,   init_design)
      inf_mat_22 <- inf_mat(grad22, init_design)
      sens_1     <- dsens(grad,   inf_mat_1)
      sens_22    <- dsens(grad22, inf_mat_22)
      eff_fn     <- function(x) (1 - alpha) *
        ((1 + alpha * as.numeric(sens_1(x))   / (1 - alpha)) /
         (1 + alpha * as.numeric(sens_22(x))  / (1 - alpha)))^(1 / length(par_int))
    } else {
      eff_fn <- .aug_eff_fun(criterion, grad, init_design, alpha, k, matB)
    }
    delta_range <- c(findminval(eff_fn, design_space, 2000L),
                     findmaxval(eff_fn, design_space, 2000L))
    if (calc_optimal_design) {
      opt  <- opt_des(criterion, model, parameters, par_values, design_space,
                      matB = matB, weight_fun = weight_fun)
      eff1 <- eff(criterion, inf_mat(grad, init_design), inf_mat(grad, opt$optdes),
                  k = k, intPars = par_int, matB = matB) * 100
      message(crayon::blue(cli::symbol$info),
              " Efficiency of initial design: ", round(eff1, 2), "%")
    }
    delta_val <- ask_delta(delta_range, delta_val)
    if (is.null(delta_val)) return(NULL)
    if (length(design_vars) == 2L)
      plot(.plot_aug_heatmap_2d(design_space, eff_fn, delta_val, init_design))
    cands <- .sample_aug_candidates(eff_fn, design_space, delta_val)
    message(crayon::blue(cli::symbol$info), " ", nrow(cands),
            " candidate points with efficiency >= ", round(delta_val, 4),
            " (from LHS sample of 5000)")
    if (nrow(cands) > 0L) {
      cat("Sample of candidate points:\n")
      print(utils::head(cands[, c(design_vars, "efficiency"), drop = FALSE], 15L))
    }
    pts <- if (!is.null(new_points)) {
      .validate_new_points_mf(new_points, design_vars, design_space, eff_fn, delta_val)
    } else {
      .select_new_points_mf_interactive(design_vars, design_space, eff_fn, delta_val)
    }
    init_norm <- normalize_design_cols(init_design, design_vars)
    aug       <- add_design(init_norm, pts, alpha)
    if (calc_optimal_design) {
      eff2 <- eff(criterion, inf_mat(grad, aug), inf_mat(grad, opt$optdes),
                  k = k, matB = matB) * 100
      message(crayon::blue(cli::symbol$info),
              " Efficiency of augmented design: ", round(eff2, 2), "%")
    }
    return(aug)
  }
  if (!is.na(distribution)) {
    weight_fun <- weight_function(model, parameters, par_values, distribution = distribution)
  }
  if (criterion == "D-Optimality") {
    daugment_design(init_design, alpha, model, parameters, par_values, design_space,
                    calc_optimal_design, weight_fun, delta_val = delta_val, new_points = new_points)
  } else if (criterion == "A-Optimality") {
    laugment_design(init_design, alpha, model, parameters, par_values, design_space,
                    calc_optimal_design, matB = diag(length(parameters)), weight_fun,
                    delta_val = delta_val, new_points = new_points)
  } else if (criterion == "I-Optimality") {
    grad <- gradient(model, parameters, par_values, weight_fun)
    matB <- integrate_reg_int(grad, length(parameters), design_space)
    laugment_design(init_design, alpha, model, parameters, par_values, design_space,
                    calc_optimal_design, matB, weight_fun, delta_val = delta_val, new_points = new_points)
  } else if (criterion == "L-Optimality") {
    laugment_design(init_design, alpha, model, parameters, par_values, design_space,
                    calc_optimal_design, matB, weight_fun, delta_val = delta_val, new_points = new_points)
  } else if (criterion == "Ds-Optimality") {
    dsaugment_design(init_design, alpha, model, parameters, par_values, par_int, design_space,
                     calc_optimal_design, weight_fun, delta_val = delta_val, new_points = new_points)
  } else {
    stop("Invalid criterion. Choose from: D-Optimality, Ds-Optimality, A-Optimality, ",
         "I-Optimality, L-Optimality", call. = FALSE)
  }
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
#'   * 'L-Optimality'
#' @param init_design dataframe with "Point" and "Weight" columns that represents the initial design to augment
#' @param alpha combined weight of the new points
#' @param model formula that represent the model with x as the independent variable
#' @param parameters character vector with the unknown parameters of the model to estimate
#' @param par_values numeric vector with the initial values of the unknown parameters
#' @param design_space numeric vector with the limits of the space of the design
#' @param calc_optimal_design boolean parameter, if TRUE, the optimal design is calculated and efficiencies of the initial and augmented design are given
#' @param weight_fun optional one variable function that represents the square of the structure of variance, in case of heteroscedastic variance of the response
#' @param par_int optional numeric vector with the index of the \code{parameters} of interest for Ds-optimality.
#' @param matB optional matrix of dimensions k x k, for L-optimality.
#' @param distribution character specifying the probability distribution of the response. Can be one of the following:
#'   * 'Homoscedasticity'
#'   * 'Gamma', which can be used for exponential or normal heteroscedastic with constant relative error
#'   * 'Poisson'
#'   * 'Logistic'
#'   * 'Log-Normal' (work in progress)
#' @param delta_val optional numeric value for the minimum relative efficiency. If \code{NULL} (default),
#'   the user is prompted interactively. Providing this value enables non-interactive use.
#'
#' @return A vector of the points limiting the candidate points region
#' @export
#'
#' @examples
#' \donttest{
#' init_des <- data.frame("Point" = c(30, 60, 90), "Weight" = c(1/3, 1/3, 1/3))
#' get_augment_region("D-Optimality", init_des, 0.25, y ~ 10^(a - b/(c + x)),
#'   c("a", "b", "c"), c(8.07131, 1730.63, 233.426), c(1, 100), FALSE,
#'   delta_val = 0.85)
#' }
get_augment_region <- function(criterion, init_design, alpha, model, parameters, par_values,
                               design_space, calc_optimal_design,
                               par_int = NA, matB = NA, distribution = NA,
                               weight_fun = function(x) 1,
                               delta_val = NULL) {
  design_vars <- detect_design_vars(model, parameters)
  if (is_multifactor(design_vars)) {
    design_space <- canonicalise_design_space(design_space, design_vars)
    if (!is.na(distribution))
      weight_fun <- weight_function(model, parameters, par_values, distribution = distribution)
    grad <- gradient(model, parameters, par_values, weight_fun)
    k    <- length(parameters)
    if (criterion == "A-Optimality") matB <- diag(k)
    if (criterion == "I-Optimality") matB <- integrate_reg_int(grad, k, design_space)
    if (criterion == "Ds-Optimality") {
      grad22     <- gradient22(model, parameters, par_values, par_int, weight_fun)
      inf_mat_1  <- inf_mat(grad,   init_design)
      inf_mat_22 <- inf_mat(grad22, init_design)
      sens_1     <- dsens(grad,   inf_mat_1)
      sens_22    <- dsens(grad22, inf_mat_22)
      eff_fn     <- function(x) (1 - alpha) *
        ((1 + alpha * as.numeric(sens_1(x))  / (1 - alpha)) /
         (1 + alpha * as.numeric(sens_22(x)) / (1 - alpha)))^(1 / length(par_int))
    } else {
      eff_fn <- .aug_eff_fun(criterion, grad, init_design, alpha, k, matB)
    }
    delta_range <- c(findminval(eff_fn, design_space, 2000L),
                     findmaxval(eff_fn, design_space, 2000L))
    if (calc_optimal_design) {
      opt  <- opt_des(criterion, model, parameters, par_values, design_space,
                      matB = if (criterion == "L-Optimality") matB else NULL,
                      weight_fun = weight_fun)
      eff1 <- eff(criterion, inf_mat(grad, init_design), inf_mat(grad, opt$optdes),
                  k = k, intPars = par_int, matB = matB) * 100
      message(crayon::blue(cli::symbol$info),
              " Efficiency of initial design: ", round(eff1, 2), "%")
    }
    delta_val <- ask_delta(delta_range, delta_val)
    if (is.null(delta_val)) return(NULL)
    p      <- if (length(design_vars) == 2L)
                .plot_aug_heatmap_2d(design_space, eff_fn, delta_val, init_design) else NULL
    if (!is.null(p)) plot(p)
    cands  <- .sample_aug_candidates(eff_fn, design_space, delta_val)
    message(crayon::blue(cli::symbol$info), " ", nrow(cands),
            " candidate points with efficiency >= ", round(delta_val, 4),
            " (from LHS sample of 5000)")
    return(invisible(.make_augment_region(cands, delta_val, eff_fn, design_vars, p)))
  }
  if (!is.na(distribution)) {
    weight_fun <- weight_function(model, parameters, par_values, distribution = distribution)
  }
  if (criterion == "D-Optimality") {
    get_daugment_region(init_design, alpha, model, parameters, par_values, design_space,
                        calc_optimal_design, weight_fun, delta_val = delta_val)
  } else if (criterion == "A-Optimality") {
    get_laugment_region(init_design, alpha, model, parameters, par_values, design_space,
                        calc_optimal_design, matB = diag(length(parameters)), weight_fun,
                        delta_val = delta_val)
  } else if (criterion == "I-Optimality") {
    grad <- gradient(model, parameters, par_values, weight_fun)
    matB <- integrate_reg_int(grad, length(parameters), design_space)
    get_laugment_region(init_design, alpha, model, parameters, par_values, design_space,
                        calc_optimal_design, matB, weight_fun, delta_val = delta_val)
  } else if (criterion == "L-Optimality") {
    get_laugment_region(init_design, alpha, model, parameters, par_values, design_space,
                        calc_optimal_design, matB, weight_fun, delta_val = delta_val)
  } else if (criterion == "Ds-Optimality") {
    get_dsaugment_region(init_design, alpha, model, parameters, par_values, par_int,
                         design_space, calc_optimal_design, weight_fun, delta_val = delta_val)
  } else {
    stop("Invalid criterion. Choose from: D-Optimality, Ds-Optimality, A-Optimality, ",
         "I-Optimality, L-Optimality", call. = FALSE)
  }
}


# --- Private helpers for interactive augmentation ---------------------------

# Prompts the user to choose a delta value within [delta_range[1], delta_range[2]].
# If delta_val is provided, validates it and returns it directly (non-interactive mode).
# Returns the chosen value, or NULL after 1000 failed interactive attempts.
ask_delta <- function(delta_range, delta_val = NULL) {
  if (!is.null(delta_val)) {
    if (delta_val < delta_range[[1]] || delta_val > delta_range[[2]])
      stop("delta_val (", round(delta_val, 4), ") is outside the valid range [",
           round(delta_range[[1]], 4), ", ", round(delta_range[[2]], 4), "].", call. = FALSE)
    return(delta_val)
  }
  if (!interactive())
    stop("Supply delta_val for non-interactive use.", call. = FALSE)
  val <- -Inf
  eval_count <- 0
  while (val < delta_range[[1]] || val > delta_range[[2]]) {
    val <- suppressWarnings(as.numeric(readline(prompt = paste(
      "Choose a value for the minimum relative efficiency between",
      crayon::magenta(ceiling(delta_range[[1]] * 100) / 100), "and",
      crayon::magenta(floor(delta_range[[2]] * 100) / 100), ": \n"
    ))))
    if (is.na(val)) {
      cat(crayon::red(cli::symbol$cross), "The efficiency must be a number")
      val <- -Inf
    } else if (val < delta_range[[1]] || val > delta_range[[2]]) {
      cat(crayon::red(cli::symbol$cross), "The efficiency must be in the given range")
    }
    eval_count <- eval_count + 1
    if (eval_count > 1000) return(NULL)
  }
  val
}

# Builds the augment region plot: efficiency curve, candidate region segments,
# crosspoints, and the delta range bar.
plot_augment_region <- function(eff_fun, cross, delta_range, cand_points_reg, design_space) {
  x_value <- eff <- NULL
  x_val <- seq(design_space[[1]], design_space[[2]], length.out = 10000)
  y_val <- purrr::map_dbl(x_val, eff_fun)
  efficiency <- eff_fun(cross[[1]])
  p <- ggplot2::ggplot() +
    ggplot2::geom_line(mapping = ggplot2::aes(x = x_val, y = y_val), color = "steelblue3") +
    ggplot2::geom_hline(yintercept = efficiency, color = "goldenrod3") +
    ggplot2::xlim(design_space[[1]], design_space[[2]]) +
    ggplot2::labs(x = "x", y = "Efficiency") +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.border = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(colour = "black"))
  for (i in 1:(length(cand_points_reg) / 2)) {
    p <- p + ggplot2::annotate("segment", x = cand_points_reg[2*i-1], xend = cand_points_reg[2*i],
                               y = efficiency, yend = efficiency, linewidth = 1.5, color = "green3")
  }
  cutoffpoints <- data.frame("x_value" = cross, "eff" = purrr::map_dbl(cross, eff_fun))
  p +
    ggplot2::geom_point(data = cutoffpoints, ggplot2::aes(x = x_value, y = eff),
                        shape = 16, size = 2, color = "firebrick3") +
    ggplot2::annotate("segment", x = design_space[[1]], xend = design_space[[1]],
                      y = delta_range[1], yend = delta_range[2],
                      colour = "mediumpurple2", linewidth = 1.5)
}

# Selects new points from the candidate region, either from new_points (non-interactive)
# or via readline prompts (interactive). Returns a data.frame with Point and Weight columns.
select_new_points <- function(cand_points_reg, design_space, new_points = NULL) {
  n_regions <- length(cand_points_reg) / 2
  in_region <- function(pt) any(vapply(seq_len(n_regions), function(i)
    pt >= cand_points_reg[2*i-1] & pt <= cand_points_reg[2*i], logical(1)))
  if (!is.null(new_points)) {
    if (!is.data.frame(new_points) || !identical(names(new_points), c("Point", "Weight")))
      stop("new_points must be a data.frame with 'Point' and 'Weight' columns.", call. = FALSE)
    if (any(new_points$Weight <= 0))
      stop("All weights in new_points must be positive.", call. = FALSE)
    out_space <- new_points$Point < design_space[[1]] | new_points$Point > design_space[[2]]
    if (any(out_space))
      stop("Points outside the design space [", design_space[[1]], ", ", design_space[[2]], "]: ",
           paste(round(new_points$Point[out_space], 4), collapse = ", "), call. = FALSE)
    bad <- !vapply(new_points$Point, in_region, logical(1))
    if (any(bad))
      stop("Points outside the candidate region for the chosen efficiency: ",
           paste(round(new_points$Point[bad], 4), collapse = ", "), call. = FALSE)
    return(new_points)
  }
  if (!interactive())
    stop("Supply new_points for non-interactive use.", call. = FALSE)
  cutoff_text <- paste(
    vapply(seq_len(n_regions), function(i)
      paste0("[", ceiling(cand_points_reg[2*i-1] * 100) / 100, "-",
             floor(cand_points_reg[2*i] * 100) / 100, "]"),
      character(1)),
    collapse = ", ")
  result <- data.frame(Point = double(), Weight = double())
  point_to_add <- -1
  while (!is.na(point_to_add)) {
    cat("The region(s) are ", crayon::green(cutoff_text))
    point_to_add <- suppressWarnings(as.numeric(
      readline(prompt = "Choose a point to add or enter another character to finish: \n")
    ))
    if (!is.na(point_to_add)) {
      if (in_region(point_to_add)) {
        weight_ok <- FALSE
        while (!weight_ok) {
          weight_to_add <- suppressWarnings(as.numeric(
            readline(prompt = "Choose the weight of the point: \n")
          ))
          if (is.na(weight_to_add)) {
            cat(crayon::red(cli::symbol$cross), "The weight must be a positive number")
          } else if (weight_to_add <= 0) {
            cat(crayon::red(cli::symbol$cross), "The weight must be positive")
          } else {
            result[nrow(result) + 1, ] <- c(point_to_add, weight_to_add)
            weight_ok <- TRUE
          }
        }
      } else {
        cat(crayon::red(cli::symbol$cross), "The point is outside the candidate points region \n")
      }
    }
  }
  result
}


# --- Criterion-specific augment functions -----------------------------------

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
daugment_design <- function(init_design, alpha, model, parameters, par_values, design_space,
                            calc_optimal_design, weight_fun = function(x) 1,
                            delta_val = NULL, new_points = NULL) {
  grad <- gradient(model, parameters, par_values, weight_fun)
  inf_mat_1 <- inf_mat(grad, init_design)
  sens_1 <- dsens(grad, inf_mat_1)
  eff_fun <- function(x) (1 - alpha) * (1 + alpha * sens_1(x) / (1 - alpha))^(1 / length(parameters))
  delta_range <- c(findminval(eff_fun, design_space, 10000),
                   findmaxval(eff_fun, design_space, 10000))
  if (calc_optimal_design) {
    optimal_design <- opt_des("D-Optimality", model, parameters, par_values, design_space, weight_fun = weight_fun)
    inf_mat_opt <- inf_mat(grad, optimal_design$optdes)
    eff_1 <- (det(inf_mat_1) / det(inf_mat_opt))^(1 / length(parameters)) * 100
    message(crayon::blue(cli::symbol$info), " The efficiency of the initial design is ", round(eff_1, digits = 2), "%")
  }
  delta_val <- ask_delta(delta_range, delta_val)
  if (is.null(delta_val)) return(NULL)
  val_to_add <- (1 - alpha) / alpha * ((delta_val / (1 - alpha))^length(parameters) - 1)
  cross <- sort(crosspoints(val_to_add, sens_1, 10000, 10^(-3), design_space[[1]], design_space[[2]]))
  cand_points_reg <- getCross2(cross, design_space[[1]], design_space[[2]],
                               getStart(cross, design_space[[1]], design_space[[2]], val_to_add, sens_1),
                               getPar(cross))
  plot(plot_augment_region(eff_fun, cross, delta_range, cand_points_reg, design_space))
  pts <- select_new_points(cand_points_reg, design_space, new_points)
  aug_design <- if (nrow(pts) > 0) add_design(init_design, pts, alpha) else init_design
  if (calc_optimal_design) {
    inf_mat_aug <- inf_mat(grad, aug_design)
    eff_2 <- (det(inf_mat_aug) / det(inf_mat_opt))^(1 / length(parameters)) * 100
    message(crayon::blue(cli::symbol$info), " The efficiency of the augmented design is ", round(eff_2, digits = 2), "%")
  }
  aug_design
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
#' \dontrun{
#' init_des <- data.frame("Point" = c(30, 60, 90), "Weight" = c(1/3, 1/3, 1/3))
#' augment_design("I-Optimality", init_des, 0.25, y ~ 10^(a-b/(c+x)), c("a","b","c"),
#'   c(8.07131,  1730.63, 233.426), c(1, 100), TRUE)
#' }
#'
#' @family augment designs
#'
laugment_design <- function(init_design, alpha, model, parameters, par_values, design_space,
                            calc_optimal_design, matB, weight_fun = function(x) 1,
                            delta_val = NULL, new_points = NULL) {
  grad <- gradient(model, parameters, par_values, weight_fun)
  inf_mat_1 <- inf_mat(grad, init_design)
  dsens_1 <- dsens(grad, inf_mat_1)
  isens_1 <- isens(grad, inf_mat_1, matB)
  crit_1 <- icrit(inf_mat_1, matB)
  eff_fun <- function(x) (1 - alpha) * (crit_1 / (crit_1 - alpha * isens_1(x) / (1 - alpha + alpha * dsens_1(x))))
  delta_range <- c(findminval(eff_fun, design_space, 10000),
                   findmaxval(eff_fun, design_space, 10000))
  if (calc_optimal_design) {
    optimal_design <- opt_des("L-Optimality", model, parameters, par_values, design_space, matB = matB, weight_fun = weight_fun)
    inf_mat_opt <- inf_mat(grad, optimal_design$optdes)
    eff_1 <- (tr(matB %*% inv_spd(inf_mat_opt)) / tr(matB %*% inv_spd(inf_mat_1))) * 100
    message(crayon::blue(cli::symbol$info), " The efficiency of the initial design is ", round(eff_1, digits = 2), "%")
  }
  delta_val <- ask_delta(delta_range, delta_val)
  if (is.null(delta_val)) return(NULL)
  cross <- sort(crosspoints(delta_val, eff_fun, 10000, 10^(-3), design_space[[1]], design_space[[2]]))
  cand_points_reg <- getCross2(cross, design_space[[1]], design_space[[2]],
                               getStart(cross, design_space[[1]], design_space[[2]], delta_val, eff_fun),
                               getPar(cross))
  plot(plot_augment_region(eff_fun, cross, delta_range, cand_points_reg, design_space))
  pts <- select_new_points(cand_points_reg, design_space, new_points)
  aug_design <- if (nrow(pts) > 0) add_design(init_design, pts, alpha) else init_design
  if (calc_optimal_design) {
    inf_mat_aug <- inf_mat(grad, aug_design)
    eff_2 <- (tr(matB %*% inv_spd(inf_mat_opt)) / tr(matB %*% inv_spd(inf_mat_aug))) * 100
    message(crayon::blue(cli::symbol$info), " The efficiency of the augmented design is ", round(eff_2, digits = 2), "%")
  }
  aug_design
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
#' \dontrun{
#' init_des <- data.frame("Point" = c(30, 60, 90), "Weight" = c(1/3, 1/3, 1/3))
#' augment_design("Ds-Optimality", init_des, 0.25, y ~ 10^(a-b/(c+x)), c("a","b","c"),
#'   c(8.07131,  1730.63, 233.426), c(1, 100), par_int = c(1), TRUE)
#' }
#'
#' @family augment designs
#'
dsaugment_design <- function(init_design, alpha, model, parameters, par_values, par_int,
                             design_space, calc_optimal_design, weight_fun = function(x) 1,
                             delta_val = NULL, new_points = NULL) {
  grad <- gradient(model, parameters, par_values, weight_fun)
  grad22 <- gradient22(model, parameters, par_values, par_int, weight_fun)
  inf_mat_1 <- inf_mat(grad, init_design)
  inf_mat_22 <- inf_mat(grad22, init_design)
  sens_1 <- dsens(grad, inf_mat_1)
  sens_22 <- dsens(grad22, inf_mat_22)
  dseff <- function(x) (1 - alpha) * ((1 + alpha * sens_1(x) / (1 - alpha)) / (1 + alpha * sens_22(x) / (1 - alpha)))^(1 / length(par_int))
  delta_range <- c(findminval(dseff, design_space, 10000),
                   findmaxval(dseff, design_space, 10000))
  if (calc_optimal_design) {
    optimal_design <- opt_des("Ds-Optimality", model, parameters, par_values, design_space, par_int = par_int, weight_fun = weight_fun)
    inf_mat_opt <- inf_mat(grad, optimal_design$optdes)
    if (length(inf_mat_1[-par_int, -par_int]) == 1) {
      eff_1 <- (inf_mat_opt[-par_int, -par_int] / det(inf_mat_opt) / (inf_mat_1[-par_int, -par_int] / det(inf_mat_1)))^(1 / length(par_int)) * 100
    } else {
      eff_1 <- (det(inf_mat_opt[-par_int, -par_int]) / det(inf_mat_opt) / (det(inf_mat_1[-par_int, -par_int]) / det(inf_mat_1)))^(1 / length(par_int)) * 100
    }
    message(crayon::blue(cli::symbol$info), " The efficiency of the initial design is ", round(eff_1, digits = 2), "%")
  }
  delta_val <- ask_delta(delta_range, delta_val)
  if (is.null(delta_val)) return(NULL)
  cross <- sort(crosspoints(delta_val, dseff, 10000, 10^(-3), design_space[[1]], design_space[[2]]))
  cand_points_reg <- getCross2(cross, design_space[[1]], design_space[[2]],
                               getStart(cross, design_space[[1]], design_space[[2]], delta_val, dseff),
                               getPar(cross))
  plot(plot_augment_region(dseff, cross, delta_range, cand_points_reg, design_space))
  pts <- select_new_points(cand_points_reg, design_space, new_points)
  aug_design <- if (nrow(pts) > 0) add_design(init_design, pts, alpha) else init_design
  if (calc_optimal_design) {
    inf_mat_aug <- inf_mat(grad, aug_design)
    if (length(inf_mat_aug[-par_int, -par_int]) == 1) {
      eff_2 <- (inf_mat_opt[-par_int, -par_int] / det(inf_mat_opt) / (inf_mat_aug[-par_int, -par_int] / det(inf_mat_aug)))^(1 / length(par_int)) * 100
    } else {
      eff_2 <- (det(inf_mat_opt[-par_int, -par_int]) / det(inf_mat_opt) / (det(inf_mat_aug[-par_int, -par_int]) / det(inf_mat_aug)))^(1 / length(par_int)) * 100
    }
    message(crayon::blue(cli::symbol$info), " The efficiency of the augmented design is ", round(eff_2, digits = 2), "%")
  }
  aug_design
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
get_daugment_region <- function(init_design, alpha, model, parameters, par_values, design_space,
                                calc_optimal_design, weight_fun = function(x) 1,
                                delta_val = NULL) {
  grad <- gradient(model, parameters, par_values, weight_fun)
  inf_mat_1 <- inf_mat(grad, init_design)
  sens_1 <- dsens(grad, inf_mat_1)
  eff_fun <- function(x) (1 - alpha) * (1 + alpha * sens_1(x) / (1 - alpha))^(1 / length(parameters))
  delta_range <- c(findminval(eff_fun, design_space, 10000),
                   findmaxval(eff_fun, design_space, 10000))
  if (calc_optimal_design) {
    optimal_design <- opt_des("D-Optimality", model, parameters, par_values, design_space, weight_fun = weight_fun)
    inf_mat_opt <- inf_mat(grad, optimal_design$optdes)
    eff_1 <- (det(inf_mat_1) / det(inf_mat_opt))^(1 / length(parameters)) * 100
    message(crayon::blue(cli::symbol$info), " The efficiency of the initial design is ", round(eff_1, digits = 2), "%")
  }
  delta_val <- ask_delta(delta_range, delta_val)
  if (is.null(delta_val)) return(NULL)
  val_to_add <- (1 - alpha) / alpha * ((delta_val / (1 - alpha))^length(parameters) - 1)
  cross <- sort(crosspoints(val_to_add, sens_1, 10000, 10^(-3), design_space[[1]], design_space[[2]]))
  cand_points_reg <- getCross2(cross, design_space[[1]], design_space[[2]],
                               getStart(cross, design_space[[1]], design_space[[2]], val_to_add, sens_1),
                               getPar(cross))
  p <- plot_augment_region(eff_fun, cross, delta_range, cand_points_reg, design_space)
  plot(p)
  .make_augment_region(cand_points_reg, delta_val, eff_fun, "x", p)
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
get_laugment_region <- function(init_design, alpha, model, parameters, par_values, design_space,
                                calc_optimal_design, matB, weight_fun = function(x) 1,
                                delta_val = NULL) {
  grad <- gradient(model, parameters, par_values, weight_fun)
  inf_mat_1 <- inf_mat(grad, init_design)
  dsens_1 <- dsens(grad, inf_mat_1)
  isens_1 <- isens(grad, inf_mat_1, matB)
  crit_1 <- icrit(inf_mat_1, matB)
  eff_fun <- function(x) (1 - alpha) * (crit_1 / (crit_1 - alpha * isens_1(x) / (1 - alpha + alpha * dsens_1(x))))
  delta_range <- c(findminval(eff_fun, design_space, 10000),
                   findmaxval(eff_fun, design_space, 10000))
  if (calc_optimal_design) {
    optimal_design <- opt_des("I-Optimality", model, parameters, par_values, design_space, matB = matB, weight_fun = weight_fun)
    inf_mat_opt <- inf_mat(grad, optimal_design$optdes)
    eff_1 <- (tr(matB %*% inv_spd(inf_mat_opt)) / tr(matB %*% inv_spd(inf_mat_1))) * 100
    message(crayon::blue(cli::symbol$info), " The efficiency of the initial design is ", round(eff_1, digits = 2), "%")
  }
  delta_val <- ask_delta(delta_range, delta_val)
  if (is.null(delta_val)) return(NULL)
  cross <- sort(crosspoints(delta_val, eff_fun, 10000, 10^(-3), design_space[[1]], design_space[[2]]))
  cand_points_reg <- getCross2(cross, design_space[[1]], design_space[[2]],
                               getStart(cross, design_space[[1]], design_space[[2]], delta_val, eff_fun),
                               getPar(cross))
  p <- plot_augment_region(eff_fun, cross, delta_range, cand_points_reg, design_space)
  plot(p)
  .make_augment_region(cand_points_reg, delta_val, eff_fun, "x", p)
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
get_dsaugment_region <- function(init_design, alpha, model, parameters, par_values, par_int,
                                 design_space, calc_optimal_design, weight_fun = function(x) 1,
                                 delta_val = NULL) {
  grad <- gradient(model, parameters, par_values, weight_fun)
  grad22 <- gradient22(model, parameters, par_values, par_int, weight_fun)
  inf_mat_1 <- inf_mat(grad, init_design)
  inf_mat_22 <- inf_mat(grad22, init_design)
  sens_1 <- dsens(grad, inf_mat_1)
  sens_22 <- dsens(grad22, inf_mat_22)
  dseff <- function(x) (1 - alpha) * ((1 + alpha * sens_1(x) / (1 - alpha)) / (1 + alpha * sens_22(x) / (1 - alpha)))^(1 / length(par_int))
  delta_range <- c(findminval(dseff, design_space, 10000),
                   findmaxval(dseff, design_space, 10000))
  if (calc_optimal_design) {
    optimal_design <- opt_des("Ds-Optimality", model, parameters, par_values, design_space, par_int = par_int, weight_fun = weight_fun)
    inf_mat_opt <- inf_mat(grad, optimal_design$optdes)
    if (length(inf_mat_1[-par_int, -par_int]) == 1) {
      eff_1 <- (inf_mat_opt[-par_int, -par_int] / det(inf_mat_opt) / (inf_mat_1[-par_int, -par_int] / det(inf_mat_1)))^(1 / length(par_int)) * 100
    } else {
      eff_1 <- (det(inf_mat_opt[-par_int, -par_int]) / det(inf_mat_opt) / (det(inf_mat_1[-par_int, -par_int]) / det(inf_mat_1)))^(1 / length(par_int)) * 100
    }
    message(crayon::blue(cli::symbol$info), " The efficiency of the initial design is ", round(eff_1, digits = 2), "%")
  }
  delta_val <- ask_delta(delta_range, delta_val)
  if (is.null(delta_val)) return(NULL)
  cross <- sort(crosspoints(delta_val, dseff, 10000, 10^(-3), design_space[[1]], design_space[[2]]))
  cand_points_reg <- getCross2(cross, design_space[[1]], design_space[[2]],
                               getStart(cross, design_space[[1]], design_space[[2]], delta_val, dseff),
                               getPar(cross))
  p <- plot_augment_region(dseff, cross, delta_range, cand_points_reg, design_space)
  plot(p)
  .make_augment_region(cand_points_reg, delta_val, dseff, "x", p)
}


# --- Unified augment_region object ------------------------------------------

# Constructor for the "augment_region" S3 class.
# region: numeric crosspoints vector (1D) or data.frame of candidates (multi-factor)
# design_vars: "x" for 1D or c("x1","x2",...) for multi-factor
.make_augment_region <- function(region, delta_val, eff_fun, design_vars, plot = NULL) {
  structure(
    list(region      = region,
         delta_val   = delta_val,
         eff_fun     = eff_fun,
         design_vars = design_vars,
         plot        = plot),
    class = "augment_region"
  )
}

#' Print method for augment_region objects
#'
#' @param x An object of class \code{augment_region} returned by \code{get_augment_region}.
#' @param ... Unused.
#' @export
print.augment_region <- function(x, ...) {
  cat(sprintf("Augment candidate region  (delta = %.4f)\n", x$delta_val))
  if (is.numeric(x$region)) {
    # 1D: format as intervals
    n_int <- length(x$region) / 2L
    if (n_int == 0L) {
      cat("  No candidate intervals found.\n")
    } else {
      intervals <- vapply(seq_len(n_int), function(i)
        sprintf("[%.4g, %.4g]", x$region[2L*i-1L], x$region[2L*i]), character(1L))
      cat("  Intervals:", paste(intervals, collapse = ", "), "\n")
    }
  } else {
    # Multi-factor: summarise candidate data frame
    d <- length(x$design_vars)
    cat(sprintf("  %d candidate points  (%d factor%s)\n",
                nrow(x$region), d, if (d > 1L) "s" else ""))
    if (nrow(x$region) > 0L) {
      cat(sprintf("  Efficiency range: [%.4f, %.4f]\n",
                  min(x$region$efficiency), max(x$region$efficiency)))
    }
  }
  invisible(x)
}


# --- Multi-factor augment helpers -------------------------------------------

# Build the augment efficiency function for D / A / I / L criteria.
# Returns a scalar function eff(x) where x is a scalar (1D) or named vector (multi-factor).
.aug_eff_fun <- function(criterion, grad, init_design, alpha, k, matB = NULL) {
  M1 <- inf_mat(grad, init_design)
  if (identical(criterion, "D-Optimality")) {
    s1 <- dsens(grad, M1)
    function(x) (1 - alpha) * (1 + alpha * as.numeric(s1(x)) / (1 - alpha))^(1/k)
  } else {
    ds1   <- dsens(grad, M1)
    is1   <- isens(grad, M1, matB)
    crit1 <- icrit(M1, matB)
    function(x) {
      dv <- as.numeric(ds1(x)); iv <- as.numeric(is1(x))
      (1 - alpha) * crit1 / (crit1 - alpha * iv / (1 - alpha + alpha * dv))
    }
  }
}


# Heatmap of the augment efficiency function for d=2 design spaces.
# Returns a ggplot; the candidate region (efficiency >= delta_val) is above the white contour.
.plot_aug_heatmap_2d <- function(design_space, eff_fn, delta_val, init_design) {
  efficiency <- NULL   # avoid R CMD check NOTE on ggplot2 aes variable
  dvars   <- names(design_space)
  n_grid  <- 40L
  grid_df <- do.call(expand.grid, lapply(design_space, function(ds)
    seq(ds[1L], ds[2L], length.out = n_grid)))
  grid_df$efficiency <- apply(grid_df, 1L, function(x)
    as.numeric(eff_fn(stats::setNames(x, dvars))))

  dc  <- intersect(dvars, names(init_design))
  x1s <- rlang::sym(dvars[1L]); x2s <- rlang::sym(dvars[2L])
  ggplot2::ggplot(grid_df, ggplot2::aes(x = !!x1s, y = !!x2s, fill = efficiency)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_viridis_c(name = "Efficiency", option = "plasma") +
    ggplot2::geom_contour(ggplot2::aes(z = efficiency, fill = NULL),
                          breaks = delta_val, colour = "white", linewidth = 0.7) +
    ggplot2::geom_point(data = init_design[, dc, drop = FALSE],
                        ggplot2::aes(x = !!x1s, y = !!x2s, fill = NULL),
                        colour = "cyan", size = 3.5, shape = 17) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = dvars[1L], y = dvars[2L],
                  caption = sprintf(
                    "White contour: candidate region boundary (delta = %.4f). Triangles: current design.",
                    delta_val))
}


# Sample candidate points from the design space via LHS and return those
# with efficiency >= delta_val.  Includes an 'efficiency' column.
.sample_aug_candidates <- function(eff_fn, design_space, delta_val, n_lhs = 5000L) {
  dvars   <- names(design_space)
  pts     <- lhs_sample(n_lhs, design_space)
  eff_vec <- apply(pts, 1L, function(x) as.numeric(eff_fn(stats::setNames(x, dvars))))
  keep    <- eff_vec >= delta_val
  cands   <- as.data.frame(pts[keep, , drop = FALSE])
  cands$efficiency <- eff_vec[keep]
  cands
}


# Interactive point selection for multi-factor augment.
# Shows the candidate list, then prompts for each coordinate and weight in a loop.
.select_new_points_mf_interactive <- function(design_vars, design_space, eff_fn, delta_val) {
  if (!interactive())
    stop("Supply new_points for non-interactive use.", call. = FALSE)
  result <- data.frame(matrix(ncol = length(design_vars) + 1L, nrow = 0L))
  names(result) <- c(design_vars, "Weight")
  repeat {
    cat(crayon::blue(cli::symbol$info),
        " Enter coordinates for a new point, or press Enter with no value to finish.\n")
    coords <- numeric(length(design_vars))
    valid  <- TRUE
    for (v in design_vars) {
      raw <- readline(prompt = paste0("  ", v, " [", design_space[[v]][1L],
                                      ", ", design_space[[v]][2L], "]: "))
      if (nchar(trimws(raw)) == 0L) { valid <- FALSE; break }
      val <- suppressWarnings(as.numeric(raw))
      if (is.na(val) || val < design_space[[v]][1L] || val > design_space[[v]][2L]) {
        cat(crayon::red(cli::symbol$cross), " Value out of range or not numeric. Point skipped.\n")
        valid <- FALSE; break
      }
      coords[match(v, design_vars)] <- val
    }
    if (!valid) break
    names(coords) <- design_vars
    e <- as.numeric(eff_fn(coords))
    if (e < delta_val) {
      cat(crayon::red(cli::symbol$cross),
          sprintf(" Efficiency %.4f < delta_val (%.4f). Point outside candidate region.\n",
                  e, delta_val))
      next
    }
    cat(crayon::green(cli::symbol$tick),
        sprintf(" Efficiency: %.4f >= %.4f\n", e, delta_val))
    w_raw <- readline(prompt = "  Weight (positive number): ")
    w     <- suppressWarnings(as.numeric(w_raw))
    if (is.na(w) || w <= 0) {
      cat(crayon::red(cli::symbol$cross), " Weight must be a positive number. Point skipped.\n")
      next
    }
    new_row            <- as.data.frame(as.list(c(coords, Weight = w)))
    names(new_row)     <- c(design_vars, "Weight")
    result             <- rbind(result, new_row)
  }
  result
}


# Validate a multi-factor new_points data.frame:
# - must have all design variable columns + Weight
# - all weights positive
# - all points within design_space
# - efficiency of each point >= delta_val
# Returns new_points (possibly with Weight normalised to sum = 1).
.validate_new_points_mf <- function(new_points, design_vars, design_space, eff_fn, delta_val) {
  expected <- c(design_vars, "Weight")
  missing  <- setdiff(expected, names(new_points))
  if (length(missing) > 0L)
    stop("new_points must have columns: ", paste(expected, collapse = ", "),
         " (missing: ", paste(missing, collapse = ", "), ")", call. = FALSE)
  if (any(new_points$Weight <= 0))
    stop("All weights in new_points must be positive.", call. = FALSE)
  for (i in seq_len(nrow(new_points))) {
    pt <- unlist(new_points[i, design_vars])
    for (v in design_vars) {
      if (pt[[v]] < design_space[[v]][1L] || pt[[v]] > design_space[[v]][2L])
        stop("Row ", i, ": coordinate ", v, " = ", round(pt[[v]], 4),
             " is outside design space [", design_space[[v]][1L], ", ",
             design_space[[v]][2L], "].", call. = FALSE)
    }
    e <- as.numeric(eff_fn(pt))
    if (e < delta_val)
      stop("Row ", i, ": efficiency ", round(e, 4), " < delta_val (", round(delta_val, 4),
           "). Point is outside the candidate region.", call. = FALSE)
  }
  new_points
}


# --- Other private functions ------------------------------------------------

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
  design_vars   <- detect_design_vars(model, char_vars)
  ext_char_vars <- c(char_vars, design_vars)
  arglist <- lapply(ext_char_vars, function(x) NULL)
  f  <- as.function(append(stats::setNames(arglist, ext_char_vars), quote({})))
  f1 <- stats::deriv(model, char_vars[-par_int], f)
  f2 <- function(x_val) {
    attr(do.call(f1, as.list(c(values, x_val))), "gradient")
  }
  f3 <- function(x) f2(x) * weight_fun(x)
  attr(f3, "design_vars") <- design_vars
  f3
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
#' @param gridlength Number of points in the initial grid used to bracket roots
#' @param tol Tolerance for root refinement passed to \code{uniroot}
#' @param xmin Minimum of the space of the design
#' @param xmax Maximum of the space of the design
#'
#' @return A numeric vector of crosspoints that define the candidate points region
#'
crosspoints <- function(val, sens, gridlength, tol, xmin, xmax) {
  f <- function(x) sens(x) - val
  grid <- seq(xmin, xmax, length.out = gridlength)
  fvals <- purrr::map_dbl(grid, f)
  # Brackets: consecutive grid cells where the function changes sign
  brackets <- which(fvals[-gridlength] * fvals[-1] < 0)
  if (length(brackets) == 0) return(numeric(0))
  # Refine each bracket to the exact root with uniroot
  roots <- vapply(brackets, function(i) {
    tryCatch(
      stats::uniroot(f, lower = grid[i], upper = grid[i + 1L], tol = tol)$root,
      error = function(e) NA_real_
    )
  }, numeric(1))
  sort(roots[!is.na(roots)])
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
