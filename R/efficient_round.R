#' Efficient Round
#'
#' @description
#' Takes an approximate design, and a number of points and converts the design to
#' an approximate design. It uses the multiplier (n - l/2) and evens the total
#' number of observations afterwards.
#'
#' @param design a dataframe with columns "Point" and "Weight" that represents a design
#' @param n an integer that represents the desired number of observations of the exact design
#' @param tol optional parameter for the consideration of an integer in the rounding process
#' @param seed optional integer seed for reproducibility. When the rounded weights sum to less
#'   than \code{n}, a random tie-breaking step is used; supplying \code{seed} makes that step
#'   deterministic by calling \code{set.seed(seed)} immediately before it. \code{NULL} (default)
#'   leaves the global RNG state unchanged.
#'
#' @return a data.frame with columns "Point" and "Weight" representing an exact design
#' with n observations
#' @export
#'
#' @examples
#' design_test <- data.frame("Point" = seq(1, 5, length.out = 7),
#'          "Weight" = c(0.1, 0.0001, 0.2, 0.134, 0.073, 0.2111, 0.2818))
#'
#' efficient_round(design_test, 20)
#'
#' exact_design <- efficient_round(design_test, 21)
#' aprox_design <- exact_design
#' aprox_design$Weight <- aprox_design$Weight/sum(aprox_design$Weight)
#'
#' # Reproducible tie-breaking
#' efficient_round(design_test, 20, seed = 42)
efficient_round <- function(design, n, tol = 0.00001, seed = NULL){
  if(n%%1!=0 | n <= 0){
    stop("n must be a possitive integer")
  }
  else if(!identical(names(design), c("Point", "Weight"))){
    stop("the design must be a data.frame with 'Point' and 'Weight' columns")
  }
  else{
    l <- nrow(design)
    app_weights <- design[["Weight"]] * (n - l/2)
    candidates_to_increase <- min(abs(c(app_weights %% 1, app_weights %% 1 - 1))) < tol
    app_weights[candidates_to_increase] <- round(app_weights[candidates_to_increase])
    app_weights <- ceiling(app_weights)
    if(sum(app_weights) > n){
      message(crayon::blue(cli::symbol$info), " The proposed size of rounding is greater than n: \n", paste(app_weights, collapse = " "))
      dif <- app_weights - design[["Weight"]] * n
      decrease_order <- order(dif, decreasing = TRUE)
      index <- 1
      while(sum(app_weights) > n){
        if(app_weights[decrease_order[index]] > 1)
          app_weights[decrease_order[index]] <- app_weights[decrease_order[index]] - 1
        index <- index + 1
        if(index > l){
          dif <- app_weights - design[["Weight"]] * n
          decrease_order <- order(dif, decreasing = TRUE)
          index <- 1
          while(sum(app_weights) > n){
            app_weights[decrease_order[index]] <- app_weights[decrease_order[index]] - 1
            index <- index + 1
          }
        }
      }
      message(crayon::blue(cli::symbol$info), " An alternative with size n is returned")
    }
    else if(sum(app_weights) < n){
      if (!is.null(seed)) set.seed(seed)
      candidates_to_increase <- sample(candidates_to_increase)
      index <- 1
      while(sum(app_weights) < n){
        message(crayon::blue(cli::symbol$info), " The proposed size of rounding is smaller than n: \n", paste(app_weights, collapse = " "))
        app_weights[candidates_to_increase[index]] <- app_weights[candidates_to_increase[index]] + 1
        index <- index + 1
        if(index > length(candidates_to_increase)){
          dif <- app_weights - design[["Weight"]] * n
          increase_order <- order(dif, decreasing = FALSE)
          index <- 1
          while(sum(app_weights) < n){
            app_weights[increase_order[index]] <- app_weights[increase_order[index]] + 1
            index <- index + 1
          }
        }
      }
      message(crayon::blue(cli::symbol$info), " An alternative with size n is returned")
    }
    design[["Weight"]] <- app_weights
    return(design)
  }
}



# Returns a human-readable order-of-magnitude time estimate for 2^k evaluations.
estimate_combo_time <- function(k) {
  n <- 2^k
  if      (n <   1e4) "< 1 second"
  else if (n <   1e5) "a few seconds"
  else if (n <   5e5) "tens of seconds"
  else if (n <   1e7) "minutes"
  else if (n <   1e8) "tens of minutes"
  else                "hours or more"
}


#' Combinatorial round
#'
#' @description
#' Given an approximate design and a number of points, computes all the possible combinations of
#' roundings of each point to the nearest integer, keeps the ones that amount to the requested number of points,
#' and returns the one with the best value for the criterion function.
#'
#' The search is exhaustive and requires \eqn{2^k} evaluations where \eqn{k} is the number of
#' support points.  For designs with more than \code{max_support} support points the function
#' requests confirmation (interactive sessions) or stops with an informative error
#' (non-interactive sessions), unless \code{ask = FALSE}.
#'
#' @param design either a dataframe with the design to round, or an object of class "optdes". If the former,
#' the criterion, model and parameters must be specified. The dataframe should have two columns:
#'   * \code{Point} contains the support points of the design.
#'   * \code{Weight} contains the corresponding weights of the \code{Point}s.
#' @param n integer with the desired number of points of the resulting design.
#' @param criterion character variable with the chosen optimality criterion. Can be one of the following:
#'   * 'D-Optimality'
#'   * 'Ds-Optimality'
#'   * 'A-Optimality'
#'   * 'I-Optimality'
#'   * 'L-Optimality'
#' @param model formula describing the model. Must use x as the variable.
#' @param parameters character vector with the parameters of the models, as written in the \code{formula}.
#' @param par_values numeric vector with the parameters nominal values, in the same order as given in \code{parameters}.
#' @param weight_fun optional one variable function that represents the square of the structure of variance, in case of heteroscedastic variance of the response.
#' @param par_int optional numeric vector with the index of the \code{parameters} of interest for Ds-optimality.
#' @param reg_int optional numeric vector with the ranges of integration, for I-optimality.
#' @param matB optional matrix of dimensions k x k, for L-optimality.
#' @param max_support integer. Number of support points above which the function triggers the
#'   confirmation mechanism. Default is 15 (\eqn{2^{15} \approx 32\,000} combinations).
#' @param ask logical. If \code{TRUE} (default) and the design exceeds \code{max_support}:
#'   in an interactive session the user is prompted; in a non-interactive session an error is raised.
#'   Set \code{ask = FALSE} to skip confirmation in scripts or pipelines (a message is still emitted).
#'
#' @return A data.frame with the rounded design to n number of points, or \code{NULL} invisibly
#'   if the user declines the confirmation prompt.
#' @export
#'
#' @examples
#' aprox_design <- opt_des("D-Optimality", y ~ a * exp(-b / x), c("a", "b"), c(1, 1500), c(212, 422))
#' combinatorial_round(aprox_design, 27)
combinatorial_round <- function(design, n,
                                criterion  = NULL,
                                model      = NULL,
                                parameters = NULL,
                                par_values = NULL,
                                weight_fun = function(x) 1,
                                par_int    = NULL,
                                reg_int    = NULL,
                                matB       = NULL,
                                max_support = 15L,
                                ask         = TRUE) {
  # --- Step 1: extract design_df (needed to know k before the guard) --------
  if (inherits(design, "optdes")) {
    design_df  <- design$optdes
    criterion  <- design$criterion
    crit_funct <- attr(design, "crit_function")
  } else {
    design_df <- design
    design_df$Weight <- design_df$Weight / sum(design_df$Weight)
  }

  # --- Step 2: large-design guard -------------------------------------------
  k <- nrow(design_df)
  if (k > max_support) {
    n_combos <- 2^k
    time_est <- estimate_combo_time(k)
    info <- paste0(
      "combinatorial_round: the design has ", k, " support points",
      " (max_support = ", max_support, ").\n",
      "  Combinations to evaluate : ",
      format(n_combos, big.mark = ",", scientific = FALSE),
      "  (2^", k, ")\n",
      "  Estimated time           : ", time_est, " (hardware-dependent)\n",
      "  Fast alternative         : efficient_round(design, n)"
    )
    if (ask) {
      if (!interactive()) {
        stop(info, "\n",
             "Set ask = FALSE to proceed without confirmation.",
             call. = FALSE)
      }
      cat(info, "\n")
      ans <- readline("Continue? [y/N]: ")
      if (!tolower(trimws(ans)) %in% c("y", "yes")) {
        message("Aborted. Use efficient_round(design, n) for a fast approximate result.")
        return(invisible(NULL))
      }
    } else {
      message(info)
    }
  }

  # --- Step 3: build criterion function (data.frame path only) --------------
  if (!inherits(design, "optdes")) {
    if (is.null(criterion))
      stop(crayon::red(cli::symbol$cross),
           " criterion must be specified or an 'optdes' object must be provided",
           call. = FALSE)
    grad <- gradient(model, parameters, par_values, weight_fun)
    if (identical(criterion, "D-Optimality")) {
      crit_funct <- function(design_df) {
        M <- inf_mat(grad, design_df); dcrit(M, length(parameters))
      }
    } else if (identical(criterion, "Ds-Optimality")) {
      crit_funct <- function(design_df) {
        M <- inf_mat(grad, design_df); dscrit(M, par_int)
      }
    } else if (identical(criterion, "A-Optimality")) {
      crit_funct <- function(design_df) {
        M <- inf_mat(grad, design_df); icrit(M, diag(length(parameters)))
      }
    } else if (identical(criterion, "I-Optimality")) {
      if (is.null(reg_int))
        stop(crayon::red(cli::symbol$cross),
             " reg_int must be specified for I-Optimality", call. = FALSE)
      matB <- integrate_reg_int(grad, length(par_values), reg_int)
      crit_funct <- function(design_df) {
        M <- inf_mat(grad, design_df); icrit(M, matB)
      }
    } else if (identical(criterion, "L-Optimality")) {
      crit_funct <- function(design_df) {
        M <- inf_mat(grad, design_df); icrit(M, matB)
      }
    } else {
      stop(crayon::red(cli::symbol$cross),
           " Invalid criterion. Choose from: D-Optimality, Ds-Optimality, ",
           "A-Optimality, I-Optimality, L-Optimality", call. = FALSE)
    }
  }

  # --- Step 4: exhaustive search --------------------------------------------
  prod_weights    <- design_df$Weight * n
  combinations_df <- data.frame(
    matrix(c(ceiling(prod_weights), floor(prod_weights)), nrow = 2, byrow = TRUE)
  )
  combinations_df        <- expand.grid(data = combinations_df)
  combinations_df$n      <- rowSums(combinations_df)
  combinations_df        <- combinations_df[combinations_df$n == n, ]
  combinations_df$crit   <- purrr::map_dbl(
    seq_len(nrow(combinations_df)),
    function(x) {
      temp <- data.frame(
        "Point"  = design_df$Point,
        "Weight" = unlist(combinations_df[x, seq_len(ncol(combinations_df) - 1L)]) / n
      )
      crit_funct(temp)
    }
  )
  opt_comb <- combinations_df[which.min(combinations_df$crit), ]
  output   <- data.frame(
    "Point"  = design_df$Point,
    "Weight" = unlist(opt_comb[1L, seq_len(ncol(combinations_df) - 2L)])
  )
  rownames(output) <- NULL
  output
}
