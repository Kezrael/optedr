#' Efficient Round
#'
#' @description
#' Takes an approximate design, and a number of points and converts the design to
#' an approximate design. It uses the multiplier (n - l/2) and evens the total
#' number of observations afterwards.
#'
#' @param design a data.frame with columns "Point" and "Weight" that represents a design
#' @param n an integer that represents the desired number of observations of the exact design
#' @param tol optional parameter for the consideration of a integer in the rounding process
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
efficient_round <- function(design, n, tol = 0.00001){
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


