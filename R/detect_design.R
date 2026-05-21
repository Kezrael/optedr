# Infrastructure for auto-detecting design variables and supporting multi-factor designs.
# Single-factor models use the variable name "x"; multi-factor models use "x1", "x2", ...
# Detection is performed by inspecting the formula and excluding the response and parameters.


#' Detect design variables from a model formula
#'
#' @description
#' Inspects the formula to identify which symbols are design variables (as opposed to
#' parameters or the response).  Enforces the naming convention: \code{"x"} for
#' single-factor models and \code{"x1"}, \code{"x2"}, \ldots for multi-factor models.
#'
#' @param model A formula describing the model.
#' @param parameters Character vector of parameter names (excluded from design variables).
#'
#' @return Character vector of design variable names, sorted in numerical order for
#'   multi-factor models (e.g. \code{c("x1", "x2", "x10")}).
#'
detect_design_vars <- function(model, parameters) {
  response  <- as.character(model[[2L]])
  all_vars  <- all.vars(model)
  remaining <- setdiff(all_vars, c(response, parameters))

  has_x   <- "x" %in% remaining
  x_multi <- grep("^x[0-9]+$", remaining, value = TRUE)

  # Sort numerically: x1 < x2 < x10 (not lexicographic x1 < x10 < x2)
  if (length(x_multi) > 0L)
    x_multi <- x_multi[order(as.integer(sub("^x", "", x_multi)))]

  if (has_x && length(x_multi) > 0L)
    stop(
      "The model formula mixes the single-factor variable 'x' with multi-factor ",
      "variables 'x1', 'x2', ... — use one convention or the other.",
      call. = FALSE
    )

  if (!has_x && length(x_multi) == 0L)
    stop(
      "No design variable found in the formula. ",
      "Use 'x' for single-factor models or 'x1', 'x2', ... for multi-factor models.",
      call. = FALSE
    )

  if (has_x) "x" else x_multi
}


# Returns TRUE when design_vars has more than one element (multi-factor model).
is_multifactor <- function(design_vars) length(design_vars) > 1L


# Returns the coordinate column names of a design data.frame (everything except "Weight").
coord_cols <- function(design) setdiff(names(design), "Weight")


# Renames "Point" to the design variable name for single-factor user-supplied designs,
# enabling backward-compatible use of data.frame(Point, Weight).
normalize_design_cols <- function(design, design_vars) {
  if ("Point" %in% names(design) && length(design_vars) == 1L)
    names(design)[names(design) == "Point"] <- design_vars[1L]
  design
}


# Latin Hypercube Sample: n points in d dimensions.
# design_space: named list with one element c(min, max) per factor.
# Returns an n x d numeric matrix with column names matching names(design_space).
lhs_sample <- function(n, design_space) {
  d     <- length(design_space)
  nms   <- names(design_space)
  perms <- replicate(d, sample.int(n))                        # n x d stratification
  u     <- (perms - 1L + matrix(stats::runif(n * d), n, d)) / n
  for (j in seq_len(d))
    u[, j] <- design_space[[j]][1L] + u[, j] * diff(design_space[[j]])
  colnames(u) <- nms
  u
}


# Coerce a user-supplied design_space to a canonical named list.
# Accepts c(min, max) for 1D (for backward compat) or a named list for multi-factor.
# design_vars: character vector from detect_design_vars().
canonicalise_design_space <- function(design_space, design_vars) {
  if (is.list(design_space)) {
    if (!setequal(names(design_space), design_vars))
      stop(
        "names(design_space) must match the design variables detected in the formula (",
        paste(design_vars, collapse = ", "), ").",
        call. = FALSE
      )
    return(design_space[design_vars])          # ensure consistent ordering
  }
  # Numeric vector: only valid for single-factor models
  if (length(design_vars) > 1L)
    stop(
      "design_space must be a named list for multi-factor models, e.g. ",
      "list(x1 = c(0, 10), x2 = c(0, 5)).",
      call. = FALSE
    )
  if (!is.numeric(design_space) || length(design_space) != 2L)
    stop("design_space must be a numeric vector of length 2.", call. = FALSE)
  stats::setNames(list(design_space), design_vars)
}
