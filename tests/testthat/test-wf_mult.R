library(optedr)

# Shared fixtures -------------------------------------------------------
model  <- y ~ a * exp(-b / x)
params <- c("a", "b")
pvals  <- c(1, 1500)
dspace <- c(212, 422)

# Result structure (uses D-opt to avoid repeating the slow call) --------

test_that("opt_des result has all documented components", {
  res <- evaluate_promise(opt_des("D-Optimality", model, params, pvals, dspace))$result

  expect_named(res, c("optdes", "convergence", "sens", "criterion",
                       "crit_value", "atwood"), ignore.order = TRUE)
  expect_s3_class(res, "optdes")
  expect_equal(res$criterion, "D-Optimality")
  expect_true(is.numeric(res$crit_value))
  expect_true(is.numeric(res$atwood))
  expect_gte(res$atwood, 0)
  expect_lte(res$atwood, 100)
  expect_true(!is.null(attr(res, "gradient")))
  expect_true(!is.null(attr(res, "hidden_value")))
  expect_true(!is.null(attr(res, "crit_function")))
})

test_that("print.optdes, summary.optdes and plot.optdes work without error", {
  res <- evaluate_promise(opt_des("D-Optimality", model, params, pvals, dspace))$result
  expect_output(print(res))
  expect_output(summary(res))
  expect_s3_class(plot(res), "ggplot")
})

# max_iter parameter ----------------------------------------------------

test_that("max_iter controls outer iterations: more -> same or better atwood", {
  r5  <- evaluate_promise(opt_des("D-Optimality", model, params, pvals, dspace, max_iter = 3L))$result
  r21 <- evaluate_promise(opt_des("D-Optimality", model, params, pvals, dspace, max_iter = 21L))$result
  expect_gte(r21$atwood, r5$atwood - 1)   # within 1 percentage point
})

# A-Optimality ----------------------------------------------------------

test_that("opt_des A-Optimality returns valid design", {
  res <- evaluate_promise(opt_des("A-Optimality", model, params, pvals, dspace))$result
  expect_s3_class(res, "optdes")
  expect_equal(res$criterion, "A-Optimality")
  expect_equal(sum(res$optdes$Weight), 1, tolerance = 1e-6)
  expect_true(all(res$optdes$Point >= dspace[1] & res$optdes$Point <= dspace[2]))
  expect_gte(res$atwood, 0)
  expect_lte(res$atwood, 100)
})

# I-Optimality ----------------------------------------------------------

test_that("opt_des I-Optimality returns valid design", {
  res <- evaluate_promise(opt_des(
    "I-Optimality", model, params, pvals, dspace, reg_int = c(380, 422)
  ))$result
  expect_s3_class(res, "optdes")
  expect_equal(res$criterion, "I-Optimality")
  expect_equal(sum(res$optdes$Weight), 1, tolerance = 1e-6)
  expect_gte(res$atwood, 0)
  expect_lte(res$atwood, 100)
})

# L-Optimality ----------------------------------------------------------

test_that("opt_des L-Optimality with matB = I returns valid design", {
  res <- evaluate_promise(opt_des(
    "L-Optimality", model, params, pvals, dspace, matB = diag(2)
  ))$result
  expect_s3_class(res, "optdes")
  expect_equal(res$criterion, "L-Optimality")
  expect_equal(sum(res$optdes$Weight), 1, tolerance = 1e-6)
  expect_gte(res$atwood, 0)
  expect_lte(res$atwood, 100)
})

# distribution parameter ------------------------------------------------

test_that("opt_des with distribution = Gamma returns valid design", {
  res <- evaluate_promise(opt_des(
    "D-Optimality", model, params, pvals, dspace, distribution = "Gamma"
  ))$result
  expect_s3_class(res, "optdes")
  expect_equal(sum(res$optdes$Weight), 1, tolerance = 1e-6)
})

# Non-identifiable model ------------------------------------------------

test_that("non-identifiable model (A-opt) warns about degenerate Atwood bound", {
  expect_warning(
    suppressMessages(opt_des(
      "A-Optimality",
      y ~ (w * c * k * x) / ((1 - x) * (1 + (c - 1) * k * x)),
      c("w", "c", "k"), c(1, 10, 0.5), c(0.05, 0.8)
    )),
    "outside \\[0, 100\\]"
  )
})

test_that("non-identifiable model (D-opt) errors with informative message", {
  expect_error(
    suppressWarnings(opt_des(
      "D-Optimality",
      y ~ (w * c * k * x) / ((1 - x) * (1 + (c - 1) * k * x)),
      c("w", "c", "k"), c(1, 10, 0.5), c(0.05, 0.8)
    )),
    "identifiable"
  )
})

# crit_function attribute -----------------------------------------------

test_that("crit_function attribute evaluates correctly on the optimal design", {
  res      <- evaluate_promise(opt_des("D-Optimality", model, params, pvals, dspace))$result
  crit_fn  <- attr(res, "crit_function")
  crit_val <- crit_fn(res$optdes)
  expect_equal(crit_val, res$crit_value, tolerance = 1e-6)
})
