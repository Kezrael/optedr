library(optedr)

# All tests are error-only: fast, no algorithm execution.
# Shared fixtures -------------------------------------------------------
model  <- y ~ a * exp(-b / x)
params <- c("a", "b")
pvals  <- c(1, 1500)
dspace <- c(212, 422)

# Criterion -------------------------------------------------------------

test_that("invalid criterion strings error", {
  expect_error(opt_des("E-Optimality",  model, params, pvals, dspace))
  expect_error(opt_des("d-optimality",  model, params, pvals, dspace))
  expect_error(opt_des("",              model, params, pvals, dspace))
})

# Model -----------------------------------------------------------------

test_that("non-formula model errors", {
  expect_error(opt_des("D-Optimality", "a * exp(-b/x)", params, pvals, dspace))
  expect_error(opt_des("D-Optimality", 42,              params, pvals, dspace))
})

test_that("model with parameters not matching formula errors", {
  expect_error(opt_des("D-Optimality", y ~ a * exp(-b / x), c("a", "c"), pvals, dspace))
})

test_that("model without x variable errors", {
  expect_error(opt_des("D-Optimality", y ~ a + b, params, pvals, dspace))
})

# par_values ------------------------------------------------------------

test_that("non-numeric par_values errors", {
  expect_error(opt_des("D-Optimality", model, params, c("1", "1500"), dspace))
})

test_that("mismatched parameters / par_values length errors", {
  # Three values for two parameters -> error
  expect_error(opt_des("D-Optimality", model, params, c(1, 1500, 3), dspace))
  # Note: c(1) is the sentinel default and is expanded internally to rep(1, k),
  # so it does NOT error — that behaviour is intentional and tested elsewhere.
})

# design_space ----------------------------------------------------------

test_that("design_space with wrong length errors", {
  expect_error(opt_des("D-Optimality", model, params, pvals, c(212)))
  expect_error(opt_des("D-Optimality", model, params, pvals, c(212, 300, 422)))
})

test_that("non-numeric design_space errors", {
  expect_error(opt_des("D-Optimality", model, params, pvals, c("a", "b")))
})

# init_design -----------------------------------------------------------

test_that("non-dataframe init_design errors", {
  expect_error(
    opt_des("D-Optimality", model, params, pvals, dspace,
            init_design = list(Point = c(212, 422), Weight = c(0.5, 0.5)))
  )
})

test_that("init_design with wrong column names errors", {
  expect_error(
    opt_des("D-Optimality", model, params, pvals, dspace,
            init_design = data.frame(X = c(212, 422), Weight = c(0.5, 0.5)))
  )
})

# Numeric thresholds ----------------------------------------------------

test_that("non-numeric delete_thresh errors", {
  expect_error(opt_des("D-Optimality", model, params, pvals, dspace,
                       delete_thresh = "big"))
})

test_that("delete_thresh at boundary values errors", {
  expect_error(opt_des("D-Optimality", model, params, pvals, dspace, delete_thresh = 0))
  expect_error(opt_des("D-Optimality", model, params, pvals, dspace, delete_thresh = 1))
})

test_that("delta at boundary values errors", {
  expect_error(opt_des("D-Optimality", model, params, pvals, dspace, delta = 0))
  expect_error(opt_des("D-Optimality", model, params, pvals, dspace, delta = 1))
})

test_that("non-positive tol or tol2 errors", {
  expect_error(opt_des("D-Optimality", model, params, pvals, dspace, tol  = 0))
  expect_error(opt_des("D-Optimality", model, params, pvals, dspace, tol  = -1))
  expect_error(opt_des("D-Optimality", model, params, pvals, dspace, tol2 = 0))
})

# Ds-specific -----------------------------------------------------------

test_that("Ds-Optimality without par_int errors", {
  expect_error(opt_des("Ds-Optimality", model, params, pvals, dspace))
})

test_that("Ds-Optimality with non-integer par_int errors", {
  expect_error(opt_des("Ds-Optimality", model, params, pvals, dspace, par_int = c(1.5)))
})

test_that("Ds-Optimality with out-of-bounds par_int errors", {
  expect_error(opt_des("Ds-Optimality", model, params, pvals, dspace, par_int = c(5L)))
})

test_that("Ds-Optimality with par_int covering all parameters errors", {
  expect_error(opt_des("Ds-Optimality", model, params, pvals, dspace, par_int = c(1L, 2L)))
})

# I-specific ------------------------------------------------------------

test_that("I-Optimality without reg_int errors", {
  expect_error(opt_des("I-Optimality", model, params, pvals, dspace))
})

test_that("I-Optimality with wrong-length reg_int errors", {
  expect_error(opt_des("I-Optimality", model, params, pvals, dspace, reg_int = c(380)))
})

test_that("I-Optimality with non-numeric reg_int errors", {
  expect_error(opt_des("I-Optimality", model, params, pvals, dspace,
                       reg_int = c("380", "422")))
})

# L-specific ------------------------------------------------------------

test_that("L-Optimality without matB errors", {
  expect_error(opt_des("L-Optimality", model, params, pvals, dspace))
})

test_that("L-Optimality with wrong-size matB errors", {
  expect_error(opt_des("L-Optimality", model, params, pvals, dspace, matB = matrix(1, 3, 3)))
  expect_error(opt_des("L-Optimality", model, params, pvals, dspace, matB = matrix(1:6, 2, 3)))
})

# weight_fun ------------------------------------------------------------

test_that("non-function weight_fun errors", {
  expect_error(opt_des("D-Optimality", model, params, pvals, dspace, weight_fun = 1))
  expect_error(opt_des("D-Optimality", model, params, pvals, dspace, weight_fun = "id"))
})
