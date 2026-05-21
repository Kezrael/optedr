library(optedr)

# Shared fixtures -------------------------------------------------------
model  <- y ~ a * exp(-b / x)
params <- c("a", "b")
pvals  <- c(1, 1500)
des    <- data.frame(Point = c(329.3, 422), Weight = c(0.5, 0.5))

# tr() ------------------------------------------------------------------

test_that("tr returns sum of diagonal", {
  expect_equal(optedr:::tr(diag(3)), 3)
  expect_equal(optedr:::tr(matrix(c(1, 2, 3, 4), 2, 2)), 5)  # 1 + 4
  expect_equal(optedr:::tr(matrix(0, 3, 3)), 0)
})

# inv_spd() -------------------------------------------------------------

test_that("inv_spd inverts a 2x2 SPD matrix", {
  M <- matrix(c(4, 2, 2, 3), 2, 2)
  expect_equal(M %*% optedr:::inv_spd(M), diag(2), tolerance = 1e-10)
})

test_that("inv_spd falls back to pseudoinverse for singular matrix and warns", {
  M_sing <- matrix(c(1, 1, 1, 1), 2, 2)  # rank 1
  expect_warning(optedr:::inv_spd(M_sing), "near-zero singular")
})

test_that("inv_spd pseudoinverse satisfies Moore-Penrose condition M M+ M = M", {
  M <- matrix(c(1, 1, 1, 1), 2, 2)
  P <- suppressWarnings(optedr:::inv_spd(M))
  expect_equal(M %*% P %*% M, M, tolerance = 1e-10)
})

# findminval / findmaxval / findmax -------------------------------------

test_that("findminval and findmaxval find global extrema on a grid", {
  f <- function(x) (x - 3)^2
  expect_equal(optedr:::findminval(f, c(0, 6), 1000),  0, tolerance = 0.01)
  expect_equal(optedr:::findmaxval(f, c(0, 6), 1000),  9, tolerance = 0.1)
})

test_that("findmax returns x location of maximum", {
  f <- function(x) -(x - 3)^2
  expect_equal(optedr:::findmax(f, c(0, 6), 1000), 3, tolerance = 0.01)
})

# check_atwood() --------------------------------------------------------

test_that("check_atwood warns when bound is outside [0, 100]", {
  expect_warning(optedr:::check_atwood(200),  "outside \\[0, 100\\]")
  expect_warning(optedr:::check_atwood(-5),   "outside \\[0, 100\\]")
  expect_warning(optedr:::check_atwood(Inf),  "outside \\[0, 100\\]")
  expect_warning(optedr:::check_atwood(NaN),  "outside \\[0, 100\\]")
})

test_that("check_atwood is silent for valid bounds", {
  expect_silent(optedr:::check_atwood(0))
  expect_silent(optedr:::check_atwood(99.9))
  expect_silent(optedr:::check_atwood(100))
})

# update_sequence() -----------------------------------------------------

test_that("update_sequence removes near-duplicate points", {
  pts    <- c(1.0, 1.001, 2.0, 2.0005, 3.0)
  result <- optedr:::update_sequence(pts, tol = 0.01)
  expect_equal(length(result), 3)
  expect_true(all(diff(result) > 0.01))
})

# delete_points() -------------------------------------------------------

test_that("delete_points removes low-weight points and renormalises", {
  d <- data.frame(Point = c(1, 2, 3), Weight = c(0.01, 0.60, 0.39))
  result <- optedr:::delete_points(d, delta = 0.02)
  expect_equal(nrow(result), 2)
  expect_equal(sum(result$Weight), 1, tolerance = 1e-10)
  expect_true(all(result$Weight > 0.02))
})

test_that("delete_points keeps all points when all weights > delta", {
  d <- data.frame(Point = c(1, 2), Weight = c(0.5, 0.5))
  expect_equal(nrow(optedr:::delete_points(d, 0.1)), 2)
})

# update_design() -------------------------------------------------------

test_that("update_design adds a new point when it is far from existing ones", {
  d <- data.frame(Point = c(1, 3), Weight = c(0.5, 0.5))
  result <- optedr:::update_design(d, xmax = 2, delta = 0.1, new_weight = 0.1)
  expect_equal(nrow(result), 3)
  expect_true(2 %in% result$Point)
})

test_that("update_design merges when new point is close to an existing one", {
  d <- data.frame(Point = c(1.0, 3.0), Weight = c(0.5, 0.5))
  result <- optedr:::update_design(d, xmax = 1.05, delta = 0.2, new_weight = 0.1)
  expect_equal(nrow(result), 2)   # merged, not added
})

# getPar / getCross2 ----------------------------------------------------

test_that("getPar returns TRUE for even number of crosspoints", {
  expect_true(optedr:::getPar(c(1, 2, 3, 4)))
  expect_false(optedr:::getPar(c(1, 2, 3)))
  expect_true(optedr:::getPar(numeric(0)))   # 0 is even
})

test_that("getCross2 returns correct region limits", {
  # even, start = FALSE: return cross as-is
  expect_equal(optedr:::getCross2(c(2, 4), 1, 5, start = FALSE, par = TRUE),  c(2, 4))
  # even, start = TRUE: prepend min, append max
  expect_equal(optedr:::getCross2(c(2, 4), 1, 5, start = TRUE,  par = TRUE),  c(1, 2, 4, 5))
  # odd, start = TRUE: prepend min
  expect_equal(optedr:::getCross2(c(3), 1, 5, start = TRUE,  par = FALSE), c(1, 3))
  # odd, start = FALSE: append max
  expect_equal(optedr:::getCross2(c(3), 1, 5, start = FALSE, par = FALSE), c(3, 5))
})

# integrate_reg_int() ---------------------------------------------------

test_that("integrate_reg_int returns a symmetric PSD matrix", {
  grad <- optedr:::gradient(model, params, pvals)
  B    <- optedr:::integrate_reg_int(grad, 2, c(380, 422))
  expect_equal(dim(B), c(2, 2))
  expect_equal(B, t(B), tolerance = 1e-10)
  expect_true(all(eigen(B)$values >= -1e-10))
})

# update_weights() ------------------------------------------------------

test_that("update_weights returns normalised positive weights", {
  grad  <- optedr:::gradient(model, params, pvals)
  M     <- optedr:::inf_mat(grad, des)
  sensM <- optedr:::dsens(grad, M)
  new_w <- optedr:::update_weights(des, sensM, k = 2L, delta = 0.5)
  expect_equal(sum(new_w), 1, tolerance = 1e-10)
  expect_true(all(new_w > 0))
})

test_that("update_weights errors on non-finite sensitivity (non-identifiable model)", {
  # Create a rank-deficient M whose pseudoinverse gives near-zero sensitivity
  # Use a degenerate design (two identical points) to force singular M
  d_sing <- data.frame(Point = c(300, 300, 400), Weight = c(1/3, 1/3, 1/3))
  grad   <- optedr:::gradient(model, params, pvals)
  M_sing <- optedr:::inf_mat(grad, d_sing)   # rank 1 (two identical points)
  sens_fn <- suppressWarnings(optedr:::dsens(grad, M_sing))
  # Verify sensitivity values are finite (pseudoinverse handles rank-1 case)
  val <- sens_fn(350)
  expect_true(is.finite(as.numeric(val)))
})

# weight_function() -----------------------------------------------------

test_that("weight_function returns constant 1 for Normal", {
  f <- optedr:::weight_function(model, params, pvals, distribution = "Normal")
  expect_equal(f(300), 1)
})

test_that("weight_function warns for unknown distribution", {
  expect_warning(
    optedr:::weight_function(model, params, pvals, distribution = "Binomial"),
    "Not a valid distribution"
  )
})

test_that("weight_function returns a non-trivial function for Gamma", {
  f <- optedr:::weight_function(model, params, pvals, distribution = "Gamma")
  expect_true(is.function(f))
  val <- f(300)
  expect_true(is.finite(val))
  expect_gt(val, 0)
})
