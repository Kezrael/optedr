library(optedr)

# Shared fixtures -------------------------------------------------------
model  <- y ~ a * exp(-b / x)
params <- c("a", "b")
pvals  <- c(1, 1500)
des    <- data.frame(Point = c(329.3, 422), Weight = c(0.5, 0.5))

# gradient() ------------------------------------------------------------

test_that("gradient returns a function", {
  grad <- optedr:::gradient(model, params, pvals)
  expect_true(is.function(grad))
})

test_that("gradient output has length k (number of parameters)", {
  grad <- optedr:::gradient(model, params, pvals)
  g    <- grad(300)
  expect_equal(length(g), 2L)
  expect_true(all(is.finite(g)))
})

test_that("gradient is scaled by weight_fun", {
  grad_1 <- optedr:::gradient(model, params, pvals)
  grad_2 <- optedr:::gradient(model, params, pvals, weight_fun = function(x) 2)
  expect_equal(as.numeric(grad_2(300)), 2 * as.numeric(grad_1(300)), tolerance = 1e-10)
})

test_that("gradient for 3-parameter model has length 3", {
  g3 <- optedr:::gradient(
    y ~ a * exp(-b / x) + c, c("a", "b", "c"), c(1, 1500, 0)
  )
  expect_equal(length(g3(300)), 3L)
})

# inf_mat() -------------------------------------------------------------

test_that("inf_mat returns a k x k symmetric PSD matrix", {
  grad <- optedr:::gradient(model, params, pvals)
  M    <- optedr:::inf_mat(grad, des)
  expect_equal(dim(M), c(2L, 2L))
  expect_equal(M, t(M), tolerance = 1e-10)
  expect_true(all(eigen(M)$values >= -1e-10))
})

test_that("inf_mat result does not depend on point ordering", {
  grad <- optedr:::gradient(model, params, pvals)
  d1   <- data.frame(Point = c(329.3, 422), Weight = c(0.5, 0.5))
  d2   <- data.frame(Point = c(422, 329.3), Weight = c(0.5, 0.5))
  expect_equal(optedr:::inf_mat(grad, d1), optedr:::inf_mat(grad, d2), tolerance = 1e-10)
})

test_that("inf_mat is additive: equal-weight 2-point = sum of 1-point matrices", {
  grad <- optedr:::gradient(model, params, pvals)
  d1   <- data.frame(Point = 329.3, Weight = 1)
  d2   <- data.frame(Point = 422,   Weight = 1)
  M1   <- optedr:::inf_mat(grad, d1)
  M2   <- optedr:::inf_mat(grad, d2)
  M12  <- optedr:::inf_mat(grad, data.frame(Point = c(329.3, 422), Weight = c(0.5, 0.5)))
  expect_equal(M12, 0.5 * M1 + 0.5 * M2, tolerance = 1e-10)
})

test_that("inf_mat works for a single-parameter model (1x1 output)", {
  g1 <- optedr:::gradient(y ~ a * x, c("a"), c(1))
  M  <- optedr:::inf_mat(g1, data.frame(Point = c(1, 2), Weight = c(0.5, 0.5)))
  expect_equal(dim(M), c(1L, 1L))
  expect_true(M[1, 1] > 0)
})

# dsens() ---------------------------------------------------------------

test_that("dsens returns a function giving non-negative values", {
  grad  <- optedr:::gradient(model, params, pvals)
  M     <- optedr:::inf_mat(grad, des)
  sens  <- optedr:::dsens(grad, M)
  expect_true(is.function(sens))
  expect_gte(as.numeric(sens(300)), 0)
})

test_that("dsens at D-optimal support points equals k (Equivalence Theorem)", {
  res  <- evaluate_promise(opt_des("D-Optimality", model, params, pvals, c(212, 422)))$result
  grad <- optedr:::gradient(model, params, pvals)
  M    <- optedr:::inf_mat(grad, res$optdes)
  sens <- optedr:::dsens(grad, M)
  for (x in res$optdes$Point) {
    expect_equal(as.numeric(sens(x)), 2, tolerance = 0.02)
  }
})

# dssens() --------------------------------------------------------------

test_that("dssens returns a function", {
  grad  <- optedr:::gradient(model, params, pvals)
  M     <- optedr:::inf_mat(grad, des)
  sens  <- optedr:::dssens(grad, M, par_int = c(1L))
  expect_true(is.function(sens))
  expect_true(is.finite(as.numeric(sens(300))))
})

test_that("dssens differs from dsens (Ds != D sensitivity)", {
  grad  <- optedr:::gradient(model, params, pvals)
  M     <- optedr:::inf_mat(grad, des)
  sd    <- optedr:::dsens(grad, M)
  sds   <- optedr:::dssens(grad, M, par_int = c(1L))
  expect_false(isTRUE(all.equal(as.numeric(sd(300)), as.numeric(sds(300)))))
})

# isens() ---------------------------------------------------------------

test_that("isens returns a function giving non-negative values", {
  grad  <- optedr:::gradient(model, params, pvals)
  M     <- optedr:::inf_mat(grad, des)
  sens  <- optedr:::isens(grad, M, diag(2))
  expect_true(is.function(sens))
  expect_gte(as.numeric(sens(300)), 0)
})

# sens() dispatcher -----------------------------------------------------

test_that("sens dispatches to the correct function for each criterion", {
  grad <- optedr:::gradient(model, params, pvals)
  M    <- optedr:::inf_mat(grad, des)
  for (crit in c("D-Optimality", "A-Optimality")) {
    fn <- optedr:::sens(crit, grad, M)
    expect_true(is.function(fn))
    expect_true(is.finite(as.numeric(fn(300))))
  }
  fn_ds <- optedr:::sens("Ds-Optimality", grad, M, par_int = c(1L))
  expect_true(is.function(fn_ds))
  fn_i  <- optedr:::sens("I-Optimality",  grad, M, matB = diag(2))
  expect_true(is.function(fn_i))
  fn_l  <- optedr:::sens("L-Optimality",  grad, M, matB = diag(2))
  expect_true(is.function(fn_l))
})
