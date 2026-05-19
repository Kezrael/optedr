library(optedr)

# Shared fixtures -------------------------------------------------------
model  <- y ~ a * exp(-b / x)
params <- c("a", "b")
pvals  <- c(1, 1500)
dspace <- c(212, 422)
grad   <- optedr:::gradient(model, params, pvals)
# Suboptimal 3-point design used where we need something clearly worse than optimal
des      <- data.frame(Point = c(329.3, 422),       Weight = c(0.5, 0.5))
des_sub  <- data.frame(Point = c(220, 300, 400),    Weight = c(1/3, 1/3, 1/3))
M      <- optedr:::inf_mat(grad, des)

# dcrit() ---------------------------------------------------------------

test_that("dcrit returns a finite positive value for non-singular M", {
  val <- optedr:::dcrit(M, k = 2L)
  expect_true(is.finite(val))
  expect_gt(val, 0)
})

test_that("dcrit with k = 0 infers k from matrix dimensions", {
  expect_equal(optedr:::dcrit(M, k = 0L), optedr:::dcrit(M, k = 2L))
})

test_that("dcrit is minimised at the D-optimal design vs a suboptimal one", {
  res    <- evaluate_promise(opt_des("D-Optimality", model, params, pvals, dspace))$result
  M_opt  <- optedr:::inf_mat(grad, res$optdes)
  M_s    <- optedr:::inf_mat(grad, des_sub)
  expect_lte(optedr:::dcrit(M_opt, 2L), optedr:::dcrit(M_s, 2L))
})

test_that("dcrit is invariant to weight permutation", {
  d1 <- data.frame(Point = c(329.3, 422), Weight = c(0.3, 0.7))
  d2 <- data.frame(Point = c(422, 329.3), Weight = c(0.7, 0.3))
  expect_equal(
    optedr:::dcrit(optedr:::inf_mat(grad, d1), 2L),
    optedr:::dcrit(optedr:::inf_mat(grad, d2), 2L),
    tolerance = 1e-10
  )
})

# dscrit() --------------------------------------------------------------

test_that("dscrit returns a finite positive value", {
  val <- optedr:::dscrit(M, par_int = c(1L))
  expect_true(is.finite(val))
  expect_gt(val, 0)
})

test_that("dscrit handles 1x1 nuisance submatrix (scalar case)", {
  val_sc <- optedr:::dscrit(M, par_int = c(2L))
  expect_true(is.finite(val_sc))
  expect_gt(val_sc, 0)
})

# icrit() ---------------------------------------------------------------

test_that("icrit returns a finite positive value", {
  val <- optedr:::icrit(M, diag(2))
  expect_true(is.finite(val))
  expect_gt(val, 0)
})

test_that("icrit with identity matB equals tr(M^-1)", {
  expect_equal(
    optedr:::icrit(M, diag(2)),
    optedr:::tr(optedr:::inv_spd(M)),
    tolerance = 1e-10
  )
})

test_that("icrit is minimised at the A-optimal design", {
  res   <- evaluate_promise(opt_des("A-Optimality", model, params, pvals, dspace))$result
  M_opt <- optedr:::inf_mat(grad, res$optdes)
  expect_lte(optedr:::icrit(M_opt, diag(2)), optedr:::icrit(M, diag(2)))
})

# design_efficiency() ---------------------------------------------------

test_that("design_efficiency returns value in (0, 1] for D-Optimality", {
  res <- evaluate_promise(opt_des("D-Optimality", model, params, pvals, dspace))$result
  eff <- evaluate_promise(design_efficiency(des_sub, res))$result
  expect_gte(eff, 0)
  expect_lte(eff, 1)
})

test_that("design_efficiency returns 1 when comparing optimal design to itself", {
  res <- evaluate_promise(opt_des("D-Optimality", model, params, pvals, dspace))$result
  eff <- evaluate_promise(design_efficiency(res$optdes, res))$result
  expect_equal(eff, 1, tolerance = 0.001)
})

test_that("design_efficiency accepts an optdes object as first argument", {
  res <- evaluate_promise(opt_des("D-Optimality", model, params, pvals, dspace))$result
  eff <- evaluate_promise(design_efficiency(res, res))$result
  expect_equal(eff, 1, tolerance = 0.001)
})

test_that("design_efficiency works for Ds-Optimality", {
  res <- evaluate_promise(opt_des(
    "Ds-Optimality",
    y ~ th0 * exp(x / th1), c("th0", "th1"), c(10.4963, -3.2940), c(0.94, 30),
    par_int = c(1L)
  ))$result
  grad_ds <- optedr:::gradient(y ~ th0 * exp(x / th1), c("th0", "th1"), c(10.4963, -3.2940))
  des_ds  <- data.frame(Point = c(1, 15), Weight = c(0.5, 0.5))
  eff     <- evaluate_promise(design_efficiency(des_ds, res))$result
  expect_gte(eff, 0)
})

test_that("design_efficiency works for A-Optimality", {
  res <- evaluate_promise(opt_des("A-Optimality", model, params, pvals, dspace))$result
  eff <- evaluate_promise(design_efficiency(des, res))$result
  expect_gte(eff, 0)
})

test_that("design_efficiency works for I-Optimality", {
  res <- evaluate_promise(opt_des(
    "I-Optimality", model, params, pvals, dspace, reg_int = c(380, 422)
  ))$result
  eff <- evaluate_promise(design_efficiency(des, res))$result
  expect_gte(eff, 0)
})

test_that("design_efficiency errors when second arg is not an optdes object", {
  expect_error(design_efficiency(des, des), "second argument must be an optdes object")
})

test_that("design_efficiency errors on wrong column names in first arg", {
  res     <- evaluate_promise(opt_des("D-Optimality", model, params, pvals, dspace))$result
  bad_des <- data.frame(Punto = c(300, 400), Weight = c(0.5, 0.5))
  expect_error(design_efficiency(bad_des, res), "'Point' and 'Weight'")
})
