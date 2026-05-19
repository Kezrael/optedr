library(optedr)

test_that("opt_des D-Optimality converges to a high-efficiency design", {
  result <- evaluate_promise(opt_des(
    "D-Optimality",
    model = y ~ a * exp(-b / x),
    parameters = c("a", "b"),
    par_values = c(1, 1500),
    design_space = c(212, 422)
  ))$result

  # D-optimal design for this model has 2 support points
  expect_equal(nrow(result$optdes), 2L)
  expect_named(result$optdes, c("Point", "Weight"))

  # Points within design space
  expect_true(all(result$optdes$Point >= 212 & result$optdes$Point <= 422))

  # Weights sum to 1
  expect_equal(sum(result$optdes$Weight), 1, tolerance = 1e-6)

  # Atwood lower bound ≥ 99 % (tight convergence)
  expect_gte(result$atwood, 99)

  # Support points roughly at the known D-optimal positions (329, 422)
  expect_equal(round(sort(result$optdes$Point)), c(329, 422))

  # Roughly equal weights
  expect_equal(result$optdes$Weight, c(0.5, 0.5), tolerance = 0.01)
})

test_that("opt_des Ds-Optimality converges to a high-efficiency design", {
  result <- evaluate_promise(opt_des(
    "Ds-Optimality",
    y ~ th0 * exp(x / th1),
    c("th0", "th1"),
    c(10.4963, -3.2940),
    c(0.94, 30),
    par_int = c(1)
  ))$result

  expect_equal(nrow(result$optdes), 2L)
  expect_named(result$optdes, c("Point", "Weight"))
  expect_true(all(result$optdes$Point >= 0.94 & result$optdes$Point <= 30))
  expect_equal(sum(result$optdes$Weight), 1, tolerance = 1e-6)
  expect_gte(result$atwood, 99)
})

test_that("opt_des wrong input throws error", {
  # Wrong criterion
  expect_error(opt_des(
    "E-Optimality",
    model = y ~ a * exp(-b / x),
    parameters = c("a", "b"),
    par_values = c(1, 1500),
    design_space = c(212, 422)
  ))

  # Missing design_space
  expect_error(opt_des(
    "D-Optimality",
    model = y ~ a * exp(-b / x),
    parameters = c("a", "b"),
    par_values = c(1, 1500)
  ))

  # Ds-Optimality without par_int
  expect_error(opt_des(
    "Ds-Optimality",
    y ~ th0 * exp(x / th1),
    c("th0", "th1"),
    c(10.4963, -3.2940),
    c(0.94, 30)
  ))
})
