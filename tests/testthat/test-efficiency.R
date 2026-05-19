library(optedr)

test_that("efficiency works for D-Optimality", {
  resArr.D <- evaluate_promise(opt_des(
    "D-Optimality",
    model = y ~ a * exp(-b / x),
    parameters = c("a", "b"),
    par_values = c(1, 1500),
    design_space = c(212, 422)
  ))$result
  design <- data.frame("Point" = c(220, 240, 400), "Weight" = c(1/3, 1/3, 1/3))

  efficiency1 <- evaluate_promise(design_efficiency(design, resArr.D))
  expect_equal(round(efficiency1$result, 7), 0.3063763)
})

test_that("efficiency errors on wrong column names in first argument", {
  resArr.D <- evaluate_promise(opt_des(
    "D-Optimality",
    model = y ~ a * exp(-b / x),
    parameters = c("a", "b"),
    par_values = c(1, 1500),
    design_space = c(212, 422)
  ))$result
  design_bad <- data.frame("Puntos" = c(220, 240, 400), "Weight" = c(1/3, 1/3, 1/3))
  expect_error(design_efficiency(design_bad, resArr.D), "'Point' and 'Weight'")
})

test_that("efficiency errors when second argument is not an optdes object", {
  design <- data.frame("Point" = c(220, 240, 400), "Weight" = c(1/3, 1/3, 1/3))
  expect_error(
    design_efficiency(design, design),
    "second argument must be an optdes object"
  )
})
