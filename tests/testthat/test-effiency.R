library(optedr)

test_that("efficiency works", {
  resArr.D <- opt_des("D-Optimality",
          model = y ~ a*exp(-b/x),
          parameters = c("a", "b"),
          par_values = c(1, 1500),
          design_space = c(212, 422))
  design <- data.frame("Point" = c(220, 240, 400), "Weight" = c(1/3, 1/3, 1/3))

  efficiency1 <- evaluate_promise(design_efficiency(resArr.D, design))
  expect_equal(round(efficiency1$result, 7), 0.3063763)
  expect_equal(efficiency1$messages, "i The efficiency of the design is 30.6376288682445%\n")
})

test_that("efficiency errors", {
  resArr.D <- opt_des("D-Optimality",
                      model = y ~ a*exp(-b/x),
                      parameters = c("a", "b"),
                      par_values = c(1, 1500),
                      design_space = c(212, 422))
  design2 <- data.frame("Puntos" = c(220, 240, 400), "Weight" = c(1/3, 1/3, 1/3))

  # wrong column names
  expect_error(design_efficiency(resArr.D, design2), "non-conformable arrays")

  # No optdes object
  design <- data.frame("Point" = c(220, 240, 400), "Weight" = c(1/3, 1/3, 1/3))

  expect_error(design_efficiency(design, design), "could not find function")
})
