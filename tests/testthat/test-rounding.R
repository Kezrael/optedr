library(optedr)

test_that("rounding works", {
  design_test <- data.frame("Point" = seq(1, 5, length.out = 7), "Weight" = c(0.1, 0.0001, 0.2, 0.134, 0.073, 0.2111, 0.2818))

  eff1 <- evaluate_promise(efficient_round(design_test, 21))
  rounded1 <- data.frame("Point" = c(1.000000, 1.666667, 2.333333, 3, 3.666667, 4.333333, 5), "Weight" = c(2, 1, 4, 3, 2, 4, 5))
  expect_true(all(mapply(`==`, round(eff1$result, 6), round(rounded1, 6))))

  eff2 <- evaluate_promise(efficient_round(design_test, 20))
  rounded2 <- data.frame("Point" = c(1.000000, 1.666667, 2.333333, 3, 3.666667, 4.333333, 5), "Weight" = c(2, 1, 4, 3, 1, 4, 5))
  expect_true(all(mapply(`==`, round(eff2$result, 6), round(rounded2, 6))))
  expect_equal(eff2$messages, c("i The proposed size of rounding is greater than n: \n2 1 4 3 2 4 5\n", "i An alternative with size n is returned\n" ))
})

test_that("rounding errors", {
  design_test <- data.frame("Point" = seq(1, 5, length.out = 7), "Weight" = c(0.1, 0.0001, 0.2, 0.134, 0.073, 0.2111, 0.2818))

  # Negative n
  expect_error(efficient_round(design_test, -21), "n must be a possitive integer")

  # Decimal n
  expect_error(efficient_round(design_test, 6.3), "n must be a possitive integer")

  # Wrong column names
  design_test2 <- data.frame("Points" = seq(1, 5, length.out = 7), "Weight" = c(0.1, 0.0001, 0.2, 0.134, 0.073, 0.2111, 0.2818))
  expect_error(efficient_round(design_test2, 15), "'Point' and 'Weight'")
})

