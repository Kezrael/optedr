library(optedr)

# efficient_round -------------------------------------------------------

test_that("rounding works", {
  design_test <- data.frame("Point" = seq(1, 5, length.out = 7),
                             "Weight" = c(0.1, 0.0001, 0.2, 0.134, 0.073, 0.2111, 0.2818))

  eff1    <- evaluate_promise(efficient_round(design_test, 21))
  rounded1 <- data.frame("Point"  = c(1.000000, 1.666667, 2.333333, 3, 3.666667, 4.333333, 5),
                          "Weight" = c(2, 1, 4, 3, 2, 4, 5))
  expect_true(all(mapply(`==`, round(eff1$result, 6), round(rounded1, 6))))

  eff2    <- evaluate_promise(efficient_round(design_test, 20))
  rounded2 <- data.frame("Point"  = c(1.000000, 1.666667, 2.333333, 3, 3.666667, 4.333333, 5),
                          "Weight" = c(2, 1, 4, 3, 1, 4, 5))
  expect_true(all(mapply(`==`, round(eff2$result, 6), round(rounded2, 6))))
  expect_equal(eff2$messages, c("i The proposed size of rounding is greater than n: \n2 1 4 3 2 4 5\n",
                                "i An alternative with size n is returned\n"))
})

test_that("rounding errors", {
  design_test <- data.frame("Point" = seq(1, 5, length.out = 7),
                             "Weight" = c(0.1, 0.0001, 0.2, 0.134, 0.073, 0.2111, 0.2818))

  expect_error(efficient_round(design_test, -21), "n must be a possitive integer")
  expect_error(efficient_round(design_test,  6.3), "n must be a possitive integer")

  design_test2 <- data.frame("Points" = seq(1, 5, length.out = 7),
                              "Weight" = c(0.1, 0.0001, 0.2, 0.134, 0.073, 0.2111, 0.2818))
  expect_error(efficient_round(design_test2, 15), "'Point' and 'Weight'")
})

test_that("efficient_round seed parameter gives reproducible results", {
  design_test <- data.frame("Point" = seq(1, 5, length.out = 7),
                             "Weight" = c(0.1, 0.0001, 0.2, 0.134, 0.073, 0.2111, 0.2818))
  r1 <- evaluate_promise(efficient_round(design_test, 21, seed = 42L))$result
  r2 <- evaluate_promise(efficient_round(design_test, 21, seed = 42L))$result
  expect_equal(r1, r2)
})

test_that("efficient_round with seed gives same result as without seed when sample() not needed", {
  design_test <- data.frame("Point" = seq(1, 5, length.out = 7),
                             "Weight" = c(0.1, 0.0001, 0.2, 0.134, 0.073, 0.2111, 0.2818))
  r_seed    <- evaluate_promise(efficient_round(design_test, 21, seed = 1L))$result
  r_no_seed <- evaluate_promise(efficient_round(design_test, 21))$result
  expect_equal(r_seed, r_no_seed)
})

# combinatorial_round ---------------------------------------------------

test_that("combinatorial_round works for D-Optimality from optdes object", {
  aprox <- evaluate_promise(
    opt_des("D-Optimality", y ~ a * exp(-b / x), c("a", "b"), c(1, 1500), c(212, 422))
  )$result
  result <- combinatorial_round(aprox, 27)
  expect_s3_class(result, "data.frame")
  expect_named(result, c("Point", "Weight"))
  expect_equal(sum(result$Weight), 27L)
  expect_true(all(result$Weight >= 0))
})

test_that("combinatorial_round errors in non-interactive context when k > max_support", {
  aprox <- evaluate_promise(
    opt_des("D-Optimality", y ~ a * exp(-b / x), c("a", "b"), c(1, 1500), c(212, 422))
  )$result
  # max_support = 1 forces the guard even for a 2-point design
  expect_error(
    combinatorial_round(aprox, 10, max_support = 1L),
    "support points"
  )
})

test_that("combinatorial_round with ask = FALSE emits message and proceeds", {
  aprox <- evaluate_promise(
    opt_des("D-Optimality", y ~ a * exp(-b / x), c("a", "b"), c(1, 1500), c(212, 422))
  )$result
  # max_support = 1: would normally ask, but ask = FALSE skips to computation
  expect_message(
    combinatorial_round(aprox, 10, max_support = 1L, ask = FALSE),
    "support points"
  )
})

test_that("combinatorial_round max_support = Inf never triggers the guard", {
  aprox <- evaluate_promise(
    opt_des("D-Optimality", y ~ a * exp(-b / x), c("a", "b"), c(1, 1500), c(212, 422))
  )$result
  # With Inf threshold, no message or error regardless of design size
  expect_no_condition(combinatorial_round(aprox, 10, max_support = Inf))
})
