library(optedr)

test_that("opt_des works", {
  result1 <- evaluate_promise(opt_des("D-Optimality",
                                model = y ~ a*exp(-b/x),
                                parameters = c("a", "b"),
                                par_values = c(1, 1500),
                                design_space = c(212, 422)))
  optimum <- data.frame("Point" = c(329.2966, 422.0000), "Weight" = c(0.5000068, 0.4999932))
  expect_true(all(mapply(`==`, round(result1$result$optdes, 4), round(optimum, 4))))
  expect_equal(round(result1$result$crit_value), 9972806)
  expect_equal(result1$messages, c("i Stop condition not reached, max iterations performed\n", "i The lower bound for efficiency is 99.9986396401789%\n"))


  result2 <- evaluate_promise(opt_des("Ds-Optimality",
                    y ~ th0*exp(x/th1),
                    c("th0", "th1"),
                    c(10.4963, -3.2940),
                    c(0.94, 30),
                    par_int = c(1)))
  optimum2 <- data.frame("Point" = c(0.940000, 5.147919), "Weight" = c(0.6042053, 0.3957947))
  expect_true(all(mapply(`==`, round(result2$result$optdes, 6), round(optimum2, 6))))
  expect_equal(round(result2$result$crit_value, 6), 7.254818)
  expect_equal(result2$messages, c("i Stop condition reached: difference between sensitivity and criterion < 1e-05\n", "i The lower bound for efficiency is 99.9998969351636%\n"))
})

test_that("wrong input", {
  # Wrong criterion
  expect_error(opt_des("E-Optimality",
                       model = y ~ a*exp(-b/x),
                       parameters = c("a", "b"),
                       par_values = c(1, 1500)))

  # Missing parameters
  expect_error(opt_des("D-Optimality",
                       model = y ~ a*exp(-b/x),
                       parameters = c("a", "b"),
                       par_values = c(1, 1500)))

  expect_error(opt_des("Ds-Optimality",
                       y ~ th0*exp(x/th1),
                       c("th0", "th1"),
                       c(10.4963, -3.2940),
                       c(0.94, 30)))
})
