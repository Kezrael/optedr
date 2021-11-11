library(optedr)

test_that("augmented designs equal", {
  resArr.D <- opt_des("D-Optimality",
                      model = y ~ a*exp(-b/x),
                      parameters = c("a", "b"),
                      par_values = c(1, 1500),
                      design_space = c(212, 422))
  mockery::stub(augment_design, "readline", mockery::mock(0.8, 260, 0.15, 380, 0.15, "f"))
  result1 <- evaluate_promise(augment_design(resArr.D$optdes, 0.3, y ~ a * exp(-b/x),
                                parameters = c("a", "b"),
                                par_values = c(1, 1500),
                                design_space = c(212, 422),
                                F))
  augmented1 <- data.frame("Point" = c(329.2966, 422.0000, 260, 380), "Weight" = c(0.3500048, 0.3499952, 0.15, 0.15))
  expect_true(all(mapply(`==`, round(result1$result, 4), round(augmented1, 4))))
  expect_equal(result1$output, "The region(s) are  [250.98-422]The region(s) are  [250.98-422]The region(s) are  [250.98-422]")
})

test_that("augmented designs wrong", {
  resArr.D <- opt_des("D-Optimality",
                      model = y ~ a*exp(-b/x),
                      parameters = c("a", "b"),
                      par_values = c(1, 1500),
                      design_space = c(212, 422))

  # Missing parameters
  expect_error(augment_design(resArr.D$optdes, 0.3, y ~ a * exp(-b/x),
                              par_values = c(1, 1500),
                              design_space = c(212, 422),
                              F), "invalid variable names")

  mockery::stub(augment_design, "readline", mockery::mock(0.8, 260, 0.15, 380, 0.15, "f"))
  # alpha >= 1
  expect_error(augment_design(resArr.D$optdes, 1.1, y ~ a * exp(-b/x),
                              parameters = c("a", "b"),
                              par_values = c(1, 1500),
                              design_space = c(212, 422),
                              F), "missing value")
  # alpha < 1
  mockery::stub(augment_design, "readline", mockery::mock(0.8, 260, 0.15, 380, 0.15, "f"))
  expect_error(augment_design(resArr.D$optdes, -3, y ~ a * exp(-b/x),
                              parameters = c("a", "b"),
                              par_values = c(1, 1500),
                              design_space = c(212, 422),
                              F), "missing value")
})
