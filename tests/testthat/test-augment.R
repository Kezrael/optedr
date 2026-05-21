library(optedr)

# Fixtures -------------------------------------------------------------------
# k = 2 model, used in other test files for consistency
model_2p    <- y ~ a * exp(-b / x)
params_2p   <- c("a", "b")
par_vals_2p <- c(1, 1500)
dspace_2p   <- c(212, 422)
# Three-point design, not optimal — used as init_design throughout
init_des_2p <- data.frame("Point"  = c(220, 300, 400),
                          "Weight" = c(1/3, 1/3, 1/3))
alpha_val <- 0.25

# delta_val = 0.85 is expected to lie comfortably within the efficiency range
# [findminval(eff_fun,...), findmaxval(eff_fun,...)] for this model with alpha=0.25, k=2.
# eff_fun = 0.75 * (1 + sens_1(x)/3)^0.5, minimum ~0.75, maximum well above 1.
# Adjust this constant if a test fails with "outside the valid range".
DELTA_D <- 0.85

# Ds model
model_ds    <- y ~ th0 * exp(x / th1)
params_ds   <- c("th0", "th1")
par_vals_ds <- c(10.4963, -3.2940)
dspace_ds   <- c(0.94, 30)
init_des_ds <- data.frame("Point"  = c(1, 10, 28),
                          "Weight" = c(1/3, 1/3, 1/3))
DELTA_DS <- 0.85


# get_augment_region — validation errors -------------------------------------

test_that("get_augment_region errors without delta_val in non-interactive context", {
  expect_error(
    get_augment_region("D-Optimality", init_des_2p, alpha_val,
                       model_2p, params_2p, par_vals_2p, dspace_2p, FALSE),
    "Supply delta_val"
  )
})

test_that("get_augment_region errors on invalid criterion", {
  expect_error(
    get_augment_region("E-Optimality", init_des_2p, alpha_val,
                       model_2p, params_2p, par_vals_2p, dspace_2p, FALSE,
                       delta_val = DELTA_D),
    "Invalid criterion"
  )
})

test_that("get_augment_region errors when delta_val is below valid range", {
  expect_error(
    get_augment_region("D-Optimality", init_des_2p, alpha_val,
                       model_2p, params_2p, par_vals_2p, dspace_2p, FALSE,
                       delta_val = 0),
    "outside the valid range"
  )
})

test_that("get_augment_region errors when delta_val is above valid range", {
  expect_error(
    get_augment_region("D-Optimality", init_des_2p, alpha_val,
                       model_2p, params_2p, par_vals_2p, dspace_2p, FALSE,
                       delta_val = 100),
    "outside the valid range"
  )
})


# get_augment_region — output properties ------------------------------------

test_that("get_augment_region D-Optimality returns an augment_region with numeric region", {
  region <- suppressMessages(
    get_augment_region("D-Optimality", init_des_2p, alpha_val,
                       model_2p, params_2p, par_vals_2p, dspace_2p, FALSE,
                       delta_val = DELTA_D)
  )
  expect_s3_class(region, "augment_region")
  r <- region$region
  expect_true(is.numeric(r))
  expect_gte(length(r), 2L)
  expect_equal(length(r) %% 2L, 0L)
  expect_true(all(r >= dspace_2p[1] & r <= dspace_2p[2]))
  for (i in seq_len(length(r) / 2L))
    expect_lt(r[2*i - 1], r[2*i])
})

test_that("get_augment_region Ds-Optimality returns an augment_region with numeric region", {
  region <- suppressMessages(
    get_augment_region("Ds-Optimality", init_des_ds, alpha_val,
                       model_ds, params_ds, par_vals_ds, dspace_ds, FALSE,
                       par_int = c(1L), delta_val = DELTA_DS)
  )
  expect_s3_class(region, "augment_region")
  r <- region$region
  expect_true(is.numeric(r))
  expect_equal(length(r) %% 2L, 0L)
  expect_true(all(r >= dspace_ds[1] & r <= dspace_ds[2]))
})


# augment_design — validation errors ----------------------------------------

test_that("augment_design errors without delta_val in non-interactive context", {
  expect_error(
    augment_design("D-Optimality", init_des_2p, alpha_val,
                   model_2p, params_2p, par_vals_2p, dspace_2p, FALSE),
    "Supply delta_val"
  )
})

test_that("augment_design errors without new_points in non-interactive context", {
  expect_error(
    augment_design("D-Optimality", init_des_2p, alpha_val,
                   model_2p, params_2p, par_vals_2p, dspace_2p, FALSE,
                   delta_val = DELTA_D),
    "Supply new_points"
  )
})

test_that("augment_design errors on invalid criterion", {
  new_pts <- data.frame(Point = 300, Weight = 1)
  expect_error(
    augment_design("Z-Optimality", init_des_2p, alpha_val,
                   model_2p, params_2p, par_vals_2p, dspace_2p, FALSE,
                   delta_val = DELTA_D, new_points = new_pts),
    "Invalid criterion"
  )
})

test_that("augment_design errors when delta_val out of range", {
  new_pts <- data.frame(Point = 300, Weight = 1)
  expect_error(
    augment_design("D-Optimality", init_des_2p, alpha_val,
                   model_2p, params_2p, par_vals_2p, dspace_2p, FALSE,
                   delta_val = 0, new_points = new_pts),
    "outside the valid range"
  )
})


# new_points validation ------------------------------------------------------

test_that("new_points must be a data.frame with Point and Weight columns", {
  expect_error(
    augment_design("D-Optimality", init_des_2p, alpha_val,
                   model_2p, params_2p, par_vals_2p, dspace_2p, FALSE,
                   delta_val = DELTA_D,
                   new_points = data.frame(Punto = 300, Weight = 1)),
    "'Point' and 'Weight'"
  )
  expect_error(
    augment_design("D-Optimality", init_des_2p, alpha_val,
                   model_2p, params_2p, par_vals_2p, dspace_2p, FALSE,
                   delta_val = DELTA_D,
                   new_points = data.frame(Point = 300, Peso = 1)),
    "'Point' and 'Weight'"
  )
})

test_that("new_points weights must be strictly positive", {
  expect_error(
    augment_design("D-Optimality", init_des_2p, alpha_val,
                   model_2p, params_2p, par_vals_2p, dspace_2p, FALSE,
                   delta_val = DELTA_D,
                   new_points = data.frame(Point = 300, Weight = -1)),
    "positive"
  )
  expect_error(
    augment_design("D-Optimality", init_des_2p, alpha_val,
                   model_2p, params_2p, par_vals_2p, dspace_2p, FALSE,
                   delta_val = DELTA_D,
                   new_points = data.frame(Point = 300, Weight = 0)),
    "positive"
  )
})

test_that("new_points must lie within the design space", {
  expect_error(
    augment_design("D-Optimality", init_des_2p, alpha_val,
                   model_2p, params_2p, par_vals_2p, dspace_2p, FALSE,
                   delta_val = DELTA_D,
                   new_points = data.frame(Point = 500, Weight = 1)),
    "outside the design space"
  )
  expect_error(
    augment_design("D-Optimality", init_des_2p, alpha_val,
                   model_2p, params_2p, par_vals_2p, dspace_2p, FALSE,
                   delta_val = DELTA_D,
                   new_points = data.frame(Point = 100, Weight = 1)),
    "outside the design space"
  )
})

test_that("new_points outside candidate region are rejected", {
  region <- suppressMessages(
    get_augment_region("D-Optimality", init_des_2p, alpha_val,
                       model_2p, params_2p, par_vals_2p, dspace_2p, FALSE,
                       delta_val = DELTA_D)
  )
  # Find a point inside the design space but outside the candidate region
  n_regions <- length(region$region) / 2L
  outside_pt <- NULL
  for (pt in seq(dspace_2p[1], dspace_2p[2], length.out = 100)) {
    in_reg <- any(vapply(seq_len(n_regions),
                         function(i) pt >= region$region[2*i-1] & pt <= region$region[2*i],
                         logical(1)))
    if (!in_reg) { outside_pt <- pt; break }
  }
  skip_if(is.null(outside_pt),
          "All sampled points happen to lie in the candidate region — skip")
  expect_error(
    augment_design("D-Optimality", init_des_2p, alpha_val,
                   model_2p, params_2p, par_vals_2p, dspace_2p, FALSE,
                   delta_val = DELTA_D,
                   new_points = data.frame(Point = outside_pt, Weight = 1)),
    "candidate region"
  )
})


# augment_design — output correctness ----------------------------------------

test_that("augment_design D-Optimality: output has correct structure and sums to 1", {
  region <- suppressMessages(
    get_augment_region("D-Optimality", init_des_2p, alpha_val,
                       model_2p, params_2p, par_vals_2p, dspace_2p, FALSE,
                       delta_val = DELTA_D)
  )
  new_pt  <- (region$region[1] + region$region[2]) / 2
  new_pts <- data.frame(Point = new_pt, Weight = 1)

  result <- suppressMessages(
    augment_design("D-Optimality", init_des_2p, alpha_val,
                   model_2p, params_2p, par_vals_2p, dspace_2p, FALSE,
                   delta_val = DELTA_D, new_points = new_pts)
  )

  expect_s3_class(result, "data.frame")
  expect_named(result, c("Point", "Weight"))
  expect_equal(nrow(result), nrow(init_des_2p) + 1L)
  expect_equal(sum(result$Weight), 1, tolerance = 1e-10)
  expect_true(all(result$Weight > 0))
  expect_true(all(result$Point >= dspace_2p[1] & result$Point <= dspace_2p[2]))
})

test_that("augment_design D-Optimality: existing weights scaled by (1 - alpha), new weight = alpha", {
  region <- suppressMessages(
    get_augment_region("D-Optimality", init_des_2p, alpha_val,
                       model_2p, params_2p, par_vals_2p, dspace_2p, FALSE,
                       delta_val = DELTA_D)
  )
  new_pt  <- (region$region[1] + region$region[2]) / 2
  new_pts <- data.frame(Point = new_pt, Weight = 1)

  result <- suppressMessages(
    augment_design("D-Optimality", init_des_2p, alpha_val,
                   model_2p, params_2p, par_vals_2p, dspace_2p, FALSE,
                   delta_val = DELTA_D, new_points = new_pts)
  )

  orig_rows <- match(init_des_2p$Point, result$Point)
  expect_equal(result$Weight[orig_rows],
               init_des_2p$Weight * (1 - alpha_val),
               tolerance = 1e-10)
  new_row <- match(new_pt, result$Point)
  expect_equal(result$Weight[new_row], alpha_val, tolerance = 1e-10)
})

test_that("augment_design with empty new_points returns init_design unchanged", {
  result <- suppressMessages(
    augment_design("D-Optimality", init_des_2p, alpha_val,
                   model_2p, params_2p, par_vals_2p, dspace_2p, FALSE,
                   delta_val = DELTA_D,
                   new_points = data.frame(Point = double(), Weight = double()))
  )
  expect_equal(result$Point,  init_des_2p$Point)
  expect_equal(result$Weight, init_des_2p$Weight)
})

test_that("augment_design with multiple new_points adds them all", {
  region <- suppressMessages(
    get_augment_region("D-Optimality", init_des_2p, alpha_val,
                       model_2p, params_2p, par_vals_2p, dspace_2p, FALSE,
                       delta_val = DELTA_D)
  )
  n_regions <- length(region$region) / 2L
  skip_if(n_regions < 2L, "Need at least 2 candidate regions for this test")
  new_pts <- data.frame(
    Point  = c((region$region[1] + region$region[2]) / 2,
               (region$region[3] + region$region[4]) / 2),
    Weight = c(0.5, 0.5)
  )
  result <- suppressMessages(
    augment_design("D-Optimality", init_des_2p, alpha_val,
                   model_2p, params_2p, par_vals_2p, dspace_2p, FALSE,
                   delta_val = DELTA_D, new_points = new_pts)
  )
  expect_equal(nrow(result), nrow(init_des_2p) + 2L)
  expect_equal(sum(result$Weight), 1, tolerance = 1e-10)
})

test_that("augment_design Ds-Optimality: output has correct structure and sums to 1", {
  region <- suppressMessages(
    get_augment_region("Ds-Optimality", init_des_ds, alpha_val,
                       model_ds, params_ds, par_vals_ds, dspace_ds, FALSE,
                       par_int = c(1L), delta_val = DELTA_DS)
  )
  new_pt  <- (region$region[1] + region$region[2]) / 2
  new_pts <- data.frame(Point = new_pt, Weight = 1)

  result <- suppressMessages(
    augment_design("Ds-Optimality", init_des_ds, alpha_val,
                   model_ds, params_ds, par_vals_ds, dspace_ds, FALSE,
                   par_int = c(1L), delta_val = DELTA_DS, new_points = new_pts)
  )

  expect_s3_class(result, "data.frame")
  expect_named(result, c("Point", "Weight"))
  expect_equal(sum(result$Weight), 1, tolerance = 1e-10)
  expect_true(all(result$Weight > 0))
})
