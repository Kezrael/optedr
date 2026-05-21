library(optedr)

# ── Infrastructure helpers ────────────────────────────────────────────────────

test_that("detect_design_vars finds 'x' for single-factor models", {
  expect_equal(optedr:::detect_design_vars(y ~ a * exp(-b / x), c("a", "b")), "x")
})

test_that("detect_design_vars finds x1, x2 for two-factor models", {
  dvars <- optedr:::detect_design_vars(y ~ a * x1 + b * x2, c("a", "b"))
  expect_equal(dvars, c("x1", "x2"))
})

test_that("detect_design_vars sorts multi-factor variables numerically", {
  dvars <- optedr:::detect_design_vars(y ~ a * x1 + b * x2 + c * x10, c("a", "b", "c"))
  expect_equal(dvars, c("x1", "x2", "x10"))
})

test_that("detect_design_vars errors when x and x1/x2 are mixed", {
  expect_error(
    optedr:::detect_design_vars(y ~ a * x + b * x1, c("a", "b")),
    "mixes"
  )
})

test_that("detect_design_vars errors when no design variable is found", {
  expect_error(
    optedr:::detect_design_vars(y ~ a + b, c("a", "b")),
    "No design variable"
  )
})

test_that("canonicalise_design_space accepts numeric c(min,max) for 1D", {
  ds <- optedr:::canonicalise_design_space(c(0, 10), "x")
  expect_equal(ds, list(x = c(0, 10)))
})

test_that("canonicalise_design_space accepts named list for multi-factor", {
  inp <- list(x1 = c(0, 5), x2 = c(1, 8))
  ds  <- optedr:::canonicalise_design_space(inp, c("x1", "x2"))
  expect_equal(ds, list(x1 = c(0, 5), x2 = c(1, 8)))
})

test_that("canonicalise_design_space errors when list names do not match design vars", {
  expect_error(
    optedr:::canonicalise_design_space(list(z1 = c(0, 5), z2 = c(0, 5)), c("x1", "x2")),
    "names\\(design_space\\)"
  )
})

test_that("canonicalise_design_space errors when numeric vector used for multi-factor model", {
  expect_error(
    optedr:::canonicalise_design_space(c(0, 10), c("x1", "x2")),
    "named list"
  )
})

test_that("normalize_design_cols renames 'Point' to design variable for 1D", {
  d <- data.frame(Point = c(1, 2), Weight = c(0.5, 0.5))
  d2 <- optedr:::normalize_design_cols(d, "x")
  expect_true("x" %in% names(d2))
  expect_false("Point" %in% names(d2))
})

test_that("normalize_design_cols leaves multi-factor columns unchanged", {
  d <- data.frame(x1 = c(1, 2), x2 = c(3, 4), Weight = c(0.5, 0.5))
  d2 <- optedr:::normalize_design_cols(d, c("x1", "x2"))
  expect_equal(names(d2), c("x1", "x2", "Weight"))
})

test_that("lhs_sample returns matrix with correct dimensions and bounds", {
  ds  <- list(x1 = c(0, 5), x2 = c(10, 20))
  pts <- optedr:::lhs_sample(50L, ds)
  expect_equal(dim(pts), c(50L, 2L))
  expect_equal(colnames(pts), c("x1", "x2"))
  expect_true(all(pts[, "x1"] >= 0 & pts[, "x1"] <= 5))
  expect_true(all(pts[, "x2"] >= 10 & pts[, "x2"] <= 20))
})

test_that("is_multifactor correctly identifies single vs multi factor", {
  expect_false(optedr:::is_multifactor("x"))
  expect_true(optedr:::is_multifactor(c("x1", "x2")))
  expect_true(optedr:::is_multifactor(c("x1", "x2", "x3")))
})


# ── 2-factor opt_des (bisubstrate Michaelis-Menten) ──────────────────────────

# Shared fixture — kept fast with max_iter = 10
local({
  mm2d_res <<- evaluate_promise(opt_des(
    "D-Optimality",
    model       = y ~ Vmax * x1 * x2 / ((K1 + x1) * (K2 + x2)),
    parameters  = c("Vmax", "K1", "K2"),
    par_values  = c(1, 1, 1),
    design_space = list(x1 = c(0.1, 10), x2 = c(0.1, 10)),
    max_iter    = 10L
  ))$result
})

test_that("opt_des 2D result has class optdes", {
  expect_s3_class(mm2d_res, "optdes")
})

test_that("opt_des 2D result has all required components", {
  expect_named(mm2d_res,
               c("optdes", "convergence", "sens", "criterion", "crit_value", "atwood"),
               ignore.order = TRUE)
})

test_that("opt_des 2D design has correct columns (x1, x2, Weight)", {
  expect_true(all(c("x1", "x2", "Weight") %in% names(mm2d_res$optdes)))
  expect_false("Point" %in% names(mm2d_res$optdes))
})

test_that("opt_des 2D weights sum to 1", {
  expect_equal(sum(mm2d_res$optdes$Weight), 1, tolerance = 1e-6)
})

test_that("opt_des 2D support points lie within design_space", {
  des <- mm2d_res$optdes
  expect_true(all(des$x1 >= 0.1 & des$x1 <= 10))
  expect_true(all(des$x2 >= 0.1 & des$x2 <= 10))
})

test_that("opt_des 2D atwood is a non-negative number", {
  expect_true(is.numeric(mm2d_res$atwood))
  expect_gte(mm2d_res$atwood, 0)
})

test_that("opt_des 2D has at least 3 support points (>= num parameters)", {
  expect_gte(nrow(mm2d_res$optdes), 3L)
})

test_that("opt_des 2D gradient has design_vars attribute set to c('x1','x2')", {
  dvars <- attr(attr(mm2d_res, "gradient"), "design_vars")
  expect_equal(dvars, c("x1", "x2"))
})

test_that("opt_des 2D design_space attribute is stored on result", {
  ds <- attr(mm2d_res, "design_space")
  expect_equal(ds$x1, c(0.1, 10))
  expect_equal(ds$x2, c(0.1, 10))
})


# ── print / summary for multi-factor ─────────────────────────────────────────

test_that("print.optdes for 2D shows '2 factors' header", {
  out <- capture.output(print(mm2d_res))
  expect_true(any(grepl("2 factors", out, fixed = TRUE)))
})

test_that("summary.optdes for 2D shows design_space and criterion", {
  out <- capture.output(summary(mm2d_res))
  expect_true(any(grepl("Design space", out, fixed = TRUE)))
  expect_true(any(grepl("D-Optimality", out, fixed = TRUE)))
  expect_true(any(grepl("Atwood", out, fixed = TRUE)))
})

test_that("plot.optdes for 2D returns heatmap (ggplot)", {
  expect_s3_class(plot(mm2d_res), "ggplot")
})


# ── plot for d > 2 (pairs scatter matrix) ────────────────────────────────────

local({
  mm3d_res <<- evaluate_promise(opt_des(
    "D-Optimality",
    model        = y ~ Vmax * x1 * x2 * x3 / ((K1+x1) * (K2+x2) * (K3+x3)),
    parameters   = c("Vmax", "K1", "K2", "K3"),
    par_values   = c(1, 1, 1, 1),
    design_space = list(x1 = c(0.1, 10), x2 = c(0.1, 10), x3 = c(0.1, 10)),
    max_iter     = 5L
  ))$result
})

test_that("plot.optdes for 3D returns a ggplot", {
  expect_s3_class(plot(mm3d_res), "ggplot")
})

test_that("plot.optdes for 3D uses facet_grid with C(3,2)=3 xvar/yvar combos", {
  p <- plot(mm3d_res)
  expect_s3_class(p$facet, "FacetGrid")
  # 3 unique (xvar, yvar) pairs
  combos <- nrow(unique(p$data[, c("xvar","yvar")]))
  expect_equal(combos, 3L)
})

test_that("plot.optdes for 3D xvar/yvar are design variable names", {
  p <- plot(mm3d_res)
  expect_true(all(levels(p$data$xvar) %in% c("x1","x2","x3")))
  expect_true(all(levels(p$data$yvar) %in% c("x1","x2","x3")))
})

test_that("plot.optdes for 4D has C(4,2)=6 xvar/yvar combos", {
  r4d <- evaluate_promise(opt_des(
    "D-Optimality",
    y ~ a*x1 + b*x2 + c*x3 + d*x4,
    c("a","b","c","d"), c(1,1,1,1),
    list(x1=c(0,1), x2=c(0,1), x3=c(0,1), x4=c(0,1)),
    max_iter = 5L
  ))$result
  p <- plot(r4d)
  combos <- nrow(unique(p$data[, c("xvar","yvar")]))
  expect_equal(combos, 6L)
})


# ── design_efficiency for multi-factor ───────────────────────────────────────

test_that("design_efficiency accepts x1/x2 column names for 2D designs", {
  # Use the full optimal design with equal weights so M is non-singular
  des_eq <- mm2d_res$optdes
  des_eq$Weight <- rep(1 / nrow(des_eq), nrow(des_eq))
  eff <- evaluate_promise(design_efficiency(des_eq, mm2d_res))$result
  expect_true(is.numeric(eff))
  expect_gte(eff, 0)
  expect_lte(eff, 1 + 1e-4)   # allow small floating-point overshoot from equal-weight design
})

test_that("design_efficiency for 2D errors on wrong column names", {
  bad <- data.frame(z1 = c(1, 2), z2 = c(1, 2), Weight = c(0.5, 0.5))
  expect_error(design_efficiency(bad, mm2d_res), "'x1'")
})

test_that("design_efficiency efficiency of optimal design is 1 for 2D", {
  eff <- evaluate_promise(design_efficiency(mm2d_res, mm2d_res))$result
  expect_equal(eff, 1, tolerance = 1e-6)
})


# ── augment multi-factor ──────────────────────────────────────────────────────

local({
  init_aug <<- data.frame(x1 = c(1, 10), x2 = c(1, 10), Weight = c(0.5, 0.5))

  region_res <<- evaluate_promise(get_augment_region(
    "D-Optimality", init_aug, 0.25,
    y ~ Vmax * x1 * x2 / ((K1 + x1) * (K2 + x2)),
    c("Vmax", "K1", "K2"), c(1, 1, 1),
    list(x1 = c(0.1, 10), x2 = c(0.1, 10)),
    calc_optimal_design = FALSE,
    delta_val = 0.85
  ))$result
})

test_that("get_augment_region 2D returns an augment_region with region and eff_fun", {
  expect_s3_class(region_res, "augment_region")
  expect_true("region"    %in% names(region_res))
  expect_true("eff_fun"   %in% names(region_res))
  expect_true("delta_val" %in% names(region_res))
  expect_equal(region_res$delta_val, 0.85)
})

test_that("get_augment_region 2D region has x1, x2 and efficiency columns", {
  cands <- region_res$region
  expect_true(all(c("x1", "x2", "efficiency") %in% names(cands)))
  expect_true(all(cands$efficiency >= 0.85))
})

test_that("get_augment_region 2D returns a ggplot for d=2", {
  expect_s3_class(region_res$plot, "ggplot")
})

test_that("get_augment_region 2D eff_fun is callable and returns scalar", {
  f <- region_res$eff_fun
  v <- f(c(x1 = 5, x2 = 5))
  expect_length(as.numeric(v), 1L)
  expect_true(is.finite(as.numeric(v)))
})

test_that("augment_design 2D adds new_points within candidate region", {
  init_aug2 <- data.frame(x1 = c(1, 10), x2 = c(1, 10), Weight = c(0.5, 0.5))
  # Pick a candidate point
  cands <- region_res$region
  pt    <- cands[1L, c("x1", "x2")]
  pt$Weight <- 1
  aug <- evaluate_promise(augment_design(
    "D-Optimality", init_aug2, 0.25,
    y ~ Vmax * x1 * x2 / ((K1 + x1) * (K2 + x2)),
    c("Vmax", "K1", "K2"), c(1, 1, 1),
    list(x1 = c(0.1, 10), x2 = c(0.1, 10)),
    calc_optimal_design = FALSE,
    delta_val = 0.85,
    new_points = pt
  ))$result
  expect_s3_class(aug, "data.frame")
  expect_true(all(c("x1", "x2", "Weight") %in% names(aug)))
  expect_equal(sum(aug$Weight), 1, tolerance = 1e-6)
  expect_equal(nrow(aug), 3L)
})

test_that("augment_design 2D errors when new_point is outside candidate region", {
  init_bad <- data.frame(x1 = c(1, 10), x2 = c(1, 10), Weight = c(0.5, 0.5))
  bad_pt   <- data.frame(x1 = 0.11, x2 = 0.11, Weight = 1)  # very low efficiency
  expect_error(
    augment_design(
      "D-Optimality", init_bad, 0.25,
      y ~ Vmax * x1 * x2 / ((K1 + x1) * (K2 + x2)),
      c("Vmax", "K1", "K2"), c(1, 1, 1),
      list(x1 = c(0.1, 10), x2 = c(0.1, 10)),
      calc_optimal_design = FALSE,
      delta_val = 0.85,
      new_points = bad_pt
    ),
    "outside the candidate region"
  )
})

test_that("augment_design Ds-Optimality works for multi-factor with valid new_points", {
  init_ds <- data.frame(x1 = c(0.8, 10, 5), x2 = c(10, 0.8, 5), Weight = rep(1/3, 3))
  reg_ds  <- evaluate_promise(get_augment_region(
    "Ds-Optimality", init_ds, 0.25,
    y ~ Vmax * x1 * x2 / ((K1 + x1) * (K2 + x2)),
    c("Vmax", "K1", "K2"), c(1, 1, 1),
    list(x1 = c(0.1, 10), x2 = c(0.1, 10)),
    calc_optimal_design = FALSE,
    par_int = c(1), delta_val = 0.85
  ))$result
  expect_s3_class(reg_ds, "augment_region")
  expect_true("region" %in% names(reg_ds))
})


# ── A / I / L augment 2D ─────────────────────────────────────────────────────

local({
  init_ail <<- data.frame(x1 = c(0.8, 10, 5), x2 = c(10, 0.8, 5), Weight = rep(1/3, 3))
  model_ail <<- y ~ Vmax * x1 * x2 / ((K1 + x1) * (K2 + x2))
  pars_ail  <<- c("Vmax", "K1", "K2")
  vals_ail  <<- c(1, 1, 1)
  ds_ail    <<- list(x1 = c(0.1, 10), x2 = c(0.1, 10))
})

test_that("get_augment_region A-Optimality 2D returns augment_region", {
  reg <- evaluate_promise(get_augment_region(
    "A-Optimality", init_ail, 0.25, model_ail, pars_ail, vals_ail, ds_ail,
    calc_optimal_design = FALSE, delta_val = 0.85
  ))$result
  expect_s3_class(reg, "augment_region")
  expect_gte(nrow(reg$region), 1L)
  expect_true(all(reg$region$efficiency >= 0.85))
})

test_that("augment_design A-Optimality 2D augments correctly", {
  reg <- evaluate_promise(get_augment_region(
    "A-Optimality", init_ail, 0.25, model_ail, pars_ail, vals_ail, ds_ail,
    calc_optimal_design = FALSE, delta_val = 0.85
  ))$result
  best <- reg$region[which.max(reg$region$efficiency), ]
  new_pt <- data.frame(x1 = best$x1, x2 = best$x2, Weight = 1)
  aug <- evaluate_promise(augment_design(
    "A-Optimality", init_ail, 0.25, model_ail, pars_ail, vals_ail, ds_ail,
    calc_optimal_design = FALSE, delta_val = 0.85, new_points = new_pt
  ))$result
  expect_s3_class(aug, "data.frame")
  expect_equal(sum(aug$Weight), 1, tolerance = 1e-6)
  expect_equal(nrow(aug), 4L)
})

test_that("get_augment_region I-Optimality 2D returns augment_region", {
  reg <- evaluate_promise(get_augment_region(
    "I-Optimality", init_ail, 0.25, model_ail, pars_ail, vals_ail, ds_ail,
    calc_optimal_design = FALSE, delta_val = 0.85
  ))$result
  expect_s3_class(reg, "augment_region")
  expect_gte(nrow(reg$region), 1L)
})

test_that("get_augment_region L-Optimality 2D returns augment_region", {
  matB_l <- diag(3)
  reg <- evaluate_promise(get_augment_region(
    "L-Optimality", init_ail, 0.25, model_ail, pars_ail, vals_ail, ds_ail,
    calc_optimal_design = FALSE, matB = matB_l, delta_val = 0.85
  ))$result
  expect_s3_class(reg, "augment_region")
  expect_gte(nrow(reg$region), 1L)
})

# ── efficient_round and combinatorial_round multi-factor ─────────────────────

test_that("efficient_round works for multi-factor designs", {
  des <- mm2d_res$optdes
  exact <- evaluate_promise(efficient_round(des, 12L))$result
  expect_equal(sum(exact$Weight), 12L)
  expect_true(all(c("x1", "x2", "Weight") %in% names(exact)))
})

test_that("combinatorial_round works for multi-factor designs", {
  rounded <- combinatorial_round(mm2d_res, 9L)
  expect_equal(sum(rounded$Weight), 9L)
  expect_true(all(c("x1", "x2", "Weight") %in% names(rounded)))
})

test_that("print.augment_region works for 1D and 2D", {
  # 1D
  init1d <- data.frame(Point = c(30, 60, 90), Weight = rep(1/3, 3))
  reg1d  <- evaluate_promise(get_augment_region(
    "D-Optimality", init1d, 0.25,
    y ~ 10^(a - b/(c + x)), c("a","b","c"), c(8.07131, 1730.63, 233.426),
    c(1, 100), FALSE, delta_val = 0.85
  ))$result
  expect_output(print(reg1d), "Intervals")

  # 2D
  expect_output(print(region_res), "candidate points")
})
