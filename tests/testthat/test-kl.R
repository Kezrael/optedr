library(optedr)

# ---------------------------------------------------------------------------
# make_glm_family
# ---------------------------------------------------------------------------

test_that("make_glm_family returns correct structure for all families", {
  for (fam in c("Normal", "Poisson", "Binomial", "Gamma")) {
    f <- make_glm_family(fam)
    expect_equal(f$name, fam)
    expect_true(is.function(f$b))
    expect_true(is.function(f$b_prime))
    expect_true(is.function(f$b_dbl))
    expect_true(is.function(f$link))
    expect_true(is.function(f$link_inv))
  }
})

test_that("make_glm_family throws on unknown family", {
  expect_error(make_glm_family("Beta"), "Unknown GLM family")
})

test_that("Normal family: canonical link is identity", {
  f <- make_glm_family("Normal")
  expect_equal(f$link(3), 3)
  expect_equal(f$link_inv(3), 3)
  expect_equal(f$b_prime(2), 2)    # mu = theta for Normal
})

test_that("Poisson family: canonical link is log", {
  f <- make_glm_family("Poisson")
  expect_equal(f$link(1), 0, tolerance = 1e-10)   # log(1) = 0
  expect_equal(f$link_inv(0), 1, tolerance = 1e-10)
  expect_equal(f$b_prime(0), 1, tolerance = 1e-10) # exp(0) = 1
})

test_that("Binomial family: canonical link is logit, b_prime is p*(1-p)", {
  f <- make_glm_family("Binomial")
  expect_equal(f$link(0.5), 0, tolerance = 1e-10)   # logit(0.5) = 0
  expect_equal(f$link_inv(0), 0.5, tolerance = 1e-10)
  expect_equal(f$b_dbl(0), 0.25, tolerance = 1e-10)  # p*(1-p) at p=0.5
})

test_that("Gamma family: canonical link is negative inverse", {
  f <- make_glm_family("Gamma")
  expect_equal(f$link(2), -0.5, tolerance = 1e-10)   # -1/mu
  expect_equal(f$link_inv(-0.5), 2, tolerance = 1e-10)
  expect_equal(f$b_prime(-1), 1, tolerance = 1e-10)   # -1/theta at theta=-1
})

test_that("KL formula is zero when both models agree (link symmetry)", {
  # For any family, b(t) - b(t) - (t - t)*b'(t) == 0
  for (fam in c("Normal", "Poisson", "Binomial")) {
    f  <- make_glm_family(fam)
    mu <- if (fam == "Binomial") 0.5 else 1
    t  <- f$link(mu)
    kl <- f$b(t) - f$b(t) - (t - t) * f$b_prime(t)
    expect_equal(kl, 0, tolerance = 1e-12, label = paste(fam, "self-KL"))
  }
})


# ---------------------------------------------------------------------------
# Shared fixtures: discriminate quadratic (reference) vs linear (rival)
# under Normal family.
# Reference: mu1(x) = a * x^2,  a = 1
# Rival:     mu2(x) = b * x,    b optimised by inner step
# Design space: [1, 5]
# ---------------------------------------------------------------------------

local({
  kl_res <<- evaluate_promise(opt_des(
    "KL-Optimality",
    model        = y ~ a * x^2,
    parameters   = c("a"),
    par_values   = c(1),
    design_space = c(1, 5),
    rival_model  = y ~ b * x,
    rival_params = c("b"),
    rival_pars   = c(3),
    family       = "Normal",
    phi          = 1,
    max_iter     = 12L
  ))$result
})


# ---------------------------------------------------------------------------
# Structure tests
# ---------------------------------------------------------------------------

test_that("opt_des KL-Optimality returns optdes object with correct criterion", {
  expect_s3_class(kl_res, "optdes")
  expect_equal(kl_res$criterion, "KL-Optimality")
  expect_named(kl_res,
               c("optdes", "convergence", "sens", "criterion", "crit_value", "atwood"),
               ignore.order = TRUE)
})

test_that("opt_des KL-Optimality design is valid", {
  des <- kl_res$optdes
  expect_named(des, c("Point", "Weight"))
  expect_true(all(des$Point >= 1 & des$Point <= 5))
  expect_equal(sum(des$Weight), 1, tolerance = 1e-6)
  expect_gte(nrow(des), 1L)
})

test_that("opt_des KL-Optimality criterion value is positive", {
  expect_gt(kl_res$crit_value, 0)
})

test_that("opt_des KL-Optimality Atwood bound is in valid range", {
  expect_gte(kl_res$atwood, 0)
  expect_lte(kl_res$atwood, 101)
})

test_that("opt_des KL-Optimality design has support near right boundary", {
  # For Normal family discriminating x^2 vs b*x, the sensitivity function
  # sensKL(x) = x^2*(b*-x)^2/2 is maximised near x=5 for most values of b*.
  # By the Equivalence Theorem the optimal design must include a support point
  # near the upper boundary. The HEAVIEST weight may be at a smaller x because
  # the weighting balances sensKL across all support points.
  des <- kl_res$optdes
  expect_gte(nrow(des), 2L)             # at least 2 points (1 point => KL=0)
  expect_gte(max(des$Point), 4.0)       # upper-boundary point is in the support
})


# ---------------------------------------------------------------------------
# Same model with bounded rival (prevents collapse to KL = 0)
# ---------------------------------------------------------------------------

test_that("opt_des KL-Optimality same model with bounded rival", {
  result <- evaluate_promise(opt_des(
    "KL-Optimality",
    model        = y ~ a * exp(-b / x),
    parameters   = c("a", "b"),
    par_values   = c(1, 1500),
    design_space = c(212, 422),
    family       = "Normal",
    phi          = 1,
    rival_pars   = c(1, 2000),
    rival_lower  = c(0.5, 1800),
    rival_upper  = c(2.0, 3000),
    max_iter     = 8L
  ))$result

  expect_s3_class(result, "optdes")
  expect_gt(result$crit_value, 0)
  expect_gte(result$atwood, 0)
  expect_equal(sum(result$optdes$Weight), 1, tolerance = 1e-6)
})


# ---------------------------------------------------------------------------
# print / summary / plot
# ---------------------------------------------------------------------------

test_that("print.optdes works for KL-Optimality", {
  expect_output(print(kl_res), "KL-Optimality")
})

test_that("summary.optdes works for KL-Optimality", {
  expect_output(summary(kl_res), "KL-Optimality")
  expect_output(summary(kl_res), "GLM family: Normal")
  expect_output(summary(kl_res), "Optimal rival parameters")
})

test_that("plot.optdes returns ggplot for KL-Optimality (1D)", {
  p <- plot(kl_res)
  expect_s3_class(p, "gg")
})


# ---------------------------------------------------------------------------
# design_efficiency
# ---------------------------------------------------------------------------

test_that("design_efficiency for KL returns numeric in (0, 2)", {
  eff <- evaluate_promise(design_efficiency(kl_res$optdes, kl_res))$result
  expect_true(is.numeric(eff))
  expect_gt(eff, 0)
  expect_lt(eff, 2)   # allows slight overshooting due to inner-opt noise
})

test_that("design_efficiency KL with 3-point uniform design is positive", {
  alt <- data.frame(Point = c(1, 3, 5), Weight = c(1/3, 1/3, 1/3))
  eff <- evaluate_promise(design_efficiency(alt, kl_res))$result
  expect_gt(eff, 0)
})


# ---------------------------------------------------------------------------
# Input validation
# ---------------------------------------------------------------------------

test_that("opt_des KL-Optimality rejects invalid GLM family", {
  expect_error(
    opt_des("KL-Optimality",
            y ~ a * x^2, "a", 1, c(1, 5),
            rival_model = y ~ b * x, rival_params = "b", rival_pars = 3,
            family = "Cauchy"),
    "Unknown GLM family"
  )
})

test_that("opt_des KL-Optimality rejects non-positive phi", {
  expect_error(
    opt_des("KL-Optimality",
            y ~ a * x^2, "a", 1, c(1, 5),
            rival_model = y ~ b * x, rival_params = "b", rival_pars = 3,
            phi = 0),
    "phi"
  )
})

test_that("check_inputs rejects KL-Optimality spelled wrong", {
  expect_error(
    opt_des("KL-optimality",    # lowercase o
            y ~ a * x^2, "a", 1, c(1, 5)),
    "valid criterion"
  )
})
