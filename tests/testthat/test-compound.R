library(optedr)

# Shared fixtures
model  <- y ~ a * exp(-b / x)
params <- c("a", "b")
pvals  <- c(1, 1500)
dspace <- c(212, 422)

compound_di <- list(
  list(criterion = "D-Optimality", weight = 0.7),
  list(criterion = "I-Optimality", weight = 0.3, reg_int = c(380, 422))
)
compound_da <- list(
  list(criterion = "D-Optimality", weight = 0.5),
  list(criterion = "A-Optimality", weight = 0.5)
)

local({
  res_di <<- evaluate_promise(
    opt_des("Compound", model, params, pvals, dspace, compound = compound_di)
  )$result
  res_da <<- evaluate_promise(
    opt_des("Compound", model, params, pvals, dspace, compound = compound_da)
  )$result
})


# ── Result structure ──────────────────────────────────────────────────────────

test_that("Compound result has class optdes", {
  expect_s3_class(res_di, "optdes")
})

test_that("Compound result has all required components", {
  expect_named(res_di,
               c("optdes", "convergence", "sens", "criterion", "crit_value", "atwood"),
               ignore.order = TRUE)
  expect_equal(res_di$criterion, "Compound")
})

test_that("Compound weights sum to 1", {
  expect_equal(sum(res_di$optdes$Weight), 1, tolerance = 1e-6)
})

test_that("Compound atwood is non-negative and <= 100", {
  expect_gte(res_di$atwood, 0)
  expect_lte(res_di$atwood, 100)
})

test_that("Compound design has Point and Weight columns (1D)", {
  expect_true(all(c("Point", "Weight") %in% names(res_di$optdes)))
})

test_that("Compound hidden_value stores compound_specs list", {
  specs <- attr(res_di, "hidden_value")
  expect_type(specs, "list")
  expect_length(specs, 2L)
  expect_true(all(sapply(specs, function(s) "weight" %in% names(s))))
})

test_that("Compound weights are normalised to sum 1 even when input doesn't", {
  res_unnorm <- evaluate_promise(
    opt_des("Compound", model, params, pvals, dspace,
            compound = list(
              list(criterion = "D-Optimality", weight = 7),
              list(criterion = "A-Optimality", weight = 3)
            ))
  )$result
  specs <- attr(res_unnorm, "hidden_value")
  expect_equal(sum(sapply(specs, `[[`, "weight")), 1, tolerance = 1e-10)
})


# ── Sub-criterion combinations ────────────────────────────────────────────────

test_that("D + A Compound converges with atwood > 0", {
  expect_s3_class(res_da, "optdes")
  expect_gte(res_da$atwood, 0)
})

test_that("D + Ds Compound works", {
  res_dds <- evaluate_promise(
    opt_des("Compound", model, params, pvals, dspace,
            compound = list(
              list(criterion = "D-Optimality",  weight = 0.6),
              list(criterion = "Ds-Optimality", weight = 0.4, par_int = c(1L))
            ))
  )$result
  expect_s3_class(res_dds, "optdes")
  expect_equal(sum(res_dds$optdes$Weight), 1, tolerance = 1e-6)
})

test_that("Three-criterion compound (D + A + I) works", {
  res_3 <- evaluate_promise(
    opt_des("Compound", model, params, pvals, dspace,
            compound = list(
              list(criterion = "D-Optimality", weight = 0.5),
              list(criterion = "A-Optimality", weight = 0.3),
              list(criterion = "I-Optimality", weight = 0.2, reg_int = c(380, 422))
            ))
  )$result
  expect_s3_class(res_3, "optdes")
  specs <- attr(res_3, "hidden_value")
  expect_length(specs, 3L)
})


# ── print / summary ───────────────────────────────────────────────────────────

test_that("print.optdes for Compound shows composition", {
  out <- capture.output(print(res_di))
  expect_true(any(grepl("Compound", out)))
  expect_true(any(grepl("0.70", out) | grepl("0.7", out)))
})

test_that("summary.optdes for Compound shows criterion list", {
  out <- capture.output(summary(res_di))
  expect_true(any(grepl("Compound criterion", out)))
  expect_true(any(grepl("D-Optimality", out)))
  expect_true(any(grepl("I-Optimality", out)))
})

test_that("plot.optdes for Compound returns a ggplot", {
  p <- plot(res_di)
  expect_s3_class(p, "ggplot")
})


# ── design_efficiency ─────────────────────────────────────────────────────────

test_that("design_efficiency works for Compound: optimal vs itself = 1", {
  eff <- evaluate_promise(design_efficiency(res_di, res_di))$result
  expect_equal(eff, 1, tolerance = 1e-6)
})

test_that("design_efficiency for Compound: suboptimal design < 1", {
  des_unif <- data.frame(
    Point  = seq(212, 422, length.out = 5),
    Weight = rep(0.2, 5)
  )
  eff <- evaluate_promise(design_efficiency(des_unif, res_di))$result
  expect_gte(eff, 0)
})


# ── Validation errors ─────────────────────────────────────────────────────────

test_that("Compound errors when compound list is NULL", {
  expect_error(
    opt_des("Compound", model, params, pvals, dspace),
    "compound list"
  )
})

test_that("Compound errors when compound list has only one element", {
  expect_error(
    opt_des("Compound", model, params, pvals, dspace,
            compound = list(list(criterion = "D-Optimality", weight = 1))),
    "compound list"
  )
})

test_that("Compound errors when a sub-criterion is unknown", {
  expect_error(
    opt_des("Compound", model, params, pvals, dspace,
            compound = list(
              list(criterion = "D-Optimality",  weight = 0.5),
              list(criterion = "E-Optimality",  weight = 0.5)
            )),
    "Unknown sub-criterion"
  )
})

test_that("Compound warns when weights don't sum to 1", {
  expect_message(
    opt_des("Compound", model, params, pvals, dspace,
            compound = list(
              list(criterion = "D-Optimality", weight = 7),
              list(criterion = "A-Optimality", weight = 3)
            )),
    "normalising"
  )
})

test_that("Compound normalises weights to sum 1 after warning", {
  r <- suppressMessages(
    opt_des("Compound", model, params, pvals, dspace,
            compound = list(
              list(criterion = "D-Optimality", weight = 7),
              list(criterion = "A-Optimality", weight = 3)
            ))
  )
  ws <- sapply(attr(r, "hidden_value"), `[[`, "weight")
  expect_equal(sum(ws), 1, tolerance = 1e-10)
  expect_equal(ws[1], 0.7, tolerance = 1e-10)
})

test_that("Compound warns on exact duplicate criterion", {
  expect_warning(
    suppressMessages(
      opt_des("Compound", model, params, pvals, dspace,
              compound = list(
                list(criterion = "D-Optimality", weight = 0.6),
                list(criterion = "D-Optimality", weight = 0.4)
              ))
    ),
    "Duplicate"
  )
})

test_that("Compound does NOT warn for Ds with different par_int", {
  warns <- character(0)
  withCallingHandlers(
    suppressMessages(
      opt_des("Compound", model, params, pvals, dspace,
              compound = list(
                list(criterion = "Ds-Optimality", weight = 0.5, par_int = c(1L)),
                list(criterion = "Ds-Optimality", weight = 0.5, par_int = c(2L))
              ))
    ),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  dup_warns <- grep("Duplicate", warns, value = TRUE)
  expect_length(dup_warns, 0L)
})

test_that("Compound errors when L-Optimality component has no matB", {
  expect_error(
    opt_des("Compound", model, params, pvals, dspace,
            compound = list(
              list(criterion = "D-Optimality", weight = 0.5),
              list(criterion = "L-Optimality", weight = 0.5)
            )),
    "matB"
  )
})
