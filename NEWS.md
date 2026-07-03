# optedr 3.0.1

## Bug fix

- Fixed a test failure on CRAN's ATLAS platform (R-devel, r90186): when a
  non-identifiable model causes the gradient to return `NA`/`NaN`, the
  criterion value `dcrit()` now returns `NA` instead of `Inf` under the
  ATLAS BLAS/LAPACK implementation. The previous trimming of the convergence
  vector (`1L:(length(v) - sum(v == 0))`) crashed on `NA` input. The vector
  is now truncated via `seq_len(index)` and a guard checks for any non-finite
  criterion value (NA, NaN, or Inf), throwing an informative "not identifiable"
  error consistently across all BLAS implementations and all optimality criteria
  (D, Ds, A, I, L, Compound).

# optedr 3.0.0

## New features — KL-Optimality (model discrimination)
- `opt_des()` now accepts `criterion = "KL-Optimality"` to find designs that
  best discriminate between a reference model and a rival model, maximising
  the Kullback-Leibler divergence between them (T-optimality generalised to
  exponential-family GLMs).
- `make_kl_fun()` builds the point KL-divergence function `kl_fun(x, beta2)`
  from two model/family specifications, supporting `"Normal"`, `"Poisson"`,
  `"Binomial"` and `"Gamma"` families, including cross-dispersion Normal vs.
  Normal and Gamma vs. Gamma pairs. Other family pairs can be supplied via a
  user-written `kl_fun`.
- The inner adversarial optimisation searches for the rival parameters that
  minimise the integrated KL divergence (`rival_pars`, `rival_lower`,
  `rival_upper`); the cocktail algorithm then finds the design maximising it.
- `design_efficiency()` and `summary.optdes()` support KL-Optimality results,
  reporting the family/model information or "user-supplied" for a custom
  `kl_fun`.

## New features — compound optimality criteria
- `opt_des()` now accepts `criterion = "Compound"` with a `compound` argument:
  a list of two or more per-criterion specifications, each with `criterion`
  and `weight` fields (plus criterion-specific parameters: `par_int` for
  Ds, `reg_int` for I, `matB` for L). Weights are normalised automatically.
- The compound sensitivity function `d_c(x) = sum_i w_i * d_i(x)` is
  a valid convex combination; the cocktail algorithm converges via the same
  multiplicative weight update used for I/A/L-Optimality.
- `print.optdes` and `summary.optdes` show the compound composition
  (weights and sub-criteria) when `criterion = "Compound"`.
- `design_efficiency()` computes compound efficiency as
  `phi_c(M*) / phi_c(M_xi)` where `phi_c = sum_i w_i * phi_i`.
- Any combination of D, Ds, A, I and L sub-criteria is supported, including
  three or more components.

# optedr 2.4.0.9000

## Breaking changes
- `get_augment_region()` now returns an `"augment_region"` S3 object instead
  of a plain numeric vector. The crosspoints vector is accessible via
  `region$region`; multi-factor calls return a candidate data frame there.
  A `print` method summarises intervals (1D) or candidate count and efficiency
  range (multi-factor).

## New features — multi-factor support
- `opt_des()` now accepts multi-factor models using the naming convention
  `x1`, `x2`, … for design variables and `design_space` as a named list,
  e.g. `list(x1 = c(0, 10), x2 = c(0, 5))`. Single-factor models using `x`
  are fully backward compatible.
- The cocktail algorithm dispatches to 1D or multi-factor paths transparently;
  for `d = 2` the sensitivity plot is a viridis heatmap with the Equivalence
  Theorem contour and support-point labels overlaid.
- `design_efficiency()` accepts designs with `x1`, `x2`, … column names in
  addition to the legacy `Point` / `Weight` format.
- `augment_design()` and `get_augment_region()` support multi-factor models
  for all five criteria (D, Ds, A, I, L). For `d = 2` the candidate region is
  shown as a plasma heatmap with a white contour at `delta_val`; for any `d`
  a sample of candidate points is printed. Interactive coordinate-by-coordinate
  prompting works analogously to the 1D readline workflow.
- `efficient_round()` and `combinatorial_round()` accept any data frame with
  a `Weight` column, including multi-factor designs with `x1`, `x2`, … columns.
- I-Optimality in `opt_des()` and `get_augment_region()` / `augment_design()`
  accepts `reg_int` as a named list for multi-factor models.

## Bug fixes
- Fixed `DsWFMult` and `IWFMult`: the sensitivity plot was built from the
  stale closure `sensDs`/`sensI` (last inner-loop iteration) instead of the
  final recomputed `sensM`. Caused `Inf`/`NaN` values in A/I/L-optimality
  heatmaps.
- Fixed `geom_contour` inheriting `fill` aesthetic from the global `aes`,
  producing a spurious warning in 2D sensitivity plots.
- Fixed `gradient22()`: hardcoded `"x"` replaced by `detect_design_vars()`,
  enabling Ds-Optimality augment for multi-factor models.
- Fixed `join_thresh` heuristic: changed from `mean(ranges)/10` to
  `min(ranges)/10` so designs with heterogeneous factor scales (e.g. MPa vs K)
  converge correctly.
- Fixed bug in `DWFMult` where the inner weight loop stopped after a single
  iteration due to a misplaced `<` operator inside `max()`.
- Fixed undefined `error_msg` variable in `design_efficiency()`.
- Fixed `weight_fun`/`grad` ordering in `opt_des()` when `distribution` is
  specified (gradient was computed before the weight function was updated).
- Fixed wrong URL in `shiny_optimal()` error message for missing `markdown`.

## New features
- `augment_design()` and `get_augment_region()` gain `delta_val` and
  `new_points` parameters enabling fully non-interactive / programmatic use.
  `new_points` is validated for structure, positive weights, design-space
  membership, and candidate-region membership.
- `opt_des()` gains `max_iter` parameter (default 21) controlling the number
  of outer iterations of the cocktail algorithm.
- `efficient_round()` gains a `seed` parameter for reproducible tie-breaking.
- `combinatorial_round()` gains `max_support` (default 15) and `ask`
  parameters. When the design exceeds `max_support` support points the
  function shows an estimated computation time and prompts for confirmation
  in interactive sessions, or stops with an informative error in
  non-interactive ones. `ask = FALSE` bypasses the check.

## Improvements
- `solve()` replaced throughout by `inv_spd()`, a Cholesky-based stable
  inverse with a Moore-Penrose pseudoinverse fallback and an informative
  warning that diagnoses the number of non-identifiable parameters.
- `inf_mat()` vectorised using `vapply` + `crossprod` (LAPACK DSYRK).
- `crosspoints()` rewritten with sign-change detection + `uniroot`; removes
  10 000 sequential `nleqslv` calls. `nleqslv` moved from Imports to Suggests.
- `update_weightsDS()` now normalises weights to sum to 1.
- `update_weights*()` functions stop with an informative error when weights
  become non-finite (diagnosis: model parameter redundancy).
- All three cocktail algorithms check that the Atwood efficiency bound lies
  in [0, 100] and warn otherwise — the silent-failure signature of
  A/I/L-optimality applied to non-identifiable models.
- `result$atwood` is now a documented list element (was a hidden attribute).
- ggplot2 `size` aesthetic in line/segment geoms replaced by `linewidth`.
- `shiny_optimal()` and `shiny_augment()` emit an informative message with
  the web URL when called outside an interactive session; check all required
  packages simultaneously and list every missing one in a single error.

# Optedr 1.0.1
Removed dependency from package "nloptr" as it's set to be archived

# Optedr 2.0.0

- Added functionality to augment designs for Ds- and L-optimality.
- `opt_des` now calculates designs for different probability distributions, included some defaults
- Added function `get_augment_region`

# Optedr 2.2.0

- Added functionality for a new rounding method, `combinatorial_round`.
- Unified notation throughout functions.
- Changed the orther of parameters in `design_efficiency` to be in line with theory conventions.
