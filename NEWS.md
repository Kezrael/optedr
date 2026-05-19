# optedr 2.3.0.9000

## Bug fixes
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
