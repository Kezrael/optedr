# optedr (development version)

- Fixed bug in `DWFMult` where the inner weight loop stopped prematurely due to
  misplaced `<` operator inside `max()`.
- Fixed undefined `error_msg` variable in `design_efficiency()`.
- Fixed `weight_fun`/`grad` ordering in `opt_des()` when `distribution` is specified.
- `solve()` replaced throughout by `inv_spd()`, a Cholesky-based inverse with a
  Moore-Penrose pseudoinverse fallback and informative warning.
- `inf_mat()` vectorised using `vapply` + `crossprod` (LAPACK DSYRK); eliminates
  the R-level accumulation loop.
- `crosspoints()` rewritten using sign-change detection + `uniroot`; removes the
  10 000 sequential `nleqslv` calls. `nleqslv` dropped from Imports.
- `update_weightsDS()` now normalises weights to sum to 1, consistent with the
  D- and I-optimality update functions.
- `augment_design()` and `get_augment_region()` gain `delta_val` and `new_points`
  parameters enabling fully non-interactive / programmatic use.
- `new_points` validated for structure, positive weights, design-space membership,
  and candidate-region membership.
- `opt_des()` gains `max_iter` parameter (default 21) controlling cocktail
  algorithm outer iterations; removes non-functional `desired_output` parameter.
- `result$atwood` is now a documented list element (previously a hidden attribute).
- ggplot2 `size` aesthetic in line/segment geoms replaced by `linewidth`.

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
