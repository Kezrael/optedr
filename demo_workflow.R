# =============================================================================
# optedr — Demo completo del workflow
# =============================================================================
# Cubre: opt_des, design_efficiency, augment_design, get_augment_region,
#        rounding, y las nuevas funcionalidades (max_iter, atwood, API
#        no interactiva, inv_spd fallback).
# =============================================================================

# library(optedr)
devtools::load_all()

# -----------------------------------------------------------------------------
# 1. Diseño D-óptimo — modelo biparamétrico
# -----------------------------------------------------------------------------
cat("\n--- 1. D-Optimality (2 parámetros) ---\n")

result_D <- opt_des(
  criterion    = "D-Optimality",
  model        = y ~ a * exp(-b / x),
  parameters   = c("a", "b"),
  par_values   = c(1, 1500),
  design_space = c(212, 422)
)

print(result_D)
cat("Atwood bound:", result_D$atwood, "%\n")
cat("Criterion value:", result_D$crit_value, "\n")

# Gráfico de la función de sensibilidad (verifica el Teorema de Equivalencia)
plot(result_D)

# Gráfico de convergencia del algoritmo
plot(result_D$convergence)


# -----------------------------------------------------------------------------
# 2. Eficiencia de un diseño ad-hoc vs el óptimo
# -----------------------------------------------------------------------------
cat("\n--- 2. Eficiencia frente al óptimo ---\n")

design_ad_hoc <- data.frame(
  "Point"  = c(220, 300, 400),
  "Weight" = c(1/3, 1/3, 1/3)
)
eff <- design_efficiency(design_ad_hoc, result_D)
cat("Eficiencia del diseño ad hoc:", round(eff * 100, 2), "%\n")


# -----------------------------------------------------------------------------
# 3. Ds-Optimality — parámetros de interés
# -----------------------------------------------------------------------------
cat("\n--- 3. Ds-Optimality (interés en th0) ---\n")

result_Ds <- opt_des(
  criterion    = "Ds-Optimality",
  model        = y ~ th0 * exp(x / th1),
  parameters   = c("th0", "th1"),
  par_values   = c(10.4963, -3.2940),
  design_space = c(0.94, 30),
  par_int      = c(1)
)

print(result_Ds)
cat("Atwood bound:", result_Ds$atwood, "%\n")
plot(result_Ds)


# -----------------------------------------------------------------------------
# 4. I-Optimality — varianza integrada en una región de interés
# -----------------------------------------------------------------------------
cat("\n--- 4. I-Optimality (región de interés [380, 422]) ---\n")

result_I <- opt_des(
  criterion    = "I-Optimality",
  model        = y ~ a * exp(-b / x),
  parameters   = c("a", "b"),
  par_values   = c(1, 1500),
  design_space = c(212, 422),
  reg_int      = c(380, 422)
)

print(result_I)
cat("Atwood bound:", result_I$atwood, "%\n")
summary(result_I)


# -----------------------------------------------------------------------------
# 5. A-Optimality
# -----------------------------------------------------------------------------
cat("\n--- 5. A-Optimality ---\n")

result_A <- opt_des(
  criterion    = "A-Optimality",
  model        = y ~ a * exp(-b / x),
  parameters   = c("a", "b"),
  par_values   = c(1, 1500),
  design_space = c(212, 422)
)

print(result_A)
cat("Atwood bound:", result_A$atwood, "%\n")


# -----------------------------------------------------------------------------
# 6. Control de iteraciones con max_iter
# -----------------------------------------------------------------------------
cat("\n--- 6. max_iter: convergencia rápida vs precisa ---\n")

result_fast <- opt_des(
  "D-Optimality", y ~ a * exp(-b / x),
  c("a", "b"), c(1, 1500), c(212, 422),
  max_iter = 5L
)
result_prec <- opt_des(
  "D-Optimality", y ~ a * exp(-b / x),
  c("a", "b"), c(1, 1500), c(212, 422),
  max_iter = 50L
)
cat("Atwood (5 iter):", result_fast$atwood, "%\n")
cat("Atwood (50 iter):", result_prec$atwood, "%\n")


# -----------------------------------------------------------------------------
# 7. Redondeo eficiente (diseño aproximado → exacto)
# -----------------------------------------------------------------------------
cat("\n--- 7. Rounding (n = 20 observaciones) ---\n")

exact_design <- efficient_round(result_D$optdes, n = 20)
print(exact_design)
cat("Total observaciones:", sum(exact_design$Weight), "\n")

# Redondeo combinatorial (busca la mejor asignación entre floor/ceil)
cat("\n--- 7b. Combinatorial round (n = 10) ---\n")
combo_design <- combinatorial_round(result_D, n = 10)
print(combo_design)


# -----------------------------------------------------------------------------
# 8. Augment no interactivo — API nueva
# -----------------------------------------------------------------------------
cat("\n--- 8. Augment no interactivo ---\n")

init_des <- data.frame(
  "Point"  = c(30, 60, 90),
  "Weight" = c(1/3, 1/3, 1/3)
)

# Paso 1: calcular la región candidata para una eficiencia mínima del 85%
region <- get_augment_region(
  criterion            = "D-Optimality",
  init_design          = init_des,
  alpha                = 0.25,
  model                = y ~ 10^(a - b/(c + x)),
  parameters           = c("a", "b", "c"),
  par_values           = c(8.07131, 1730.63, 233.426),
  design_space         = c(1, 100),
  calc_optimal_design  = FALSE,
  delta_val            = 0.85
)
cat("Región candidata:", region, "\n")

# Paso 2: elegir un punto del interior de la región y aumentar el diseño
new_pt  <- mean(region[1:2])
new_pts <- data.frame(Point = new_pt, Weight = 1)

augmented <- augment_design(
  criterion            = "D-Optimality",
  init_design          = init_des,
  alpha                = 0.25,
  model                = y ~ 10^(a - b/(c + x)),
  parameters           = c("a", "b", "c"),
  par_values           = c(8.07131, 1730.63, 233.426),
  design_space         = c(1, 100),
  calc_optimal_design  = FALSE,
  delta_val            = 0.85,
  new_points           = new_pts
)
cat("Diseño aumentado:\n")
print(augmented)
cat("Suma pesos:", sum(augmented$Weight), "\n")


# -----------------------------------------------------------------------------
# 9. Augment con calc_optimal_design = TRUE (compara con óptimo)
# -----------------------------------------------------------------------------
cat("\n--- 9. Augment con comparación al diseño óptimo ---\n")

region2 <- get_augment_region(
  criterion            = "D-Optimality",
  init_design          = init_des,
  alpha                = 0.25,
  model                = y ~ 10^(a - b/(c + x)),
  parameters           = c("a", "b", "c"),
  par_values           = c(8.07131, 1730.63, 233.426),
  design_space         = c(1, 100),
  calc_optimal_design  = TRUE,   # ← calcula el óptimo y muestra eficiencias
  delta_val            = 0.85
)
# new_pts2   <- data.frame(Point = 2, Weight = 1)
new_pts2   <- data.frame(Point = mean(region2[1:2]), Weight = 1)
augmented2 <- augment_design(
  criterion            = "D-Optimality",
  init_design          = init_des,
  alpha                = 0.25,
  model                = y ~ 10^(a - b/(c + x)),
  parameters           = c("a", "b", "c"),
  par_values           = c(8.07131, 1730.63, 233.426),
  design_space         = c(1, 100),
  calc_optimal_design  = TRUE,
  delta_val            = 0.85,
  new_points           = new_pts2
)
print(augmented2)


# -----------------------------------------------------------------------------
# 10. Validaciones de nueva API (errores informativos)
# -----------------------------------------------------------------------------
cat("\n--- 10. Validaciones new_points ---\n")

tryCatch(
  augment_design("D-Optimality", init_des, 0.25, y ~ 10^(a - b/(c + x)),
                 c("a","b","c"), c(8.07131, 1730.63, 233.426), c(1, 100), FALSE,
                 delta_val = 0.85,
                 new_points = data.frame(Point = 200, Weight = 1)),  # fuera del espacio
  error = function(e) cat("Error esperado (fuera espacio):", conditionMessage(e), "\n")
)

tryCatch(
  augment_design("D-Optimality", init_des, 0.25, y ~ 10^(a - b/(c + x)),
                 c("a","b","c"), c(8.07131, 1730.63, 233.426), c(1, 100), FALSE,
                 delta_val = 0.85,
                 new_points = data.frame(Point = 50, Weight = -1)),  # peso negativo
  error = function(e) cat("Error esperado (peso negativo):", conditionMessage(e), "\n")
)

tryCatch(
  design_efficiency(data.frame(Puntos = c(220, 400), Weight = c(0.5, 0.5)), result_D),
  error = function(e) cat("Error esperado (columnas erróneas):", conditionMessage(e), "\n")
)

cat("\n=== Demo completado ===\n")


# -----------------------------------------------------------------------------
# 11. Matriz mal condicionada
# -----------------------------------------------------------------------------
resGAB.A <- opt_des(criterion    = "D-Optimality",
                    model        = y ~ (w*c*k*x)/((1-x)*(1+(c-1)*k*x)),
                    parameters   = c("w", "c","k"),
                    par_values   = c(1, 10,0.5),
                    design_space = c(0.05, 0.8))
plot(resGAB.A)

result <- opt_des(
  criterion = "D-Optimality",
  model = y ~ (a * x) / ((1 - x) * (1 + b * x)),
  parameters = c("a", "b"),
  par_values = c(5, 4.5),
  design_space = c(0.05, 0.8)
)
