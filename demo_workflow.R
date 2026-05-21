# =============================================================================
# optedr — Demo completo del workflow
# =============================================================================
# Cubre: opt_des, design_efficiency, augment_design, get_augment_region,
#        rounding, y las nuevas funcionalidades (max_iter, atwood, API
#        no interactiva, inv_spd fallback).
# =============================================================================

# Instalar desde GitHub (solo la primera vez o para actualizar):
# remotes::install_github("Kezrael/optedr")
#
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
new_pt  <- mean(region$region[1:2])
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
new_pts2   <- data.frame(Point = mean(region2$region[1:2]), Weight = 1)
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


# =============================================================================
# EXTENSIÓN MULTIFACTOR
# =============================================================================
# Los modelos pueden tener varias variables de diseño x1, x2, ...
# El espacio de diseño se pasa como lista nombrada: list(x1 = c(min, max), ...)
# El resto del workflow (print, summary, plot, design_efficiency) es idéntico.
# =============================================================================


# -----------------------------------------------------------------------------
# 12. Diseño D-óptimo 2D — modelo bisubstrato de Michaelis-Menten
# -----------------------------------------------------------------------------
# Cinética enzimática con dos sustratos. Parámetros: Vmax, K1, K2.
# Diseño en el espacio [0.1, 10] x [0.1, 10] de concentraciones de sustrato.
cat("\n--- 12. D-Optimality 2D (bisubstrato Michaelis-Menten) ---\n")

result_2D <- opt_des(
  criterion    = "D-Optimality",
  model        = y ~ Vmax * x1 * x2 / ((K1 + x1) * (K2 + x2)),
  parameters   = c("Vmax", "K1", "K2"),
  par_values   = c(1, 1, 1),
  design_space = list(x1 = c(0.1, 10), x2 = c(0.1, 10)),
  max_iter = 400L
)

# print muestra el criterio, el número de factores y la tabla del diseño
print(result_2D)

# summary añade el design_space y el número de puntos de soporte
summary(result_2D)

# plot devuelve el heatmap de la función de sensibilidad con:
#   - gradiente de color viridis (valores de la función)
#   - contorno blanco en el umbral del Teorema de Equivalencia (k = 3)
#   - puntos de soporte en rojo con su peso encima
plot(result_2D)

# Convergencia del algoritmo cocktail
plot(result_2D$convergence)


# -----------------------------------------------------------------------------
# 12b. Eficiencia de un diseño ad hoc en 2D
# -----------------------------------------------------------------------------
cat("\n--- 12b. Eficiencia diseño ad hoc 2D ---\n")

# Diseño uniforme en las 4 esquinas del espacio
design_adhoc_2d <- data.frame(
  x1     = c(0.1, 10,  0.1, 10),
  x2     = c(0.1, 0.1, 10,  10),
  Weight = rep(0.25, 4)
)

eff_2d <- design_efficiency(design_adhoc_2d, result_2D)
cat("Eficiencia del diseño de 4 esquinas:", round(eff_2d * 100, 2), "%\n")

# El propio diseño óptimo debe tener eficiencia 1
eff_opt <- design_efficiency(result_2D, result_2D)
cat("Eficiencia del diseño óptimo vs sí mismo:", round(eff_opt * 100, 2), "%\n")


# -----------------------------------------------------------------------------
# 12c. A-Optimality 2D
# -----------------------------------------------------------------------------
cat("\n--- 12c. A-Optimality 2D ---\n")

result_2D_A <- opt_des(
  criterion    = "A-Optimality",
  model        = y ~ Vmax * x1 * x2 / ((K1 + x1) * (K2 + x2)),
  parameters   = c("Vmax", "K1", "K2"),
  par_values   = c(1, 1, 1),
  design_space = list(x1 = c(0.1, 10), x2 = c(0.1, 10))
)

print(result_2D_A)
cat("Atwood bound:", result_2D_A$atwood, "%\n")
plot(result_2D_A)   # heatmap de la función de sensibilidad A


# -----------------------------------------------------------------------------
# 13. Diseño D-óptimo 3D — modelo trisubstrato de Michaelis-Menten
# -----------------------------------------------------------------------------
# Extensión natural a tres sustratos. Parámetros: Vmax, K1, K2, K3.
# Espacio de diseño: [0.1, 10]^3. Con 4 parámetros esperamos >= 4 puntos de soporte.
# Para d > 2 no hay heatmap; se usan print/summary para inspeccionar el resultado.
cat("\n--- 13. D-Optimality 3D (trisubstrato Michaelis-Menten) ---\n")

result_3D <- opt_des(
  criterion    = "D-Optimality",
  model        = y ~ Vmax * x1 * x2 * x3 / ((K1 + x1) * (K2 + x2) * (K3 + x3)),
  parameters   = c("Vmax", "K1", "K2", "K3"),
  par_values   = c(1, 1, 1, 1),
  design_space = list(x1 = c(0.1, 10), x2 = c(0.1, 10), x3 = c(0.1, 10))
)

# print y summary muestran la tabla con columnas x1, x2, x3, Weight
print(result_3D)
summary(result_3D)   # muestra los tres rangos de diseño

# plot avisa de que no hay visualización para d > 2
plot(result_3D)

# Convergencia
plot(result_3D$convergence)

cat("Atwood bound:", result_3D$atwood, "%\n")
cat("Número de puntos de soporte:", nrow(result_3D$optdes), "\n")


# -----------------------------------------------------------------------------
# 13b. Eficiencia de un diseño ad hoc en 3D
# -----------------------------------------------------------------------------
cat("\n--- 13b. Eficiencia diseño ad hoc 3D ---\n")

# Diseño uniforme en las 8 esquinas del cubo [0.1,10]^3
corners_3D <- expand.grid(
  x1 = c(0.1, 10), x2 = c(0.1, 10), x3 = c(0.1, 10)
)
corners_3D$Weight <- rep(1/8, 8)

eff_3d <- design_efficiency(corners_3D, result_3D)
cat("Eficiencia del diseño de 8 esquinas:", round(eff_3d * 100, 2), "%\n")


# -----------------------------------------------------------------------------
# 14. Augment 2D — get_augment_region (modo no interactivo)
# -----------------------------------------------------------------------------
# Partimos de un diseño inicial de 2 puntos y queremos añadir un tercer punto
# manteniendo al menos el 85% de la eficiencia D del diseño aumentado.
#
# get_augment_region devuelve:
#   $candidates  — data.frame con puntos candidatos (eficiencia >= delta)
#   $eff_fun     — función evaluable en cualquier punto
#   $plot        — heatmap de eficiencia (plasma) con contorno en delta y
#                  triángulos cian marcando el diseño inicial
cat("\n--- 14. get_augment_region 2D (no interactivo) ---\n")

# El diseño inicial debe tener al menos k=3 puntos para que M sea no singular.
# Usamos 3 puntos que cubren el espacio pero no son el óptimo:
init_2d <- data.frame(
  x1     = c(0.8, 10,  5),
  x2     = c(10,  0.8, 5),
  Weight = c(1/3, 1/3, 1/3)
)

region_2d <- get_augment_region(
  criterion           = "D-Optimality",
  init_design         = init_2d,
  alpha               = 0.25,
  model               = y ~ Vmax * x1 * x2 / ((K1 + x1) * (K2 + x2)),
  parameters          = c("Vmax", "K1", "K2"),
  par_values          = c(1, 1, 1),
  design_space        = list(x1 = c(0.1, 10), x2 = c(0.1, 10)),
  calc_optimal_design = FALSE,
  delta_val           = 0.85
)

# Heatmap ya mostrado automáticamente. Inspeccionamos los candidatos:
cat("Candidatos encontrados:", nrow(region_2d$candidates), "\n")
cat("Primeros candidatos:\n")
print(head(region_2d$candidates, 8))

# La función de eficiencia es evaluable en cualquier punto
cat("Eficiencia en (10, 10):", round(as.numeric(region_2d$eff_fun(c(x1=10, x2=10))), 4), "\n")
cat("Eficiencia en (0.1, 0.1):", round(as.numeric(region_2d$eff_fun(c(x1=0.1, x2=0.1))), 4), "\n")


# -----------------------------------------------------------------------------
# 14b. augment_design 2D (modo no interactivo con new_points)
# -----------------------------------------------------------------------------
# Elegimos el punto de mayor eficiencia entre los candidatos y lo añadimos.
# new_points debe ser un data.frame con columnas x1, x2, Weight.
cat("\n--- 14b. augment_design 2D (no interactivo) ---\n")

best_candidate <- region_2d$candidates[which.max(region_2d$candidates$efficiency), ]
new_pt_2d <- data.frame(
  x1     = best_candidate$x1,
  x2     = best_candidate$x2,
  Weight = 1
)
cat("Punto elegido: x1 =", round(new_pt_2d$x1, 3),
    " x2 =", round(new_pt_2d$x2, 3),
    " (eficiencia:", round(best_candidate$efficiency, 4), ")\n")

aug_2d <- augment_design(
  criterion           = "D-Optimality",
  init_design         = init_2d,
  alpha               = 0.25,
  model               = y ~ Vmax * x1 * x2 / ((K1 + x1) * (K2 + x2)),
  parameters          = c("Vmax", "K1", "K2"),
  par_values          = c(1, 1, 1),
  design_space        = list(x1 = c(0.1, 10), x2 = c(0.1, 10)),
  calc_optimal_design = TRUE,   # calcula y muestra eficiencias inicial y aumentada
  delta_val           = 0.85,
  new_points          = new_pt_2d
)

cat("Diseño aumentado:\n")
print(aug_2d)
cat("Suma de pesos:", sum(aug_2d$Weight), "\n")


# -----------------------------------------------------------------------------
# 14c. Ds-Optimality augment 2D
# -----------------------------------------------------------------------------
# Misma configuración pero optimizando para el parámetro Vmax (par_int = 1).
# La región candidata y la función de eficiencia cambian respecto a D-Optimality
# porque el criterio penaliza de forma diferente la varianza de los parámetros
# no de interés.
cat("\n--- 14c. Ds-Optimality augment 2D (parámetro de interés: Vmax) ---\n")

region_ds <- get_augment_region(
  criterion           = "Ds-Optimality",
  init_design         = init_2d,
  alpha               = 0.25,
  model               = y ~ Vmax * x1 * x2 / ((K1 + x1) * (K2 + x2)),
  parameters          = c("Vmax", "K1", "K2"),
  par_values          = c(1, 1, 1),
  design_space        = list(x1 = c(0.1, 10), x2 = c(0.1, 10)),
  calc_optimal_design = FALSE,
  par_int             = c(1),       # interés en Vmax
  delta_val           = 0.85
)

cat("Candidatos Ds:", nrow(region_ds$candidates), "\n")

# Elegimos el candidato con mayor eficiencia Ds
best_ds <- region_ds$candidates[which.max(region_ds$candidates$efficiency), ]
new_pt_ds <- data.frame(x1 = best_ds$x1, x2 = best_ds$x2, Weight = 1)

aug_ds <- augment_design(
  criterion           = "Ds-Optimality",
  init_design         = init_2d,
  alpha               = 0.25,
  model               = y ~ Vmax * x1 * x2 / ((K1 + x1) * (K2 + x2)),
  parameters          = c("Vmax", "K1", "K2"),
  par_values          = c(1, 1, 1),
  design_space        = list(x1 = c(0.1, 10), x2 = c(0.1, 10)),
  calc_optimal_design = FALSE,
  par_int             = c(1),
  delta_val           = 0.85,
  new_points          = new_pt_ds
)

cat("Diseño Ds-aumentado:\n")
print(aug_ds)


# =============================================================================
# EJEMPLOS DEL PAPER:
#   Martín-Martín, Rodríguez-Aragón & Torsney (2012)
#   "Multiplicative algorithm for computing D-optimum designs
#    for pVT measurements"
#   Chemometrics and Intelligent Laboratory Systems 111, 20-27.
# =============================================================================
# Los tres ejemplos corresponden a la ecuación de Tait para datos pVT
# (presión-volumen-temperatura) bajo distintas condiciones y modificaciones.
# El modelo base es:
#   rho(p,T) = rho0(T) * (1 - C * log((B(T)+p) / (B(T)+p0)))
# =============================================================================


# -----------------------------------------------------------------------------
# Paper Ej. 1: Tait isoterma — clorodifluorometano (1D, 3 parámetros)
# -----------------------------------------------------------------------------
# Modelo:    V(p) = V0 * (1 - C * log((B + p) / (B + p0)))
# Parámetros: (V0, B, C), valores nominales (0.815, 16.81, 0.0982)
# Espacio:   P = [10, 500] MPa  (presión de referencia p0 = 10 MPa)
# Resultado del paper: 3 puntos con pesos iguales en {10, 66.5, 500} MPa
# Eficiencia del diseño experimental (23 presiones uniformes): 57.50%
cat("\n--- Paper Ej.1: Tait isoterma 1D (V0, B, C) ---\n")

result_tait1 <- opt_des(
  criterion    = "D-Optimality",
  model        = y ~ V0 * (1 - C * log((B + x) / (B + 10))),  # p0 = 10 MPa
  parameters   = c("V0", "B", "C"),
  par_values   = c(0.815, 16.81, 0.0982),
  design_space = c(10, 500)
)
print(result_tait1)
cat("Atwood:", result_tait1$atwood, "%\n")
plot(result_tait1)

# Paper: soporte en {10, 66.5, 500}, pesos 1/3 cada uno
# Verificamos eficiencia del diseño del paper
design_paper1 <- data.frame(
  Point  = c(10, 66.5, 500),
  Weight = c(1/3, 1/3, 1/3)
)
eff_paper1 <- design_efficiency(design_paper1, result_tait1)
cat("Eficiencia diseño del paper (3 puntos):", round(eff_paper1 * 100, 2), "%\n")

# Eficiencia del diseño experimental real (23 presiones uniformes sobre [10,500])
design_unif1 <- data.frame(
  Point  = seq(10, 500, length.out = 23),
  Weight = rep(1/23, 23)
)
eff_unif1 <- design_efficiency(design_unif1, result_tait1)
cat("Eficiencia diseño uniforme 23 puntos:", round(eff_unif1 * 100, 2),
    "% (paper: 57.50%)\n")

# Tabla de eficiencias de diseños uniformes de 3-10 puntos (replica Table 1, col. k=1)
cat("\nEficiencias de diseños uniformes (réplica Table 1, columna k=1):\n")
for (np in 3:10) {
  des <- data.frame(Point  = seq(10, 500, length.out = np),
                    Weight = rep(1/np, np))
  e <- suppressMessages(design_efficiency(des, result_tait1))
  cat(sprintf("  %2d puntos: %.2f%%\n", np, e * 100))
}


# -----------------------------------------------------------------------------
# Paper Ej. 2: Tait no-isoterma — 1-feniludecano (2D, 8 parámetros)
# -----------------------------------------------------------------------------
# Modelo:    rho(p,T) = rho0(T) * (1 - C * log((B(T)+p) / (B(T)+p0)))
#            rho0(T)  = A0 + A1*T + A2*T^2 + A3*T^3
#            B(T)     = B0 + B1*T + B2*T^2
# Parámetros: (A0, A1, A2, A3, B0, B1, B2, C)  — 8 parámetros
# Nominales:  véase par_values abajo (tomados del paper)
# Espacio:   p en [0.1, 65] MPa,  T en [293.15, 353.15] K  (p0 = 0.1 MPa)
# Resultado del paper: 11 puntos de soporte, eficiencia experimental 62.37%
cat("\n--- Paper Ej.2: Tait no-isoterma 2D, 8 parámetros (1-feniludecano) ---\n")

result_tait2 <- opt_des(
  criterion    = "D-Optimality",
  model        = y ~ (A0 + A1*x2 + A2*x2^2 + A3*x2^3) *
                     (1 - C * log((B0 + B1*x2 + B2*x2^2 + x1) /
                                  (B0 + B1*x2 + B2*x2^2 + 0.1))),
  parameters   = c("A0", "A1", "A2", "A3", "B0", "B1", "B2", "C"),
  par_values   = c(0.97877, -7.8964e-7, -1.9601e-6, 1.8159e-9,
                   480.83, -1.6932, 1.6652e-3, 0.087944),
  design_space = list(x1 = c(0.1, 65), x2 = c(293.15, 353.15))
)
print(result_tait2)
cat("Atwood:", result_tait2$atwood, "%  | Puntos de soporte:", nrow(result_tait2$optdes),
    "(paper: 11)\n")
plot(result_tait2)   # heatmap de la función de sensibilidad

# Eficiencia del diseño experimental del paper: cuadrícula 14×7 uniforme
grid_p2 <- seq(0.1, 65,     length.out = 14)
grid_T2 <- seq(293.15, 353.15, length.out = 7)
design_exp2 <- expand.grid(x1 = grid_p2, x2 = grid_T2)
design_exp2$Weight <- rep(1/nrow(design_exp2), nrow(design_exp2))
eff_exp2 <- design_efficiency(design_exp2, result_tait2)
cat("Eficiencia cuadrícula 14×7:", round(eff_exp2 * 100, 2),
    "% (paper: 62.37%)\n")


# -----------------------------------------------------------------------------
# Paper Ej. 3: Tait no-isoterma con ec. Rackett — JP-10 (2D, 8 parámetros)
# -----------------------------------------------------------------------------
# Modelo:    rho(p,T) = rho0(T) * (1 - C * log((B(T)+p) / (B(T)+p0)))
#            rho0(T)  = AR / BR^(1 + (1 - T/CR)^DR)   [ecuación de Rackett]
#            B(T)     = B0 + B1*T + B2*T^2
# Parámetros: (AR, BR, CR, DR, B0, B1, B2, C)  — 8 parámetros
# Nominales:  véase par_values abajo (tomados del paper)
# Espacio:   p en [0.083, 30] MPa,  T en [270, 470] K  (p0 = 0.083 MPa)
# Resultado del paper: 10 puntos de soporte, eficiencia experimental 55.34%
cat("\n--- Paper Ej.3: Tait-Rackett 2D, 8 parámetros (JP-10) ---\n")

result_tait3 <- opt_des(
  criterion    = "D-Optimality",
  model        = y ~ (AR / BR^(1 + (1 - x2/CR)^DR)) *
                     (1 - C * log((B0 + B1*x2 + B2*x2^2 + x1) /
                                  (B0 + B1*x2 + B2*x2^2 + 0.083))),
  parameters   = c("AR", "BR", "CR", "DR", "B0", "B1", "B2", "C"),
  par_values   = c(293.822, 0.504584, 621.385, 0.574121,
                   417.1, -1.35603, 1.15586e-3, 79.1823e-3),
  design_space = list(x1 = c(0.083, 30), x2 = c(270, 470))
  # Nota: con espacios heterogéneos (30 MPa vs 200 K) el join_thresh
  # automático (min(rangos)/10 ≈ 3) es crítico para una buena convergencia.
)
print(result_tait3)
cat("Atwood:", result_tait3$atwood, "%  | Puntos de soporte:", nrow(result_tait3$optdes),
    "(paper: 10)\n")
plot(result_tait3)   # heatmap

# Eficiencia del diseño experimental del paper: cuadrícula 12×11 uniforme
grid_p3 <- seq(0.083, 30,  length.out = 12)
grid_T3 <- seq(270, 470,   length.out = 11)
design_exp3 <- expand.grid(x1 = grid_p3, x2 = grid_T3)
design_exp3$Weight <- rep(1/nrow(design_exp3), nrow(design_exp3))
eff_exp3 <- design_efficiency(design_exp3, result_tait3)
cat("Eficiencia cuadrícula 12×11:", round(eff_exp3 * 100, 2),
    "% (paper: 55.34%)\n")



result_Imf <- opt_des(
  criterion    = "I-Optimality",
  model        = y ~ Vmax * x1 * x2 / ((K1 + x1) * (K2 + x2)),
  parameters   = c("Vmax", "K1", "K2"),
  par_values   = c(1, 1, 1),
  design_space = list(x1 = c(0.1, 10), x2 = c(0.1, 10)),
  reg_int      = list(x1 = c(1, 5),   x2 = c(1, 5))
)


# =============================================================================
# CRITERIOS COMPUESTOS
# =============================================================================
# criterion = "Compound" combina varios criterios individuales mediante una
# suma ponderada de sus funciones de sensibilidad.  Los pesos se normalizan
# automáticamente a 1.  Se admite cualquier combinación de D, Ds, A, I y L
# con dos o más componentes.
# =============================================================================


# -----------------------------------------------------------------------------
# 15. D + I (70 / 30) — modelo de Antoine, espacio [1, 100]
# -----------------------------------------------------------------------------
# Equilibra la estimación global de los 3 parámetros (D) con la precisión
# de predicción en la región de interés [60, 100] (I).
cat("\n--- 15. Compound D+I (70/30) — Antoine ---\n")

result_DI <- opt_des(
  criterion    = "Compound",
  model        = y ~ 10^(a - b / (c + x)),
  parameters   = c("a", "b", "c"),
  par_values   = c(8.07131, 1730.63, 233.426),
  design_space = c(1, 100),
  compound     = list(
    list(criterion = "D-Optimality", weight = 0.7),
    list(criterion = "I-Optimality", weight = 0.3, reg_int = c(60, 100))
  )
)

print(result_DI)
cat("Atwood:", result_DI$atwood, "%\n")
plot(result_DI)       # curva de sensibilidad compuesta
plot(result_DI$convergence)

# Comparar con el D-óptimo puro
result_D_antoine <- opt_des("D-Optimality",
  y ~ 10^(a - b / (c + x)), c("a","b","c"),
  c(8.07131, 1730.63, 233.426), c(1, 100))

cat("\nD-óptimo puro:\n");        print(result_D_antoine)
cat("\nD+I compuesto (70/30):\n"); print(result_DI)
# El compuesto desplaza algún punto hacia la región [60,100]
# respecto al D puro, sacrificando algo de eficiencia D a cambio
# de mayor precisión predictiva en esa zona.

# Eficiencia del compuesto medida con cada criterio individual
cat("\nEficiencia D del diseño compuesto vs D-óptimo:\n")
design_efficiency(result_DI, result_D_antoine)

cat("\nEficiencia compuesta del D-óptimo puro vs diseño compuesto:\n")
design_efficiency(result_D_antoine, result_DI)


# -----------------------------------------------------------------------------
# 16. D + A (50 / 50) — modelo biparamétrico clásico
# -----------------------------------------------------------------------------
# D minimiza el volumen del elipsoide de confianza de los parámetros;
# A minimiza la traza de la inversa (promedio de varianzas individuales).
# Al combinarlos al 50% el diseño pondera tanto el volumen como el promedio.
cat("\n--- 16. Compound D+A (50/50) ---\n")

result_DA <- opt_des(
  criterion    = "Compound",
  model        = y ~ a * exp(-b / x),
  parameters   = c("a", "b"),
  par_values   = c(1, 1500),
  design_space = c(212, 422),
  compound     = list(
    list(criterion = "D-Optimality", weight = 0.5),
    list(criterion = "A-Optimality", weight = 0.5)
  )
)

print(result_DA)
cat("Atwood:", result_DA$atwood, "%\n")
plot(result_DA)
summary(result_DA)   # muestra la composición del criterio


# -----------------------------------------------------------------------------
# 17. Tres criterios: D + A + I (50 / 25 / 25) — Antoine 2D
# -----------------------------------------------------------------------------
# Ejemplo con tres componentes y modelo multifactor para mostrar que el
# criterio compuesto funciona también en d > 1.
cat("\n--- 17. Compound D+A+I (50/25/25) 2D ---\n")

result_DAI_2d <- opt_des(
  criterion    = "Compound",
  model        = y ~ Vmax * x1 * x2 / ((K1 + x1) * (K2 + x2)),
  parameters   = c("Vmax", "K1", "K2"),
  par_values   = c(1, 1, 1),
  design_space = list(x1 = c(0.1, 10), x2 = c(0.1, 10)),
  compound     = list(
    list(criterion = "D-Optimality", weight = 0.50),
    list(criterion = "A-Optimality", weight = 0.25),
    list(criterion = "I-Optimality", weight = 0.25,
         reg_int = list(x1 = c(1, 5), x2 = c(1, 5)))
  )
)

print(result_DAI_2d)
cat("Atwood:", result_DAI_2d$atwood, "%\n")
plot(result_DAI_2d)   # heatmap de sensibilidad compuesta (d=2)


# =============================================================================
# VISUALIZACIÓN PARA d > 2
# =============================================================================
# Para modelos con 3 o más factores, plot() devuelve un scatter matrix:
# un panel por cada par de variables de diseño (C(d,2) paneles en total),
# con tamaño de punto proporcional al peso y facetas "xi vs xj".
# No hay heatmap de sensibilidad disponible para d > 2.
# =============================================================================


# -----------------------------------------------------------------------------
# 18. Scatter matrix — diseño 3D (trisubstrato, 4 parámetros)
# -----------------------------------------------------------------------------
cat("\n--- 18. Scatter matrix plot — diseño 3D ---\n")

result_3D_vis <- opt_des(
  criterion    = "D-Optimality",
  model        = y ~ Vmax * x1 * x2 * x3 / ((K1 + x1) * (K2 + x2) * (K3 + x3)),
  parameters   = c("Vmax", "K1", "K2", "K3"),
  par_values   = c(1, 1, 1, 1),
  design_space = list(x1 = c(0.1, 10), x2 = c(0.1, 10), x3 = c(0.1, 10))
)

# C(3,2) = 3 paneles: x1 vs x2 | x1 vs x3 | x2 vs x3
plot(result_3D_vis)

# Convergencia del algoritmo
plot(result_3D_vis$convergence)

cat("Puntos de soporte:", nrow(result_3D_vis$optdes), "\n")
print(result_3D_vis)


# -----------------------------------------------------------------------------
# 19. Scatter matrix — diseño 4D (modelo lineal, 4 factores)
# -----------------------------------------------------------------------------
# C(4,2) = 6 paneles. Los 4 factores en [0,1] dan un D-óptimo
# en las 2^4 = 16 esquinas del hipercubo (diseño factorial completo).
cat("\n--- 19. Scatter matrix plot — diseño 4D ---\n")

result_4D_vis <- opt_des(
  criterion    = "D-Optimality",
  model        = y ~ a*x1 + b*x2 + c*x3 + d*x4,
  parameters   = c("a", "b", "c", "d"),
  par_values   = c(1, 1, 1, 1),
  design_space = list(x1 = c(0, 1), x2 = c(0, 1), x3 = c(0, 1), x4 = c(0, 1))
)

# 6 paneles: x1 vs x2 | x1 vs x3 | x1 vs x4 | x2 vs x3 | x2 vs x4 | x3 vs x4
plot(result_4D_vis)
print(result_4D_vis)
