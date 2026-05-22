# =============================================================================
# optedr — Workflow completo de ejemplos
# =============================================================================
# Carga del paquete (desarrollo local):
# remotes::install_github("Kezrael/optedr")   # primera vez
devtools::load_all()


# =============================================================================
# SECCIÓN 1  DISEÑO ÓPTIMO UNIFACTORIAL
# =============================================================================
# Modelos con una sola variable de diseño x en un intervalo [a, b].
# El espacio de diseño se pasa como design_space = c(a, b).
# El objeto devuelto por opt_des() es de clase "optdes" con componentes
# $optdes (tabla Point/Weight), $criterion, $atwood, $crit_value, $sens,
# $convergence. Los métodos print(), summary() y plot() están disponibles.
# =============================================================================


# -----------------------------------------------------------------------------
# 1.1  Criterios de optimalidad — modelo exponencial de 2 parámetros
# -----------------------------------------------------------------------------
# D-Optimality: minimiza det(M^{-1}), equivalente a minimizar el volumen del
# elipsoide de confianza de los parámetros.

result_D <- opt_des(
  criterion    = "D-Optimality",
  model        = y ~ a * exp(-b / x),
  parameters   = c("a", "b"),
  par_values   = c(1, 1500),
  design_space = c(212, 422)
)
print(result_D)
cat("Atwood:", result_D$atwood, "%  | Criterio:", result_D$crit_value, "\n")
plot(result_D)            # curva de sensibilidad + puntos de soporte
plot(result_D$convergence)


# Ds-Optimality: enfoca la estimación en un subconjunto de parámetros (par_int).
result_Ds <- opt_des(
  criterion    = "Ds-Optimality",
  model        = y ~ th0 * exp(x / th1),
  parameters   = c("th0", "th1"),
  par_values   = c(10.4963, -3.2940),
  design_space = c(0.94, 30),
  par_int      = c(1)       # interés solo en th0
)
print(result_Ds)
cat("Atwood:", result_Ds$atwood, "%\n")
plot(result_Ds)


# I-Optimality: minimiza la varianza de predicción integrada sobre una región
# de interés reg_int ⊆ design_space.
result_I <- opt_des(
  criterion    = "I-Optimality",
  model        = y ~ a * exp(-b / x),
  parameters   = c("a", "b"),
  par_values   = c(1, 1500),
  design_space = c(212, 422),
  reg_int      = c(380, 422)
)
print(result_I)
cat("Atwood:", result_I$atwood, "%\n")
summary(result_I)


# A-Optimality: minimiza tr(M^{-1}), equivalente al promedio de varianzas
# individuales de los estimadores.
result_A <- opt_des(
  criterion    = "A-Optimality",
  model        = y ~ a * exp(-b / x),
  parameters   = c("a", "b"),
  par_values   = c(1, 1500),
  design_space = c(212, 422)
)
print(result_A)
cat("Atwood:", result_A$atwood, "%\n")


# -----------------------------------------------------------------------------
# 1.2  Eficiencia de un diseño ad hoc frente al óptimo
# -----------------------------------------------------------------------------
# La eficiencia mide qué fracción de la información del diseño óptimo obtiene
# el diseño propuesto: eff = (det M_prop / det M_opt)^{1/k}.
# Un valor del 70% significa que el diseño propuesto necesita 1/0.7 ≈ 1.43
# veces más observaciones para igualar la precisión del óptimo.

design_ad_hoc <- data.frame(
  "Point"  = c(220, 300, 400),
  "Weight" = c(1/3, 1/3, 1/3)
)
eff <- design_efficiency(design_ad_hoc, result_D)
cat("Eficiencia del diseño ad hoc:", round(eff * 100, 2), "%\n")


# -----------------------------------------------------------------------------
# 1.3  Control de convergencia con max_iter
# -----------------------------------------------------------------------------
# El algoritmo cocktail alterna un paso de gradiente y un paso de vertex
# direction. max_iter controla cuántas iteraciones externas se realizan.

result_fast <- opt_des("D-Optimality", y ~ a * exp(-b / x),
                       c("a", "b"), c(1, 1500), c(212, 422), max_iter = 5L)
result_prec <- opt_des("D-Optimality", y ~ a * exp(-b / x),
                       c("a", "b"), c(1, 1500), c(212, 422), max_iter = 50L)
cat("Atwood  5 iter:", result_fast$atwood, "%\n")
cat("Atwood 50 iter:", result_prec$atwood, "%\n")


# -----------------------------------------------------------------------------
# 1.4  Redondeo a diseños exactos
# -----------------------------------------------------------------------------
# Los diseños aproximados asignan pesos continuos.  Para obtener un diseño
# exacto de n observaciones se aplica uno de los dos métodos de redondeo.

# efficient_round: usa el multiplicador (n - l/2) y ajusta el total.
exact_design <- efficient_round(result_D$optdes, n = 20)
print(exact_design)
cat("Total observaciones:", sum(exact_design$Weight), "\n")

# combinatorial_round: busca la asignación floor/ceil óptima (O(2^k)).
combo_design <- combinatorial_round(result_D, n = 10)
print(combo_design)


# -----------------------------------------------------------------------------
# 1.5  Augment 1D — añadir puntos con pérdida controlada de eficiencia
# -----------------------------------------------------------------------------
# Dado un diseño inicial, get_augment_region() calcula la región de puntos
# candidatos que garantiza que la eficiencia del diseño aumentado supere delta.
# augment_design() añade el punto elegido y reescala los pesos.
#
# Modo no interactivo: pasar delta_val y new_points explícitamente.
# Modo interactivo:    omitir ambos; el paquete pregunta interactivamente.

init_des <- data.frame(
  "Point"  = c(30, 60, 90),
  "Weight" = c(1/3, 1/3, 1/3)
)

# Paso 1: región candidata
region <- get_augment_region(
  criterion           = "D-Optimality",
  init_design         = init_des,
  alpha               = 0.25,
  model               = y ~ 10^(a - b/(c + x)),
  parameters          = c("a", "b", "c"),
  par_values          = c(8.07131, 1730.63, 233.426),
  design_space        = c(1, 100),
  calc_optimal_design = FALSE,
  delta_val           = 0.85
)
print(region)   # intervalos candidatos + delta

# Paso 2: elegir un punto del interior de la región y aumentar
new_pt  <- mean(region$region[1:2])
new_pts <- data.frame(Point = new_pt, Weight = 1)

augmented <- augment_design(
  criterion           = "D-Optimality",
  init_design         = init_des,
  alpha               = 0.25,
  model               = y ~ 10^(a - b/(c + x)),
  parameters          = c("a", "b", "c"),
  par_values          = c(8.07131, 1730.63, 233.426),
  design_space        = c(1, 100),
  calc_optimal_design = FALSE,
  delta_val           = 0.85,
  new_points          = new_pts
)
print(augmented)
cat("Suma pesos:", sum(augmented$Weight), "\n")

# Augment con comparación al diseño óptimo (calc_optimal_design = TRUE)
region2 <- get_augment_region(
  criterion           = "D-Optimality",
  init_design         = init_des,
  alpha               = 0.25,
  model               = y ~ 10^(a - b/(c + x)),
  parameters          = c("a", "b", "c"),
  par_values          = c(8.07131, 1730.63, 233.426),
  design_space        = c(1, 100),
  calc_optimal_design = TRUE,
  delta_val           = 0.85
)
new_pts2   <- data.frame(Point = mean(region2$region[1:2]), Weight = 1)
augmented2 <- augment_design(
  criterion           = "D-Optimality",
  init_design         = init_des,
  alpha               = 0.25,
  model               = y ~ 10^(a - b/(c + x)),
  parameters          = c("a", "b", "c"),
  par_values          = c(8.07131, 1730.63, 233.426),
  design_space        = c(1, 100),
  calc_optimal_design = TRUE,
  delta_val           = 0.85,
  new_points          = new_pts2
)
print(augmented2)

# Validaciones: errores informativos ante entradas incorrectas
tryCatch(
  augment_design("D-Optimality", init_des, 0.25, y ~ 10^(a - b/(c + x)),
                 c("a","b","c"), c(8.07131, 1730.63, 233.426), c(1, 100), FALSE,
                 delta_val = 0.85,
                 new_points = data.frame(Point = 200, Weight = 1)),
  error = function(e) cat("Error (fuera del espacio):", conditionMessage(e), "\n")
)
tryCatch(
  design_efficiency(data.frame(Puntos = c(220, 400), Weight = c(0.5, 0.5)), result_D),
  error = function(e) cat("Error (columnas erróneas):", conditionMessage(e), "\n")
)


# -----------------------------------------------------------------------------
# 1.6  Modelos con problemas de identificabilidad
# -----------------------------------------------------------------------------
# Cuando los parámetros no son identificables la matriz de información es
# singular o casi singular.  inv_spd() usa pseudoinversa y advierte al usuario.
# El modelo BET de adsorción tiene tres parámetros pero solo dos combinaciones
# identificables con datos de densidad volumétrica.

resGAB <- opt_des(
  criterion    = "D-Optimality",
  model        = y ~ (w*c*k*x) / ((1-x) * (1 + (c-1)*k*x)),
  parameters   = c("w", "c", "k"),
  par_values   = c(1, 10, 0.5),
  design_space = c(0.05, 0.8)
)
plot(resGAB)   # Atwood muy alejado de 100% indica no-identificabilidad

# Modelo reparametrizado (dos parámetros identificables): converge correctamente
result_repar <- opt_des(
  criterion    = "D-Optimality",
  model        = y ~ (a * x) / ((1 - x) * (1 + b * x)),
  parameters   = c("a", "b"),
  par_values   = c(5, 4.5),
  design_space = c(0.05, 0.8)
)
print(result_repar)


# -----------------------------------------------------------------------------
# 1.7  L-Optimality — minimización de una combinación lineal de varianzas
# -----------------------------------------------------------------------------
# matB es una matriz k×k simétrica semidefinida positiva que pondera los
# parámetros.  matB = diag(c(1,0)) focaliza en la varianza de 'a' únicamente.
# Casos especiales: matB = diag(k) → A-Optimality; matB proporcional a
# la inversa del óptimo D → aproxima la D-Optimality.

result_L <- opt_des(
  criterion    = "L-Optimality",
  model        = y ~ a * exp(-b / x),
  parameters   = c("a", "b"),
  par_values   = c(1, 1500),
  design_space = c(212, 422),
  matB         = diag(c(1, 0))   # solo varianza del parámetro 'a'
)
print(result_L)
cat("Atwood:", result_L$atwood, "%\n")
plot(result_L)

# Comparación: el L-óptimo concentra puntos donde la precisión de 'a' es mayor,
# a diferencia del D-óptimo que equilibra la información de ambos parámetros.
cat("\nD-óptimo (ambos parámetros):\n");    print(result_D)
cat("L-óptimo (solo parámetro 'a'):\n");   print(result_L)


# =============================================================================
# SECCIÓN 2  DISEÑO ÓPTIMO MULTIFACTORIAL — d = 2
# =============================================================================
# Para modelos con dos variables de diseño x1, x2, el espacio de diseño se
# especifica como una lista nombrada: design_space = list(x1 = c(...), x2 = c(...)).
# plot() devuelve un heatmap viridis de la función de sensibilidad con:
#   - contorno blanco en el umbral del Teorema de Equivalencia
#   - puntos de soporte en rojo con etiquetas de peso
# =============================================================================


# -----------------------------------------------------------------------------
# 2.1  D-Optimality 2D — modelo bisubstrato de Michaelis-Menten
# -----------------------------------------------------------------------------
# Cinética enzimática con dos sustratos: y = Vmax*x1*x2/((K1+x1)*(K2+x2)).
# Con k=3 parámetros el diseño óptimo tiene al menos 3 puntos de soporte.

result_2D <- opt_des(
  criterion    = "D-Optimality",
  model        = y ~ Vmax * x1 * x2 / ((K1 + x1) * (K2 + x2)),
  parameters   = c("Vmax", "K1", "K2"),
  par_values   = c(1, 1, 1),
  design_space = list(x1 = c(0.1, 10), x2 = c(0.1, 10))
)
print(result_2D)
summary(result_2D)   # muestra el design_space y los puntos de soporte
plot(result_2D)      # heatmap de sensibilidad
plot(result_2D$convergence)


# -----------------------------------------------------------------------------
# 2.2  A-Optimality 2D
# -----------------------------------------------------------------------------
result_2D_A <- opt_des(
  criterion    = "A-Optimality",
  model        = y ~ Vmax * x1 * x2 / ((K1 + x1) * (K2 + x2)),
  parameters   = c("Vmax", "K1", "K2"),
  par_values   = c(1, 1, 1),
  design_space = list(x1 = c(0.1, 10), x2 = c(0.1, 10))
)
print(result_2D_A)
cat("Atwood:", result_2D_A$atwood, "%\n")
plot(result_2D_A)


# -----------------------------------------------------------------------------
# 2.3  I-Optimality 2D con región de interés multidimensional
# -----------------------------------------------------------------------------
# reg_int se pasa como lista nombrada con las mismas variables que design_space.
# La matriz B se calcula por integración Monte Carlo (LHS, 10 000 puntos).

result_2D_I <- opt_des(
  criterion    = "I-Optimality",
  model        = y ~ Vmax * x1 * x2 / ((K1 + x1) * (K2 + x2)),
  parameters   = c("Vmax", "K1", "K2"),
  par_values   = c(1, 1, 1),
  design_space = list(x1 = c(0.1, 10), x2 = c(0.1, 10)),
  reg_int      = list(x1 = c(1, 5),   x2 = c(1, 5))
)
print(result_2D_I)
cat("Atwood:", result_2D_I$atwood, "%\n")
plot(result_2D_I)


# -----------------------------------------------------------------------------
# 2.3b  L-Optimality 2D — combinación lineal de varianzas
# -----------------------------------------------------------------------------
# matB es una matriz k×k semidefinida positiva que pondera qué varianzas
# minimizar.  Aquí minimizamos solo la varianza de K1 (segundo parámetro),
# indiferentes a la de Vmax o K2.
#   matB = diag(c(0, 1, 0))  →  phi_L = Var(K1^)
#
# Comparación con D-óptimo: el L-óptimo puede concentrar puntos en zonas
# distintas para reducir específicamente la varianza de K1.

result_2D_L <- opt_des(
  criterion    = "L-Optimality",
  model        = y ~ Vmax * x1 * x2 / ((K1 + x1) * (K2 + x2)),
  parameters   = c("Vmax", "K1", "K2"),
  par_values   = c(1, 1, 1),
  design_space = list(x1 = c(0.1, 10), x2 = c(0.1, 10)),
  matB         = diag(c(0, 1, 0))   # solo varianza de K1
)
print(result_2D_L)
cat("Atwood:", result_2D_L$atwood, "%\n")
plot(result_2D_L)   # heatmap: la función de sensibilidad refleja solo K1

# Comparación entre criterios sobre el mismo modelo
cat("\n--- Comparación de diseños bajo distintos criterios ---\n")
cat("D-óptimo:\n");   print(result_2D)
cat("L-óptimo (K1):\n"); print(result_2D_L)
# La eficiencia L del D-óptimo frente al L-óptimo (¿cuánto pierde?)
eff_D_en_L <- design_efficiency(result_2D, result_2D_L)
cat("Eficiencia L del D-óptimo vs L-óptimo:", round(eff_D_en_L * 100, 2), "%\n")


# -----------------------------------------------------------------------------
# 2.4  Eficiencia de diseños ad hoc en 2D
# -----------------------------------------------------------------------------
# Diseño de las 4 esquinas del espacio con pesos iguales (uniforme en vertices)
design_adhoc_2d <- data.frame(
  x1     = c(0.1, 10,  0.1, 10),
  x2     = c(0.1, 0.1, 10,  10),
  Weight = rep(0.25, 4)
)
eff_2d <- design_efficiency(design_adhoc_2d, result_2D)
cat("Eficiencia 4 esquinas vs D-óptimo:", round(eff_2d * 100, 2), "%\n")

eff_opt2d <- design_efficiency(result_2D, result_2D)
cat("Eficiencia óptimo vs sí mismo:    ", round(eff_opt2d * 100, 2), "%\n")


# -----------------------------------------------------------------------------
# 2.5  Augment 2D — región candidata y ampliación del diseño
# -----------------------------------------------------------------------------
# El diseño inicial debe tener al menos k=3 puntos no singulares.
init_2d <- data.frame(
  x1     = c(0.8, 10,  5),
  x2     = c(10,  0.8, 5),
  Weight = c(1/3, 1/3, 1/3)
)

# get_augment_region muestra el heatmap de eficiencia automáticamente
# y devuelve un objeto "augment_region" con $region (candidatos), $eff_fun, $plot
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
print(region_2d)
cat("Eficiencia en (10,10):", round(as.numeric(region_2d$eff_fun(c(x1=10,x2=10))), 4), "\n")

# Uso de design_efficiency para decidir si merece la pena aumentar:
# 1) calculamos la eficiencia del diseño inicial frente al óptimo
# 2) aumentamos y calculamos la eficiencia del diseño resultante
# 3) comparamos la mejora obtenida
eff_antes <- suppressMessages(design_efficiency(init_2d, result_2D))
cat("Eficiencia ANTES de aumentar:", round(eff_antes * 100, 2), "%\n")

best_2d <- region_2d$region[which.max(region_2d$region$efficiency), ]
aug_2d  <- augment_design(
  criterion           = "D-Optimality",
  init_design         = init_2d,
  alpha               = 0.25,
  model               = y ~ Vmax * x1 * x2 / ((K1 + x1) * (K2 + x2)),
  parameters          = c("Vmax", "K1", "K2"),
  par_values          = c(1, 1, 1),
  design_space        = list(x1 = c(0.1, 10), x2 = c(0.1, 10)),
  calc_optimal_design = FALSE,
  delta_val           = 0.85,
  new_points          = data.frame(x1 = best_2d$x1, x2 = best_2d$x2, Weight = 1)
)
print(aug_2d)

eff_despues <- suppressMessages(design_efficiency(aug_2d, result_2D))
cat("Eficiencia DESPUÉS de aumentar:", round(eff_despues * 100, 2), "%\n")
cat("Mejora:", round((eff_despues - eff_antes) * 100, 2), "puntos porcentuales\n")
# La eficiencia nunca supera la del diseño óptimo (1.0), pero sí debe ser
# mayor que la del diseño inicial para confirmar que el augment fue útil.

# Augment con criterio Ds (interés solo en Vmax)
region_ds <- get_augment_region(
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
  n_lhs               = 10000
)
best_ds <- region_ds$region[which.max(region_ds$region$efficiency), ]
aug_ds  <- augment_design(
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
  new_points          = data.frame(x1 = best_ds$x1, x2 = best_ds$x2, Weight = 1),
  n_lhs               = 10000
)
print(aug_ds)


# =============================================================================
# SECCIÓN 3  DISEÑO ÓPTIMO MULTIFACTORIAL — d >= 3
# =============================================================================
# Para d > 2 el heatmap de sensibilidad no está disponible.  plot() devuelve
# un scatter matrix con C(d,2) paneles "xi vs xj", tamaño de punto proporcional
# al peso, escala absoluta en [0,1] para que diferencias pequeñas sean visualmente
# pequeñas.  get_augment_region muestra un scatter análogo coloreando candidatos
# (azul) vs no-candidatos (gris) y el diseño actual (triángulos rojos).
# =============================================================================


# -----------------------------------------------------------------------------
# 3.1  D-Optimality 3D — modelo trisubstrato (4 parámetros)
# -----------------------------------------------------------------------------
result_3D <- opt_des(
  criterion    = "D-Optimality",
  model        = y ~ Vmax * x1 * x2 * x3 / ((K1 + x1) * (K2 + x2) * (K3 + x3)),
  parameters   = c("Vmax", "K1", "K2", "K3"),
  par_values   = c(1, 1, 1, 1),
  design_space = list(x1 = c(0.1, 10), x2 = c(0.1, 10), x3 = c(0.1, 10))
)
print(result_3D)
summary(result_3D)        # muestra los 3 rangos del design_space
plot(result_3D)           # scatter matrix: C(3,2) = 3 paneles
plot(result_3D$convergence)
cat("Atwood:", result_3D$atwood, "%  | Puntos:", nrow(result_3D$optdes), "\n")


# -----------------------------------------------------------------------------
# 3.2  Eficiencia 3D — diseño de 8 esquinas vs óptimo
# -----------------------------------------------------------------------------
corners_3D        <- expand.grid(x1 = c(0.1, 10), x2 = c(0.1, 10), x3 = c(0.1, 10))
corners_3D$Weight <- rep(1/8, 8)
eff_3d <- design_efficiency(corners_3D, result_3D)
cat("Eficiencia 8 esquinas vs óptimo:", round(eff_3d * 100, 2), "%\n")
# Resultado bajo (~11%): el diseño óptimo concentra puntos en lugares muy
# específicos que un grid uniforme en esquinas no captura bien.


# -----------------------------------------------------------------------------
# 3.3  Augment 3D — región candidata como scatter matrix
# -----------------------------------------------------------------------------
# El diseño inicial necesita >= k+1 = 5 puntos en esquinas para que M sea
# no singular con k=4 parámetros.
init_3d_aug <- data.frame(
  x1     = c(0.8, 10,  10, 0.8, 10),
  x2     = c(10,  0.8, 10, 10,  0.8),
  x3     = c(10,  10,  0.8, 0.8, 10),
  Weight = rep(0.2, 5)
)

region_3d <- get_augment_region(
  criterion           = "D-Optimality",
  init_design         = init_3d_aug,
  alpha               = 0.45,
  model               = y ~ Vmax * x1 * x2 * x3 / ((K1+x1) * (K2+x2) * (K3+x3)),
  parameters          = c("Vmax", "K1", "K2", "K3"),
  par_values          = c(1, 1, 1, 1),
  design_space        = list(x1 = c(0.1, 10), x2 = c(0.1, 10), x3 = c(0.1, 10)),
  calc_optimal_design = FALSE,
  delta_val           = 0.93
)
print(region_3d)
cat("Candidatos:", nrow(region_3d$region), "\n")
plot(region_3d$plot)   # scatter matrix: azul = candidatos, rojo = diseño actual


# -----------------------------------------------------------------------------
# 3.4  D-Optimality 4D — modelo lineal, scatter matrix de 6 paneles
# -----------------------------------------------------------------------------
# Para un modelo lineal en 4 factores el D-óptimo es el diseño factorial 2^4
# (16 esquinas del hipercubo unitario).  C(4,2) = 6 pares de variables.
result_4D <- opt_des(
  criterion    = "D-Optimality",
  model        = y ~ a*x1 + b*x2 + c*x3 + d*x4,
  parameters   = c("a", "b", "c", "d"),
  par_values   = c(1, 1, 1, 1),
  design_space = list(x1 = c(0, 1), x2 = c(0, 1), x3 = c(0, 1), x4 = c(0, 1))
)
print(result_4D)
plot(result_4D)   # 6 paneles: x1vs x2, x1 vs x3, ..., x3 vs x4


# =============================================================================
# SECCIÓN 4  CRITERIOS COMPUESTOS
# =============================================================================
# criterion = "Compound" combina criterios individuales como suma ponderada
# de sus funciones de sensibilidad: d_c(x) = sum_i w_i * d_i(x).
# El Teorema de Equivalencia sigue siendo válido y el algoritmo cocktail
# converge de la misma forma.
#
# API:
#   compound = list(
#     list(criterion = "D-Optimality", weight = 0.7),
#     list(criterion = "I-Optimality", weight = 0.3, reg_int = c(60, 100))
#   )
#
# Los pesos se normalizan a 1 automáticamente.
# Se emite un warning si se detectan componentes duplicados.
# =============================================================================


# -----------------------------------------------------------------------------
# 4.1  D + I (70/30) — Antoine 1D, región de interés [60, 100]
# -----------------------------------------------------------------------------
# D asegura buena estimación global de los 3 parámetros.
# I concentra precisión predictiva en [60,100], zona de mayor interés operativo.

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
plot(result_DI)
plot(result_DI$convergence)

# Comparación con D-óptimo puro: el compuesto desplaza un punto hacia [60,100]
result_D_antoine <- opt_des("D-Optimality",
  y ~ 10^(a - b / (c + x)), c("a","b","c"),
  c(8.07131, 1730.63, 233.426), c(1, 100))
cat("\nD-óptimo puro:\n");        print(result_D_antoine)
cat("\nD+I compuesto (70/30):\n"); print(result_DI)

# Eficiencia cruzada entre los dos diseños
cat("Eficiencia D del compuesto vs D-puro:\n")
design_efficiency(result_DI, result_D_antoine)
cat("Eficiencia compuesta del D-puro vs compuesto:\n")
design_efficiency(result_D_antoine, result_DI)


# -----------------------------------------------------------------------------
# 4.2  D + A (50/50) — modelo biparamétrico
# -----------------------------------------------------------------------------
# Compromiso entre el volumen del elipsoide de confianza (D) y el promedio
# de varianzas marginales (A). summary() muestra la composición del criterio.

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
summary(result_DA)


# -----------------------------------------------------------------------------
# 4.3  D + A + I (50/25/25) — bisubstrato 2D, tres componentes
# -----------------------------------------------------------------------------
# Ejemplo con tres criterios y modelo multifactor: el heatmap de sensibilidad
# corresponde a la función compuesta d_c(x) = 0.5*d_D + 0.25*d_A + 0.25*d_I.

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
plot(result_DAI_2d)


# =============================================================================
# SECCIÓN 5  CASO DE ESTUDIO — ECUACIÓN DE TAIT PARA DATOS pVT
# =============================================================================
# Martín-Martín, Rodríguez-Aragón & Torsney (2012)
# "Multiplicative algorithm for computing D-optimum designs for pVT
#  measurements", Chemometrics and Intelligent Laboratory Systems 111, 20-27.
#
# La ecuación de Tait describe densidad o volumen de líquidos en función de
# presión p y temperatura T:
#   rho(p,T) = rho0(T) * (1 - C*log((B(T)+p)/(B(T)+p0)))
# Los tres ejemplos del paper se replican directamente con optedr.
# =============================================================================


# -----------------------------------------------------------------------------
# 5.1  Tait isoterma — clorodifluorometano (1D, k=3)
# -----------------------------------------------------------------------------
# V(p) = V0 * (1 - C * log((B+p)/(B+p0))), p0 = 10 MPa.
# Paper: 3 puntos con pesos 1/3 en {10, 66.5, 500} MPa.
# El diseño experimental real (23 presiones uniformes) tiene eficiencia 57.50%.

result_tait1 <- opt_des(
  criterion    = "D-Optimality",
  model        = y ~ V0 * (1 - C * log((B + x) / (B + 10))),
  parameters   = c("V0", "B", "C"),
  par_values   = c(0.815, 16.81, 0.0982),
  design_space = c(10, 500)
)
print(result_tait1)
cat("Atwood:", result_tait1$atwood, "%\n")
plot(result_tait1)

# Eficiencia del diseño del paper
eff_paper1 <- design_efficiency(
  data.frame(Point = c(10, 66.5, 500), Weight = rep(1/3, 3)),
  result_tait1)
cat("Eficiencia diseño paper (3 pts):", round(eff_paper1*100, 2), "%\n")

# Réplica de Table 1 (col. k=1) del paper
cat("\nEficiencias de diseños uniformes (réplica Table 1):\n")
for (np in 3:10) {
  des <- data.frame(Point = seq(10, 500, length.out=np), Weight=rep(1/np,np))
  e   <- suppressMessages(design_efficiency(des, result_tait1))
  cat(sprintf("  %2d puntos: %.2f%%\n", np, e*100))
}


# -----------------------------------------------------------------------------
# 5.2  Tait no-isoterma — 1-feniludecano (2D, k=8)
# -----------------------------------------------------------------------------
# rho0(T) = A0+A1*T+A2*T^2+A3*T^3,  B(T) = B0+B1*T+B2*T^2,  C constante.
# Espacio: p en [0.1, 65] MPa, T en [293.15, 353.15] K.
# Paper: 11 puntos, eficiencia del diseño experimental (14x7): 62.37%.

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
cat("Atwood:", result_tait2$atwood, "%  | Puntos:", nrow(result_tait2$optdes),
    "(paper: 11)\n")
plot(result_tait2)

# Eficiencia del diseño experimental (cuadrícula 14x7 uniforme)
design_exp2        <- expand.grid(x1 = seq(0.1,65,l=14), x2 = seq(293.15,353.15,l=7))
design_exp2$Weight <- rep(1/nrow(design_exp2), nrow(design_exp2))
eff_exp2 <- design_efficiency(design_exp2, result_tait2)
cat("Eficiencia 14x7:", round(eff_exp2*100,2), "% (paper: 62.37%)\n")


# -----------------------------------------------------------------------------
# 5.3  Tait-Rackett — JP-10 (2D, k=8)
# -----------------------------------------------------------------------------
# rho0(T) mediante ecuación de Rackett: AR/BR^(1+(1-T/CR)^DR).
# Espacio: p en [0.083, 30] MPa, T en [270, 470] K.
# Nota: con rangos muy distintos (30 MPa vs 200 K) el join_thresh automático
# min(rangos)/10 ≈ 3 es crítico; sin él la convergencia se degrada.
# Paper: 10 puntos, eficiencia del diseño experimental (12x11): 55.34%.

result_tait3 <- opt_des(
  criterion    = "D-Optimality",
  model        = y ~ (AR / BR^(1 + (1 - x2/CR)^DR)) *
                     (1 - C * log((B0 + B1*x2 + B2*x2^2 + x1) /
                                  (B0 + B1*x2 + B2*x2^2 + 0.083))),
  parameters   = c("AR", "BR", "CR", "DR", "B0", "B1", "B2", "C"),
  par_values   = c(293.822, 0.504584, 621.385, 0.574121,
                   417.1, -1.35603, 1.15586e-3, 79.1823e-3),
  design_space = list(x1 = c(0.083, 30), x2 = c(270, 470))
)
print(result_tait3)
cat("Atwood:", result_tait3$atwood, "%  | Puntos:", nrow(result_tait3$optdes),
    "(paper: 10)\n")
plot(result_tait3)

# Eficiencia del diseño experimental (cuadrícula 12x11 uniforme)
design_exp3        <- expand.grid(x1 = seq(0.083,30,l=12), x2 = seq(270,470,l=11))
design_exp3$Weight <- rep(1/nrow(design_exp3), nrow(design_exp3))
eff_exp3 <- design_efficiency(design_exp3, result_tait3)
cat("Eficiencia 12x11:", round(eff_exp3*100,2), "% (paper: 55.34%)\n")
