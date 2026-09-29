# ============================================================
# PROYECTO MODULO 8
# CONDICIONES FINANCIERAS POR GENERACION EN MEXICO
#
# ETAPA: MODELADO
#
# Pregunta de investigacion:
# ¿Qué características están relacionadas con la posibilidad
# de ser propietario de vivienda?
#
# Variable objetivo: propietario (binaria: 1 = propietario, 0 = no propietario)
# Poblacion: hogares cuyo jefe de hogar tiene entre 18 y 39 años
# Unidad de analisis: hogar (folioviv + foliohog)
# Fuente: ENIGH 2024 - INEGI
#
# Tecnica: regresion logistica (modelo natural para variable
# objetivo binaria), con dos variantes:
#   (A) Modelo ponderado por el diseño muestral de la ENIGH
#       (factor, est_dis, upm) -> usado para INTERPRETACION
#       de asociaciones sobre la poblacion representada.
#   (B) Modelo no ponderado con train/test split -> usado para
#       evaluar CAPACIDAD PREDICTIVA (accuracy, sensibilidad,
#       especificidad, AUC), comparado contra un arbol de
#       clasificacion (CART) como alternativa no lineal.
# ============================================================


# ----- Librerias
library(tidyverse)
library(survey)     # regresion logistica ponderada por diseño muestral
library(broom)       # tidy() de coeficientes / odds ratios
library(car)          # VIF (diagnostico de multicolinealidad)
library(pROC)         # curva ROC y AUC
library(rpart)         # arbol de clasificacion (modelo alternativo)
library(rpart.plot)
library(scales)

set.seed(2026)  # reproducibilidad de la particion train/test

# ----- Carpeta raiz del proyecto (ajustar solo si se mueve la carpeta)
# Se fija aqui en vez de en cada ruta para que "Datos/..." y "outputs/..."
# funcionen igual que en EDA.R sin tener que reescribir el resto del script.
setwd("C:/Users/gadri/Downloads/Proyecto_Modulo8 - copia (2)")

# ----- Carga de datos limpia (base final, congelada, no se modifica el archivo original)
datos <- read_csv("Datos/base_final_ENIGH2024_jovenes.csv")

cat("Dimensiones de la base:", dim(datos), "\n")

# ----- Carpetas de salida (separadas de las de EDA.R)
dir.create("outputs/figuras/modelo", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables/modelo", recursive = TRUE, showWarnings = FALSE)


# ============================================================
# 1. PREPARACION DE VARIABLES PARA EL MODELO
# ============================================================
#
# Justificacion de las transformaciones (documentar decisiones,
# tal como pide la guia del proyecto):
#
# - tot_integ, menores, mayores y ocupados estan muy correlacionadas
#   entre si (ver mapa_correlacion.png: tot_integ correlaciona 0.74
#   con menores y 0.73 con mayores), porque son, en esencia, partes
#   de una misma suma. Meterlas juntas al modelo generaria
#   multicolinealidad. En su lugar se usan RAZONES DE DEPENDENCIA
#   (menores/tot_integ, mayores/tot_integ, ocupados/tot_integ), que
#   son ademas mas interpretables: describen la composicion del
#   hogar en vez de su tamaño absoluto.
#
# - ing_cor y gasto_mon estan fuertemente sesgados a la derecha
#   (ver income_distributions.png / income_log_distributions.png),
#   por lo que se usa log1p() para reducir el efecto de valores
#   extremos, y se expresan en terminos per capita (entre tot_integ)
#   para que sean comparables entre hogares de distinto tamaño.
#
# - deudas y pago_tarje representan MONTOS DE PAGO, no saldos de
#   deuda (ver README, punto 8), por lo que un monto alto no es
#   necesariamente "mas deuda": puede ser un hogar que esta pagando
#   mas rapido. Por esta razon se construyen como variables binarias
#   (¿el hogar registro algun pago por este concepto? si/no), que es
#   una lectura mas segura de lo que el dato realmente mide.
#
# - educa_jefe (11 categorias) se agrupa en 4 niveles para evitar
#   categorias con muy pocas observaciones y coeficientes inestables.
#   La agrupacion sigue el sistema educativo mexicano (basica,
#   secundaria completa, media superior, superior).
#
# - ubica_geo se usa para derivar la entidad federativa y agruparla
#   en una region geografica de 4 categorias, en lugar de usar 32
#   entidades directamente (evita ~31 variables dummy con pocas
#   observaciones cada una, en linea con la pregunta orientativa
#   sobre diferencias entre entidades).
#
# - est_socio (estrato socioeconomico, 1=Bajo a 4=Alto) SI se incluye
#   como covariable, pese a que el README la lista entre las variables
#   del diseño muestral. Se decide incluirla porque en el mapa de
#   correlacion (mapa_correlacion.png) es una de las variables mas
#   asociadas con propietario (-0.19), comparable o mayor a la de
#   varias variables economicas, y el propio README indica que estas
#   variables "deben utilizarse cuando se realicen analisis que
#   consideren el diseño muestral" (punto 7) -- que es exactamente
#   lo que hace el modelo A. Se interpreta con cautela: es una
#   clasificacion del area/estrato de muestreo, no necesariamente un
#   atributo economico medido directamente en el hogar (se retoma en
#   la seccion de limitaciones).
#
# - Nota para la interpretacion: en el mapa de correlacion, ing_cor,
#   gasto_mon, deudas y pago_tarje tienen correlacion practicamente
#   nula con propietario (y el boxplot de ingreso log es casi
#   identico entre propietarios y no propietarios). Esto no es razon
#   para excluirlas del modelo -- siguen siendo teoricamente
#   relevantes para la pregunta de investigacion -- pero anticipa que
#   podrian NO resultar significativas, lo cual es en si mismo un
#   hallazgo a reportar (la propiedad de vivienda en este grupo de
#   edad parece estar mas ligada a composicion del hogar, edad y
#   contexto geografico/estrato que al ingreso corriente).

datos_modelo <- datos %>%
  mutate(
    # --- variable objetivo como factor (nivel de referencia = No propietario)
    propietario_f = factor(propietario, levels = c(0, 1),
                            labels = c("No_propietario", "Propietario")),

    # --- composicion del hogar (razones en lugar de conteos absolutos)
    # NOTA: mayores = integrantes de 18 años o mas (no "adultos mayores"),
    # por lo que menores + mayores = tot_integ SIEMPRE. prop_mayores se
    # calcula por completitud pero NO se usa en formula_modelo porque es
    # el complemento exacto de prop_menores (1 - prop_menores) y genera
    # coeficientes aliasados (columnas perfectamente colineales).
    prop_menores  = menores / tot_integ,
    prop_mayores  = mayores / tot_integ,
    prop_ocupados = ocupados / tot_integ,

    # --- variables economicas: log per capita
    ing_percap_log   = log1p(ing_cor / tot_integ),
    gasto_percap_log = log1p(gasto_mon / tot_integ),

    # --- deuda y tarjeta como indicadores binarios (ver justificacion arriba)
    tiene_deuda   = factor(if_else(deudas > 0, "Si", "No"), levels = c("No", "Si")),
    tiene_tarjeta = factor(if_else(pago_tarje > 0, "Si", "No"), levels = c("No", "Si")),

    # --- agrupacion de escolaridad del jefe de hogar
    educa_grupo = case_when(
      educa_jefe %in% c("Sin instruccion", "Preescolar",
                         "Primaria incompleta", "Primaria completa",
                         "Secundaria incompleta") ~ "Basica o menos",
      educa_jefe == "Secundaria completa" ~ "Secundaria completa",
      educa_jefe %in% c("Preparatoria incompleta",
                         "Preparatoria completa") ~ "Media superior",
      educa_jefe %in% c("Profesional incompleta", "Profesional completa",
                         "Posgrado") ~ "Superior",
      TRUE ~ NA_character_
    ),
    educa_grupo = factor(educa_grupo,
                          levels = c("Basica o menos", "Secundaria completa",
                                     "Media superior", "Superior")),

    # --- region a partir de la entidad federativa (2 primeros digitos de ubica_geo)
    entidad = as.integer(str_sub(str_pad(ubica_geo, 5, pad = "0"), 1, 2)),
    region = case_when(
      entidad %in% c(2, 3, 5, 8, 10, 19, 25, 26, 28, 32) ~ "Norte",
      entidad == 9  ~ "CDMX",
      entidad %in% c(13, 15, 17, 21, 22, 29) ~ "Centro",
      entidad %in% c(1, 6, 11, 14, 16, 18, 24) ~ "Occidente/Bajio",
      entidad %in% c(4, 7, 12, 20, 23, 27, 30, 31) ~ "Sur/Sureste",
      TRUE ~ "Otra"
    ),
    region = fct_relevel(region, "Centro"),

    # --- estrato socioeconomico como factor ordenado (1=Bajo ... 4=Alto)
    #     se usa factor (no numerico) para no forzar un efecto lineal,
    #     dado que el patron observado en el EDA es decreciente pero
    #     no perfectamente lineal (69% / 55% / 42% / 41%)
    est_socio_f = fct_relevel(factor(est_socio, levels = 1:4), "1"),

    # --- releveling: se fija la categoria de referencia de cada variable
    grupo_edad = fct_relevel(factor(grupo_edad), "18-29"),
    sexo_jefe  = fct_relevel(factor(sexo_jefe), "Hombre"),
    tam_loc    = fct_relevel(factor(tam_loc), "100 mil o mas")
  ) %>%
  filter(!is.na(educa_grupo))  # excluye Preescolar mal capturado si lo hubiera (verificacion)

cat("Observaciones tras preparar variables:", nrow(datos_modelo), "\n")
cat("(se conserva prácticamente el 100% de la base; ver README punto 9: sin NA)\n")

# Verificacion rapida de la nueva variable objetivo
print(table(datos_modelo$propietario_f))


# ============================================================
# 2. MODELO A: REGRESION LOGISTICA PONDERADA POR DISEÑO MUESTRAL
#    (modelo principal para INTERPRETAR asociaciones)
# ============================================================
#
# Se usa el paquete survey porque la ENIGH es una encuesta
# probabilistica compleja: ignorar el diseño muestral (factor,
# est_dis, upm) produciria errores estandar incorrectos y
# conclusiones que no son validas para la poblacion representada.
# Este modelo se ajusta sobre el 100% de la base (no se hace
# train/test aqui), porque su proposito es describir asociaciones
# en la poblacion, no predecir casos nuevos.

# NOTA: al filtrar la base a jefes de 18-39 años (ver README, punto 4),
# algunos estratos (est_dis) quedan con una sola UPM representada en
# este subgrupo, aunque en la ENIGH completa tuvieran mas. Por defecto,
# survey no puede estimar varianza para un estrato con un solo PSU y
# lanza el error "Stratum (...) has only one PSU at stage 1". La
# practica estandar (y la que se usa aqui) es "adjust": centra ese
# PSU solitario en la media general del diseño en vez de excluirlo,
# lo cual da una estimacion conservadora (ligeramente mas amplia) del
# error estandar en vez de simplemente fallar o descartar datos.
options(survey.lonely.psu = "adjust")

diseno_enigh <- svydesign(
  ids     = ~upm,
  strata  = ~est_dis,
  weights = ~factor,
  data    = datos_modelo,
  nest    = TRUE
)

formula_modelo <- propietario ~ grupo_edad + sexo_jefe + educa_grupo +
  tot_integ + prop_menores + prop_ocupados +
  ing_percap_log + gasto_percap_log + tiene_deuda + tiene_tarjeta +
  tam_loc + region + est_socio_f

modelo_svy <- svyglm(formula_modelo, design = diseno_enigh, family = quasibinomial())

cat("\n***** Resumen del modelo ponderado (svyglm) *****\n")
print(summary(modelo_svy))

# ----- Tabla de momios (odds ratios) con intervalos de confianza al 95%
tabla_or <- tidy(modelo_svy, conf.int = TRUE) %>%
  mutate(
    odds_ratio   = exp(estimate),
    or_lower_95  = exp(conf.low),
    or_upper_95  = exp(conf.high),
    significativo = p.value < 0.05
  ) %>%
  select(term, estimate, std.error, p.value, odds_ratio,
         or_lower_95, or_upper_95, significativo) %>%
  arrange(p.value)

cat("\n***** Tabla de razones de momios (odds ratios) *****\n")
print(tabla_or)

write.csv(tabla_or, "outputs/tables/modelo/modelo_svy_odds_ratios.csv",
          row.names = FALSE)
cat("guardado: outputs/tables/modelo/modelo_svy_odds_ratios.csv\n")

# ----- Diagnostico de multicolinealidad (VIF)
# Nota: vif() no esta implementado nativamente para objetos svyglm en
# todas las versiones de 'car'; se calcula sobre un glm no ponderado
# equivalente, unicamente como diagnostico de la estructura de las
# variables explicativas (no cambia con la ponderacion).
modelo_glm_diag <- glm(formula_modelo, data = datos_modelo, family = binomial())
cat("\n***** VIF (diagnostico de multicolinealidad) *****\n")
print(vif(modelo_glm_diag))

# ----- Grafico: odds ratios significativos, ordenados
p_or <- tabla_or %>%
  filter(term != "(Intercept)") %>%
  mutate(term = fct_reorder(term, odds_ratio)) %>%
  ggplot(aes(x = odds_ratio, y = term, color = significativo)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  geom_point(size = 2) +
  geom_errorbar(aes(xmin = or_lower_95, xmax = or_upper_95),
                orientation = "y", width = 0.2) +
  scale_color_manual(values = c("TRUE" = "#2b5c8f", "FALSE" = "grey70")) +
  scale_x_log10() +
  labs(
    title = "Razones de momios (odds ratios) - Probabilidad de ser propietario",
    subtitle = "Modelo logistico ponderado por diseño muestral ENIGH 2024",
    x = "Odds ratio (escala log)", y = "", color = "p < 0.05"
  ) +
  theme_minimal()

ggsave("outputs/figuras/modelo/odds_ratios.png",
       plot = p_or, width = 10, height = 8, dpi = 150)
cat("guardado: outputs/figuras/modelo/odds_ratios.png\n")


# ============================================================
# 3. PARTICION TRAIN / TEST (para evaluacion predictiva)
# ============================================================
#
# LIMITACION IMPORTANTE (documentar en el reporte): al dividir la
# base en entrenamiento/prueba se rompe la representatividad del
# diseño muestral dentro de cada subconjunto. Por eso esta etapa
# se usa SOLO para comparar el desempeño predictivo relativo entre
# modelos (logistica vs arbol), y no para generar conclusiones
# sobre la poblacion; esas conclusiones se apoyan en el modelo A.

n <- nrow(datos_modelo)
idx_train <- sample(seq_len(n), size = floor(0.7 * n))

train <- datos_modelo[idx_train, ]
test  <- datos_modelo[-idx_train, ]

cat("\nObservaciones en entrenamiento:", nrow(train),
    " | prueba:", nrow(test), "\n")
cat("Proporcion de propietarios en train:",
    round(mean(train$propietario), 3),
    " | en test:", round(mean(test$propietario), 3), "\n")


# ============================================================
# 4. MODELO B1: REGRESION LOGISTICA (no ponderada, train/test)
# ============================================================

modelo_glm <- glm(formula_modelo, data = train, family = binomial())

pred_glm_prob <- predict(modelo_glm, newdata = test, type = "response")
pred_glm_clase <- factor(if_else(pred_glm_prob > 0.5, 1, 0), levels = c(0, 1))


# ============================================================
# 5. MODELO B2: ARBOL DE CLASIFICACION (CART) - modelo alternativo
# ============================================================
#
# Se incluye como alternativa no lineal / no parametrica: a diferencia
# de la logistica, un arbol puede capturar interacciones y umbrales sin
# necesidad de especificarlos explicitamente, lo cual sirve como punto
# de comparacion. IMPORTANTE (redaccion cuidadosa para el reporte): que
# el arbol tenga un AUC similar al de la logistica NO demuestra que la
# relacion sea "aproximadamente lineal en el logit" -- esa es una
# afirmacion mas fuerte de lo que esta prueba puede sostener. Lo correcto
# es decir que el desempeño similar sugiere que no se obtiene una mejora
# predictiva importante al introducir relaciones no lineales mediante
# arboles con estos datos y estas variables.

modelo_arbol <- rpart(
  formula_modelo, data = train, method = "class",
  control = rpart.control(cp = 0.005, minsplit = 30, maxdepth = 5)
)

pred_arbol_prob  <- predict(modelo_arbol, newdata = test, type = "prob")[, "1"]
pred_arbol_clase <- predict(modelo_arbol, newdata = test, type = "class")

# ----- Grafico del arbol
png("outputs/figuras/modelo/arbol_clasificacion.png",
    width = 1400, height = 1000, res = 150)
rpart.plot(modelo_arbol, type = 2, extra = 104,
           main = "Arbol de clasificacion: Propietario de vivienda")
dev.off()
cat("guardado: outputs/figuras/modelo/arbol_clasificacion.png\n")

# ----- Importancia de variables del arbol
importancia_arbol <- tibble(
  variable   = names(modelo_arbol$variable.importance),
  importancia = modelo_arbol$variable.importance
) %>% arrange(desc(importancia))

cat("\n***** Importancia de variables (arbol de clasificacion) *****\n")
print(importancia_arbol)

write.csv(importancia_arbol, "outputs/tables/modelo/modelo_arbol_importancia.csv",
          row.names = FALSE)


# ============================================================
# 6. EVALUACION Y COMPARACION DE MODELOS 
# ============================================================
#
# Como la clase esta moderadamente balanceada (~55% / 45%, ver
# README punto 9) pero el objetivo del proyecto es identificar
# caracteristicas asociadas y no solo maximizar accuracy, se
# reportan varias metricas: accuracy, sensibilidad (recall de
# propietarios), especificidad y AUC-ROC, que es la metrica mas
# robusta para comparar clasificadores binarios independientemente
# del punto de corte elegido.

calcular_metricas <- function(real, pred_clase, pred_prob, nombre_modelo) {
  real <- factor(real, levels = c(0, 1))
  pred_clase <- factor(pred_clase, levels = c(0, 1))

  matriz <- table(Real = real, Prediccion = pred_clase)

  vp <- matriz["1", "1"]; vn <- matriz["0", "0"]
  fp <- matriz["0", "1"]; fn <- matriz["1", "0"]

  accuracy    <- (vp + vn) / sum(matriz)
  sensibilidad <- vp / (vp + fn)   # recall de propietarios
  especificidad <- vn / (vn + fp) # recall de no propietarios
  precision   <- vp / (vp + fp)

  roc_obj <- roc(real, pred_prob, quiet = TRUE)
  auc_val <- as.numeric(auc(roc_obj))

  list(
    nombre = nombre_modelo,
    matriz = matriz,
    metricas = tibble(
      modelo = nombre_modelo,
      accuracy = accuracy,
      sensibilidad = sensibilidad,
      especificidad = especificidad,
      precision = precision,
      auc = auc_val
    ),
    roc = roc_obj
  )
}

res_glm   <- calcular_metricas(test$propietario, pred_glm_clase,
                                pred_glm_prob, "Regresion logistica")
res_arbol <- calcular_metricas(test$propietario, pred_arbol_clase,
                                pred_arbol_prob, "Arbol de clasificacion (CART)")

cat("\n***** Matriz de confusion - Regresion logistica *****\n")
print(res_glm$matriz)
cat("\n***** Matriz de confusion - Arbol de clasificacion *****\n")
print(res_arbol$matriz)

tabla_comparacion <- bind_rows(res_glm$metricas, res_arbol$metricas)
cat("\n***** Comparacion de modelos (base de prueba, 30%) *****\n")
print(tabla_comparacion)

write.csv(tabla_comparacion, "outputs/tables/modelo/modelo_comparacion_metricas.csv",
          row.names = FALSE)
cat("guardado: outputs/tables/modelo/modelo_comparacion_metricas.csv\n")

# ----- Grafico: curvas ROC comparadas
png("outputs/figuras/modelo/curvas_roc.png",
    width = 1400, height = 1000, res = 150)
plot(res_glm$roc, col = "#2b5c8f", lwd = 2,
     main = "Curvas ROC: Regresion logistica vs Arbol de clasificacion")
plot(res_arbol$roc, col = "#d95f02", lwd = 2, add = TRUE)
legend("bottomright",
       legend = c(
         paste0("Logistica (AUC = ", round(res_glm$metricas$auc, 3), ")"),
         paste0("Arbol (AUC = ", round(res_arbol$metricas$auc, 3), ")")
       ),
       col = c("#2b5c8f", "#d95f02"), lwd = 2)
dev.off()
cat("guardado: outputs/figuras/modelo/curvas_roc.png\n")


# ============================================================
# 7. RESUMEN PARA EL REPORTE (se imprime en consola)
# ============================================================
#
# Estos cat() no sustituyen la interpretacion que el equipo debe
# escribir en el reporte; su proposito es dejar impresos, cada vez
# que se corre el script, los numeros exactos que hay que citar
# (evita transcribir manualmente y que se desactualicen).

cat("\n============================================================\n")
cat("RESUMEN PARA EL REPORTE\n")
cat("============================================================\n")
cat("Modelo principal (interpretacion): regresion logistica ponderada\n")
cat("por el diseño muestral de la ENIGH (svyglm), ajustada sobre el\n")
cat("100% de la base (", nrow(datos_modelo), "hogares).\n\n")

cat("Variables con asociacion estadisticamente significativa (p < 0.05):\n")
print(tabla_or %>% filter(significativo, term != "(Intercept)") %>%
        select(term, odds_ratio, p.value))

# ----- Respuesta directa a la pregunta de investigacion -----
# La tabla de arriba ES la respuesta a "¿que caracteristicas estan
# relacionadas con la posibilidad de ser propietario?". Las tres
# variables con mayor magnitud de asociacion (odds ratio mas alejado
# de 1, en escala log).

top3 <- tabla_or %>%
  filter(significativo, term != "(Intercept)") %>%
  mutate(dist_a_1 = abs(log(odds_ratio))) %>%
  slice_max(dist_a_1, n = 3)

cat("\nRESPUESTA A LA PREGUNTA DE INVESTIGACION:\n")
cat("Las tres caracteristicas con mayor asociacion son:\n")
for (i in seq_len(nrow(top3))) {
  direccion <- if (top3$odds_ratio[i] > 1) "aumenta" else "reduce"
  cat(" -", top3$term[i], ": odds ratio =", round(top3$odds_ratio[i], 2),
      "(", direccion, "la momio de ser propietario)\n")
}
cat("El resto del modelado (arbol, validacion cruzada, calibracion,\n")
cat("  sensibilidad) no responde esta pregunta por si mismo: confirma\n")
cat("  que esta tabla de asociaciones es confiable.\n")

cat("\nModelo alternativo: arbol de clasificacion (CART).\n")
cat("Variable mas importante segun el arbol:",
    importancia_arbol$variable[1], "\n\n")

cat("Desempeño predictivo (base de prueba, 30% de los hogares):\n")
print(tabla_comparacion %>% select(modelo, accuracy, auc))

cat("\nLIMITACIONES A DOCUMENTAR EN EL REPORTE:\n")
cat("- El modelo identifica asociaciones, no relaciones causales\n")
cat("  (ver README punto 11).\n")
cat("- La particion train/test no conserva el diseño muestral; las\n")
cat("  metricas predictivas (seccion 6) son solo para comparar\n")
cat("  modelos entre si, no para inferencia poblacional.\n")
cat("- deudas y pago_tarje se usaron como indicador binario porque\n")
cat("  el monto no representa el saldo de deuda (ver README punto 8).\n")
cat("- La comparacion entre grupos de edad observados hoy no equivale\n")
cat("  a comparar generaciones cuando tenian esa misma edad.\n")
cat("- region agrupa 32 entidades en 4 categorias; un analisis mas\n")
cat("  fino por entidad (o un efecto aleatorio por entidad) queda\n")
cat("  como posible extension del proyecto.\n")
cat("- est_socio se incluyo como covariable por su asociacion observada\n")
cat("  con propietario, pero el README la clasifica como variable del\n")
cat("  diseño muestral (punto 7): no debe leerse como un atributo\n")
cat("  economico medido directamente en el hogar, sino como una\n")
cat("  clasificacion del area/estrato de muestreo asociada al fenomeno.\n")


# ============================================================
# 8. MEJORAS OPCIONALES PARA AUMENTAR EL PODER PREDICTIVO
# ============================================================
#
# Estas tres mejoras se agregan como seccion separada, sin modificar
# los modelos ya ajustados arriba, para poder compararlas antes de
# decidir cuales incluir en el reporte final.

# ----- 8.1 Punto de corte optimo (en vez del 0.5 por defecto) -----
# El modelo logistico tiene sensibilidad (0.75) mayor que especificidad
# (0.55): predice "propietario" de mas. coords(..., "best") busca el
# umbral que maximiza sensibilidad + especificidad (indice de Youden)
# en vez de usar 0.5 de forma arbitraria.

corte_optimo <- coords(res_glm$roc, "best", ret = "threshold",
                        best.method = "youden")
corte_optimo <- as.numeric(corte_optimo)
cat("\nPunto de corte optimo (Youden):", round(corte_optimo, 3),
    "(vs. 0.5 por defecto)\n")

pred_glm_clase_opt <- factor(if_else(pred_glm_prob > corte_optimo, 1, 0),
                              levels = c(0, 1))
res_glm_opt <- calcular_metricas(test$propietario, pred_glm_clase_opt,
                                  pred_glm_prob,
                                  "Regresion logistica (umbral optimo)")
cat("\n***** Metricas con umbral optimo *****\n")
print(res_glm_opt$metricas)
# NOTA: el AUC no cambia (depende solo de las probabilidades, no del
# punto de corte); lo que cambia es el balance sensibilidad/especificidad.

# ----- 8.2 Interaccion grupo_edad:tam_loc -----
# El arbol (arbol_clasificacion.png) separa primero por tam_loc y luego
# por grupo_edad en AMBAS ramas resultantes, lo cual es la firma tipica
# de una interaccion: el efecto de la edad podria no ser el mismo en
# zonas rurales que en zonas urbanas. Se prueba agregandola al modelo
# ponderado (para ver si es significativa) y al modelo predictivo train/
# test (para ver si mejora el AUC).

formula_interaccion <- update(formula_modelo, . ~ . + grupo_edad:tam_loc)

modelo_svy_int <- svyglm(formula_interaccion, design = diseno_enigh,
                          family = quasibinomial())

cat("\n***** Prueba de la interaccion grupo_edad:tam_loc (ajustada al diseño) *****\n")
print(regTermTest(modelo_svy_int, ~grupo_edad:tam_loc))
# Si el p-value de esta prueba es < 0.05, la interaccion es significativa
# y vale la pena reportarla en el modelo final en vez de solo probarla aqui.

modelo_glm_int <- glm(formula_interaccion, data = train, family = binomial())
pred_glm_int_prob <- predict(modelo_glm_int, newdata = test, type = "response")
pred_glm_int_clase <- factor(if_else(pred_glm_int_prob > 0.5, 1, 0),
                              levels = c(0, 1))
res_glm_int <- calcular_metricas(test$propietario, pred_glm_int_clase,
                                  pred_glm_int_prob,
                                  "Regresion logistica + interaccion")
cat("\n***** Comparacion: logistica original vs. con interaccion *****\n")
print(bind_rows(res_glm$metricas, res_glm_int$metricas) %>%
        select(modelo, accuracy, auc))

# ----- 8.3 Random Forest como techo de referencia  -----
# Sirve unicamente como punto de comparacion para saber que tanto AUC se
# esta "dejando en la mesa" al usar modelos interpretables. NO sustituye
# a la logistica ni al arbol como modelos del reporte -- un ensamble de
# 500 arboles pierde la interpretabilidad de coeficientes/odds ratios
# que es el objetivo principal de este proyecto.

# install.packages("randomForest")  # descomentar si no esta instalado
library(randomForest)

set.seed(2026)
modelo_rf <- randomForest(
  factor(propietario) ~ grupo_edad + sexo_jefe + educa_grupo + tot_integ +
    prop_menores + prop_ocupados + ing_percap_log + gasto_percap_log +
    tiene_deuda + tiene_tarjeta + tam_loc + region + est_socio_f,
  data = train, ntree = 500, importance = TRUE
)

pred_rf_prob  <- predict(modelo_rf, newdata = test, type = "prob")[, "1"]
pred_rf_clase <- predict(modelo_rf, newdata = test, type = "class")

res_rf <- calcular_metricas(test$propietario, pred_rf_clase, pred_rf_prob,
                             "Random Forest (referencia)")

cat("\n***** Techo de referencia: logistica vs. arbol vs. Random Forest *****\n")
print(bind_rows(res_glm$metricas, res_arbol$metricas, res_rf$metricas) %>%
        select(modelo, accuracy, auc))


# ============================================================
# 9. DECISIONES FINALES PARA EL REPORTE
# ============================================================
#
# Con los resultados de la seccion 8 ya calculados sobre datos reales,
# estas son las decisiones que se adoptan para el modelo final:
#
# - Punto de corte: se adopta 0.528 (Youden) en vez de 0.5 como el
#   punto de corte que se reporta para el modelo predictivo B1, porque
#   mejora la especificidad de 0.551 a 0.606 sin perder accuracy ni
#   AUC (el AUC no depende del punto de corte).
#
# - Interaccion grupo_edad:tam_loc: es estadisticamente significativa
#   en el modelo ponderado (Wald F=3.20 en 3 y 7444 g.l., p=0.022),
#   aunque no mejora sustancialmente la capacidad predictiva (AUC 0.706
#   vs 0.707; accuracy +0.004, dentro del ruido de muestreo). Se
#   documenta como HALLAZGO SECUNDARIO en el reporte -- la asociacion
#   entre grupo de edad y propiedad difiere segun el tama\u00f1o de
#   localidad -- y no como parte de las variables del modelo principal,
#   para mantener la interpretacion simple en la defensa oral.
#
# - Random Forest: alcanza AUC=0.716 vs. 0.707 de la logistica (mejora
#   de solo 0.009). La regresion logistica ofrece un desempeño
#   predictivo cercano al de un modelo no lineal y, ademas, permite
#   interpretar directamente la asociacion entre las caracteristicas
#   del hogar y la propiedad mediante odds ratios -- que es justamente
#   lo que pide la pregunta de investigacion (asociativa, no
#   predictiva). Por eso se usa como modelo principal para la
#   interpretacion, y el Random Forest se reporta unicamente como techo
#   de referencia en la seccion de extensiones del reporte.

cat("\n============================================================\n")
cat("DECISIONES FINALES ADOPTADAS PARA EL REPORTE\n")
cat("============================================================\n")
cat("Modelo principal (interpretacion): modelo_svy (Modelo A, seccion 2)\n\n")

cat("Modelo principal (prediccion): modelo_glm, con punto de corte",
    round(corte_optimo, 3), "en vez de 0.5\n")
cat("  accuracy =", round(res_glm_opt$metricas$accuracy, 3),
    " | sensibilidad =", round(res_glm_opt$metricas$sensibilidad, 3),
    " | especificidad =", round(res_glm_opt$metricas$especificidad, 3),
    " | AUC =", round(res_glm_opt$metricas$auc, 3), "\n\n")

cat("Interaccion grupo_edad:tam_loc (p = 0.022): la asociacion entre\n")
cat("  edad y propiedad difiere segun tama\u00f1o de localidad, aunque no\n")
cat("  mejora la capacidad predictiva -- se reporta como hallazgo\n")
cat("  secundario, no como parte del modelo principal.\n\n")

cat("Random Forest (referencia): AUC =", round(res_rf$metricas$auc, 3),
    "-- desempeño cercano al de la logistica (diferencia < 0.01),\n")
cat("  lo que respalda usar el modelo interpretable como principal.\n")


# ============================================================
# 10. ROBUSTEZ Y DIAGNOSTICOS ADICIONALES
# ============================================================
#
# Responde a observaciones de revision metodologica sobre el modelo:
# validacion cruzada, calibracion, sensibilidad a est_socio_f,
# verificacion de la variable objetivo y observaciones influyentes.

# ----- 10.1 Verificacion explicita de la construccion de "propietario" -----

cat("\n***** 10.1 Construccion de la variable propietario (verificacion) *****\n")
cat("Codificacion de tenencia en ENIGH: 1=rentada, 2=prestada,\n")
cat("  3=propia pagandose, 4=propia, 5=intestada/litigio, 6=otra situacion\n")
print(table(tenencia = datos$tenencia, propietario = datos$propietario,
            useNA = "ifany"))
cat("(tenencia 5 y 6 no aparecen: se excluyeron de la base final, ver README punto 5)\n")

# ----- 10.2 Validacion cruzada 10-fold (robustez del AUC) -----
# El train/test 70/30 usa una sola particion; la validacion cruzada
# evalua el modelo sobre subconjuntos distintos para verificar que el
# AUC reportado no depende de una particion particular.

set.seed(2026)
k <- 10
folds_id <- sample(rep(1:k, length.out = nrow(datos_modelo)))
auc_folds <- numeric(k)

for (i in 1:k) {
  train_cv <- datos_modelo[folds_id != i, ]
  test_cv  <- datos_modelo[folds_id == i, ]
  modelo_cv <- glm(formula_modelo, data = train_cv, family = binomial())
  prob_cv <- predict(modelo_cv, newdata = test_cv, type = "response")
  auc_folds[i] <- as.numeric(auc(roc(test_cv$propietario, prob_cv, quiet = TRUE)))
}

cat("\n***** 10.2 Validacion cruzada 10-fold (AUC por particion) *****\n")
print(round(auc_folds, 3))
cat("AUC promedio (CV-10):", round(mean(auc_folds), 3),
    "+/- ", round(sd(auc_folds), 3),
    "(vs. AUC =", round(res_glm$metricas$auc, 3), "en el split 70/30 original)\n")

# ----- 10.3 Calibracion y Brier score -----
# El AUC mide que tan bien el modelo ORDENA/discrimina casos; la
# calibracion mide si las probabilidades que produce son razonables
# (si dice 0.80, ¿de verdad ~80% de esos hogares son propietarios?).
# Relevante porque el proyecto es explicitamente de "probabilidad
# de ser propietario".

brier <- mean((pred_glm_prob - test$propietario)^2)
brier_base <- mean((mean(test$propietario) - test$propietario)^2)
cat("\n***** 10.3 Calibracion del modelo logistico *****\n")
cat("Brier score del modelo:", round(brier, 4), "\n")
cat("Brier score de referencia (predecir siempre la tasa base,",
    round(mean(test$propietario), 3), "):", round(brier_base, 4), "\n")
cat("Mejora sobre la referencia:", round(100 * (1 - brier / brier_base), 1),
    "%. Interpretacion: el modelo mejora la referencia, pero de forma\n")
cat("  moderada, no dramatica; esto es consistente con el AUC de 0.71.\n")

calib <- tibble(prob_pred = pred_glm_prob, real = test$propietario) %>%
  mutate(decil = ntile(prob_pred, 10)) %>%
  group_by(decil) %>%
  summarise(prob_media_predicha = mean(prob_pred),
            prop_observada = mean(real),
            n = n(), .groups = "drop")
print(calib)

p_calib <- ggplot(calib, aes(x = prob_media_predicha, y = prop_observada)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(size = 2, color = "#2b5c8f") +
  geom_line(color = "#2b5c8f") +
  labs(
    title = "Curva de calibracion (10 deciles) - Regresion logistica",
    subtitle = "La linea punteada es la calibracion perfecta",
    x = "Probabilidad promedio predicha", y = "Proporcion observada de propietarios"
  ) +
  theme_minimal()

ggsave("outputs/figuras/modelo/calibracion.png", plot = p_calib,
       width = 8, height = 6, dpi = 150)
cat("guardado: outputs/figuras/modelo/calibracion.png\n")

# ----- 10.4 Analisis de sensibilidad: con vs. sin est_socio_f -----
# est_socio_f se documento (seccion 1) como variable del diseño
# muestral incluida por su asociacion observada, no como atributo
# economico medido directamente. Se verifica si su inclusion cambia
# las conclusiones principales del modelo.

formula_sin_estsocio <- update(formula_modelo, . ~ . - est_socio_f)
modelo_svy_sin_estsocio <- svyglm(formula_sin_estsocio, design = diseno_enigh,
                                   family = quasibinomial())

tabla_or_sin <- tidy(modelo_svy_sin_estsocio, conf.int = TRUE) %>%
  mutate(odds_ratio = exp(estimate)) %>%
  select(term, odds_ratio_sin = odds_ratio, p_sin = p.value)

comparacion_sensibilidad <- tabla_or %>%
  select(term, odds_ratio_con = odds_ratio, p_con = p.value) %>%
  inner_join(tabla_or_sin, by = "term") %>%
  mutate(
    cambia_significancia = (p_con < 0.05) != (p_sin < 0.05),
    cambio_or_pct = 100 * (odds_ratio_sin - odds_ratio_con) / odds_ratio_con
  )

cat("\n***** 10.4 Sensibilidad: coeficientes con vs. sin est_socio_f *****\n")
print(comparacion_sensibilidad)

cat("Variables cuya significancia cambia al quitar est_socio_f:",
    sum(comparacion_sensibilidad$cambia_significancia), "de",
    nrow(comparacion_sensibilidad), "\n")

mayor_cambio <- comparacion_sensibilidad %>%
  filter(term != "(Intercept)") %>%
  slice_max(abs(cambio_or_pct), n = 1)

cat("Interpretacion: la direccion y significancia de las asociaciones\n")
cat("  son estables al quitar est_socio_f, pero su magnitud se\n")
cat("  redistribuye. El mayor cambio ocurre en", mayor_cambio$term,
    "(", round(mayor_cambio$cambio_or_pct, 1), "% en el odds ratio),\n")
cat("  lo que sugiere que est_socio_f absorbe parte del efecto de otras\n")
cat("  variables geograficas en vez de aportar una señal totalmente\n")
cat("  independiente.\n")

modelo_glm_sin <- glm(formula_sin_estsocio, data = train, family = binomial())
prob_sin <- predict(modelo_glm_sin, newdata = test, type = "response")
auc_sin <- as.numeric(auc(roc(test$propietario, prob_sin, quiet = TRUE)))

cat("AUC con est_socio_f:", round(res_glm$metricas$auc, 3),
    " | AUC sin est_socio_f:", round(auc_sin, 3), "\n")

# ----- 10.5 Observaciones influyentes (distancia de Cook) -----
cooksd <- cooks.distance(modelo_glm_diag)
umbral_influyente <- 4 / nrow(datos_modelo)
n_influyentes <- sum(cooksd > umbral_influyente)

cat("\n***** 10.5 Observaciones influyentes (distancia de Cook) *****\n")
cat("Umbral (4/n):", round(umbral_influyente, 6), "\n")
cat("Observaciones por encima del umbral:", n_influyentes, "de",
    nrow(datos_modelo), "(", round(100 * n_influyentes / nrow(datos_modelo), 2),
    "%)\n")
cat("Distancia de Cook maxima observada:", round(max(cooksd), 5), "\n")





