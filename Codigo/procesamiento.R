# ============================================================
# PROYECTO MODULO 8
# CONDICIONES FINANCIERAS POR GENERACION EN MEXICO
#
# Construccion de la base analitica para hogares con
# jefe de hogar de 18 a 39 anos
#
# Fuente: ENIGH 2024 - INEGI
# Unidad de analisis: hogar
# ============================================================


# ============================================================
# 1. CARGA DE LIBRERIAS
# ============================================================

library(dplyr)


# ============================================================
# 2. CARGA DE LAS BASES ORIGINALES
# ============================================================

viviendas <- read.csv(
  "viviendas.csv"
)

concentradohogar <- read.csv(
  "concentradohogar.csv"
)


# ============================================================
# 3. CONSTRUCCION DE LA BASE A NIVEL HOGAR
# ============================================================

base_analisis <- concentradohogar %>%
  left_join(
    viviendas %>%
      select(
        folioviv,
        tenencia
      ),
    by = "folioviv"
  )


# ============================================================
# 4. CONSTRUCCION DE LA VARIABLE PROPIETARIO
# ============================================================

base_analisis$propietario <- ifelse(
  base_analisis$tenencia %in% c(3, 4),
  1,
  ifelse(
    base_analisis$tenencia %in% c(1, 2),
    0,
    NA
  )
)


# ============================================================
# 5. CREACION DE GRUPOS DE EDAD
# ============================================================

base_analisis$grupo_edad <- cut(
  base_analisis$edad_jefe,
  breaks = c(18, 30, 40, 50, 60, Inf),
  right = FALSE,
  labels = c(
    "18-29",
    "30-39",
    "40-49",
    "50-59",
    "60+"
  )
)


# ============================================================
# 6. SELECCION DE HOGARES CON JEFE DE 18 A 39 ANOS
# ============================================================

base_jovenes <- base_analisis %>%
  filter(
    edad_jefe >= 18,
    edad_jefe < 40
  )


# ============================================================
# 7. EXCLUSION DE CASOS NO CLASIFICABLES
# ============================================================

base_jovenes <- base_jovenes %>%
  filter(
    !is.na(propietario)
  )


# ============================================================
# 8. CREACION DE LA BASE FINAL
# ============================================================

base_final <- base_jovenes


# ============================================================
# 9. RECODIFICACION DE VARIABLES CATEGORICAS
# ============================================================

base_final <- base_final %>%
  mutate(
    
    sexo_jefe = factor(
      sexo_jefe,
      levels = c(1, 2),
      labels = c(
        "Hombre",
        "Mujer"
      )
    ),
    
    educa_jefe = factor(
      educa_jefe,
      levels = 1:11,
      labels = c(
        "Sin instruccion",
        "Preescolar",
        "Primaria incompleta",
        "Primaria completa",
        "Secundaria incompleta",
        "Secundaria completa",
        "Preparatoria incompleta",
        "Preparatoria completa",
        "Profesional incompleta",
        "Profesional completa",
        "Posgrado"
      )
    ),
    
    tam_loc = factor(
      tam_loc,
      levels = 1:4,
      labels = c(
        "100 mil o mas",
        "15 mil a 99,999",
        "2,500 a 14,999",
        "Menos de 2,500"
      )
    )
  )


# ============================================================
# 10. SELECCION DE LAS VARIABLES FINALES
# ============================================================

base_final <- base_final %>%
  select(
    folioviv,
    foliohog,
    
    propietario,
    tenencia,
    
    edad_jefe,
    grupo_edad,
    sexo_jefe,
    educa_jefe,
    
    tot_integ,
    menores,
    mayores,
    ocupados,
    
    ing_cor,
    ingtrab,
    gasto_mon,
    deudas,
    pago_tarje,
    
    ubica_geo,
    tam_loc,
    
    factor,
    est_socio,
    est_dis,
    upm
  )


# ============================================================
# 11. EXPORTACION DE LA BASE FINAL
# ============================================================

write.csv(
  base_final,
  "base_final_ENIGH2024_jovenes.csv",
  row.names = FALSE
)


# ============================================================
# 12. COMPROBACIONES FINALES
# ============================================================

dim(base_final)

colSums(is.na(base_final))

table(base_final$propietario)

table(base_final$grupo_edad)

table(base_final$tenencia)