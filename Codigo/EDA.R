# ============================================================
# PROYECTO MODULO 8
# CONDICIONES FINANCIERAS POR GENERACION EN MEXICO
#
# Análisis Exploratorio de Datos (EDA)
#
# Fuente: ENIGH 2024 - INEGI
# Unidad de analisis: hogar
# ============================================================


# ----- Librerías
library(tidyverse)
library(ggplot2)
library(ggcorrplot)
library(gridExtra)
library(scales)

#------ carga de datos limpia 

datos <- read_csv("Datos/base_final_ENIGH2024_jovenes.csv")

head(datos)

#------crear una carpeta de salidas si no existe
dir.create("outputs/figuras", recursive = TRUE, showWarnings = FALSE)

#------crear una carpeta de tablas si no existe
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)


cat("Datos cargados correctamente\n")
cat("Dimensiones del dataset:", dim(datos), "\n")
cat("Variables dentro del dataset:", names(datos), "\n")
cat("Estructura del dataset", str(datos), "\n")

#------Verificación de valores faltantes

cat("Valores faltantes dentro de cada variable en el dataset:", colSums(is.na(datos)), "\n")

#-----valores duplicados dentro del dataset-----
cat("Valores duplicados en el dataset:", sum(duplicated(datos)),"\n")


#verificación de unicidad en las claves de hogar

datos %>%
  count(folioviv, foliohog) %>%
  filter(n > 1)


#------ Análisis de la variable objetivo (propietario)

# Frecuencia y porcentaje sin ponderar
# Personas que hayan respondido "Si" "No" en la encuesta

tabla_propietarios <- datos %>% 
  count(propietario) %>% 
  mutate(porcentaje = n / sum(n) * 100)

cat("\n*****Tabla de proporción propietarios ******\n")
print(tabla_propietarios)
# Guardar tabla
write.csv(tabla_propietarios,
          "outputs/tables/tabla_propietarios_proporcion.csv",
          row.names = FALSE)
cat("guardado\n")

# Frecuencia y porcentaje ponderado con factor de expansión
# Calcular a cuánta población representa nuestra muestra:

poblacion_estimada <- datos %>% 
  group_by(propietario) %>% 
  summarise(poblacion_estimada = sum(factor)) %>% 
  mutate(porcentaje_ponderado = poblacion_estimada / sum(poblacion_estimada) * 100)

cat("\n*****Tabla de problación estimada: factor de expansión ******\n")
print(poblacion_estimada)
# Guardar tabla
write.csv(poblacion_estimada,
          "outputs/tables/poblacion_estimada.csv",
          row.names = FALSE)
cat("guardado\n")

# Relación entre 'propietario' y tipo de 'tenencia'
# cruce de variables 'propietario' y 'tenencia'
tipo_tenencia <- table(datos$propietario, datos$tenencia)

cat("\n*****Tabla de tipo de tenencia entre propietarios******\n")
# Guardar tabla
write.csv(tipo_tenencia,
          "outputs/tables/tipo_tenencia.csv",
          row.names = FALSE)
cat("guardado\n")

#------ Análisis Bivariado de Variables Catégoricas

# función para calcular porcentaje de propiedad por categoría 

analizar_categoria <- function(data, var) {
  datos %>%
    group_by({{ var }}) %>%
    summarise(
      total = n(),
      pct_propietario = mean(propietario == 1) * 100,
      pct_propietario_pond = weighted.mean(propietario == 1, w = factor) * 100
    ) %>%
    arrange(desc(pct_propietario))
}

# Evaluacion por variables clave
propiedad_por_edad <- analizar_categoria(datos, grupo_edad)
cat("\n*****Tabla de porcentaje de propiedad por edad******\n")
print(propiedad_por_edad)
# Guardar tabla
write.csv(propiedad_por_edad,
          "outputs/tables/porcentaje_propiedad_edad.csv",
          row.names = FALSE)

propiedad_por_tamanio <- analizar_categoria(datos, tam_loc)
cat("\n*****Tabla de porcentaje de propiedad por tamaño de localidad******\n")
print(propiedad_por_tamanio)
# Guardar tabla
write.csv(propiedad_por_tamanio,
          "outputs/tables/porcentaje_propiedad_tamanio.csv",
          row.names = FALSE)


propiedad_por_estrato <- analizar_categoria(df, est_socio)
cat("\n*****Tabla de porcentaje de propiedad por estrato socioeconómico******\n")
print(propiedad_por_estrato)
# Guardar tabla
write.csv(propiedad_por_estrato,
          "outputs/tables/porcentaje_propiedad_estrato.csv",
          row.names = FALSE)

propiedad_por_sexo <- analizar_categoria(df, sexo_jefe)
cat("\n*****Tabla de porcentaje de propiedad por sexo******\n")
print(propiedad_por_sexo)
# Guardar tabla
write.csv(propiedad_por_sexo,
          "outputs/tables/porcentaje_propiedad_sexo.csv",
          row.names = FALSE)

cat("guardado\n")


#------ Estadística Descriptiva

#summary de nuestros datos

summary(datos)


# Estadísticas Descriptivas de variables numéricas asociadas al propietario 

vars_num <- c("edad_jefe", "tot_integ", "menores", "mayores", "ocupados", 
             "ing_cor", "ingtrab", "gasto_mon", "deudas", "pago_tarje")

stats_propietario <- datos %>%
  group_by(propietario) %>%
  summarise(across(all_of(vars_num), list(mediana = median, media = mean), .names = "{.col}_{.fn}")) %>%
  pivot_longer(-propietario, names_to = "metrica", values_to = "valor") %>%
  pivot_wider(names_from = propietario, values_from = valor, names_prefix = "prop_")

cat("\n*****Tabla de estadísticas asociadas al propietario******\n")
print(stats_propietario)
# Guardar tabla
write.csv(stats_propietario,
          "outputs/tables/estadisticas_propietario.csv",
          row.names = FALSE)


#------ Visualizaciones 

#mapa de correlación

# seleccionar variables numéricas y renombrar
vars_corr <- datos %>%
  select(
    Propietario = propietario,
    `Edad Jefe` = edad_jefe,
    `Tot. Integrantes` = tot_integ,
    `Nº Menores` = menores,
    `Nº Mayores` = mayores,
    `Nº Ocupados` = ocupados,
    `Ingreso Corriente` = ing_cor,
    `Ingreso Trabajo` = ingtrab,
    `Gasto Monetario` = gasto_mon,
    Deudas = deudas,
    `Pago Tarjetas` = pago_tarje,
    `Estrato Socioec.` = est_socio
  )

# calcula matriz de correlaciones
matriz_corr <- cor(vars_corr, use = "complete.obs")

# grafica mapa de correlación
p_corr <- ggcorrplot(
  matriz_corr,
  hc.order = FALSE,
  type = "lower",          # Mostrar solo la mitad inferior
  lab = TRUE,              # Mostrar los coeficientes numéricos
  lab_size = 3,
  method = "square",
  colors = c("#6D9EC1", "white", "#E46726"), # Escala de colores
  title = "Matriz de Correlación de Pearson",
  ggtheme = theme_minimal()
)



# Guardar la gráfica en la carpeta de outputs
ggsave("outputs/figuras/mapa_correlacion.png",
       plot = p_corr, width = 10, height = 8, dpi = 150)


theme_set(theme_minimal(base_size = 11))

# Gráfico: Propiedad por Grupo de Edad
g1 <- datos %>%
  group_by(grupo_edad) %>%
  summarise(pct = mean(propietario)) %>%
  ggplot(aes(x = grupo_edad, y = pct, fill = grupo_edad)) +
  geom_col(show.legend = FALSE, width = 0.6) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_fill_brewer(palette = "Reds", direction = -1) +
  labs(
    title = "Propietarios por Grupo de Edad",
    x = "Grupo de Edad",
    y = "% Propietarios"
  )

ggsave("outputs/figuras/age_group_proportion.png",
       plot = g1, width = 10, height = 8, dpi = 150)

# Gráfico: Propiedad por Tamaño de Localidad
orden_loc <- c("Menos de 2,500", "2,500 a 14,999", "15 mil a 99,999", "100 mil o mas")

g2 <- datos %>%
  mutate(tam_loc = factor(tam_loc, levels = orden_loc)) %>%
  group_by(tam_loc) %>%
  summarise(pct = mean(propietario)) %>%
  ggplot(aes(x = tam_loc, y = pct, fill = tam_loc)) +
  geom_col(show.legend = FALSE, width = 0.6) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_fill_brewer(palette = "Greens", direction = -1) +
  labs(
    title = "Propietarios por Tamaño de Localidad",
    x = "Tamaño de Localidad",
    y = "% Propietarios"
  ) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

ggsave("outputs/figuras/locality_size_proportion.png",
       plot = g2, width = 10, height = 8, dpi = 150)

# Gráfico: Propiedad por Estrato Socioeconómico
g3 <- datos %>%
  mutate(est_socio = as.factor(est_socio)) %>%
  group_by(est_socio) %>%
  summarise(pct = mean(propietario)) %>%
  ggplot(aes(x = est_socio, y = pct, fill = est_socio)) +
  geom_col(show.legend = FALSE, width = 0.6) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_fill_brewer(palette = "Oranges", direction = -1) +
  labs(
    title = "Propietarios por Estrato Socioeconómico",
    x = "Estrato (1=Bajo, 4=Alto)",
    y = "% Propietarios"
  )

ggsave("outputs/figuras/socioeconomics_property_proportion.png",
       plot = g3, width = 10, height = 8, dpi = 150)

# Gráfica: Distribución del ingreso corriente (Escala Original)

p1 <- ggplot(datos, aes(x = ing_cor)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = "#1F497D", alpha = 0.7) +
  geom_density(color = "#000000", linewidth = 0.8) +
  scale_x_continuous(labels = comma) +
  labs(
    title = "Distribución del Ingreso Corriente Trimestral",
    x = "Ingreso Corriente (MXN)",
    y = "Densidad"
  ) +
  theme_minimal()

ggsave("outputs/figuras/income_distributions.png",
       plot = p1, width = 10, height = 8, dpi = 150)



# Gráfico: Distribución del ingreso corriente (escala logarítmica). 
p2 <- ggplot(datos, aes(x = log1p(ing_cor))) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = "#008080", alpha = 0.7) +
  geom_density(color = "#000000", linewidth = 0.8) +
  labs(
    title = "Distribución del Ingreso Corriente (Escala Logarítmica)",
    x = "Log(Ingreso Corriente + 1)",
    y = "Densidad"
  ) +
  theme_minimal()

ggsave("outputs/figuras/income_log_distributions.png",
       plot = p2, width = 10, height = 8, dpi = 150)

# Gráfico: Boxplot del Ingreso Corriente (Escala Logarítmica)
g4 <- datos %>%
  mutate(propietario_lbl = if_else(propietario == 1, "Propietario", "No Propietario")) %>%
  ggplot(aes(x = propietario_lbl, y = log1p(ing_cor), fill = propietario_lbl)) +
  geom_boxplot(show.legend = FALSE, alpha = 0.8) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Distribución del Ingreso Corriente (Log)",
    x = "Condición de Vivienda",
    y = "Log(Ingreso Corriente + 1)"
  )

ggsave("outputs/figuras/boxplot_income_logscale.png",
       plot = g4, width = 10, height = 8, dpi = 150)


# Gráfico: Propiedad por Nivel Educativo
p_edu <- datos %>%
  group_by(educa_jefe) %>%
  summarise(pct = mean(propietario)) %>%
  ggplot(aes(x = reorder(educa_jefe, pct), y = pct)) +
  geom_col(fill = "#2b5c8f") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  labs(title = "Propiedad por Nivel Educativo", x = "", y = "% Propietarios") +
  theme_minimal()

ggsave("outputs/figuras/plot_propiedad_edu.png",
       plot = p_edu, width = 10, height = 8, dpi = 150)


# Gráfico: Interacción Género y Edad
p_sexo_edad <- datos %>%
  group_by(grupo_edad, sexo_jefe) %>%
  summarise(pct = mean(propietario), .groups = "drop") %>%
  ggplot(aes(x = grupo_edad, y = pct, fill = sexo_jefe)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_fill_manual(values = c("Hombre" = "#d95f02", "Mujer" = "#7570b3")) +
  labs(title = "Propiedad por Edad y Género", x = "Grupo de Edad", y = "% Propietarios", fill = "Sexo") +
  theme_minimal()

ggsave("outputs/figuras/plot_genero_edad.png",
       plot = p_sexo_edad, width = 10, height = 8, dpi = 150)


# Gráfico: Presencia de Menores
p_menores <- datos %>%
  mutate(tiene_menores = if_else(menores > 0, "Con menores", "Sin menores")) %>%
  group_by(tiene_menores) %>%
  summarise(pct = mean(propietario)) %>%
  ggplot(aes(x = tiene_menores, y = pct, fill = tiene_menores)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_fill_manual(values = c("Con menores" = "#7570b3", "Sin menores" = "#cbd5e1")) +
  labs(title = "Propiedad según Presencia de Menores", x = "", y = "% Propietarios") +
  theme_minimal()

ggsave("outputs/figuras/plot_presencia_menores.png",
       plot = p_menores, width = 10, height = 8, dpi = 150)

