PROYECTO MODULO 8
CONDICIONES FINANCIERAS POR GENERACION EN MEXICO

1. OBJETIVO

Esta carpeta contiene la base de datos analitica preparada para el proyecto final del Modulo 8.

La pregunta principal que orienta esta base es:

¿Qué características están relacionadas con la posibilidad de ser propietario de vivienda?

El análisis se concentra en hogares cuyo jefe de hogar tiene entre 18 y 39 años.

2. FUENTE DE DATOS

Fuente principal:
Encuesta Nacional de Ingresos y Gastos de los Hogares (ENIGH) 2024.

Institución:
Instituto Nacional de Estadística y Geografía (INEGI).

Fuente oficial:
https://www.inegi.org.mx/programas/enigh/nc/2024/

La ENIGH es una encuesta probabilística y utiliza factores de expansión para producir estimaciones representativas.

3. UNIDAD DE ANALISIS

La unidad de análisis es el hogar.

Cada hogar se identifica mediante la combinación de:

folioviv + foliohog

La vivienda puede contener más de un hogar, por lo que folioviv por sí solo no identifica de manera única a cada hogar.

4. POBLACION ANALIZADA

Se conservaron los hogares cuyo jefe de hogar tiene:

* 18 a 39 años.

La clasificación utilizada es:

* 18-29 años
* 30-39 años

La base final contiene:

23,127 hogares.

5. VARIABLE OBJETIVO

La variable principal es:

propietario

Fue construida a partir de la variable tenencia.

Codificación:

1 = vivienda propia o propia pero la están pagando
0 = vivienda rentada o prestada

Los casos de tenencia:

5 = intestada o en litigio
6 = otra situación

fueron excluidos de la base final porque no permiten clasificarse dentro de las dos categorías utilizadas para la variable propietario.

La base final contiene únicamente observaciones clasificadas como propietario = 0 o propietario = 1.

6. VARIABLES PRINCIPALES

La base final contiene variables relacionadas con:

* Edad y grupo de edad del jefe del hogar.
* Sexo del jefe del hogar.
* Nivel educativo del jefe del hogar.
* Número de integrantes del hogar.
* Número de menores.
* Número de adultos mayores.
* Número de integrantes ocupados.
* Ingreso corriente del hogar.
* Ingreso por trabajo.
* Gasto corriente monetario.
* Pagos realizados por deudas.
* Pagos realizados mediante tarjeta de crédito.
* Ubicación geográfica.
* Tamaño de localidad.

7. VARIABLES DEL DISEÑO MUESTRAL

También se conservaron las variables:

factor
est_socio
est_dis
upm

Estas variables corresponden al diseño muestral de la ENIGH.

En particular, factor es el factor de expansión de la encuesta.

Estas variables no deben interpretarse automáticamente como características económicas del hogar. Deben utilizarse cuando se realicen análisis que consideren el diseño muestral y la ponderación de la encuesta.

8. VARIABLES ECONOMICAS

Las variables:

ing_cor
ingtrab
gasto_mon
deudas
pago_tarje

corresponden a montos registrados de acuerdo con los periodos de referencia definidos por la ENIGH 2024.

No deben interpretarse automáticamente como cantidades mensuales.

En particular:

deudas representa pagos registrados por concepto de deudas, no necesariamente el saldo total de deuda.

pago_tarje representa pagos realizados mediante tarjeta de crédito, no el saldo total adeudado en tarjetas.

9. CALIDAD DE LA BASE FINAL

La base final contiene:

23,127 observaciones
23 variables

No presenta valores NA en las variables incluidas en la base final.

La variable propietario tiene:

10,468 hogares no propietarios
12,659 hogares propietarios

Estos conteos corresponden a la muestra utilizada y no deben interpretarse directamente como porcentajes de la población mexicana sin considerar el factor de expansión de la ENIGH.

10. ARCHIVOS

datos/base_final_ENIGH2024_jovenes.csv

Base analítica final. Esta es la base principal que deberá utilizar el equipo para continuar con el análisis.

datos/diccionario_variables.csv

Diccionario de las 23 variables de la base final, incluyendo significado, tipo, valores y origen.

codigo/procesamiento.R

Script utilizado para documentar el proceso de construcción de la base analítica a partir de las bases originales de ENIGH 2024.

11. CONSIDERACIONES METODOLOGICAS

La variable propietario identifica una condición observada de tenencia de vivienda.

La pregunta de investigación busca identificar características relacionadas con la propiedad de vivienda. Por lo tanto, los resultados posteriores deben interpretarse como asociaciones y no como relaciones causales, salvo que se utilice una metodología que permita justificar una interpretación causal.

La definición de adultos jóvenes utilizada en este proyecto es operativa: hogares cuyo jefe tiene entre 18 y 39 años.

Esta definición no representa necesariamente una generación en sentido longitudinal.

12. ESTADO DE LA BASE

La base_final_ENIGH2024_jovenes.csv se considera la versión final y congelada de la base analítica entregada al equipo.

No se deben realizar modificaciones directamente sobre esta base maestra.

Los análisis posteriores, transformaciones o nuevas variables deberán realizarse sobre copias de trabajo o mediante scripts adicionales.

El diccionario y el script de procesamiento acompañan a la base para facilitar su comprensión y reproducibilidad.
