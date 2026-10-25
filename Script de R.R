# ============================================================
# CURVA DE PHILLIPS EMPÍRICA EN ESTADOS UNIDOS
# ============================================================
# ============================================================
# 0. INSTALACIÓN Y CARGA DE PAQUETES
# ============================================================

install.packages("readxl")
install.packages("ggplot2")
install.packages("urca")
install.packages("dynlm")
install.packages("forecast")
install.packages("ARDL") # Solo una vez
install.packages("AER")  # Solo una vez

library(readxl)    # Importar archivos Excel
library(ggplot2)   # Crear gráficos
library(urca)      # Pruebas de raíz unitaria ADF
library(dynlm)     # Modelos de regresión con retardos
library(forecast)  # Herramientas para series temporales
library(ARDL)      # Herramientas para espeficiación de modelos
library(AER)       # Datos del IPC y desempleo en USA 

# ============================================================
# 1. PLANTEAMIENTO DEL PROBLEMA ECONÓMICO
# ============================================================

# La inflación mide el aumento porcentual del nivel general
# de precios. El desempleo mide la proporción de la población
# activa que no tiene empleo y busca trabajo.
#
# La curva de Phillips empírica estudia la relación estadística
# entre inflación y desempleo.
#
# En términos generales, se plantea la hipótesis de que un
# aumento del desempleo puede estar asociado con presiones
# inflacionarias menores.
#
# Sin embargo, esta relación no es necesariamente constante,
# ni demuestra por sí sola causalidad.
#
# Stock y Watson (capítulo 14) utilizan la inflación pasada
# y los valores pasados del desempleo para mejorar la predicción
# de la inflación estadounidense.

# PREGUNTA DE INVESTIGACIÓN:
# ¿Los valores pasados de la tasa de desempleo, junto con los
# valores pasados de la inflación, permiten predecir la inflación
# actual en Estados Unidos?

# VARIABLE DEPENDIENTE:
# Inflación trimestral anualizada.

# VARIABLE PREDICTORA PRINCIPAL:
# Tasa de desempleo.
#
# VARIABLES PREDICTORAS ADICIONALES:
# Retardos de la inflación y del desempleo.

# NOTA:
# Una asociación estadística útil para predecir no implica,
# necesariamente, un efecto causal.


# ============================================================
# 2. ESTRUCTURA DE LOS DATOS
# ============================================================

# Trabajaremos con datos de series temporales trimestrales.

# Cada observación corresponde a un trimestre:
# 1962-I, 1962-II, 1962-III, 1962-IV, etc.

data("USMacroSW")

# Seleccionar 1962-T1 a 2004-T4

bdd <- window(
  USMacroSW,
  start = c(1962, 1),
  end   = c(2004, 4)
)

# Conservar las dos variables

bdd <- bdd[, c("cpi", "unemp")]
bdd <- as.data.frame(bdd)

# Renombrar según tu script

colnames(bdd) <- c("punew", "lhur")

head(bdd)
tail(bdd)

# Consultamos los nombres de las columnas.

colnames(bdd)

# Revisamos la estructura de los datos.

str(bdd)

# Resumen estadístico inicial.

summary(bdd)

# ¿Por qué realizamos estas comprobaciones?

# Antes de estimar un modelo, debemos conocer las variables,
# sus tipos de datos, sus unidades y sus posibles valores
# ausentes.

# ============================================================
# 4. COMPROBACIÓN DE LAS VARIABLES NECESARIAS
# ============================================================

# Verificamos cuántos valores ausentes existen.

colSums(is.na(bdd[, c("lhur", "punew")]))

# Verificamos que los datos sean numéricos.

str(bdd[, c("lhur", "punew")])

# ADVERTENCIA:
# No debemos eliminar observaciones automáticamente sin
# comprender por qué faltan. En series temporales, eliminar
# filas puede alterar la continuidad cronológica.

# ============================================================
# 5. CONSTRUCCIÓN DE LAS SERIES TEMPORALES
# ============================================================

# La función ts() crea un objeto de serie temporal.

# frequency = 4 indica que existen cuatro observaciones por año.

# start = c(1962, 1) indica que la primera observación corresponde
# al primer trimestre de 1962 y end = c(2004, 4) indica que la
# última observación corresponde al último trimestre de 2004

desempleo <- ts(
  bdd$lhur,
  start = c(1962, 1),
  end = c(2004,4),
  frequency = 4
)

ipc <- ts(
  bdd$punew,
  start = c(1962, 1),
  end = c(2004, 4),
  frequency = 4
)

# Consultamos la información temporal de cada serie.

start(desempleo)
end(desempleo)
frequency(desempleo)

start(ipc)
end(ipc)
frequency(ipc)

# Comprobamos el número de observaciones.

length(desempleo)
length(ipc)

# Si la muestra está completa desde 1962-I hasta 2004-IV,
# esperamos 172 observaciones trimestrales.

# Si el número de observaciones es distinto, revisamos la base,
# la fecha inicial y la fecha final antes de continuar.


# ============================================================
# 6. ANÁLISIS GRÁFICO INICIAL
# ============================================================

# 6.1. Gráfico de la tasa de desempleo.

autoplot(desempleo) +
  labs(
    title = "Tasa de desempleo en Estados Unidos",
    subtitle = "Datos trimestrales, 1962-I a 2004-IV",
    x = "Fecha",
    y = "Tasa de desempleo"
  ) +
  theme_minimal()

# 6.2. Gráfico del índice de precios al consumidor.

autoplot(ipc) +
  labs(
    title = "Índice de precios al consumidor en Estados Unidos",
    subtitle = "Datos trimestrales, 1962-I a 2004-IV",
    x = "Fecha",
    y = "Índice de precios"
  ) +
  theme_minimal()

# INTERPRETACIÓN TEÓRICA:

# Se recomienda comenzar el análisis de una serie
# temporal mediante su representación gráfica.

# El gráfico permite identificar:
# - Tendencias de largo plazo.
# - Fluctuaciones y posibles valores atípicos.
# - Cambios en la variabilidad.
# - Posibles rupturas estructurales.

# Una tendencia persistente puede advertirnos que la serie
# no es estacionaria, aunque el gráfico no constituye una
# prueba formal de estacionariedad.

# La estacionariedad es
# fundamental para el análisis de series temporales, pues
# la regresión entre series no estacionarias puede producir
# relaciones espurias.

# ============================================================
# 7. CONSTRUCCIÓN DE LA TASA DE INFLACIÓN
# ============================================================

# El IPC mide el nivel de precios, pero no es por sí mismo
# la tasa de inflación.

# Para construir la inflación trimestral anualizada utilizamos
# la expresión presentada por Stock y Watson:

# Inflación_t = 400 * [ln(IPC_t) - ln(IPC_(t-1))]

# El factor 100 convierte la variación logarítmica en porcentaje
# aproximado; el factor 4 anualiza la variación trimestral.

# Esta tasa representa la inflación anual que se obtendría
# si el crecimiento trimestral observado se mantuviera
# durante cuatro trimestres. OJO: esta es una aproximación

if (any(ipc <= 0, na.rm = TRUE)) {
  stop("El IPC debe ser positivo para calcular su logaritmo.")
}

inflacion <- 400 * diff(log(ipc))

# La primera diferencia elimina la primera observación,
# pues no disponemos de un IPC anterior dentro de la muestra.

# Por tanto, la serie de inflación comienza en 1962-II

# Así mismo, es necesario eliminar el primer valor de la
# serie de desempleo, de forma que ambas series conciban
# el mismo periodo

desempleo <- ts(
  bdd$lhur,
  start = c(1962, 2),
  end = c(2004,4),
  frequency = 4
)

start(inflacion)
end(inflacion)
length(inflacion)

start(desempleo)
end(desempleo)
length(desempleo)

# Representamos la inflación construida.

autoplot(inflacion) +
  labs(
    title = "Inflación trimestral anualizada en Estados Unidos",
    subtitle = "Calculada mediante la primera diferencia del logaritmo del IPC",
    x = "Fecha",
    y = "Inflación anualizada (%)"
  ) +
  theme_minimal()

autoplot(desempleo) +
  labs(
    title = "Tasa de desempleo en Estados Unidos",
    subtitle = "Datos trimestrales, 1962-I a 2004-IV",
    x = "Fecha",
    y = "Tasa de desempleo"
  ) +
  theme_minimal()

# Comparamos algunos valores.

head(inflacion)

summary(inflacion)

# INTERPRETACIÓN:

# Si la inflación es 4, significa que la tasa trimestral
# anualizada es aproximadamente del 4 %.

# No significa que los precios hayan aumentado necesariamente
# un 4 % durante ese trimestre.

# La anualización es una convención que facilita la comparación
# de las tasas de crecimiento macroeconómicas.

# ============================================================
# 9. ESTACIONARIEDAD DE LAS SERIES TEMPORALES
# ============================================================

# Antes de estimar una regresión con series temporales,
# debemos estudiar la estacionariedad de las variables.

# ¿Por qué importa?

# Una regresión entre series no estacionarias puede ser espuria:
# podría mostrar coeficientes aparentemente significativos y
# un R² elevado sin que exista una relación económica válida.

# Hay que verificar si:

# I(0): serie estacionaria en niveles.
# I(1): serie que se vuelve estacionaria después de una
#       primera diferencia.
# I(2): serie que requiere dos diferenciaciones.

# Estas clasificaciones deben justificarse empíricamente.
# No basta con observar un gráfico.


# ============================================================
# 10. PRUEBA DE DICKEY-FULLER AUMENTADA (ADF)
# ============================================================

# La prueba ADF permite evaluar la hipótesis de raíz unitaria.

# Hipótesis nula:
# H0: la serie tiene una raíz unitaria.

# Hipótesis alternativa:
# H1: la serie es estacionaria según la especificación elegida.

# La prueba puede incluir:
# - Una constante.
# - Una constante y una tendencia determinística.
# - Ninguna de las dos.

# La elección depende de las características de la serie
# y de la especificación teórica apropiada.

# En Stock y Watson, capítulo 14, el estadístico ADF no sigue
# la distribución t convencional. Por ello, no debemos decidir
# únicamente con los p-valores de una regresión MCO ordinaria.

# Con urca, examinaremos el estadístico y los valores críticos.


# ------------------------------------------------------------
# 10.1. ADF PARA LA TASA DE DESEMPLEO
# ------------------------------------------------------------

# type = "drift":
# Incluye una constante, pero no una tendencia lineal.

# type = "trend":
# Incluye constante y tendencia determinística.

# selectlags = "AIC":
# Selecciona el número de retardos mediante el criterio AIC.

adf_desempleo <- ur.df(
  desempleo,
  type = "drift",
  selectlags = "AIC"
)

summary(adf_desempleo)

# Interpretación:

# Busca el estadístico de prueba correspondiente a tau2,
# puesto que se incluyó constante, pero no tendencia
# lineal.

# Compara el estadístico con los valores críticos de la tabla.

# Si el estadístico es más negativo que el valor crítico,
# se rechaza H0 para ese nivel de significación.

# Si no se rechaza H0, no podemos afirmar que la serie
# sea estacionaria en niveles.

# ATENCIÓN:

# No rechazar H0 no demuestra que exista una raíz unitaria
# con certeza absoluta; indica que la evidencia es insuficiente
# para rechazarla mediante esta prueba.

# Luego, se compara el estadístico phi1 con los valores críticos de
# la tabla. Si phi1 es menor que los valores críticos, entonces
# no se rechaza la H0 para el nivel de significancia
# propuesto.

# La H0 para el estadístico phi1 indica que:
# "existe raíz unitaria y la constante drift es cero"

# ------------------------------------------------------------
# 10.2. PRIMERA DIFERENCIA DEL DESEMPLEO
# ------------------------------------------------------------

# La primera diferencia se define como:

# Δu_t = u_t - u_(t-1)

# El desempleo presenta una raíz unitaria, así que su primera
# diferencia podría ser estacionaria. Esto debe comprobarse.

ddesempleo <- diff(desempleo)

autoplot(ddesempleo) +
  labs(
    title = "Primera diferencia de la tasa de desempleo",
    x = "Fecha",
    y = "Cambio trimestral"
  ) +
  theme_minimal()

adf_ddesempleo <- ur.df(
  ddesempleo,
  type = "none",
  selectlags = "AIC"
)

summary(adf_ddesempleo)

# Los resultados son compatibles con una serie I(1).

# ------------------------------------------------------------
# 10.3. ADF PARA LA INFLACIÓN
# ------------------------------------------------------------

# Evaluamos si esta tasa de inflación es estacionaria.

adf_inflacion <- ur.df(
  inflacion,
  type = "drift",
  selectlags = "AIC"
)

summary(adf_inflacion)

# Para una serie como la inflación, que no necesariamente
# presenta crecimiento sostenido a largo plazo, una prueba
# con constante pero sin tendencia puede ser razonable.

# Visto que, a un nivel de significancia del 5%, tau2 y
# phi1 son significativos, eso significa que se puede rechazar
# la hipótesis de una raíz unitaria y una constante igual
# a cero

# Ahora, es necesario igualar las observaciones de la
# serie de la inflación a las observaciones de la serie
# del desempleo

inflacion <- ts(inflacion,
                start = c(1962, 3),
                end = c(2004, 4),
                frequency = 4
                )

start(inflacion)
end(inflacion)
length(inflacion)

start(ddesempleo)
end(ddesempleo)
length(ddesempleo)

# ============================================================
# 11. DEFINICIÓN DE LA PREGUNTA ECONOMÉTRICA
# ============================================================

# La pregunta central es:

# ¿Los retardos de la tasa de desempleo ayudan a predecir
# la inflación actual, una vez controlada la inflación pasada?

# La curva de Phillips empírica de Stock y Watson se plantea
# en un contexto de predicción de series temporales.

# Esto significa que no se requiere interpretar automáticamente
# cada coeficiente como un efecto causal.

# Una asociación predictiva puede existir sin que el desempleo
# sea la causa única de los movimientos de la inflación.

# Además, una curva de Phillips empírica no es idéntica a una
# relación estructural derivada de un modelo teórico completo.

# En esta aplicación nos interesa la utilidad de la información
# pasada para predecir la inflación.

# ============================================================
# 12. ESPECIFICACIÓN DEL MODELO ARD
# ============================================================

# Un modelo autorregresivo de retardos distribuidos (ARD)
# incorpora retardos de la variable dependiente y de la
# variable explicativa

# Una especificación general es:

# pi_t = beta_0
#      + beta_1*pi_(t-1) + ... + beta_p*pi_(t-p)
#      + gamma_1*u_(t-1) + ... + gamma_q*u_(t-q)
#      + error_t

# Donde:

# pi_t = tasa de inflación en el trimestre t.
# u_t  = tasa de desempleo en el trimestre t.
# p    = número de retardos de la inflación.
# q    = número de retardos del desempleo.
# beta y gamma = parámetros que se estimarán.
# error_t = término de error.

# La inflación pasada recoge la persistencia inflacionaria.
# Los retardos del desempleo permiten evaluar si sus valores
# pasados añaden información predictiva.

# Si gamma_j es negativo, un mayor desempleo en el retardo j
# está asociado con una menor inflación actual, manteniendo
# constantes los demás regresores.

# La significación estadística de ese coeficiente no demuestra
# por sí sola causalidad.

# ============================================================
# 14. ESTIMACIÓN DE UN MODELO ARD DE EJEMPLO
# ============================================================

# Empezamos con una especificación que incluye:
# - Dos retardos de la inflación.
# - Dos retardos del desempleo.

# Esta especificación es ilustrativa; todavía no sabemos si
# dos retardos constituyen la mejor opción.

# dynlm utiliza L(variable, k) para representar el retardo k.

# El operador L() permite especificar los retardos de una
# serie temporal sin crear manualmente nuevas columnas.

modelo_ARD_2_2 <- dynlm(
  inflacion ~
    L(inflacion, 1:2) +
    L(desempleo, 1:2)
)

summary(modelo_ARD_2_2)

# La salida muestra:
# - Los coeficientes estimados.
# - Los errores estándar.
# - Los estadísticos t.
# - Los p-valores.
# - El R² y el R² ajustado.

# ============================================================
# 15. ELECCIÓN DEL MEJOR MODELO
# ============================================================

# Usaremos la librería ARDL para estimar automáticamente
# los mejores modelos en base al criterio de información AIC

datos <- data.frame(inflacion, ddesempleo)

seleccion <- auto_ardl(
  inflacion ~ ddesempleo,
  data = datos,
  max_order = c(4, 4),
  selection = "AIC"
)

# donde "datos" será el dataframe donde guardamos las series
# definitivas y "c(4,4)" será el comando que permita al
# algoritmo recomendar hasta 4 retardos de cada variable,
# pues estos 4 retardos corresponden a un año

seleccion$top_orders

# Los primeros 5 modelos parecen tener los AIC más
# favorables. Es justo probar estos 5 modelos

modelo1 <- ardl(
  inflacion ~ ddesempleo,
  data = datos,
  order = c(4, 4)
)
summary(modelo1)

modelo2 <- ardl(
  inflacion ~ ddesempleo,
  data = datos,
  order = c(3, 4)
)
summary(modelo2)

modelo3 <- ardl(
  inflacion ~ ddesempleo,
  data = datos,
  order = c(3, 3)
)
summary(modelo3)

modelo4 <- ardl(
  inflacion ~ ddesempleo,
  data = datos,
  order = c(2, 4)
)
summary(modelo4)

modelo5 <- ardl(
  inflacion ~ ddesempleo,
  data = datos,
  order = c(2, 3)
)
summary(modelo5)

# Se puede escoger cualquiera de estos modelos: el punto
# es evaluarlos en función de la significancia de los
# coeficientes, el R2 ajustado y el estadístico F para
# la regresión conjunta

# Para este ejemplo, seleccionamos el modelo5, pues parece
# ser el modelo más parsimonioso de entre todos; además,
# sus coeficientes son los más significativos de entre la
# selección de 5 modelos

modelo5 <- modelo

# ============================================================
# 17. DIAGNÓSTICO DE LOS RESIDUOS
# ============================================================

# Los residuos son las diferencias entre los valores observados
# de la inflación y los valores estimados por el modelo.

# residuo_t = inflación observada_t - inflación estimada_t

# Un modelo con buen ajuste no necesariamente es adecuado
# para pronosticar. Debemos estudiar si sus residuos presentan
# patrones sistemáticos que el modelo no ha capturado.

# En particular, examinaremos:
# 1. La media y la distribución de los residuos.
# 2. Su comportamiento gráfico.
# 3. La autocorrelación residual.

# En un modelo correctamente especificado, los residuos no
# deberían mostrar patrones temporales sistemáticos relevantes.

# ------------------------------------------------------------
# 17.1. OBTENER LOS RESIDUOS
# ------------------------------------------------------------

residuos <- residuals(modelo)

# Resumen estadístico.

summary(residuos)

# Media de los residuos.

mean(residuos, na.rm = TRUE)

# Desviación estándar de los residuos.

sd(residuos, na.rm = TRUE)

# ------------------------------------------------------------
# 17.2. GRÁFICO DE LOS RESIDUOS
# ------------------------------------------------------------

plot(
  residuos,
  type = "l",
  main = "Residuos del modelo ARD seleccionado",
  xlab = "Observación temporal",
  ylab = "Residuo"
)

abline(h = 0, lty = 2)

# Interpretación:

# Los residuos fluctúan alrededor de cero sin
# presentar patrones persistentes claramente identificables,
# aunque podría existir heteroscedasticidad

# Un gráfico favorable no demuestra, por sí solo, que el
# modelo esté correctamente especificado, pero puede dar 
# una idea de la precisión del modelo

# ============================================================
# 18. AUTOCORRELACIÓN DE LOS RESIDUOS
# ============================================================

# En series temporales, los errores de un periodo pueden estar
# correlacionados con los errores de periodos anteriores.

# La autocorrelación residual puede indicar que el modelo
# no incorpora adecuadamente la dinámica de la serie.

# La función acf() calcula y representa la función de
# autocorrelación muestral.

# ------------------------------------------------------------
# 18.1. FUNCIÓN DE AUTOCORRELACIÓN (FAC)
# ------------------------------------------------------------

acf(
  residuos,
  main = "FAC de los residuos del modelo ARD"
)

# No se presentan autocorrelaciones relevantes en varios retardos.
# Los residuos parecen ser ruido blanco, hecho que tendrá que
# ser confirmado

# Las bandas del gráfico son aproximadas y deben interpretarse
# con cautela, especialmente cuando se han estimado parámetros.

# ------------------------------------------------------------
# 18.2. PRUEBA DE LJUNG-BOX
# ------------------------------------------------------------

# La prueba de Ljung-Box evalúa conjuntamente si varias
# autocorrelaciones son iguales a cero.

# Hipótesis nula:
# H0: las autocorrelaciones examinadas son conjuntamente cero.

# Hipótesis alternativa:
# H1: al menos una autocorrelación examinada es distinta de cero.

# Un p-valor pequeño permite rechazar H0.

Box.test(
  residuos,
  lag = 8,
  type = "Ljung-Box",
  fitdf = 0
)

# Se elijen 8 rezagos para capturar la autocorrelación
# entre 2 años, cada uno con 4 trimestres. También se decide
# usar la prueba Ljung-Box por tener la propiedad de
# corregir el estadístico de prueba por la cantidad
# de observaciones de la muestra

# La prueba muestra que, a una significancia del 5%, no
# se rechaza la hipótesis de autocorrelación conjunta igual
# a cero

# ============================================================
# 20. INTERPRETACIÓN ECONÓMICA DEL MODELO
# ============================================================

# ------------------------------------------------------------
# 20.1. INTERPRETAR EL SIGNO DE LOS COEFICIENTES
# ------------------------------------------------------------

# Si los coeficientes de retardos del desempleo son negativos:

# Existe una asociación condicional negativa entre ese retardo
# del desempleo y la inflación actual, manteniendo constantes
# los demás regresores.

# Si son positivos:

# Existe una asociación condicional positiva.

# Ninguno de estos resultados demuestra automáticamente
# causalidad.

# ------------------------------------------------------------
# 20.3. INTERPRETAR EL R²
# ------------------------------------------------------------

# El R² mide la proporción de la variación muestral de la
# variable dependiente que explica la regresión, según
# la definición habitual del coeficiente de determinación.

# Un R² alto no garantiza:
# - Que las variables sean estacionarias.
# - Que no exista una regresión espuria.
# - Que los residuos no estén autocorrelacionados.
# - Que los coeficientes tengan interpretación causal.

# ============================================================
# 21. CONCEPTOS DE GUJARATI: SERIES NO ESTACIONARIAS
# ============================================================

# Una caminata aleatoria sin deriva puede expresarse como:

# Y_t = Y_(t-1) + u_t

# Donde u_t es una innovación aleatoria.

# Equivalentemente:

# ΔY_t = u_t

# Si las innovaciones son ruido blanco, las primeras
# diferencias pueden ser estacionarias aunque la serie
# original no lo sea

# ============================================================
# 22. CONCEPTOS DE STOCK Y WATSON: PREDICCIÓN
# ============================================================

# Stock y Watson distinguen entre:

# 1. Predicción:
# Utilizar información disponible para anticipar valores futuros

# 2. Inferencia causal:
# Estimar cómo cambia una variable como consecuencia de una
# variación causal en otra

# Un modelo puede resultar útil para predecir aunque sus
# coeficientes no tengan una interpretación causal

# La curva de Phillips empírica de este ejercicio utiliza
# la historia de la inflación y del desempleo para evaluar
# la capacidad de predecir la inflación.

# Para demostrar capacidad predictiva, el modelo debería
# evaluarse con observaciones que no hayan sido utilizadas
# en su estimación.

# ============================================================
# 23. RESUMEN DEL PROCEDIMIENTO ECONOMÉTRICO
# ============================================================

# PASO 1:
# Plantear la pregunta económica.
#
# PASO 2:
# Identificar las variables y sus unidades.
#
# PASO 3:
# Construir la serie temporal y verificar las fechas.
#
# PASO 4:
# Representar gráficamente las series.
#
# PASO 5:
# Evaluar la estacionariedad mediante pruebas apropiadas.
#
# PASO 6:
# Determinar una especificación econométrica coherente
# con las propiedades de las variables.
#
# PASO 7:
# Estimar el mejor modelo ARD después de comparar entre
# modelos.

# PASO 9:
# Diagnosticar los residuos.

# PASO 10:
# Interpretar los resultados y evaluar la capacidad predictiva.

# Este orden evita seleccionar una regresión únicamente
# por su R² o por la significación de algunos coeficientes.

# ============================================================
# 24. BANCO DE PRÁCTICA: PREGUNTAS DE ESPECIFICACIÓN
# ============================================================

# Las siguientes preguntas sirven para practicar la formulación
# del problema, la identificación de las variables y la
# especificación del modelo.
#
# Intenta responderlas antes de consultar las soluciones.

# PREGUNTA 1
# ¿Cuál es la variable dependiente de la curva de Phillips
# empírica estudiada en este ejercicio?

# RESPUESTA:
# La tasa de inflación.

# PREGUNTA 2
# ¿Cuál es la variable explicativa principal?

# RESPUESTA:
# La tasa de desempleo.

# PREGUNTA 3
# ¿Por qué se incluyen retardos de la inflación?

# RESPUESTA:
# Para capturar la persistencia de la inflación y utilizar
# información pasada para predecir su valor actual.

# PREGUNTA 4
# ¿Qué significa p en un modelo ARD(p,q)?

# RESPUESTA:
# El número de retardos de la variable dependiente.

# PREGUNTA 5
# ¿Qué significa q en un modelo ARD(p,q)?

# RESPUESTA:
# El número de retardos de la variable explicativa.

# PREGUNTA 6
# ¿Qué hipótesis contrasta la prueba ADF?

# RESPUESTA:
# La hipótesis nula de raíz unitaria frente a una alternativa
# de estacionariedad, según la especificación de la prueba.

# PREGUNTA 7
# ¿Qué significa que una serie sea I(1)?

# RESPUESTA:
# Que no es estacionaria en niveles, pero se vuelve estacionaria
# después de una primera diferencia, si se confirma empíricamente.

# PREGUNTA 8
# ¿Qué es una regresión espuria?

# RESPUESTA:
# Una regresión que aparenta mostrar una relación estadística
# entre variables sin que exista una relación económica válida,
# situación que puede presentarse con series no estacionarias.

# PREGUNTA 9
# ¿Qué criterio se utiliza para seleccionar el modelo en
# la sección 16?

# RESPUESTA:
# El criterio de información bayesiano (BIC).

# PREGUNTA 10
# Si el coeficiente de un retardo del desempleo es negativo,
# ¿qué indica?

# RESPUESTA:
# Una asociación condicional negativa entre ese retardo del
# desempleo y la inflación actual, manteniendo constantes
# los demás regresores. No demuestra causalidad por sí sola.

# PREGUNTA 11
# ¿Qué implica rechazar la hipótesis nula de la prueba
# de Ljung-Box?

# RESPUESTA:
# Que existe evidencia de autocorrelación conjunta en los
# retardos examinados, según la especificación de la prueba.

# PREGUNTA 12
# ¿Por qué un R² alto no garantiza un modelo adecuado?

# RESPUESTA:
# Porque no demuestra estacionariedad, ausencia de regresión
# espuria, residuos adecuados ni capacidad predictiva futura.

# PREGUNTA 13
# ¿La curva de Phillips empírica demuestra causalidad?

# RESPUESTA:
# No necesariamente. En el capítulo 14 de Stock y Watson,
# el enfoque central de esta aplicación es la predicción.

# PREGUNTA 14
# ¿Qué representa la inflación calculada como
# 400 * Δln(IPC)?

# RESPUESTA:
# La tasa de inflación trimestral anualizada, expresada
# aproximadamente en porcentaje.

# PREGUNTA 15
# ¿Por qué debemos examinar los residuos?

# RESPUESTA:
# Para detectar patrones no capturados por el modelo,
# especialmente autocorrelación y otros problemas que pueden
# afectar la inferencia y la capacidad predictiva.