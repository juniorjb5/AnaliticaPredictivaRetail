

# Librerías necesarias
library(dplyr)
library(ggplot2)
library(caret)


# Ejemplo de datos simulados con respuestas hipotéticas


#datos_profesores <- data.frame(
#  precio_dispuesto_pagar = c(),
#  tazas_mensuales = c(), # Número de tazas al mes
#  preferencia_consumo = c("Casa", "Cafetería")
#)


set.seed(123)

datos_profesores <- data.frame(
  precio_dispuesto_pagar = round(runif(12, 2.5, 5), 2), # Precio dispuesto a pagar
  tazas_mensuales = sample(10:30, 12, replace = TRUE), # Número de tazas al mes
  preferencia_consumo = sample(c("Casa", "Cafetería"), 12, replace = TRUE) # Preferencia de consumo
)

# Asumimos normalidad para datos cuantitativos, categorías para cualitativos

# Crear función para simulación basada en datos iniciales

simular_datos <- function(datos, n) {
  data.frame(
    precio_dispuesto_pagar = rnorm(n, mean(datos$precio_dispuesto_pagar), sd(datos$precio_dispuesto_pagar)),
    tazas_mensuales = rnorm(n, mean(datos$tazas_mensuales), sd(datos$tazas_mensuales)),
    preferencia_consumo = sample(datos$preferencia_consumo, n, replace = TRUE)
  )
}

# Generar muestra de 200 individuos
datos_simulados <- simular_datos(datos_profesores, 200)


# Convertir la variable 'preferencia_consumo' en una dummy variable para el modelo
datos_simulados$preferencia_consumo <- factor(datos_simulados$preferencia_consumo, levels = c("Casa", "Cafetería"))

# Dividir datos en conjunto de entrenamiento y prueba (80/20)

trainIndex <- createDataPartition(datos_simulados$precio_dispuesto_pagar, p = .8, list = FALSE, times = 1)
datos_entrenamiento <- datos_simulados[trainIndex, ]
datos_prueba <- datos_simulados[-trainIndex, ]

# Entrenar el modelo de regresión lineal
modelo <- train(
  precio_dispuesto_pagar ~ tazas_mensuales + preferencia_consumo,
  data = datos_entrenamiento,
  method = "lm"
)

# Resumen del modelo
summary(modelo)

# Realizar predicción en el conjunto de prueba
predicciones <- predict(modelo, datos_prueba)

# Calcular RMSE para medir el desempeño
rmse <- RMSE(predicciones, datos_prueba$precio_dispuesto_pagar)
cat("El modelo se desvía aproximadamente RMSE unidades del valor real en su predicción de la disposición a pagar: RMSE =", rmse, "\n")



# Visualizar relación de tazas_mensuales y precio_dispuesto_pagar con preferencia_consumo
ggplot(datos_simulados, aes(x = tazas_mensuales, y = precio_dispuesto_pagar, color = preferencia_consumo)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Relación entre Consumo y Precio Dispuesto a Pagar por Café",
       x = "Tazas al Mes",
       y = "Precio Dispuesto a Pagar",
       color = "Preferencia de Consumo") +
  theme_minimal()


# Distribución de las predicciones
ggplot(data.frame(predicciones), aes(x = predicciones)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "skyblue", color = "black") +
  geom_density(color = "blue", size = 1) +
  geom_vline(aes(xintercept = mean(predicciones)), color = "red", linetype = "dashed", size = 1) +
  labs(title = "Distribución de la Predicción de Disposición a Pagar por Café",
       x = "Predicción de Disposición a Pagar",
       y = "Densidad") +
  theme_minimal()



# Realizar una predicción para un nuevo dato
nuevo_dato <- data.frame(tazas_mensuales = 20, preferencia_consumo = "Cafetería")
prediccion_nuevo <- predict(modelo, nuevo_dato)
cat("Predicción del precio dispuesto a pagar para el nuevo dato: ", prediccion_nuevo, "\n")
