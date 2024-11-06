

# Librerías necesarias
library(dplyr)
library(ggplot2)
library(caret)


# Ejemplo de datos simulados con respuestas hipotéticas


set.seed(123)

datos_profesores <- data.frame(
  precio_dispuesto_pagar = c(4500, 3800, 4200, 3500, 4900, 4100, 3700, 4000, 4400, 3900, 4300, 4200),
  tazas_mensuales = c(15, 18, 20, 12, 25, 17, 13, 20, 22, 16, 19, 21),
  preferencia_consumo = as.factor(c("Casa", "Cafetería", "Casa", "Casa", "Cafetería", "Casa", 
                                    "Casa", "Cafetería", "Cafetería", "Casa", "Cafetería", "Casa"))
)


datos_profesores


ggplot(datos_profesores,aes(x=tazas_mensuales,y=precio_dispuesto_pagar,color=preferencia_consumo))+
  geom_point(size=5)+
  theme_minimal()


# Simulación

modelo_inicial <- lm(precio_dispuesto_pagar ~ tazas_mensuales , data = datos_profesores)

nueva_muestra <- data.frame(
  tazas_mensuales = rnorm(200, mean = mean(datos_profesores$tazas_mensuales), 
                          sd = sd(datos_profesores$tazas_mensuales))
)

# Usamos el modelo inicial para predecir 'precio_dispuesto_pagar' en la nueva muestra
nueva_muestra$precio_dispuesto_pagar <- predict(modelo_inicial, nueva_muestra) + rnorm(200, 0, 500) # Añadimos ruido


ggplot(nueva_muestra,aes(x=tazas_mensuales,y=precio_dispuesto_pagar))+
  geom_point(size=5)+
  theme_minimal()


# Aplicar el modelo KNN para predecir la preferencia de consumo
# Escalamos las variables para asegurar que están en la misma escala
escala_datos <- scale(datos_profesores[, c("precio_dispuesto_pagar", "tazas_mensuales")])
escala_nueva_muestra <- scale(nueva_muestra[, c("precio_dispuesto_pagar", "tazas_mensuales")])


# Predicción de la preferencia de consumo con KNN
nueva_muestra$preferencia_consumo <- knn(
  train = escala_datos,
  test = escala_nueva_muestra,
  cl = datos_profesores$preferencia_consumo,
  k = 3  # Número de vecinos
)


nueva_muestra$preferencia_consumo <- factor(nueva_muestra$preferencia_consumo, levels = c("Casa", "Cafetería"))


ggplot(nueva_muestra,aes(x=tazas_mensuales,y=precio_dispuesto_pagar,color=preferencia_consumo))+
  geom_point(size=5)+
  theme_minimal()+
  labs(title = "Relación entre consumo y precio dispuesto a pagar por café",
       x = "Tazas al mes",
       y = "Precio dispuesto a pagar",
       color = "Preferencia de consumo")




# Dividir datos en conjunto de entrenamiento y prueba (80/20)

trainIndex <- createDataPartition(nueva_muestra$precio_dispuesto_pagar, p = .8, list = FALSE, times = 1)
datos_entrenamiento <- nueva_muestra[trainIndex, ]
datos_prueba <- nueva_muestra[-trainIndex, ]

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



# Visualizar relación de tazas_mensuales y precio_dispuesto_pagar con preferencia_consumo
g1<-ggplot(datos_prueba, aes(x = tazas_mensuales, y = precio_dispuesto_pagar)) +
  geom_point(aes(color=preferencia_consumo),size=3) +
  geom_smooth(method = "lm") +
  labs(title = "Relación entre Consumo y Precio Dispuesto a Pagar por Café",
       x = "Tazas al Mes",
       y = "Precio Dispuesto a Pagar",
       color = "Preferencia de Consumo") +
  theme_minimal()


plotly::ggplotly(g1)



# Distribución de las predicciones
ggplot(data.frame(predicciones), aes(x = predicciones)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "skyblue", color = "black") +
  geom_density(color = "blue", size = 1) +
  geom_vline(aes(xintercept = mean(predicciones)), color = "red", linetype = "dashed", size = 1) +
  labs(title = "Distribución de la Predicción de Disposición a Pagar por Café",
       x = "Predicción de Disposición a Pagar",
       y = "Densidad") +
  theme_minimal()


summary(predicciones)



shapiro.test(residuals(modelo))


library(lmtest)
bptest(modelo$finalModel)



# Realizar una predicción para un nuevo dato
nuevo_dato <- data.frame(tazas_mensuales = 20, preferencia_consumo = "Cafetería")
prediccion_nuevo <- predict(modelo, nuevo_dato)
cat("Predicción del precio dispuesto a pagar para el nuevo dato: ", prediccion_nuevo, "\n")
