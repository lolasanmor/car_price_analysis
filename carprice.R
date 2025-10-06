

install.packages("ggplot2")
install.packages("readr")
install.packages("gridExtra")
install.packages("plyr")
install.packages("e1071")
install.packages("readxl")
install.packages("DMtest")

library(ggplot2)
library(readr)
library(gridExtra)
library(grid)
library(plyr)
library(e1071)
library(readxl)
library(DMtest) 

# Lectura de datos.
Dataset<-read_excel("C:/Users/lolas/OneDrive/Escritorio/BIG DATA/Ejercicio estadistica/datasetcarprice-bigdata.xlsx")
#Dataset_resumido<-read_excel("C:/Users/lenovo/Desktop/BIG DATA/OCTUBRE 2017/Estad?stica/Pr?ctica MAB2.xlsx")

# Primera aproximaci?n a los datos.
str(Dataset)
names(Dataset)
summary(Dataset)
head(Dataset)

#summary(Dataset_resumido)

# M?nimo, primer cuartil, mediana, tercer cuartil y m?ximo.
fivenum(Dataset$Price)
fivenum(Dataset$Mileage)
fivenum(Dataset$Engine_Size)

#CODIGO PARA LA PESTAÑA DE DESCRIPTIVOS
# Carga de librerías necesarias
install.packages("moments")  # Para curtosis y asimetría
library(moments)

# Variables a analizar
variables <- c("Price", "Mileage", "Engine_Size", "Doors", "Owner_Count")

# Cálculo de medidas estadísticas
for (var in variables) {
  cat("\nAnálisis de la variable:", var, "\n")
  
  data <- Dataset[[var]]
  
  cat("Media:", mean(data, na.rm = TRUE), "\n")
  cat("Error típico:", sd(data, na.rm = TRUE) / sqrt(length(data)), "\n")
  cat("Mediana:", median(data, na.rm = TRUE), "\n")
  
  # Cálculo de la moda
  moda <- as.numeric(names(sort(table(data), decreasing = TRUE)[1]))
  cat("Moda:", moda, "\n")
  
  cat("Desviación estándar:", sd(data, na.rm = TRUE), "\n")
  cat("Varianza de la muestra:", var(data, na.rm = TRUE), "\n")
  cat("Curtosis:", kurtosis(data, na.rm = TRUE), "\n")
  cat("Coeficiente de asimetría:", skewness(data, na.rm = TRUE), "\n")
  cat("Rango:", diff(range(data, na.rm = TRUE)), "\n")
  cat("Mínimo:", min(data, na.rm = TRUE), "\n")
  cat("Máximo:", max(data, na.rm = TRUE), "\n")
  cat("Suma:", sum(data, na.rm = TRUE), "\n")
  cat("Cuenta:", length(data[!is.na(data)]), "\n")
  
  cat("-----------------------------\n")
}

# Medidas de posición y dispersión
mean(Dataset$Price)
median(Dataset$Price)
var(Dataset$Price)
sd(Dataset$Price)
skewness(Dataset$Price)
kurtosis(Dataset$Price)

mean(Dataset$Mileage)
median(Dataset$Mileage)
var(Dataset$Mileage)
sd(Dataset$Mileage)
skewness(Dataset$Mileage)
kurtosis(Dataset$Mileage)

mean(Dataset$Engine_Size)
median(Dataset$Engine_Size)
var(Dataset$Engine_Size)
sd(Dataset$Engine_Size)
skewness(Dataset$Engine_Size)
kurtosis(Dataset$Engine_Size)


#------- MATRIZ DE CORRELACIONES
# Instalación y carga de librerías necesarias
install.packages("corrplot")  # Si no está instalada
library(corrplot)

# Selección de variables numéricas para la matriz de correlaciones
variables <- Dataset[, c("Price", "Mileage", "Engine_Size", "Doors", "Owner_Count")]

# Cálculo de la matriz de correlaciones
cor_matrix <- cor(variables, use = "complete.obs", method = "pearson")

# Creación del mapa de calor con tonos verdes y título
corrplot(cor_matrix, 
         method = "color",      # Representación con colores
         type = "upper",        # Solo muestra la parte superior de la matriz
         col = colorRampPalette(c("darkgreen", "lightgreen", "white"))(200),  # Tonos verdes
         tl.col = "black",      # Color de las etiquetas de texto
         tl.srt = 45,           # Rotación de las etiquetas
         addCoef.col = "black", # Muestra los valores de correlación en negro
         number.cex = 0.8,      # Tamaño de los números
         title = "Matriz de Correlaciones - Análisis de Automóviles", 
         mar = c(0, 0, 2, 0))  # Margen superior para el título

#---------MATRIZ DE DISPERSION
# Matriz de diagramas de dispersión para Price, Mileage y Engine_Size1
Data_resumido <- Dataset[, c("Price", "Mileage", "Engine_Size")]
pairs(Data_resumido, main = "Matriz de Diagramas de Dispersión", pch = 19, col = "black")


#-------------------  HISTOGRAMAS MEJORADOS
# Histogramas mejorados con más detalles
histograma_personalizado <- function(x, titulo, color) {
  hist(x, 
       main = titulo, 
       col = color, 
       border = "#1b7837", 
       breaks = 30, 
       xlab = "Valores", 
       ylab = "Frecuencia", 
       cex.main = 1.2, 
       cex.axis = 0.8)
}

# Generar los histogramas con tonos de verde
par(mfrow = c(1, 3), mar = c(5, 5, 4, 2) + 0.1)

histograma_personalizado(Dataset$Price, "Histograma de Precio", "lightgreen")
histograma_personalizado(Dataset$Mileage, "Histograma de Kilometraje", "darkgreen")
histograma_personalizado(Dataset$Engine_Size, "Histograma de Tamaño de Motor", "#b8e186")

#------------------- DIAGRAMA CAJAS MEJORADO
# Diagrama de cajas mejorado y estilizado
boxplot2 <- function(x, y) {
  stats <- boxplot.stats(x)$stats
  f <- fivenum(x)
  stats2 <- c(f[1], stats, f[5])
  stats3 <- c(f[1], f[5])
  media <- mean(x)
  
  # Personalización del boxplot
  boxplot(x, 
          main = y, 
          col = "lightgreen", 
          border = "#1b7837", 
          boxwex = 0.5, 
          notch = TRUE, 
          pch = 19, 
          cex.main = 1.2, 
          cex.axis = 0.8)
  
  # Líneas de los bigotes
  abline(h = stats[1], lty = 2, col = "#d73027", lwd = 1.5)
  abline(h = stats[5], lty = 2, col = "#d73027", lwd = 1.5)
  
  # Línea de la media
  abline(h = media, lty = 3, col = "#4575b4", lwd = 1.5)
  text(1.35, media, labels = paste("Media: ", round(media, 4)), col = "#4575b4", cex = 0.7, pos = 4)
  
  # Etiquetas para los cuartiles y bigotes
  labels_stats <- c('BIGOTE INFERIOR', 'PRIMER CUARTIL', 'MEDIANA', 'TERCER CUARTIL', 'BIGOTE SUPERIOR')
  text(rep(1.35, 5), stats, labels = labels_stats, col = "#1b7837", cex = 0.7, pos = 4)
  
  # Valores numéricos
  text(rep(0.5, 7), stats2, labels = round(stats2, 4), cex = 0.6, col = "#252525")
  
  # Mínimo y máximo
  text(rep(0.75, 2), stats3, labels = c('MÍNIMO', 'MÁXIMO'), col = "#762a83", cex = 0.7, pos = 4)
}

# Visualizar los diagramas de caja con más claridad
par(mfrow = c(1, 3), mar = c(5, 5, 4, 2) + 0.1)
boxplot2(Dataset$Price, 'Diagrama de cajas: Price')
boxplot2(Dataset$Mileage, 'Diagrama de cajas: Mileage')
boxplot2(Dataset$Engine_Size, 'Diagrama de cajas: Engine Size')


#------ TABLA DE CONTINGENCIA
# Cargar librerías necesarias
install.packages("dplyr")
library(dplyr)

# Definir cuartiles de Price y Mileage
Dataset <- Dataset %>%
  mutate(Price_Cat = cut(Price, 
                         breaks = quantile(Price, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                         labels = c("Muy bajo", "Bajo", "Medio", "Alto"), 
                         include.lowest = TRUE),
         Mileage_Cat = cut(Mileage, 
                           breaks = quantile(Mileage, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                           labels = c("Muy bajo", "Bajo", "Medio", "Alto"), 
                           include.lowest = TRUE))

# Crear tabla de contingencia
tabla_contingencia <- table(Dataset$Price_Cat, Dataset$Mileage_Cat)
print(tabla_contingencia)

# Prueba de chi-cuadrado
prueba_chi <- chisq.test(tabla_contingencia)
print(prueba_chi)

# Cargar librerías necesarias
install.packages("reshape")
library(reshape2)

# Convertir la tabla de contingencia en un data frame para ggplot
tabla_df <- as.data.frame(as.table(tabla_contingencia))
colnames(tabla_df) <- c("Price_Cat", "Mileage_Cat", "Frecuencia")

# Crear el heatmap mejorado con etiquetas
ggplot(tabla_df, aes(x = Mileage_Cat, y = Price_Cat, fill = Frecuencia)) +
  geom_tile(color = "white") +  # Cuadros con borde blanco
  geom_text(aes(label = Frecuencia), color = "black", size = 4) +  # Etiquetas dentro de los cuadros
  scale_fill_gradient(low = "lightgreen", high = "darkgreen") +  # Tonos verdes
  labs(title = "Heatmap de la Tabla de Contingencia: Price vs Mileage",
       x = "Categoría de Kilometraje (Mileage)",
       y = "Categoría de Precio (Price)",
       fill = "Frecuencia") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotar etiquetas del eje X
    panel.grid = element_blank()  # Eliminar las líneas de la cuadrícula
  )


#-------------REGRESION LINEAL
regresionlineal<-lm(Price~Mileage, data=Dataset)
summary(regresionlineal)

pendiente<-cov(Dataset$Mileage,Dataset$Price)/var(Dataset$Mileage)
pendiente

independiente<-mean(Dataset$Price)-pendiente*mean(Dataset$Mileage)
independiente


# El coeficiente de determinaci?n (que mide la bondad del ajuste de la recta a los datos) 
coeficiente_R2<-(cov(Dataset$Mileage,Dataset$Price)/(sd(Dataset$Mileage)*sd(Dataset$Price)))^2
coeficiente_R2

# Ajustar el modelo de regresión lineal
modelo <- lm(Price ~ Mileage, data = Dataset)

# Resumen del modelo
summary(modelo)

# Gráfico de dispersión con la recta de regresión
ggplot(Dataset, aes(x = Mileage, y = Price)) +
  geom_point(color = "lightgreen", alpha = 0.5) +  # Puntos en azul con transparencia
  geom_smooth(method = "lm", color = "darkgreen", se = FALSE) +  # Línea de regresión en rojo
  labs(title = "Regresión Lineal: Precio vs Kilometraje",
       x = "Kilometraje",
       y = "Precio") +
  theme_minimal()

#---- Heterocedasticidad vs Homocedasticidad
residuos<- rstandard(regresionlineal)
valores.ajustados<-fitted(regresionlineal)
plot(valores.ajustados, residuos)

# Prueba de Breusch-Pagan 
install.packages("lmtest")
library(lmtest)
bptest(regresionlineal)

#-------Normalidad de los residuos
#grafico qq plot
qqnorm(residuos)
qqline(residuos)
#test
ks.test(residuos, "pnorm", mean(residuos), sd(residuos))
skewness(residuos)
kurtosis(residuos)

#-----------independencia
# Independencia de los residuos.
library(lmtest)
# Prueba de Durbin-Watson. H0: no existe autocorrelaci?n entre los residuos.
dwtest(regresionlineal)
# p-valor < 0,05: Rechacamos H0, por lo que los residuos no son independientes.


#--REGRESION LINEAL MULTIPLE CONSIDERANDO TODAS LAS VARIABLES

regresionlmultiple<-lm(Price~Mileage+Engine_Size+Doors+Owner_Count, data=Dataset)
summary(regresionlmultiple)

regresionlmultiple2<-update(regresionlmultiple, .~.-Owner_Count)
summary(regresionlmultiple2)

#Comparaci?n entre los dos modelos

anova(regresionlmultiple, regresionlmultiple2)

regresionlmultiple3<-update(regresionlmultiple2, .~.-Doors)
summary(regresionlmultiple3)

#Comparaci?n entre los dos modelos

anova(regresionlmultiple2, regresionlmultiple3)

#Regresi?n lineal con la segunda variable independiente (Engine Size)

regresionlmultiple3<-lm(Price~Mileage+Engine_Size, data=Dataset)
summary(regresionlmultiple3)

regresionlmultiple4<-lm(Price~Mileage*Engine_Size, data=Dataset)
summary(regresionlmultiple4)

#GRAFICO 3D
# Cargar las librerías necesarias
install.packages("plotly")
library(plotly)

# Definir los coeficientes del modelo
intercept <- 8905
mileage_coef <- -0.02027
engine_size_coef <- 981.2
interaction_coef <- 0.00006419

# Crear una malla de valores para Mileage y Engine_Size
mileage <- seq(0, 50000, length.out = 50)         # Kilometraje de 0 a 50000
engine_size <- seq(1, 5, length.out = 50)         # Tamaño del motor de 1 a 5

# Crear un grid de combinaciones
grid <- expand.grid(mileage = mileage, engine_size = engine_size)

# Calcular el precio según la ecuación del modelo
grid$price <- intercept + (mileage_coef * grid$mileage) + 
  (engine_size_coef * grid$engine_size) + 
  (interaction_coef * grid$mileage * grid$engine_size)

# Crear el gráfico 3D interactivo
plot_ly(grid, x = ~mileage, y = ~engine_size, z = ~price, 
        type = "scatter3d", mode = "markers",
        marker = list(size = 3, color = ~price, colorscale = "Viridis")) %>%
  layout(title = "Modelo de Regresión: Precio ~ Mileage * Engine Size",
         scene = list(xaxis = list(title = "Mileage"),
                      yaxis = list(title = "Engine Size"),
                      zaxis = list(title = "Price")))

#DIAGN?STICO DEL MODELO
#---- Heterocedasticidad vs Homocedasticidad
# Diagn?stico del modelo sin interacci?n entre las dos variables

residuosmodelo3<- rstandard(regresionlmultiple3)
valores.ajustados.rlm3<-fitted(regresionlmultiple3)
plot(valores.ajustados.rlm3, residuosmodelo3)
# Prueba de Breusch-Pagan
library(lmtest)
bptest(regresionlmultiple3)

#-------Normalidad de los residuos
#grafico qq plot
qqnorm(residuosmodelo3)
qqline(residuosmodelo3)

#test
ks.test(residuosmodelo3, "pnorm", mean(residuosmodelo3), sd(residuosmodelo3))
library(moments)
skewness(residuosmodelo3)
kurtosis(residuosmodelo3)

# Independencia de los residuos.
# Prueba de Durbin-Watson. H0: no existe autocorrelaci?n entre los residuos.
dwtest(regresionlmultiple3)
# p-valor < 0,05: rechazamos hip?otesis inicial y establecemos que hay dependencia de los residuos.

#----PUNTO 6
library(dplyr)
library(moments)

# Función para calcular todas las estadísticas necesarias
calcular_estadisticas <- function(df, grupo) {
  df %>%
    group_by(!!sym(grupo)) %>%
    summarise(
      Media = mean(Price, na.rm = TRUE),
      `Error Típico` = sd(Price, na.rm = TRUE) / sqrt(n()),
      Mediana = median(Price, na.rm = TRUE),
      Moda = as.numeric(names(sort(table(Price), decreasing = TRUE)[1])),
      `Desviación Estándar` = sd(Price, na.rm = TRUE),
      Varianza = var(Price, na.rm = TRUE),
      Curtosis = kurtosis(Price, na.rm = TRUE),
      `Coef. Asimetría` = skewness(Price, na.rm = TRUE),
      Rango = diff(range(Price, na.rm = TRUE)),
      Mínimo = min(Price, na.rm = TRUE),
      Máximo = max(Price, na.rm = TRUE),
      Suma = sum(Price, na.rm = TRUE),
      Cuenta = n()
    )
}

# Aplicar la función para analizar `Price` según `Fuel_Type`
estadisticas_price_fuel <- calcular_estadisticas(Dataset, "Fuel_Type")

# Mostrar los resultados
print(estadisticas_price_fuel)

View(estadisticas_price_fuel)
# Boxplot del precio según el tipo de combustible
ggplot(Dataset, aes(x = Fuel_Type, y = Price, fill = Fuel_Type)) +
  geom_boxplot() +
  labs(title = "Distribución del Precio según el Tipo de Combustible",
       x = "Tipo de Combustible",
       y = "Precio") +
  theme_minimal()

# Histograma del precio según el tipo de combustible
ggplot(Dataset, aes(x = Price, fill = Fuel_Type)) +
  geom_histogram(binwidth = 1000, alpha = 0.7, position = "dodge") +
  labs(title = "Histograma del Precio por Tipo de Combustible",
       x = "Precio",
       y = "Frecuencia") +
  theme_minimal()

#boxplot mejorado
library(ggplot2)
library(dplyr)

# Ordenar Fuel_Type según la mediana del precio
Dataset <- Dataset %>%
  mutate(Fuel_Type = reorder(Fuel_Type, Price, FUN = median))


# Boxplot mejorado con temática verde
ggplot(Dataset, aes(x = Fuel_Type, y = Price, fill = Fuel_Type)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +  # Outliers en rojo
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 0)), 
               vjust = -0.5, color = "black", size = 4) +  # Mostrar medianas
  scale_fill_manual(values = c("darkgreen", "#008000", "#66cc66", "#99ff99")) +  # Tonos verdes
  labs(title = "Distribución del Precio según Tipo de Combustible",
       x = "Tipo de Combustible", y = "Precio") +
  theme_minimal() +
  theme(legend.position = "none")
# Histograma mejorado con colores específicos para cada tipo de combustible
ggplot(Dataset, aes(x = Price, fill = Fuel_Type)) +
  geom_histogram(binwidth = 1000, color = "black", alpha = 0.8) +
  facet_wrap(~ Fuel_Type, scales = "free") +  # Separa los histogramas por tipo de combustible
  scale_fill_manual(values = c("#004d00", "#008000", "#66cc66", "#99ff99")) +  # Misma paleta que el boxplot
  labs(title = "Distribución del Precio por Tipo de Combustible",
       x = "Precio", y = "Frecuencia") +
  theme_minimal() +
  theme(legend.position = "none")

#heatmap
library(reshape2)

# Crear tabla resumen de medias
tabla_medias <- aggregate(Price ~ Fuel_Type + Transmission, data = Dataset, mean)

# Convertir a formato adecuado para heatmap
tabla_medias_melt <- melt(tabla_medias)

# Heatmap del Precio según Tipo de Combustible y Transmisión
ggplot(tabla_medias_melt, aes(x = Transmission, y = Fuel_Type, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen") +
  geom_text(aes(label = round(value, 0)), color = "black", size = 4) +  # Agregar valores dentro de las celdas
  labs(title = "Precio Medio por Tipo de Combustible y Transmisión",
       x = "Transmisión", y = "Tipo de Combustible", fill = "Precio Medio") +
  theme_minimal()


