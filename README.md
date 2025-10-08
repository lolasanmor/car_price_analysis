# 📊 Análisis Estadístico de las variables que influyen en el precio de los coches
## 📘 Descripción del Proyecto

Este proyecto realiza un **análisis estadístico completo** del conjunto de datos **Car Price Dataset**, obtenido de la plataforma [Kaggle](https://www.kaggle.com/).  
El objetivo principal es **identificar los factores que influyen en el precio de los automóviles** utilizando herramientas estadísticas y modelos de regresión lineal implementados en **R**.

El dataset contiene información sobre **10.000 vehículos** y **10 variables**, que incluyen datos técnicos, tipo de combustible, transmisión, kilometraje, tamaño del motor y más.

---

## 📊 Contenido del Análisis

### 1️⃣ Análisis descriptivo de los datos
- Medidas de posición y dispersión (media, mediana, varianza, desviación típica, asimetría, curtosis).
- Detección de valores atípicos mediante diagramas de caja (*boxplots*).

### 2️⃣ Matriz de correlaciones
- Cálculo del coeficiente de correlación de Pearson entre variables cuantitativas.
- Visualización con *heatmap*.

### 3️⃣ Análisis detallado de las variables
- Histogramas y boxplots de **Price**, **Mileage** y **Engine_Size**.
- Interpretación de la forma de las distribuciones.

### 4️⃣ Tabla de contingencia entre Price y Mileage
- Agrupación por cuartiles y visualización con mapa de calor.
- Prueba Chi-cuadrado de independencia.

### 5️⃣ Modelos de regresión
#### ➤ Regresión lineal simple: `Price ~ Mileage`
- Evaluación de significancia, R² y diagnóstico de residuos (normalidad, homocedasticidad e independencia).

#### ➤ Regresión lineal múltiple: `Price ~ Mileage + Engine_Size`
- Selección del modelo óptimo y validación estadística.

### 6️⃣ Análisis del precio según el tipo de combustible
- Comparación entre gasolina, diésel, híbrido y eléctrico.
- Diagramas de caja, histogramas y *heatmap* combinado con transmisión.
