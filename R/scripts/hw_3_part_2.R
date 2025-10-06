# Instalar y cargar la librería igraph si no está instalada
if (!require(igraph)) install.packages("igraph")
library(igraph)

# Definimos el grafo dirigido
edges <- matrix(c(
  "Z1", "X",
  "Z1", "Y",
  "Z2", "X",
  "Z2", "Y",
  "Z3", "Z2",
  "Z3", "Y",
  "X", "Y"
), ncol = 2, byrow = TRUE)

# Creamos el grafo dirigido
G <- graph_from_edgelist(edges, directed = TRUE)

# Definimos las posiciones de los nodos
layout <- matrix(c(
  -1, 1,   # Z1
  1, 1,   # Z2
  0, 2,   # Z3
  0, 0,   # X
  0, -1   # Y
), ncol = 2, byrow = TRUE)
rownames(layout) <- c("Z1", "Z2", "Z3", "X", "Y")

# Graficamos el grafo
plot(G,
     layout = layout,           # Usamos las posiciones definidas
     vertex.label = V(G)$name,  # Mostramos los nombres de los nodos
     vertex.size = 30,          # Tamaño de los nodos
     vertex.color = "lightblue",# Color de los nodos
     edge.arrow.size = 0.05,     # Tamaño de las flechas
     edge.color = "black",      # Color de las aristas
     main = "Grafo dirigido")   # Título del gráfico.
################################################################################

# Semilla para reproducibilidad
set.seed(123)

# Número de observaciones
n <- 1000000

# Simulación del DAG
Z1 <- rnorm(n, mean = 0, sd = 1)
Z3 <- rnorm(n, mean = 0, sd = 1)
Z2 <- Z3 + rnorm(n, mean = 0, sd = 1)
X <- Z1 + Z2 + rnorm(n, mean = 0, sd = 1)
Y <- 1 * X + Z1 + Z2 + Z3 + rnorm(n, mean = 0, sd = 1)

# Data frame con los datos
datos <- data.frame(Z1 = Z1, Z2 = Z2, Z3 = Z3, X = X, Y = Y)

# Mostrar las primeras filas del data frame
head(datos)

################################################################################
# Semilla para reproducibilidad (si se desea mantener la misma aleatoriedad que en Python)
set.seed(123)

# Generamos una muestra de 1,000,000 valores uniformes entre 0 y 1
sample <- runif(1000000, min = 0, max = 1) < 0.001

# Calculamos la suma de los valores TRUE (número de observaciones que cumplen la condición)
sum(sample)

# Seleccionamos los índices donde sample es TRUE (equivalente a np.where(sample)[0])
sample_indices <- which(sample)

# Mostramos los primeros índices (opcional, para verificar)
head(sample_indices)

################################################################################

modelo=lm(Y ~ X - 1, data = datos[sample_indices, ])

summary(modelo)

################################################################################

modelo <- lm(Y ~ X + Z1 + Z2 -1, data = datos[sample_indices, ])

summary(modelo)

################################################################################

modelo <- lm(Y ~ X + Z1 + Z2 + Z3 - 1, data = datos[sample_indices, ])

summary(modelo)

################################################################################
# Definir fórmulas de regresión (equivalente a regresiones en Python)
regresiones <- list(
  "Y ~ X" = c("X"),
  "Y ~ X + Z1" = c("X", "Z1"),
  "Y ~ X + Z2" = c("X", "Z2"),
  "Y ~ X + Z1 + Z2" = c("X", "Z1", "Z2"),
  "Y ~ X + Z1 + Z2 + Z3" = c("X", "Z1", "Z2", "Z3")
)
# Lista para almacenar los modelos (equivalente a resultados en Python)
resultados <- list()

# Ajustamos los modelos para cada fórmula
for (nombre in names(regresiones)) {
  # Crear fórmula a partir de las variables
  formula_str <- paste("Y ~", paste(regresiones[[nombre]], collapse = " + "))
  formula <- as.formula(formula_str)
  
  # Ajustar el modelo usando los datos en sample_indices
  modelo <- lm(formula, data = datos[sample_indices, ])
  
  # Almacenar el modelo en la lista resultados
  resultados[[nombre]] <- modelo
}

# Verificamos que los modelos se ajustaron correctamente
cat("Modelos ajustados:", names(resultados), "\n")

# Opcional: Mostrar un resumen de cada modelo para verificar
for (nombre in names(resultados)) {
  cat("\nResumen del modelo:", nombre, "\n")
  print(summary(resultados[[nombre]]))
}
###############################################################################
# Crear data frame df_plot con coeficientes e intervalos de confianza
estimaciones <- list()

for (nombre in names(resultados)) {
  modelo <- resultados[[nombre]]
  coef_X <- coef(modelo)["X"]
  ci <- confint(modelo, level = 0.99)["X", ]
  estimaciones[[nombre]] <- c(nombre, coef_X, ci[1], ci[2])
}

# Convertimos la lista en un data frame
df_plot <- data.frame(
  Modelo = sapply(estimaciones, function(x) x[1]),
  Coef_X = as.numeric(sapply(estimaciones, function(x) x[2])),
  CI_low = as.numeric(sapply(estimaciones, function(x) x[3])),
  CI_high = as.numeric(sapply(estimaciones, function(x) x[4]))
)

# Mostramos df_plot
cat("Contenido de df_plot:\n")
print(df_plot)

# Instalar y cargar ggplot2 si no está instalado
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# Gráfico con barras de error
ggplot(df_plot, aes(x = Modelo, y = Coef_X)) +
  geom_point(color = "blue", size = 3) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.2, linewidth = 0.8) +
  geom_hline(aes(yintercept = 1, linetype = "Efecto verdadero"), color = "red") +
  labs(y = "Estimación de coeficiente de X", 
       title = "Estimaciones de X con diferentes controles (IC 99%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  scale_linetype_manual(values = c("Efecto verdadero" = "dashed")) +
  guides(linetype = guide_legend(title = NULL))

# Guardar el gráfico 
ggsave("estimaciones_X.png", width = 8, height = 6)
