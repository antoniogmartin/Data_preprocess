# Lectura de los datos
vinos <- read.csv(file="winequality-red.csv", header = TRUE)

sapply(vinos, function(X) class(X))


#3 Limpieza de datos

#3.1. Ceros o elementos vacíos?

# Función para obtener elementos vacíos
sapply(vinos, function(x) sum(is.na(x)))

#No tenemos valores NA

#3.2. Valores externos
nombres = c("fixed acidity",
          "volatile acidity",
          "citric acid",
          "residual sugar",
          "chlorides",
          "free sulfur dioxide",
          "total sulfur dioxide",
          "density",
          "pH",
          "sulphates",
          "alcohol")
for (i in 1:11) {
  # 1. Open jpeg file
  aux = paste(c(nombres[i],".png"), collapse = "")
  png(aux , width = 700, height = 700)
  # 2. Create the plot
  boxplot(vinos[,i], names =,  main=c("Boxplot of",  nombres[i]))
  # 3. Close the file
  dev.off()
}

#Decidimos imputar como NA los valores mayores a 1 en densidad
for (i in 1:nrow(vinos)) {
  if(vinos$density[i]>1)
  {
    vinos$density[i] = NA
  }
  
}

#install.packages("VIM")
library(VIM)

#Restauramos valores con KNN
vinos$density = kNN(vinos)$density


boxplot(vinos$density,  main="Boxplot density")


