#Proyecto KNN para enseñanza (clasificador de flores iris)

#Como KNN es un algoritmo muy simple, simplemente se usará este "Proyecto" como ejercicio simple para evaluar la comprensión de la implementación de KNN. 

#Obteniendo el conjunto de datos Data Iris

#Se usará el famoso conjunto de datos de iris para este proyecto. Es un pequeño conjunto de datos con características florales que pueden usarse para intentar predecir las especies de una flor de iris.
#Se usará la librería ISLR para obtener el conjunto de datos del iris. Primero se ve el encabezado del marco de datos del iris.

library(ISLR)
head(iris)
str(iris)

#Estandarizar los datos

#En este caso, el conjunto de datos del iris tiene todas sus características en el mismo orden de magnitud, pero es una buena práctica (especialmente con KNN) para estandarizar las características en sus datos. (Vamos a seguir adelante a pesar de que no es necesario para esta información.)
#Usamos scale () para estandarizar las columnas de características del dataset del iris. Establecemos esta versión estandarizada de los datos como una nueva variable.

estand.carac <- scale(iris[1:4])

#Verificamos que la escala funcione al verificar la varianza de una de las nuevas columnas.

var(estand.carac[,1])

#Se unen los datos estandarizados con la columna respuesta con el objetivo y las labels (la columna con los nombres de las especies).

fina <- cbind(estand.carac,iris[5])
head(final)

#Conjuntos de entrenamiento y prueba
#Se usa la biblioteca caTools para dividir sus datos estandarizados en conjuntos de entrenamiento y prueba. (división 70/30).

set.seed(101)

library(caTools)

sample <- sample.split(final$Species, SplitRatio = .70)
train <- subset(final, sample == TRUE)
test <- subset(final, sample == FALSE)

#Construyendo un modelo KNN.

#Primero llamamos a la biblioteca de la clase.

library(class)

#Usamos la función knn para predecir las especies del conjunto de prueba. Se usa k = 1

predicc <- knn(train[1:4],test[1:4],train$Species,k=1)
predicc

#¿Cuál fue tu tasa de clasificación incorrecta?
mean(test$Species != predicc)

#Elegir un valor K

#Aunque los datos son bastante pequeños para que realmente se tenga idea cómo elegir un buen valor de K.
#Creamos un gráfico de la tasa de error (clasificación errónea) para valores k que van del 1 al 10.

predicc <- NULL
error <- NULL

for(i in 1:10){
  set.seed(101)
  predicc <- knn(train[1:4],test[1:4],train$Species,k=i)
  error[i] <- mean(test$Species != predicc)
}
library(ggplot2)
k.values <- 1:10
error.df <- data.frame(error.rate,k.values)
pl <- ggplot(error.df,aes(x=k.values,y=error.rate)) + geom_point()
pl + geom_line(lty="dotted",color='red')

#El error cae a su valor más bajo para k entre 2-6. Luego comienza a volver a subir, esto se debe a cuán pequeño es el conjunto de datos. En k = 10, comienza a acercarse a la configuración k = 10% de los datos, que es bastante grande.



