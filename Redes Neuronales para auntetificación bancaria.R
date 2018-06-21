# Redes Neuronales para autenficación bancaria

#Ahora se utilizará el conjunto de datos de autenticación bancaria del repositorio UCI.
#Los datos constan de 5 columnas (en inglés):
#variance of Wavelet Transformed image (continuous)
#skewness of Wavelet Transformed image (continuous)
#curtosis of Wavelet Transformed image (continuous)
#entropy of image (continuous)
#class (integer)

#Donde la clase indica si un billete de banco era o no auténtico.

#Obtener los datos
#Se usa read.csv para leer el archivo bank_note_data.csv; Se pueden obtener de aquí:
#https://archive.ics.uci.edu/ml/datasets/banknote+authentication

df <- read.csv('bank_note_data.csv')
head(df)
str(df)

#Análisis Exploratorio
#Se crean las visualizaciones en las que esté interesado. 

#Conuntos de prueba y entrenamiento
#Usamos la biblioteca caTools para dividir los datos en conjuntos de entrenamiento y prueba.

library(caTools)
set.seed(101)
div = sample.split(df$Class, SplitRatio = 0.70)
entr = subset(df, split == TRUE)
test = subset(df, split == FALSE)

#Compruebe la estructura de los datos del tren y tenga en cuenta que Class sigue siendo un tipo de datos int. Por ahora, no lo convertiremos en un factor porque la red neuronal requiere toda la información numérica.
str(entr)

#Construyendo la red neuronal

#El primer paso esLlamar a la biblioteca de neuralnet
#Posteriormente se usa la función neuralnet para entrenar una red neuronal, establezca linear.output = FALSe y se opta por 10 neuronas ocultas (hidden = 10)

library(neuralnet)
nn <- neuralnet(Class ~ Image.Var + Image.Skew + Image.Curt + Entropy,data=train,hidden=10,linear.output=FALSE)

#Predicciones

#Se usa compute () para obtener predicciones usando su modelo nn en el conjunto de prueba. 
#Verificamos el encabezado de los valores predichos. Todavía son probabilidades.
pred.nn.values <- compute(nn,test[,1:4])
head(pred.nn.values$net.result)
#Aplique la función de redondeo a los valores predichos para que solo 0s y 1s sean las clases pronosticadas.
predi <- sapply(pred.nn.values$net.result,round)
head(predi)
#Use table () para crear una matriz de confusión de sus predicciones frente a los valores reales
table(predi,test$Class)


#Comparando con otro modelo (Random Forest)

#Los resultados obtenidos anteriormente son buenos....sospechosamente buenos, para verificar su rendimiento lo comparamos con otro modelo.
#El proceso para crear un modelo de Random Forest ya se ha realizado en otro proyecto, por lo que aquí únicamente se mostrarán los resultados.

library(randomForest)
df$Class <- factor(df$Class)
library(caTools)
set.seed(101)
divi = sample.split(df$Class, SplitRatio = 0.70)
entre = subset(df, split == TRUE)
test = subset(df, split == FALSE)
model <- randomForest(Class ~ Image.Var + Image.Skew + Image.Curt + Entropy,data=train)
rf.pred <- predict(model,test)
#Matriz de confusión
table(rf.pred,test$Class)

#La comparativa de los resultados en clara dando mayor ventaja a las redes neuronales

