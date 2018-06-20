#Proyecto de regresión lineal (Linear Regression)

#Para este proyecto, se realizará el desafío Kaggle de Compras de bicicleta; El objetivo principal de este proyecto es hacer un Análisis de datos exploratorios y comenzar a comprender que a veces ciertos modelos no son una buena opción para un conjunto de datos. En este caso, Se verá la Regresión lineal puede no ser la mejor opción dados nuestros datos.

#Obtener los datos
#Se pueden descargar los datos o simplemente usar el csv proporcionado en el repositorio. Los datos tienen las siguientes características:
  
#  datetime - fecha horaria + marca de tiempo
#  season (temporada 1) - 1 = primavera, 2 = verano, 3 = otoño, 4 = invierno
#  vacations (vacaciones) - si el día se considera festivo
#  workingday (jornada de trabajo): si el día no es un fin de semana ni vacaciones
#  weather (clima) - 
#1: Despejado, Pocas nubes, Parcialmente nublado, Parcialmente nublado
#2: Niebla + Nublado, Niebla + Nubes fragmentadas, Niebla + Pocas nubes, Niebla
#3: Nieve ligera, lluvia ligera + tormenta eléctrica + nubes dispersas, lluvia ligera + nubes dispersas
#4: lluvia pesada + paletas de hielo + tormenta + niebla, nieve + niebla
#temperature - temperatura en grados Celsius
#atemp - temperatura subjetiva en grados Celsius
#humidity (humedad) - humedad relativa
#windspeed (velocidad del viento) - velocidad del viento
#casual: número de usuarios no registrados iniciados
#registered (registrado): número de alquileres de usuarios registrados iniciados
#count (recuento) - número de alquileres totales
#Se lee primero el archivo bikeshare.csv y se guarda en un dataframe llamado bici.

bici <- read.csv('C:/Users/Mario/Desktop/bikeshare.csv')

#Revisamos primeros valores del dataframe

head(bici)

#¿Cuál es el objetivo que se intenta predecir? El enlace de Kaggle de arriba da una pista.

# Count es lo que se intenta predecir

#Análisis exploratorio de datos

#Se crea un diagrama de dispersión del recuento frente a la temperatura. Hay que poner un buen valor alfa.

library(ggplot2)
ggplot(bici,aes(temp,count)) + geom_point(alpha=0.2, aes(color=temp)) + theme_bw()

# Conteo de lote versus fecha y hora como un diagrama de dispersión con un gradiente de color basado en la temperatura. OJO: Se Deberá convertir la columna de fecha y hora en POSIXct antes de trazar.

bici$datetime <- as.POSIXct(bike$datetime)
ggplot(bici,aes(datetime,count)) + geom_point(aes(color=temp),alpha=0.5)  + scale_color_continuous(low='#55D8CE',high='#FF6E2E') +theme_bw()

# Es de esperar dos cosas: una estacionalidad o "repetición" de los datos, para el invierno y el verano. Además, los recuentos de alquiler de bicicletas están aumentando en general. Esto puede presentar un problema con el uso de un modelo de regresión lineal si los datos no son lineales. Se hará una breve descripción general de los pros y los contras de la Regresión lineal:
  
#  Pros:
  
#  Simple de explicar
#Altamente interpretable
#El entrenamiento y la predicción del modelo son rápidos
#No se requiere ajuste (excluyendo la regularización)
#Las características no necesitan escala
#Puede funcionar bien con un pequeño número de observaciones
#Bien entendido
#Contras:
  
#  Asume una relación lineal entre las características y la respuesta
#El rendimiento (generalmente) no es competitivo con los mejores métodos de aprendizaje supervisado debido al alto sesgo
#No se puede aprender automáticamente las interacciones de características

#¿Cuál es la correlación entre la temperatura y el recuento?

cor(bici[,c('temp','count')])

#Se explorarán los datos de la temporada. Se crea un diagrama de caja, con el eje y que indica el recuento y el eje x comience un cuadro para cada estación.

ggplot(bike,aes(factor(season),count)) + geom_boxplot(aes(color=factor(season))) +theme_bw()

#Lo que dice esto:
  
#  Una línea no puede capturar una relación no lineal.
#Hay más alquileres en invierno que en primavera
#Estos problemas son debido al crecimiento del recuento de alquileres, ¡esto no se debe a la temporada real!
  
#  Ingeniería o ajuste de funciones
#Muchas veces se necesitará usar el conocimiento y la experiencia del dominio para diseñar y crear nuevas funciones. Se pueden ingeniar algunas características nuevas de la columna de fecha y hora.

# Se Crea una columna de "hora" que tome la hora de la columna de fecha y hora. Probablemente es necesaria alguna función a toda la columna de fecha y hora y reasignarla.

bici$hour <- sapply(bici$datetime,function(x){format(x,"%H")})
head(bici)

#Ahora se crea una gráfica de dispersión de recuento en función de la hora, con una escala de colores basada en la temperatura. Solo datos de bicicleta donde workingday == 1.

#  Usando la capa adicional: scale_color_gradientn (colors = c ('color1', color2, etc.)) donde el argumento de colores es un gradiente vectorial de colores que elija, no solo alto y bajo.
# Usando también la position = position_jitter (w = 1, h = 0) dentro de geom_point () y mira lo que hace.

library(dplyr)
pl <- ggplot(filter(bici,workingday==1),aes(hour,count)) 
pl <- pl + geom_point(position=position_jitter(w=1, h=0),aes(color=temp),alpha=0.5)
pl <- pl + scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))
pl + theme_bw()

#Ahora se crea la misma gráfica para días no laborables:

pl <- ggplot(filter(bike,workingday==0),aes(hour,count)) 
pl <- pl + geom_point(position=position_jitter(w=1, h=0),aes(color=temp),alpha=0.8)
pl <- pl + scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))
pl + theme_bw()

# Se puede notar que los días hábiles tienen una actividad máxima durante la mañana (~ 8am) y justo después de que el trabajo sale (~ 5pm), con actividad durante el almuerzo. Mientras que los días no laborables tienen un aumento constante y una caída para la tarde

# Siguiendo con la construcción de un modelo, se empieza por mirar una sola característica.

#El modelo
#Usando lm () para construir un modelo que predice el recuento basado únicamente en la característica temporal, se le da el nombre temp.modeltemp.model <- lm(count~temp,bici)

#Obteniendo el resumen del temp.model

summary(temp.model)

#Se debe de haber obtenido 6.0462 como intercepto y 9.17 como coeficiente de temperatura. ¿Cómo puedes interpretar estos valores? 

#Interpretando el intercepto (??0):
#  Es el valor de y cuando x = 0.
#Por lo tanto, es el número estimado de alquileres cuando la temperatura es de 0 grados Celsius.
#Nota: no siempre tiene sentido interpretar el intercepto.
#Interpretando el coeficiente "temp" (??1):
#  Es el cambio en y dividido por el cambio en x, o la "pendiente".
#Por lo tanto, un aumento de la temperatura de 1 grado Celsius se asocia con un aumento en el alquiler de 9.17 bicicletas.
#Esta no es una declaración de causalidad.
#??1 sería negativo si un aumento en la temperatura se asocia con una disminución en los alquileres.
#¿Cuántos alquileres de bicicletas podríamos predecir si la temperatura fuera de 25 grados Celsius? 
  
#  Usando los valores que acabamos de obtener arriba
#Usando la función de predicción ()
#Se debería obtener alrededor de 235.3 bicicletas.

# M 1
6.0462 + 9.17*25

# M 2
temp.test <- data.frame(temp=c(25))
predict(temp.model,temp.test)

#Use sapply () y as.numeric para cambiar la columna de la hora a una columna de valores numéricos.

bike$hour <- sapply(bike$hour,as.numeric)

#Finalmente se hace un modelo que intente predecir el conteo basado en las siguientes características. Hay una manera de no tener que pasar / escribir todas estas variables en la función lm (). Sugerencia: StackOverflow o Google pueden ser más rápidos que la documentación.

#temporada
#fiesta
#día de trabajo
#clima
#temperatura
#humedad
#velocidad del viento
#hora (factor)

model <- lm(count ~ . -casual - registered -datetime -atemp,bike )

summary(model)

#¿El modelo se desempeñó bien en los datos de entrenamiento? ¿Es conveniente usar un Modelo Lineal en esta información?
  
#  Un modelo lineal como el que fue elegido que usa OLS no podrá tener en cuenta la repetición de nuestros datos, y se verá afectado por el crecimiento de nuestro conjunto de datos, atribuyéndolo accidentalmente a la temporada de invierno, en lugar de darse cuenta de su justo la demanda global crece! 

# Este tipo de modelo no funciona bien dados los datos de series temporales y estacionales. Es necesario un modelo que pueda dar cuenta de este tipo de tendencia, 





