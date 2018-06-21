#Árboles de desición

#Para este proyecto exploraremos el uso de métodos de árbol para clasificar las escuelas como privadas o públicas en función de sus características.
#Comencemos por obtener los datos que se incluyen en la biblioteca ISLR:
https://cran.r-project.org/web/packages/ISLR/ISLR.pdf

# (Son los mismos datos encontrados en un proyecto similar que se realizó en Phyton)

# La información que contiene (en inglés) es: 
#Private A factor with levels No and Yes indicating private or public university
#Apps Number of applications received
#Accept Number of applications accepted
#Enroll Number of new students enrolled
#Top10perc Pct. new students from top 10% of H.S. class
#Top25perc Pct. new students from top 25% of H.S. class
#F.Undergrad Number of fulltime undergraduates
#P.Undergrad Number of parttime undergraduates
#Outstate Out-of-state tuition
#Room.Board Room and board costs
#Books Estimated book costs
#Personal Estimated personal spending
#PhD Pct. of faculty with Ph.D.'s
#Terminal Pct. of faculty with terminal degree
#S.F.Ratio Student/faculty ratio perc.alumni Pct. alumni who donate
#Expend Instructional expenditure per student
#Grad.Rate Graduation rate

#Obtener los datos
library(ISLR)
head(College)
df<-College

#Análisis Exploratorio
#Creamos un diagrama de dispersión de Grad.Rate versus Room.Board, coloreado por la columna Privado.
library(ggplot2)
ggplot(df,aes(Room.Board,Grad.Rate)) + geom_point(aes(color=Private))


#Creamos un histograma de estudiantes de pregrado de tiempo completo, damos el color por Privado.
ggplot(df,aes(F.Undergrad)) + geom_histogram(aes(fill=Private),color='black',bins=50)

#Crea un histograma de Grad.Rate coloreado por Privado. Hay algo extraño aquí.
ggplot(df,aes(Grad.Rate)) + geom_histogram(aes(fill=Private),color='black',bins=50)

#¿Qué universidad tenía una tasa de graduación superior al 100%?
subset(df,Grad.Rate > 100)

#Cambiar la tasa de graduación de esa universidad al 100%
df['Cazenovia College','Grad.Rate'] <- 100

#Divida sus datos en los conjuntos de entrenamiento y prueba 70/30.
library(caTools)

set.seed(101) 

muestra = muestra.split(df$Private, SplitRatio = .70)
entr = subset(df, muestra == TRUE)
test = subset(df, muestra == FALSE)

#Árbol de desción
library(rpart)
tree <- rpart(Private ~.,method='class',data = entr)
tree.preds <- predict(tree,test)
head(tree.preds)
tree.preds <- as.data.frame(tree.preds)
# Hay varias maneras de hacer esto
juntamos <- function(x){
  if (x>=0.5){
    return('Si')
  }else{
    return("No")
  }
}
tree.preds$Private <- sapply(tree.preds$Yes,juntamos)
head(tree.preds)
table(tree.preds$Private,test$Private)
library(rpart.plot)
prp(tree)

#Predicciones utilizando random forest
library(randomForest)
rf.model <- randomForest(Private ~ . , data = train,importance = TRUE)
rf.model$confusion
rf.model$importance
p <- predict(rf.model,test)
table(p,test$Private)

#Predicciones: podemos observar que los mejores resultados los da el random forest (podemos deducir esto con la matriz de confusión)

  