#K Means Clustering - Clasificador de vinos
#Por lo general, cuando se trata de un problema de aprendizaje no supervisado, es dif�cil obtener una buena medida de qu� tan bien funcion� el modelo. Para este proyecto, se utilizar�n los datos del archivo UCI basado en vinos tintos y blancos (este es un conjunto de datos muy com�nmente utilizado en ML).
#Se agrupar�n, (se har�n "cl�sters") de los vinos de acuerdo a sus caracter�sticas

#Obtener los datos

#Usamos read.csv para abrir ambos conjuntos de datos y configurarlos como df1 y df2. Atenci�n a lo que es el separador (sep).

df1 <- read.csv('winequality-red.csv',sep=';')
df2 <- read.csv('winequality-white.csv',sep=';')

#Ahora se pone una columna de etiqueta ("label") a df1 y df2 indicando una etiqueta 'roja' o 'blanca'.

# Using sapply with anon functions
df1$et <- sapply(df1$pH,function(x){'roja'})
df2$et <- sapply(df2$pH,function(x){'blanca'})

#Se ve el encabezado de df1 y df2.

head(df1)

#Se combina df1 y df2 en un solo marco de datos llamado vino.

vino <- rbind(df1,df2)
str(vino)

# An�lisis Exploratorio 
# Primero creamos un histograma de az�car residual a partir de los datos del vino. (Color por vinos tintos y blancos).

library(ggplot2)
pl <- ggplot(vino,aes(x=residual.sugar)) + geom_histogram(aes(fill=label),color='black',bins=50)
# Opcional, tratamiento de colores
pl + scale_fill_manual(values = c('#ae4554','#faf7ea')) + theme_bw()

# Se crea un histograma de c�trico-�cido a partir de los datos del vino. (Color por vinos tintos y blancos).

pl <- ggplot(vino,aes(x=citric.acid)) + geom_histogram(aes(fill=label),color='black',bins=50)
pl + scale_fill_manual(values = c('#ae4554','#faf7ea')) + theme_bw()

#Se crea un Histograma de alcohol a partir de los datos del vino. 

pl <- ggplot(vino,aes(x=alcohol)) + geom_histogram(aes(fill=label),color='black',bins=50)
pl + scale_fill_manual(values = c('#ae4554','#faf7ea')) + theme_bw()

#Ahora se hace un diagrama de dispersi�n.(sugar v citric.acid).

pl <- ggplot(vino,aes(x=citric.acid,y=residual.sugar)) + geom_point(aes(color=label),alpha=0.2)
pl + scale_color_manual(values = c('#ae4554','#faf7ea')) +theme_dark()

#Ahora un diagrama de dispersi�n de acidez vol�til versus az�car residual.

pl <- ggplot(vino,aes(x=volatile.acidity,y=residual.sugar)) + geom_point(aes(color=label),alpha=0.2)
pl + scale_color_manual(values = c('#ae4554','#faf7ea')) +theme_dark()

#Se dan los datos del vino sin la etiqueta y se llama clus.data
clus.data <- vino[,1:12]
head(clus.data)

#Construicci�n los Clusters

#Se llama a la funci�n kmeans en clus.data y asignamos los resultados a wine.cluster.

vino.cluster <- kmeans(vino[1:12],2)

#Impr de los Cluster Cluster de vino.cluster y posteriormente se explora la informaci�n.

print(vino.cluster$centers)

#Evaluar los clusters

#OJO: Por lo general, no podemos etiquetar los datos con KMeans.
#Se usa la funci�n table () para comparar los resultados de su cl�ster con los resultados reales. �Qu� es m�s f�cil para agrupar correctamente, vinos tintos o blancos?

table(vino$label,wine.cluster$cluster)

#Obviamente el rojo es m�s f�cil de agrupar, lo cual tiene sentido dadas dadas las visualizaciones previas. Parece que hay mucho ruido con los vinos blancos, esto tambi�n podr�a deberse a que los vinos "Rose" se categorizan como vino blanco, al tiempo que conservan las cualidades de un vino tinto. En general, esto tiene sentido, ya que el vino es b�sicamente jugo de uva fermentado y las medidas qu�micas que se estaban en los datos pueden no estar bien correlacionadas con si el vino es rojo o blanco.
#Es importante tener en cuenta que K-Means solo da los clusters, no puede decir directamente cu�les deber�an ser las etiquetas, o incluso cu�ntos cl�steres debe haber, Aqu� se corre la suerte de saber que esper�bamos dos tipos de cl�steres. vino. Aqu� es donde el conocimiento del dominio realmente entra en juego.




