----------------------------------------------------------------
#DEA (Analisis exploratorio de datos) Datos Continuos
----------------------------------------------------------------

library(mlbench)
data(Glass)
str(Glass)
dim(Glass)


#Histograma
par(mfrow=c(2,5))#2 filas 5 columnas
for(i in 1:9){
  hist(Glass[,i],main = names(Glass[i]))
}

#Resumen descriptivo de las variables predictoras
summary(Glass[,1:9])
#Resumen con mayor detalle
library(Hmisc)
describe(Glass)


#Relación de las variables
library(psych)
describe(Glass)
#correlaciones de pearson (variables Continuas)
corr.test(Glass[,1:9])
cor.plot(cor(Glass[,1:9]))
# Prueba de Esfericidad de Bartlett
#install.packages('rela')
library(rela)
cortest.bartlett(cor(Glass[,1:9]),n=dim(Glass)[1])
#h0:no hay correlación>0.05
#h1:si hay correlación


----------------------------------------------------------------
#OUTLIER  
----------------------------------------------------------------

#Grafico Boxplot
par(mfrow=c(2,5))
for(i in 1:9){
  boxplot(Glass[i],names=names(Glass[i]))
}
library(mvoutlier)
aq.plot(Glass[Glass[,10]==1,1:6],alpha=0.01)

----------------------------------------------------------------
#transformación
----------------------------------------------------------------
#Efectos de valores extremos se va a menorar con las transformación sigmoidal 
#o softmax
#Sigmoidal
library(DMwR)
sigGlass<-Glass
sigGlass[,-10]<-signorm(Glass[,-10])
summary(sigGlass)

#Haciendo plots para ver el efecto de la normalizacion
par(mfrow=c(1,2))
plot(sort(Glass[,1]))
plot(sort(sigGlass[,1]))

#Softmax (valores Positivos)
library(DMwR)
softGlass<-Glass
softGlass[,-10]<-SoftMax(Glass[,-10],lambda=2*pi)
summary(softGlass)

#Gr?fico de comparaci?n
par(mfrow=c(1,3))
boxplot(Glass[,1:6],main="Glass")
boxplot(sigGlass[,1:6],main="sigGlass")
boxplot(softGlass[,1:6],main="softGlass")


----------------------------------------------------------------
#DEA (datos Categoricos)
----------------------------------------------------------------
library(mlbench)
data(Soybean)
dim(Soybean)
str(Soybean)
names(Soybean)


# Tablas de Frecuencia
# Frecuencias Absolutas
table(Soybean[,1])
# Frecuencias Relativas
table(DMark$Age)/length(DMark$Age)
prop.table(table(Soybean[,1]))
cbind(prop.table(Soybean[,1])) 

# Gr?fico de Barras
par(mfrow=c(4,10))
for(i in 1:36){
barplot(table(Soybean[,i]), main=names(Soybean[,i]))
}
#Grafico de sectores Circulares
pie(table(Soybean[3]))

#Grafico de varas , Cuantitativos Discretos
plot(table(Soybean[3]), type="h")

summary(Soybean)

----------------------------------------------------------------
#DATOS FALTANTES  
----------------------------------------------------------------
#Para ver que columnas tienen valores perdidos
which(colSums(is.na(Soybean))!=0)
dim(Soybean)
#Para ver que filas tienen valores perdidos
rmiss=which(rowSums(is.na(Soybean))!=0,arr.ind=T)
rmiss
#Para ver el porcentaje de filas con valores perdidos
length(rmiss)*100/dim(Soybean)[1]

#Para ver el porcentaje de valores perdidos en las columnas
colmiss=c(2:36)
per.miss.col=100*colSums(is.na(Soybean[,colmiss]))/dim(Soybean)[1]
per.miss.col

library(VIM)
names(Soybean)
a=aggr(Soybean,numbers=T)
a
help(aggr)
summary(a)
#marginplot(Soybean[,c(1,2)])

matrixplot(Soybean)

# Eliminar casos
Soybean.cl=na.omit(Soybean)
dim(Soybean.cl)
dim(Soybean)
# Imputaci?n

library(DMwR)
Soybean2<-Soybean
#tIPO DE DATOS TRANFORMAR
#for(h in c(2,4,5,6,7,8,9,13,14)){
#  census[,h]<-as.factor(census[,h])
#}
str(Soybean)

library(DMwR)
Soybean.c<-centralImputation(Soybean)
Soybean.d<-initialise(Soybean,method="median")
aggr(Soybean.c)
# K-Vecinos m?s cercanos
Soybean.k<-knnImputation(Soybean)
aggr(Soybean.k)
# IRMI (METODO BASADO EN IMPUTACIÓN ROBUSTA)
imputed.Soybean <- irmi(Soybean)
summary(imputed.Soybean)
