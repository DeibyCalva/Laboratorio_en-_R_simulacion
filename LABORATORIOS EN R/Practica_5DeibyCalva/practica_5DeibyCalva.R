#                 PRACTICA 5 R


##PREGUNTA 1 Generar 20 n煤meros pseudoaleatorios 
#usando xn = 172 xn-1 (mod. 30 307), con semilla inicial x0 = 17218.

random.number = numeric(20)# generar 20 numeros
random.seed = 17218 #semilla inicial x0 = 17218.
for (j in 1:20) {
  random.seed = (172*random.seed)%%30307
  random.number[j] = random.seed/30307
}
random.number

##PREGUNTA 2  Generar 20 n煤meros pseudoaleatorios 
#usando el generador congruente multiplicativo con b = 171 y m = 32 767 .
#con una semilla inicial de 2019.

random.number = numeric(20)
random.seed = 2019 #semilla
for (j in 1:20) {
  random.seed = (171*random.seed)%%32767
  random.number[j] = random.seed/32767
}
random.number

##PREGUNTA 3  Utilice la funci贸n runif() (con set.seed(32078)) 
#para generar 10 n煤meros pseudoaleatorios a partir de

##opcion a : la distribuci贸n uniforme (0,1)
set.seed(32078)
runif(10, min = 0,max = 1)
##opcion b :  la distribuci贸n uniforme (3, 7) 
set.seed(32078)
runif(10, min = 3,max = 7)
##opcion c :  la distribuci贸n uniforme (-2, 2).
set.seed(32078)
runif(10, min = -2,max = 2)


##PREGUNTA 4   Genere 1000 variantes pseudoaleatorias 
#uniformes usando la funci贸n runif(),
#asign谩ndolas a un vector llamado U. Use set.seed(19908).

U <- numeric(1000)
set.seed(19908)
U<- runif(1000)
U
#opcion a Calcular el promedio, la varianza y la desviaci贸n est谩ndar de las cifras en U. 
###media
promedio<-sum(U)/length(U)
promedio
###Varianza
varianza<- sum(((U-promedio)^2)/length(U))
varianza
###Desviacion estandar
desv_estandar = sqrt(varianza)
desv_estandar


#opcion b Compare sus resultados con la media real, 
#la varianza y la desviaci贸n est谩ndar.
#media
promedio<-sum(U)/length(U)
promedio
mean(U)
##Varianza
varianza<- sum(((U-promedio)^2)/length(U))
varianza
var(U)
###Desviacion estandar
desv_estandar = sqrt(varianza)
desv_estandar
sd(U)

#opcion c Calcular la proporci贸n de los valores de U que son 
#inferiores a 0,6 y comparar con la probabilidad de que una 
#variable aleatoria uniforme U sea inferior a 0,6.

#proporcion
prop.table(table(U<0.6))
#probabilidad
numbers <- runif(U,min=0,max=0.6)
number<-sample(numbers, 1)
punif(number)

#opcion d Calcular el valor esperado de 1/(U + 1). 
mean(1/(U+1))

#opcion e Construir un histograma de los valores de U, y de 1/(U+1).

hist(U)

hist(1/(U+1))



##PREGUNTA 5   Simular 10 000 observaciones independientes sobre 
#una variable aleatoria distribuida uniformemente en el intervalo [3.7,5.8].

##opcion a   Calcular la media, varianza y desviaci髇 est醤dar 
#de dicha variable aleatoria uni-forma y comparar sus estimaciones 
#con los valores reales.

r<-runif(10000,min=3.7,max=5.8) 

# media
promedio<-sum(r)/length(r)
promedio
mean(r) # comparar con valor real
# Varianza
varianza<- sum(((r-promedio)^2)/length(r))
varianza
var(r)# comparar con valor real
# Desviacion estandar
desv_estandar = sqrt(varianza)
desv_estandar
sd(r)# comparar con valor real


#opcion b: Estime la probabilidad de que dicha variable aleatoria 
#sea superior a 4,0.  Comparar con el valor real. 

length(r[r>4])/length(r)
punif(4, min = 3.7, max = 5.8, lower.tail = FALSE)

## PREGUNTA 6 : Simular 10 000 valores de una variable aleatoria uniforme (0, 1), 
#U1, utilizando runif(), y simular otro conjunto de 10 000 valores de 
#una variable aleatoria uniforme (0, 1) U2. Asigna estos vectores a U1 y U2,
#respectivamente. Dado que los valores en U1 y U2 son aproximadamente independientes, 
#podemos ver U1 y U2 como variables aleatorias independientes y uniformes (0, 1).

# opcion a: Estimaci髇 E[U1 + U2]. Compare con el valor real y 
#compare con una estimaci髇 de E[U1] + E[U2]. 
u1 <- runif(10000, min = 0, max = 1)
u2 <- runif(10000, min = 0, max = 1)
mean(u1+u2)
mean(u1)+mean(u2)

# opcion b: Estimaci髇 de Var(U1 + U2) y Var(U1) + Var(U2).
#縎on iguales? Si los verdaderos valores sean iguales?.
var(u1+u2)
var(u1) + var(u2)

# opcion c: Estimaci髇 P(U1 + U2 ??? 1.5).
mean(u1+u2)
mean(u1+u2) <= 1.5

# opcion d: 
mean(sqrt(u1)+sqrt(u2))
mean(sqrt(u1)+sqrt(u2)) <= 1.5


## Pregunta 7 Supongamos que U1, U2 y U3 son variables aleatorias independientes
#y uniformes en la variable intervalo (0, 1). Utilice la simulaci髇 para estimar las siguientes cantidades: 

# opcion a 
u1 <- runif(10000, min = 0, max = 1) 
u2 <- runif(10000, min = 0, max = 1) 
u3 <- runif(10000, min = 0, max = 1) 
mean(u1+u2+u3) 

# opcion b
var(u1+u2+u3) 
var(u1) + var(u2) + var(u3) 

# opcion c
mean(sqrt(u1+u2+u3)) 

#opcion d 
mean(sqrt(u1)+sqrt(u2)+sqrt(u3)) 
mean(sqrt(u1)+sqrt(u2)+sqrt(u3)) >= 0.8 


