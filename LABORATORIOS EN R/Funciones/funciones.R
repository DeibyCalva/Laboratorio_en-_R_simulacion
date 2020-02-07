# funciones con vectores 
x = c(1,2,3,4,5)
sum(x)
cumsum(x)# suma acumulada (vector)
max(x)
min(x)
mean(x)#  promedio
median(x)# mediana
var(x)# varianza
sd(x)# desviacion standar

prod(x)#producto
cumprod(x)# producto acumulado (vector)
quantile(x)

x = c(2,6,3,7,9,1,4,7)
sort(x) # ordena el vector en orden creciente
rev(x) # coloca los componentes de un vector en orden inverso


x = c(1,2,3,4,5)
y = c(2,1,5,4,3)
cov(x,y) # covarianza
cor(x,y) # coeficiente de correlacion

# VECTORES DE CARACTER

colores = c("amarillo","rojo","verde")
colores
mas.colores = c(colores,"azul","negro")
mas.colores
otros.colores = c("naranja","rosa",1)
otros.colores

#VARIABLES Y OPERACIONES LOGICAS
x = 11:20
x==15
x<15
x>15
x<=15
x>=15
x!=15
sum(x == 15)
sum(x<15)
sum(x>15)
sum(x<=15)
sum(x>=15)
sum(x!=15)


#calcular la media y mediana de un vector 
# y el numero de valores que estan por debajo
# de la media y de la mediana

x = c(1,5,7,9,3,5,6,2,4,7,5,6,9,8,6,2,5,1,4)

mean(x)#  promedio
median(x)# mediana
sum(x <mean(x))# cuantos numeros estan por debajo de la media
sum(x < median(x))# cuantos numeros estan por debajo de la mediana
sum(x == median(x))

z =  1:7
z[c(TRUE,FALSE,TRUE,FALSE,TRUE)]

x = runif(10)
x
x<0.4
x[x<0.4]
x[(x<0.2) | (x>0.8)]
sum((x<0.2) | (x>0.8))
sum(x[(x<0.2) | (x>0.8)])
x[(x>0.2) & (x<0.8)]
sum((x>0.2) & (x<0.8))


 # CREACION DE FUNCIONES
# nombre_de_la_funcion = funcio(x,y...) {expresion de la FUNCION}
 media = function(x){sum(x)/length(x)}
 y = 1:100
 media(y)
 mean(y)
 #graficos em R
 
 x = c(1,1,1,1,1,2,2,3,3,3,5,6,6,7,7,7)
table(x)
barplot(table(x))
#histograma
x = runif(100)
hist(x)
#scatterplots (grafico de dispersion)
x = runif(100)
y = runif(100)
plot(x,y)

# QQ-plots
x = rnorm(1000)
y = rnorm(1000)
qqplot(x,y, main="x e y con la msima distribucion")


a = rnorm(1000, mean = 3, sd = 2)
qqplot(x,a, main="x N(0,1), a N(3,2)")

b = rt(1000, df=2)
qqplot(x,b, main="x N(0,1), a t(2)")

# OTRAS INSTRUCCIONES 
#FOR
#for(nombre_indice in vector) {comandos}

x = numeric(10)
fibonacci_12 = numeric(12)
fibonacci_12[1] = fibonacci_12[2] = 1
for (i in 3:12) {
  fibonacci_12[i] = fibonacci_12[i-2]  + fibonacci_12[i-1]
  
}
fibonacci_12


                #DISTRIBUCION UNIFORME
# en el intervalo comprendido entre "min" y "max"
# dunif --> proporciona la funcion de densidad
# punif --> proporciona la funcion  de distribucion
# qunif --> proporciona la funcion de cuantiles
# runif --< proporciona valores pseudo-aleatorios
# USO 
# dunif(x, min=0, max=1, log= false)
# pnif(q, min=0, max=1, lower.tail = true, log.p= false)
# qunif(p, min=0, max=1, lower.tail= true, log.p= false)
# runif(n, min=0, max=1)
#Argumentos
# x,q: vectores de cxuantiles
# p: vector de probabilidades
# n : numemros de observaciones 
# min, max : extremos inferiores y superior
# log. log.p : son valotres logicos; si son TRUE, las prob p
#             se dan como probabilidades log(p)
#lower.tail: es un valor logico; si es TRUE, las prob son 
#              p[X<=x], en otro caso p[X>x]


runif(10)
runif(15, min = -1, max = 2)
#semilla
set.seed(32789)
runif(5)

# INTRODUCCION A LA SIMULACION MONTECARLO

beads = rep(c("rojo","blue"), times= c(2,3))
beads
sample(beads,1)
B = 10000
eventos = replicate(B, sample(beads,1))
tab = table(eventos)
tab
prop.table(tab)







         
         
         











































