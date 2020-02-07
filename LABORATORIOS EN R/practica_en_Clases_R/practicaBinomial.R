#distribucion binomial

dbinom(x=4, size=6, prob =0.5)

#ejemplo 2 calcular la probabilidad de obtener como mucho 4 caras al 
#lanzar seis veces una moneda perfecta

pbinom(q=4, size=6, prob =0.5)
pbinom(4,6,0,5)

#calcular eñ valor x tal que P[X<=x] = 0.89

qbinom(0.89,6,0.5)


# calcular 10 valores pseudoaleatorios de una B(6,0.5)
rbinom(10,6,0.5)

#supongamos que el 10% de los tubos producidos por una maquina son defectuosos
#y supongamos que produce 15 tubos cada hora cada tubo es independiente de los otros
# se juzga qu elprocesoesta fuera de control cuando se producen mas de 4 tubos defectuosos en una hora concreta 
# simular el numero de tubos defectuosos por la maquinas en cada hora a lo largo de un periodo de 24 horas y determinar si el proceso esta 
# fuera de control en algun momento

tubosDefectuosos = rbinom(24,15,0.1)
tubosDefectuosos
any(tubosDefectuosos >4)
sum(tubosDefectuosos >4)
#distribucion de poisson
#dpois -->
#ppois -> funcion de distribucion
#qpois -> funcion de cuantiles 
#rpois -> genera valores aleatorios
#Uso
# dpois(x, lambda)
#ppois(q, lambda, lower.tail =  true)
#qpois(p, lambda, lower.tail = true)
#rpois(n, lambda)
#ejemplo supongams que el numero de accidentes que ocurren en una carretera
#al año tiene una distribucion de poission de media 3.7
# a) calcular la probabildadad de q en un añohaya 6 accidentes
# b) calcular la probabilidad de que en un año haya menos de 2 accidentes
# c) calcular la probabilidad de que en un año haya mas de 8 accidentes
# d) calcular el numero maximo de accidentes que se producen con probabilidad
# mayor o igual a 0.9
#simular el numero anual de accidentes que se producen en un periodo de 20 años
# a)

dpots(6,3.7)
#b
ppois(2,3.7)

#c
ppois(8,3.7, lower.tail = false)

#d
qpois(0.9,3.7)
# e
rpois(20,3.7)


#Distribucion Normal
#estandar -> media 0 y sd 1
# dnorm(x, mean = 0, sd =1)
# pnorm(q, mean =0, sd =1, lower.tail =true)
#qnorm (p, mean = 0, sd =1, lower.tail =true)
#rnorm(n, mean=0, sd=1)
datos = rnorm(100,3,2)
datos
hist(datos)
hist(datos, freq = FALSE)#freceuncia relativa
curve(dnorm(x,3,2), add = TRUE)


#Distribucion exponencial
# dexp -> funcion de densidad
# pexp -> funcion de distrubucion
# qexp -> funcion de cuantiles
# rexp -> genera valores aleatorios
# Uso
#dexp(x, rate=1, log = false)
#pexp(q, rate=1, lower.tail = true, log.p = false)
# qexp(p, rate=1, lower.tail = true, log.p = false)
# rexp(n, rate =1)
# ejemplo: suponga,os el tiempo d3e servicion en un banco se moderiza por una variable aleatorisa con distrubucin
#exponencial de razon 3 clientes por minjut. Calcular la probabiliada de en cliente sea servido 
#en menos de un minuto
#P[X<=1], X -> exp(3)

pexp(1,3)

# simualr mediante el metodo de la T I 10 valores de una
# distribucion exp(3)  usando semilla 111

set.seed(111)
r = runif(10)
datos = -(log(1-r))/3
datos

# simualr 1000 valores de una expo(3) y comparar su histo
# con la distrubucion de densidad teorica de la distribucion 

set.seed(111)
r = runif(1000)
datos = -(log(1-r))/3
hist(datos, freq = FALSE)
curve(dexp(x, 3), add = TRUE)
































