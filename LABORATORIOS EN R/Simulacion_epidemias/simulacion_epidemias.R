######### CASO DE ESTUDIO: EPIDEMIAS #############

#### Modelo SIR (Susceptibles, Infectados, Removerlos)#######
# Susceptible: Todavia no tiene la enfermedad
# Infectados: Actualmente tiene la enfermedad
# Removed: Han tenido la enfermedad  y se han recuperado y ahora son inmunes o han muerto
# en el tiempo t, se tiene un numero de S, I. R:
# S(t) = numero susceptible en eñ timepo t;
# I(t) = numero de infectados en el tiempo t;
# R(t) = numero de recuperados en el tiempo t;

# En cada tiempo t, cada  I(infectado) tiene:
# 1. probabilidad a de infectar a un susceptible; y
# 2. probabiñlidad b de ser R(Se recupere o muera)

# En el tiempo 0 (inicio)
# S(0) = N;  tamaño de la pobacion
# I(0) = 1; 
# R(0) = 0; 
# Total de la poblacion N +1:
# S(t) + I(t) + R(t) = N + 1 + 0 para todo t;


# Reglas
# Parac cad tiempo t, la probabilidad de que un susceptible
# permaneza sin la enfermedad es : (1-a)^I(t)

# Si el susceptible no se infecta, es decir:
# S(t+1) ~ binom(S(t), (1-a)^I(t))

# Ya que cada infecatdo tiene la probabilidad de ser removido
# es decir: R(t+1) ~ R(t) + binom(I(t), b)


# I(t+1) = N + 1 - R(t+1) - S(t+1)  regla 4

SIRsim = function(a, b, N, T){
  # T= numero de periodeos de timep´p para hacer nuestra simulacion 
  # a es la tasa de infeccioln
  # b es la tasa de ser removidos
  # N cantidad inicial de susceptibles, 1 (es la persona que esta infectada)
  # T tamaño de la simulacion
  # T = 100
  S = rep(0, T+1)
  I = rep(0, T+1)
  R = rep(0, T+1)
  S[1] = N
  I[1] = 1
  R[1] = 0
  for(i in 1:T){
    # de la regla 2
    S[i+1] = rbinom(1, S[i], (1-a)^I[i])
    # de la regla 3
    R[i+1] = R[i] + rbinom(1, I[i], b)
    # de la regla 4
    I[i+1] = N + 1 - R[i+1] - S[i+1]
    
  }
  return(matrix(c(S, I, R), ncol = 3))
  
}


#
N = 1000# poblacion
T = 100 # periodo de timepo
a = 0.0005 # prob de infecion
b = 0.1 # prob de removido
(N*a)
b

Z =  SIRsim(a, b, N, T)
colnames(Z) = c("S", "I", "R")
Z

require(gridExtra)
plot1 <- ggplot(Z, aes(x = 1:101, y = S)) + geom_line()
plot2 <- ggplot(Z, aes(x = 1:101, y = I)) + geom_line()
plot3 <- ggplot(Z, aes(x = 1:101, y = R)) + geom_line()
grid.arrange(plot1, plot2, plot3, nrowv=3)

#library(tidyverse)
#Z = data.frame(Z)
#ggplot(Z, aes(x = 1:101, y = S)) + geom_line()

# Vamos a estimar como afecta ell valor de a y b a una epidemia 
# es decir, estimaremos E S[t] para diferentes valores
# de a y b, a-> [0.0001, 0.001] y b ->[0.1, 0.5]
# Luego graficar los resultados en un grafico 3-D

SIR = function(a, b, N, T){
  # simula el modelo SIR desde un tiempo 0 a T
  # devuelve el numero de susceptibles, infectados
  # y removidos en el tiempo T
  S = N
  I = 1
  R = 0
  for (i in 1:T){
    S = rbinom(1, S,(1-a)^I)
    R = R + rbinom(1, I, b)
    I = N +1 -S -R
  }
  return(c(S, I, R))
}

N = 1000
T = 100
# valores multiples para a y b
a = seq(0.0001, 0.001, by = 0.0001) # 10 valores
b = seq(0.1, 0.5, by= 0.05) # 10 valores
a;b;N;T

SIR(a,b,N, T)

#tamaño de la muestra de 400
n.reps = 400
# archivo donde se almacena los resultados
f.name = "SIR_grid.dat"
# estimar E S[T] para cada combinacion de a y b

write(c("a", "b", "S_T"), file = f.name, ncolumns = 3)
for (i in 1:length(a)){
  for (j in 1:length(b)){
    S.sum = 0
    for (k in 1:n.reps){
      S.sum = S.sum + SIR(a[i], b[j], N, T)[1]
      
    }
    write(c(a[i], b[i], S.sum/n.reps),
          file = f.name,
          ncolumns = 3, append = TRUE)
  }
  
}

g = read.table(f.name, header = TRUE)
library(lattice)
print(wireframe(S_T ~ a*b, data = na.omit(g), 
                scales = list(arrows= FALSE),
                aspect = c(.5, 1), drape = TRUE,
                xlab = "a", ylab = "b",
                zlab = "E S[T]"))


#Se puede observar lo siguiente:
# Na = B. es el numero esperado de nuevos infectados en el tiempo 1 y B es 
# el numero esperado de infectados quienes han sido removidos en tiempo 1
#

# En etapas iniciales de la epidemia:
# Cuando Na > B ----> GRAN EPIDEMIA. Implica que el numero esperado de nuevos infectados
# en el tiempo 1 es mayor que el numero esperado de infectados que han sido removidos
# en el tiempo 1

# Cuando Na <= B ----> REDUCCION DE LA EPIDEMIA
# 


##### DISTRIBUCION EXPONENCIAL Y POISSON #####
 # dos cajeros en un banco atienden a clientes desde una sola cola el tiempo que le tome a un cajero atender
# un cliente selecionado al azar se destribuye exponencialmente con elñ valoir de EXP(1/5)
# para que su tiempo promedio se servicio sea de 5 min. Los otros tiempos de serrvicio del cajero mas experimentado se destribuye
# como EXP(1/4), co un promedio de 4 min.
# loss cajeros trabajan independientemente.

# ESCENARIO 1: Ambos cajeros estan ocupados y eres el proximo en ser atendido
# cual es la probabilidad de que tome mas de 5 min antes ser atendido

# sea X1 y X2 los tiempos de espera hasta que los cajeres
# estenj llibres para atender nuevos clientes
#  El tiempo hasta que uno de ellos comience me atienda V=min(X1,X2) 


m = 100000
lambda1=1/5 # taza que se demora el primer cajero
lambda2=1/4
x1 = rexp(m,lambda1)
x2 = rexp(m,lambda2)
v = pmin(x1,x2)# permite obtener los minimos de estos vectpores
hist(v,probability = T)
mean(v > 5) #se van a demorar mas de 5 minutos
#P{v > 5} P{X1> 5, X2 > 5} = e^(-(1/5)*5) e^(-(1/4)*5) = e^(-9/4)



# ESCENARIO 2: Tu necesitas ser atendido por ambos cajeros en secuencia.
#  Afortunadamente, no es un momento muy ocupado y no necesita esperar
#  para comenzar a ser atendido por cualquiera de los dos cajeros. Cual
#  es la probabilidad de que le lleve mas de 5 minutos terminar con el
#  servicio Con X1 + X2 distribuidos como el escenario 1, el tiempo que le lleva  terminar
# T = X1 + X2 ==>P{T>5}

m = 100000
lambda1 = 1/5
lambda2 = 1/4
x1 = rexp(m, lambda1)
x2 = rexp(m, lambda2)
t = x1 + x2
mean(t>5)
hist(t,probability = T)

#### REDUNDANCIA PARALELA EN COMUNICACIONES SATELITALES###
# Una computadora a bordo, controla las f funciones cruciales
# de un satelite de comunicaciones, fallara si su cpu esta 
# desactivado por la radiacion cosmica
# la confiabilidad de la cpu es importanye porque no es
# factible preparar un satelite deshabilitado. Supnga que el nivel de 
# radiacion es tal que los eventos fatales suceden a una cpu de ese tipo
# Sucede en promedio una vez cada 4 años ---> lambda = 1/4 
# la vida util aleatoria de esa cpu es x ~ EXP(1/4)
# Funcion de probabilidad de confiabilidad es 
# Rx(t) = P{X>t} = 1-Fx(t) = e^-t/4

# para mayor confiabilidad , supongamos que conectamos 5 cpu en paralelo
# entonces la vida util del sistema de cpu resultante es 
# W = max(x1, x2 ,x3, x4,x5), donde Xi se destribuyen como
# EXP(1/4). Queremos evaluar E(W)


m=100000
n=5
lambda=1/4# tiempo de vida promdeio de los chips
x = rexp(m*n,lambda) #porque son 5 iteraciones se mltiplica 100 mil por cinco
DTA = matrix(x,nrow=m) #cada fila representa a un chp en el sistema
DTA
#debemos entontrar los valores maximos para cada uno de estos valores simualdos de los chips
w=apply(DTA, 1, max) # da los tiempos de vida de m sistemas
mean(w)#el valor esperado para w, es decir el promedio
mean(w>5)#prob que el sistema pueda vivir mas de 5 años
#para el percentil
quantile(w ,0.9)# 90 percentil 90%























































































































