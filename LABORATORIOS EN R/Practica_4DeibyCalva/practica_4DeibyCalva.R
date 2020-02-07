#  1 ¿Qué devolverá esta expresión condicional?

x  <-  c(1,2,-3,4)

if(all(x>0)){  print("All Postives")
} else{
  print("Not all positives")
}



#  2.	¿Cuál de las siguientes expresiones es siempre FALSA cuando 
# al menos una de las entradas de un vector lógico es VERDADERA?

all(x)
any(x)
any(!x)
all(!x) #Rspta es siempre falsa



#  3.	La función nchar le dice cuántos caracteres tiene un vector de caracteres. 
# Escriba una línea de código que asigne al objeto new_names la abreviatura del 
# estado cuando el nombre del estado tenga más de 8 caracteres.

library(dslabs)

new_names <- ifelse(nchar(murders$state)>8,#Asigna la abreviatura del estado cuando el nombre del 
murders$abb,murders$state)               #estado tiene más de 8 caracteres
new_names




#   4.	Crea una función sum_n que para cualquier valor dado, 
#digamos n, calcula la suma de los enteros de 1 a n (inclusive).
#Utilice la función para determinar la suma de números enteros de 1 a 5.000

sum_n <- function(x){
  sum(1:x)
}
# Determinar la suma de los números enteros del 1 al 5000
sum_n(5000)




# 5.	Cree una función altman_plot que tome dos argumentos, 
#x e y, y trace la diferencia contra la suma

altman_plot <- function(x,y){
  plot(x-y,x+y)
}
altman_plot(100,50)



altman_plot <- function(x,y){
  plot(x-y,x+y)
}
altman_plot(50,100)



# 6.  Después de ejecutar el código de abajo, ¿cuál es el valor de x?

x <- 3
my_func <- function(y){
  x <- 5
  y+5
}
#  y luego `print (x)` . Tendrás la respuesta
print (x)
        #No se usa x en la función, por lo tanto x=3.



# 7 	Escribe una función compute_s_n que para cualquier n dado 
#calcule la suma S_n=1^2+2+3^2+4^2+...... + n^2 
#Reporta el valor de la suma cuando n=10.

compute_s_n <- function(n){
  x<-1:n
  sum(x^2)
}
compute_s_n(10)


# 8.	Defina un vector numérico vacío s_n de tamaño 25 utilizando s_n <- vector("numérico",25)
#y almacenar en los resultados de S1,S2,...S25 usando un for-loop


s_n <-vector("numeric",25)
for (i in 1:25) {
  s_n[i] <-i
}
s_n


# 9.	Repita el ejercicio 8, pero esta vez use sapply.

s_n <-vector("numeric",25)
for (i in 1:25) {
  s_n[i]<-sapply(i,sqrt)
}
s_n


# 10.	Repite el ejercicio 8, pero esta vez usa map_dbl.

install.packages("purrr")
library(purrr)
s_n <-vector("numeric",25)
for (i in 1:25) {
  s_n[i]<-map_dbl(i,sqrt)
}
s_n


#11.	Gráfica Sn versus n. Puntos de uso definidos por n=1,...,25.

s_n <-vector("numeric",25)
for (i in 1:25) {
  s_n[i]<-map_dbl(i,sqrt)
}
n<-1:25
plot(s_n,n)

# 12.	Confirmar que la fórmula para esta suma es Sn=n(n+1)(2n+1)/6.

#  Definir la función
compute_s_n <- function(n){
  x <- 1:n
  sum(x^2)
}
# Definir el vector de n
n <- 1:25
# Definir el vector para almacenar datos
s_n <- vector("numeric", 25)
for(i in n){
  s_n[i] <- compute_s_n(i)
}
#  Verifique que s_n sea idéntico a la fórmula dada en las instrucciones
identical (s_n, n*(n+1)*(2*n+1)/6)



#FUNCTION LAPPLY

s_n <-vector("numeric",5)
for (i in 1:5) {
  s_n[i]<-lapply(i,sqrt)
}
s_n





# FUCTION TAPPLY IN R
attach(iris)
# longitud media del sépalo por especie
tapply(iris$Sepal.Length, Species, mean)


# FUCTION Mapply in R
mapply(sum, 1:4, 1:4, 1:4)


# FUCTION VAPPLY in R
vapply(1:5, sqrt, 1i)


#Ejemplo_1 replicar valores con un número específico de veces
rep(1,10)


# Ejemplo_ 2  Función replicar en R con atributo de longitud
rep(1:4,len=20)


