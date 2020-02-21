##1_______________________________________
#a. ModifiquelaaproximacióndeRiemannparaencontrarP{
#exponencial con promedio de 2. La función de densidad es� 0. 
#Ejecute elprograma y compare el resultado con el valor exacto obtenido usando la calculadora.#
set.seed(12)
m = 500000 #numero de rectangulo, miestras mayor se el numeri de rectangulos mayor
#Exactitud y se reduce el error
## opcion a
a = 0 #
b = 1
w = (b - a) / m   #ancho del rectangulo, valor max - valor min / entre el nuemor de rectangulos\

x=rexp(m,2)

h=exp(-x/2)/2

rieman = sum(w*h)#suma areas de rectangulos
rieman



# opcion b
standard = pnorm(2) - pnorm(0.5)
standard
m = 10000 #numeros o puntos aleatorios
a = 0.5
b = 2 #valores minimos y maximos
w = (b - a) / m # anchura
u = a + (b - a) * runif(m) #Puntos aleatorios, se multiplica 
#los valores minimos y maximos para establecer
#el rango o intervalo que sera multiplicado por 
#el valor a;eatorio arrojafo por la funcion runif 
h = dnorm(u) #Alturas calculadar utilizando la funcion de densidad
MonteC = sum(w*h)
MonteC
errorMC =standard - MonteC
errorMC


#opcion c ______________________________________
m = 100000 #numeros o puntos aleatorios
a = 0
b = 0.5 #valores minimos y maximos
w = (b - a) / m  # anchura
u = a + (b - a) * runif(m)
#u = a + (b - a) * runif(m) #Puntos aleatorios, se multiplica 
#los valores minimos y maximos para establecer
#el rango o intervalo que sera multiplicado por 
#el valor a;eatorio arrojafo por la funcion runif 
h = dnorm(u) #Alturas calculadar utilizando la funcion de densidad
MonteC = sum(pi*b)*2
MonteC




