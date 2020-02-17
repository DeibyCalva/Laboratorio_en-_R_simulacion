### Metodos de Integracion ####

### Integracion de Riemam ####
# P{-1 < z <=1} = phi(-1) = 68.2%
# J = P{a<z<= b} = phi(b) - phi(a) = integrate a, b de la funcion (z)dz, donde a<b 
# de la funcion (z) = 1/sqrt(2*??)* e^(-(z^2)/2)

# P{-1 < z <= 1}

pnorm(1) - pnorm(-1)


### Aproximacion de Riemann ##
m = 500000 # numero de rectangulos
a=-1 #valor minimo
b=1 #valor maximo
w=(b-a)/m#para calcular el ancho de cada rectangulo
g=seq(a+ w/2, b-w/2,length=m) # m punto medio de cada rectangulo 
g

const = 1/sqrt(2*pi) #constante z
h = const * exp(-g^2 / 2) # vector de m alturas
h
sum(w*h)#suma areas de rectangulos




#Para a=0 y b=1, con los valores de m=10,20,50 y 500 cual de estos
# valoees nos da el resutado con 5 decimales de exactitud

m = 20 # numero de rectangulos
a= 0 #valor minimo
b=1 #valor maximo
w=(b-a)/m#para calcular el ancho de cada rectangulo
g=seq(a+ w/2, b-w/2,length=m) # m punto medio de cada rectangulo 
g

const = 1/sqrt(2*pi) #constante z
h = const * exp(-g^2 / 2) # vector de m alturas
h
sum(w*h)#suma areas de rectangulos



# Para m = 5000 nmodifique el programa para encontar P{1.2 < z <= 2.5} Compare su respuesta con 
# el valor exacto usando pnorm

pnorm(2.5) - pnorm(1.2)
m = 500 # numero de rectangulos
a= 1.2 #valor minimo
b=2.5 #valor maximo
w=(b-a)/m#para calcular el ancho de cada rectangulo
g=seq(a+ w/2, b-w/2,length=m) # m punto medio de cada rectangulo 
#g

const = 1/sqrt(2*pi) #constante z
h = const * exp(-g^2 / 2) # vector de m alturas
#h
sum(w*h)#suma areas de rectangulos



##### Monte Carlo Integration####
## selecionar u puntos (0,1)
set.seed(12) # semilla para q los valores no varien
m=500000
a=0 
b=1
w=(b-a)/m 
u = a +(b-a) *runif(m) #Vector de m puntos aleatorios   runif -->siguen una dist uniforme
h = dnorm(u) #alturas calculadas de cada uno de los puntos usando la funcion de densidad
sum(w*h)



##### Metodo de aceptacion y Rechazo#####
### The acceptance-Rejection Method ###

set.seed(12) 
m=500000
u = runif(m, 0, 1) #emula coordenada x
h = runif(m, 0, 0.4) #emula coordenada y
frac.acc = mean(h < dnorm(u))#calculando funcion de densidad para numeros en x y todos los q sean menores son rechaxo
0.4*frac.acc #multiplicamos la altura por los q han sido aceptados-->area de un solo rectanguo



###  Random Sampling Method

set.seed(2020)
m = 500000
a=0
b=1
z = rnorm(m)
mean(z > a & z <= b)




















