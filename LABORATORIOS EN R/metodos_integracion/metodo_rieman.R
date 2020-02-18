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



####Trapezoid rule (Integration)#####
trapezoid = function(ftn, a, b, n = 100){
  #integral numerica de la funcion (ftn) desde a hata b usando la regla
  # del trapecio con n subdivisiones 
  #ftn  es una funcion de una variablex;
  #asumir que a < b y n es un entero positivo
  # h es el anncho de cad intervalo
  h = (b -a )/n
  # x.vec es un vector de a hasta b que tambien marca el comkienzo y el final de cada subintervalo
  x.vec = seq(a,b, by = h)
  # Dada la ftn es f(x), f.ve valores de y para cada x-sub-i
  f.vec = sapply(x.vec, ftn)
  # Implenetar la regla del trapecio sumando todas las areas
  T = h*(f.vec[1]/2 + sum(f.vec[2] + f.vec[n+1]/2))
  return(T)
}
#### estimar la integral enytre 0 y 1 de 4x^3 dx = 1


ftn6 = function(x) return(4* x^3 )

trapezoid(ftn6, 0, 1, n=20)
trapezoid(ftn6, 0, 1, n=40)
trapezoid(ftn6, 0, 1, n=60)
trapezoid(ftn6, 0, 1, n=80)
trapezoid(ftn6, 0, 1, n=100)



### Integracion utilizando la Regla de simpson ######

#  Divide bel intervalo [a , b] en n subintervalos, donde n 
# es par, luego para cada pares de intervalos consecutivos, se calcula(aproxima)
# el area de f(x) a travez de una parabola (polinomio de grado 2) 
# Sea u < v < w van estar separados por tres puntos h. Para los puntod en eje x que pertenecen al intervalo
# [u,w] queremos aproximar f(x) a travez de una parabola que pasa que pasa a travez de los puntos (u, f(u)),
# (v, f(v)), y (w, f(w)). Para estos punto hay exactamente una parabola representada por
# p(x) y se lo obtiene mediante la siguiente formula:

# p(x)= f(u)()()/()()


simpson_n = function(ftn, a,b, n = 100){
  # integral numerica de ftn, desde a hasta b
  #usando la regla de simpson con n subdivisiones
  #
  # ftn es un a funcion de uan sola variable 
  # asumimos que a<b y n es un entero positivo par
  # Asegurar que n es par(4 es el numero minimo de intervalos para la regla de simpson)
  # 
  
  n = max(c(2*(n%/%2),4))
  # h es el ancho de cada intervalo
  h = (b -a)/ n
  # x-sub-i impar
  x.vec1 = seq(a+h, b-h, by = 2*h)
  # pares 
  x.vec2 = seq(a+2*h, b-2*h, by =2*h)
  
  # calculo de los valores para y
  f.vec1 = sapply(x.vec1, ftn)
  f.vec2 = sapply(x.vec2, ftn)
  S = h/3*(ftn(a) + 4*sum(f.vec1) + 2*sum(f.vec2) + ftn(b))
  return(S)
}

ftn6 = function(x) 
  return(4 * x^3)
simpson_n(ftn6, 0, 1, 20)



