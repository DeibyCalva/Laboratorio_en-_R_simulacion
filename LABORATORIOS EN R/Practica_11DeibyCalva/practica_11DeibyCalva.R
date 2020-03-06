########   PRACTICA 11 #########
# opcion a
set.seed(1212) 
m = 100000 #numero de experimentos
lamb1= 1/5 #probabilidad de ser atendido en 5 minutos
lamb2= 1/4 #probabilidad de ser atendido en 4 minutos
x1 = rexp(m,lamb1)
x2 = rexp(m,lamb2)
v = pmin(x1,x2) #retorna los valores mínimo entre dos vectores
mean(x2==v) #probabilidad de ser atendido por una cajera

5/9

#opcion b
set.seed(1212)
m = 100000 #numero de experimentos
lamb1= 1/5 #probabilidad de ser atendido en 5 minutos
lamb2= 1/4 #probabilidad de ser atendido en 4 minutos
x1 = rexp(m,lamb1)
x2 = rexp(m,lamb2)
v = pmin(x1,x2) #retorna los valores mínimo entre dos vectores
mean(v)

20/9

#opcion c
set.seed(1212) 
m = 100000 #numero de experimentos
lamb1= 1/5 #probabilidad de ser atendido en 5 minutos
x1 = rexp(m,lamb1)
v = pmin(x1) #retorna los valores mínimo entre dos vectores
mean(v>5)

