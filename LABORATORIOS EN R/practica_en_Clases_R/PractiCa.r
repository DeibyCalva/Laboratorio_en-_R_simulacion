set.seed(2019)

#----1
barajas= numeric(40) #vector vacio de tamaño 40
cartas=c("oro.1", "oro.2", "oro.3","oro.4", "oro.5", "oro.6", "oro.7","oro.10","oro.11","oro.12",
         "copas.1", "copas.2", "copas.3", "copas.4", "copas.5", "copas.6", "copas.7", "copas.10", "copas.11", "copas.12",
         "espadas.1", "espadas.2", "espadas.3", "espadas.4", "espadas.5", "espadas.6", "espadas.7", "espadas.10", "espadas.11", "espadas.12",
         "bastos.1", "bastos.2", "bastos.3", "bastos.4", "bastos.5", "bastos.6", "bastos.7", "bastos.10", "bastos.11", "bastos.12")


#names(vectorBase)= vectorConbinar
names(barajas)=cartas
names(barajas[7])
names(barajas[33])

#----2
reparto=sample(barajas, 16)

a1=reparto[1:4]
a2=reparto[5:8]
b1=reparto[9:12]
b2=reparto[13:16]

names(a1)
names(a2)
names(b1)
names(b2)

#---3
#(primera forma para crear el frame que lea los titulos de cada carta)
juegoa1=data.frame(a1)
tipocarta_a1=rownames(juegoa1)

juegoa2= data.frame(a2)
tipocarta_a2=rownames(juegoa2)

juegob1= data.frame(b1)
tipocarta_b1=rownames(juegob1)

juegob2= data.frame(b2)
tipocarta_b2=rownames(juegob2)

#(segunda forma para crear el frame que lea los titulos de cada carta)

tipocarta_a1=names(a1)
tipocarta_a2=names(a2)
tipocarta_b1=names(b1)
tipocarta_b2=names(b2)

#asignación de puntos
asignarPuntos<-function(x){
  puntosCartas=0
  for (i in 1:4) {
    #si la carta es 10, 11 o 12 =son 10 puntos
    if(x[i]=='oro.10' || x[i]=='oro.11' || x[i]=='oro.12' || x[i]=='copas.10' ||x[i]=='copas.11' || x[i]=='copas.12'||
       x[i]=='espadas.10'|| x[i]=='espadas.11'|| x[i]=='espadas.12'|| x[i]=='bastos.10' || x[i]=='bastos.11'|| x[i]=='bastos.12'){
      puntosCartas=puntosCartas+10
    }
    if(x[i]=='oro.1'|| x[i]=='copas.1' || x[i]=='espadas.1' || x[i]=='bastos.1'){
      puntosCartas=puntosCartas+1
    }
    if(x[i]=='oro.2'|| x[i]=='copas.2' || x[i]=='espadas.2' || x[i]=='bastos.2'){
      puntosCartas=puntosCartas+2
    }
    if(x[i]=='oro.3'|| x[i]=='copas.3' || x[i]=='espadas.3' || x[i]=='bastos.3'){
      puntosCartas=puntosCartas+3
    }
    if(x[i]=='oro.4'|| x[i]=='copas.4' || x[i]=='espadas.4' || x[i]=='bastos.4'){
      puntosCartas=puntosCartas+4
    }
    if(x[i]=='oro.5'|| x[i]=='copas.5' || x[i]=='espadas.5' || x[i]=='bastos.5'){
      puntosCartas=puntosCartas+5
    }
    if(x[i]=='oro.6'|| x[i]=='copas.6' || x[i]=='espadas.6' || x[i]=='bastos.7'){
      puntosCartas=puntosCartas+6
    }
    if(x[i]=='oro.7'|| x[i]=='copas.7' || x[i]=='espadas.7' || x[i]=='bastos.7'){
      puntosCartas=puntosCartas+7
    }
    
  }
  puntosCartas
}

juego.A1=asignarPuntos(tipocarta_a1)
juego.A1
juego.A2=asignarPuntos(tipocarta_a2)
juego.A2

juego.B1=asignarPuntos(tipocarta_b1)
juego.B1
juego.B2=asignarPuntos(tipocarta_b2)
juego.B2

presentarPuntos=data.frame(juego.A1,juego.B1,juego.A2, juego.B2)
presentarPuntos

#----4
J=any(presentarPuntos>30)
J
#columna
gano=cbind(presentarPuntos, J)
gano


#----5-------
juegoMus<-function(){
  #----1
  barajas= numeric(40) #vector vacio de tamaño 40
  cartas=c("oro.1", "oro.2", "oro.3","oro.4", "oro.5", "oro.6", "oro.7","oro.10","oro.11","oro.12",
           "copas.1", "copas.2", "copas.3", "copas.4", "copas.5", "copas.6", "copas.7", "copas.10", "copas.11", "copas.12",
           "espadas.1", "espadas.2", "espadas.3", "espadas.4", "espadas.5", "espadas.6", "espadas.7", "espadas.10", "espadas.11", "espadas.12",
           "bastos.1", "bastos.2", "bastos.3", "bastos.4", "bastos.5", "bastos.6", "bastos.7", "bastos.10", "bastos.11", "bastos.12")
  
  
  #names(vectorBase)= vectorConbinar
  names(barajas)=cartas
  names(barajas[7])
  names(barajas[33])
  
  #----2
  reparto=sample(barajas, 16)
  
  a1=reparto[1:4]
  a2=reparto[5:8]
  b1=reparto[9:12]
  b2=reparto[13:16]
  
  names(a1)
  names(a2)
  names(b1)
  names(b2)
  
  #---3
  #(primera forma para crear el frame que lea los titulos de cada carta)
  juegoa1=data.frame(a1)
  tipocarta_a1=rownames(juegoa1)
  
  juegoa2= data.frame(a2)
  tipocarta_a2=rownames(juegoa2)
  
  juegob1= data.frame(b1)
  tipocarta_b1=rownames(juegob1)
  
  juegob2= data.frame(b2)
  tipocarta_b2=rownames(juegob2)
  
  #(segunda forma para crear el frame que lea los titulos de cada carta)
  
  tipocarta_a1=names(a1)
  tipocarta_a2=names(a2)
  tipocarta_b1=names(b1)
  tipocarta_b2=names(b2)
  
  #asignación de puntos
  asignarPuntos<-function(x){
    puntosCartas=0
    for (i in 1:4) {
      #si la carta es 10, 11 o 12 =son 10 puntos
      if(x[i]=='oro.10' || x[i]=='oro.11' || x[i]=='oro.12' || x[i]=='copas.10' ||x[i]=='copas.11' || x[i]=='copas.12'||
         x[i]=='espadas.10'|| x[i]=='espadas.11'|| x[i]=='espadas.12'|| x[i]=='bastos.10' || x[i]=='bastos.11'|| x[i]=='bastos.12'){
        puntosCartas=puntosCartas+10
      }
      if(x[i]=='oro.1'|| x[i]=='copas.1' || x[i]=='espadas.1' || x[i]=='bastos.1'){
        puntosCartas=puntosCartas+1
      }
      if(x[i]=='oro.2'|| x[i]=='copas.2' || x[i]=='espadas.2' || x[i]=='bastos.2'){
        puntosCartas=puntosCartas+2
      }
      if(x[i]=='oro.3'|| x[i]=='copas.3' || x[i]=='espadas.3' || x[i]=='bastos.3'){
        puntosCartas=puntosCartas+3
      }
      if(x[i]=='oro.4'|| x[i]=='copas.4' || x[i]=='espadas.4' || x[i]=='bastos.4'){
        puntosCartas=puntosCartas+4
      }
      if(x[i]=='oro.5'|| x[i]=='copas.5' || x[i]=='espadas.5' || x[i]=='bastos.5'){
        puntosCartas=puntosCartas+5
      }
      if(x[i]=='oro.6'|| x[i]=='copas.6' || x[i]=='espadas.6' || x[i]=='bastos.7'){
        puntosCartas=puntosCartas+6
      }
      if(x[i]=='oro.7'|| x[i]=='copas.7' || x[i]=='espadas.7' || x[i]=='bastos.7'){
        puntosCartas=puntosCartas+7
      }
      
    }
    puntosCartas
  }
  
  juego.A1=asignarPuntos(tipocarta_a1)
  juego.A1
  juego.A2=asignarPuntos(tipocarta_a2)
  juego.A2
  
  juego.B1=asignarPuntos(tipocarta_b1)
  juego.B1
  juego.B2=asignarPuntos(tipocarta_b2)
  juego.B2
  
  presentarPuntos=data.frame(juego.A1,juego.B1,juego.A2, juego.B2)
  presentarPuntos
  
  #----4
  J=any(presentarPuntos>30)
  J
  #columna
  gano=cbind(presentarPuntos, J)
  gano
}

set.seed(2020)
corrida_1=juegoMus()
corrida_1


set.seed(2021)
corrida_2=juegoMus()
corrida_2

set.seed(2022)
corrida_3=juegoMus()
corrida_3

set.seed(2100)
corrida_4=juegoMus()
corrida_4