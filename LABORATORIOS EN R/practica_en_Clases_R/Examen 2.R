
#PREGUNTA 1

baraja<-c("Oros."=1:10,"Copas."=11:20,"Espadas."=21:30,"Bastos."=31:40)

baraja

names(baraja[33])


#PREGUNTA 2

  A1 = sample(baraja, 4,replace =FALSE) 
  B1 = sample(baraja, 4,replace =FALSE) 
  A2 = sample(baraja, 4,replace =FALSE) 
  B2 = sample(baraja, 4,replace =FALSE) 
  
 names(A1)
 names(B1)
 names(A2)
 names(B2)
 # PREGUNTA 3
 
 df = data.frame(names(A1),names(B1),names(A2),names(B2))
 
 juego.df

  