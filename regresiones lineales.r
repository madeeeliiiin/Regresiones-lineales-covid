#Regresiones lineales casos de covid

library(dplyr)
library(ggplot2)
casos<-read.csv("infcovid.csv", sep = "")

#creamos la matriz A
matriz_A<-matrix(data = 1,nrow = nrow(casos),1)
dia<-(casos$Dia)-1
matriz_A<-cbind(matriz_A,dia)

#creamos la matriz transpuesta A
trans_A<-t(matriz_A)

# Matriz con Ln de los casos
matriz_Ln<-log(casos$Casos)

#Multiplicacion de matriz_A^T *A
Matriz_B<-trans_A %*% matriz_A

#Multiplicacion de matriz_A^T *Matriz_ln
Matriz_C<-trans_A %*% matriz_Ln

#Matriz inversa de matriz_A^ *A
Inv_B<-solve(Matriz_B) 

# Multiplicacion de (Inversa de A^t*A)*(matriz_A^T *Matriz_ln)
Matriz_Ln_K<-Inv_B %*% Matriz_C

#Tasa de crecimiento
tasa<-Matriz_Ln_K[2,1]
contagio_dia_1<-Matriz_Ln_K[1,1]

#Calculo de casos aproximados con poblacion inicial  * EXP(dia *tasa)
casos$pob_aprox<- round(x = contagio_dia_1 * exp(casos$Dia *tasa),digits = 0)

#Graficar los casos
  ggplot(data = casos,mapping = aes(x = Dia,y = Casos))+
  geom_point()+
  labs(title = '    casos coronavirus')
#Coronavirus y casos esperados  
  ggplot(data = casos,mapping = aes(x = Dia,y = pob_aprox))+
    geom_point()+
    labs(title = '    casos coronavirus aproximado')  
  
  
#Regresiones cuadraticas
  #creamos la matriz 2A
  matriz_2A<-matrix(data = 1,nrow = nrow(casos),1)
  matriz_2A<-cbind(matriz_2A,dia)
  dia_2<-dia^2
  matriz_2A<-cbind(matriz_2A,dia_2)
  
  #Multiplicacion de matriz_2A^T *matriz_2A
  Matriz_2B<-t(matriz_2A) %*% matriz_2A
  
  #Multiplicacion de matriz_2A^T *Matriz_ln
  Matriz_2C<-t(matriz_2A) %*% matriz_Ln
  
  # Multiplicacion de (Inversa de 2A^t*A)*(matriz_2A^T *Matriz_ln)
  Matriz_2Ln_K<-solve(Matriz_2B)%*% Matriz_2C
  
  #Tasa de crecimiento
  tasa2<-Matriz_2Ln_K[2,1]
  contagio2_dia_1<-Matriz_2Ln_K[1,1]
  c2<-Matriz_2Ln_K[3,1]
 
  casos$casos2_aprox<-round(x = exp(contagio2_dia_1)*exp(tasa2*dia + c2*((dia)^2)),digits = 0)
  
  #Graficar los casos
  ggplot(data = casos,mapping = aes(x = Dia,y = casos2_aprox))+
    geom_point()+
    labs(title = '    casos coronavirus')
