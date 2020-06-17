#Regresiones lineales casos de covid

library(dplyr)
library(ggplot2)
casos<-read.csv("infcovid.csv", sep = "")

#creamos la matriz A
matriz_A<-matrix(data = 1,nrow = 96,1)
dia<-casos$Dia
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
casos$pob_aprox<- contagio_dia_1 * exp(casos$Dia *tasa)

#Graficar los casos
  ggplot(data = casos,mapping = aes(x = Dia,y = Casos))+
  geom_point()+
  labs(title = '    casos coronavirus')
