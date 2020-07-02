#Regresiones lineales casos de covid

library(ggplot2)
casos<-read.csv("casos.csv", sep = ";")
names(casos)[1] <- "Dia"
names(casos)[2] <- "Casos"

########################## Regresiones lineales manual########################## 
#creamos la matriz A
matriz_A<-matrix(data = 1,nrow = nrow(casos),1)
dia<-(casos$Dia)-1
matriz_A<-cbind(matriz_A,dia)

# Matriz con Ln de los casos
matriz_Ln<-log(casos$Casos)

#Multiplicacion de matriz_A^T *A
Matriz_B<-t(matriz_A) %*% matriz_A

# Multiplicacion de (Inversa de A^t*A)*(matriz_A^T *Matriz_ln)
Matriz_Ln_K<-solve(Matriz_B)  %*% (t(matriz_A) %*% matriz_Ln)

#Tasa de crecimiento
tasa<-Matriz_Ln_K[2]
contagio_dia_1<-Matriz_Ln_K[1]

######################## Regresion lineal con la funcion lm #############################
Modelo_1<- lm(log(Casos) ~ dia, data=casos)                 
print(Modelo_1)

#Calculo de casos aproximados con exp(poblacion inicial)  * EXP(dia *tasa)
casos$casos_aprox<- exp(contagio_dia_1)*exp(casos$Dia *tasa)

########################## Regresiones cuadraticas manual########################## 
#creamos la matriz 2A
  matriz_2A<-matrix(data = 1,nrow = nrow(casos),1)
  matriz_2A<-cbind(matriz_2A,dia)
  dia_2<-dia^2
  matriz_2A<-cbind(matriz_2A,dia_2)
  
  #Multiplicacion de matriz_2A^T *matriz_2A
  Matriz_2B<-t(matriz_2A) %*% matriz_2A
  
  # Multiplicacion de (Inversa de 2A^t*A)*(matriz_2A^T *Matriz_ln)
  Matriz_2Ln_K<-solve(Matriz_2B)%*% (t(matriz_2A) %*% matriz_Ln)
  
  #Tasa de crecimiento
  tasa2<-Matriz_2Ln_K[2]
  contagio2_dia_1<-Matriz_2Ln_K[1]
  c2<-Matriz_2Ln_K[3]
  
  ######################## Regresion cuadratica con la funcion lm #############################
  Modelo_2<- lm(log(Casos) ~ (dia + dia_2), data=casos)    
  print(Modelo_2) 
  
  #Calculo de casos aproximados con exp(contagio2_dia_1)*exp(tasa2*dia + c2*((dia)^2)))
  casos$casos2_aprox<- exp(contagio2_dia_1)*exp(tasa2*dia + c2*((dia)^2))
  
  #Graficar los casos y las regresiones lineales
  ggplot(data = casos,mapping = aes(x = Dia,y = Casos))+
    geom_point()+
    labs(title = 'casos coronavirus')+
    stat_smooth(mapping = aes(x=Dia,y=casos2_aprox,colour="regresion cuadratica"))+
    stat_smooth(mapping = aes(x = Dia,y = casos_aprox,colour="regresion lineal"))
 
  #Graficos de la escala logaritmica
  ggplot(data = casos,mapping = aes(x = Dia,y = log(Casos),colour="Casos"))+
    geom_point()+
    labs(title = 'escala logaritmica')+
    stat_smooth(mapping = aes(x=Dia,y=log(casos_aprox),colour="casos aprox")) 
