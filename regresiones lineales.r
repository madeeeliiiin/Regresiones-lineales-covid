#Regresiones lineales casos de covid

library(ggplot2)
casos<-read.csv("casoscsv", sep = ";")
names(casos)[1] <- "Dia"
names(casos)[2] <- "Casos"
names(casos)[3]<-"Fallecidos"

########################## Regresiones lineales manual Muertes########################## 
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
  
  ########################## Regresiones lineales fallecimientos manual########################## 
  #creamos la matriz A
  
  fallecidos<-casos$Fallecidos
  #Es necesario no tener los 0 en el vector de fallecidos
  i<-1
  while(i<length(fallecidos)){
    if(fallecidos[i]>0){
      fallecidos<-fallecidos[i]
    }
    i<-i+1}
  fallecidos<-casos$Fallecidos[(i-1):nrow(casos)]
  
  Fallecimientos_A<-matrix(data = 1,nrow = length(fallecidos),1)
  Fallecimientos_A<-cbind(Fallecimientos_A,dia)
  
  # Matriz con Ln de los casos
  Fallecimientos_Ln<-log(fallecidos)
  
  # Multiplicacion de (Inversa de A^t*A)*(matriz_A^T *Matriz_ln)
  Fallecimientos_Ln_K<-solve(t(Fallecimientos_A) %*% Fallecimientos_A)  %*% (t(Fallecimientos_A) %*% Fallecimientos_Ln)
  
  #Calculo de muertes aproximados con exp(poblacion inicial)  * EXP(dia *tasa)
  casos$Fallecidos_aprox[(i-1):nrow(casos)]<- exp(Fallecimientos_Ln_K[1])*exp(casos$Dia *Fallecimientos_Ln_K[2])

  
  ########################## Regresiones cuadraticas para fallecidos manual########################## 
  #creamos la matriz 2A
  Fallecimientos_2A<-matrix(data = 1,nrow = length(fallecidos),1)
  Fallecimientos_2A<-cbind(Fallecimientos_2A,dia)
  Fallecimientos_2A<-cbind(Fallecimientos_2A,dia_2)
  
  # Multiplicacion de (Inversa de 2A^t*A)*(matriz_2A^T *Matriz_ln)
  Fallecimientos_2Ln_K<-(solve(t(Fallecimientos_2A) %*% Fallecimientos_2A))%*% ((t(Fallecimientos_2A) %*% Fallecimientos_Ln))
  
  #Tasa de crecimiento
  Fallecimientos_tasa2<-Fallecimientos_2Ln_K[2]
  Fallecimientos2_dia_1<-Fallecimientos_2Ln_K[1]
  Fallecimiento_c2<-Fallecimientos_2Ln_K[3]
  
  
  #Calculo de casos aproximados con exp(contagio2_dia_1)*exp(tasa2*dia + c2*((dia)^2)))
  casos$Fallecimientos2_aprox[(i-1):nrow(casos)]<- exp(Fallecimientos2_dia_1)*exp(Fallecimientos_tasa2*dia + Fallecimiento_c2*((dia)^2))

  
  
  #Graficar los casos y las regresiones lineales
  ggplot(data = casos,mapping = aes(x = Dia,y = Casos))+
    geom_point()+
    labs(title = 'casos coronavirus')+
    stat_smooth(mapping = aes(x=Dia,y=casos2_aprox,colour="regresion cuadratica"))+
    stat_smooth(mapping = aes(x = Dia,y = casos_aprox,colour="regresion lineal"))
  
  #Grafica de los fallecimientos
  ggplot(data = casos,mapping = aes(x = Dia,y = Fallecidos))+
    geom_point()+
    labs(title = 'Fallecidos')+
    stat_smooth(mapping = aes(x = Dia,y = Fallecidos_aprox,colour="Fallecidos aproximados"))+
    stat_smooth(mapping = aes(x = Dia,y = Fallecimientos2_aprox,colour="Fallecidos aproximados cuadraticos"))
 
  #Graficos de la escala logaritmica de los casos
  ggplot(data = casos,mapping = aes(x = Dia,y = log(Casos),colour="Casos"))+
    geom_point()+
    labs(title = 'escala logaritmica casos')+
    geom_smooth(mapping = aes(x = Dia,y=log(casos_aprox),colour="casos aprox"),method = "lm")
  
  #Grafica de la escala logaritmica de muertes 
  ggplot(data = casos,mapping = aes(x = Dia,y = log(Fallecidos),colour="Fallecidos"))+
    geom_point()+
    labs(title = 'escala logaritmica fallecidos')+
    geom_smooth(mapping = aes(x = Dia,y=log(Fallecidos_aprox),colour="Fallecidos aprox"),method = "lm")
  
