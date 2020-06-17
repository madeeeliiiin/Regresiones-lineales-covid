library(dplyr)
library(ggplot2)

casos<-read.csv("infcovid.csv", sep = "")

modelo1<-lm(casos$Dia~casos$Casos)
summary(modelo1)
modelo1$coefficients
casos$cont_aprox<-casos$Casos[1] * exp(casos$Dia *modelo1$coefficients[2])

