install.packages("tidyverse")
install.packages("readxl")
install.packages("psych")
install.packages("GGally")
install.packages("ggplot2")
install.packages("gridExtra")



library(readxl)

#rm(archivo)
#rm(my_data)

# ***** Ejercicio 1: 
# lectura del archivo de datos
 
archivo <- c("Personales/Javier/Academicos/UGR - Estadistica Aplicada/Materias/C2 Diseño estadístico experimental y control de calidad. Aplicaciones en Biociencias e Ingeniería/Datos Ejercicio 2.xlsx")
archivo

my_data <- read_excel(archivo)
head(my_data)
names(my_data)

# grafico de las variables
plot(y~x,my_data, main="Dispersión entre las variables",xlab="valores de reducción en los sólidos volátiles",ylab="porcentajes de reducción de gases tóxicos")

# AOV de no linealidad - hipótesis de no linealidad entre X e Y
anova <- aov(modelo,my_data)
summary(anova)
coefficients(anova)

#modelo de regresion
modelo <- lm(y~x,my_data)
summary(modelo)

plot(y~x,my_data, main="Dispersión entre las variables",xlab="valores de reducción en los sólidos volátiles",ylab="porcentajes de reducción de gases tóxicos")
abline(modelo, col="red")


#analisis de correlacion (contraste de linealidad)
#cor.test(my_data$y,my_data$x)



#validacion: Normalidad - Los errores deben seguir una distribución normal
par(mfrow=c(1,3)) # divide la ventana en una fila y tres columnas 

res<-rstandard(modelo) # residuos estándar modelo ajustado
hist(residuos) # histograma residuos estandar
boxplot(residuos) # diagrama de cajas residuos estandar 
qqnorm(residuos) # gráfico de cuantiles residuos estandar 
qqline(residuos) # Linea de la distribución teórica Normal


par(mfrow=c(1,1)) # devuelve la pantalla a su estado original

#  Varianza constante - La varianza de los errores  es constante
plot(fitted.values(modelo),rstandard(modelo), xlab="Valores ajustados", ylab="Residuos estandarizados")  #valores ajustados vs. residuos estandarizados 
abline(h=0) 


#Valores atípicos - La independencia de los errores

plot(my_data$x,rstandard(modelo),xlab="varX",ylab="Residuos estandarizados") 
abline(h=0)

#prueba Durbin-Watson
library(car)
durbinWatsonTest(modelo)






# ***** Ejercicio 2: 


# lectura del archivo de datos
archivo2 <- c("Personales/Javier/Academicos/UGR - Estadistica Aplicada/Materias/C2 Diseño estadístico experimental y control de calidad. Aplicaciones en Biociencias e Ingeniería/Datos Ejercicio 2b.xlsx")
archivo2

my_data2 <- read_excel(archivo2)
head(my_data2)
names(my_data2)


# 1.Analizar la relación entre variables





library(GGally)
ggpairs(my_data2, lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")

#modelo de regresion
modelo2 <- lm(y~x1+x2+x3+x4+x5,my_data2)
summary(modelo2)

# seleccion de predictores
step(object = modelo2, direction = "both", trace = 1)

#modelo de regresion v2
modelo3 <- lm(y~x2+x3+x4+x5,my_data2)
summary(modelo3)
coefficients(modelo3)
# validacion

#linealidad

library(ggplot2)
library(gridExtra)

plot1 <- ggplot(data = my_data2, aes(x2, modelo3$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot2 <- ggplot(data = my_data2, aes(x3, modelo3$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot3 <- ggplot(data = my_data2, aes(x4, modelo3$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot4 <- ggplot(data = my_data2, aes(x5, modelo3$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
grid.arrange(plot1, plot2, plot3, plot4)



plot1(my_data2$x2,rstandard(modelo3),xlab="varX",ylab="Residuos estandarizados") 
abline(h=0)
plot2(my_data2$x3,rstandard(modelo3),xlab="varX",ylab="Residuos estandarizados") 
abline(h=0)
plot3(my_data2$x4,rstandard(modelo3),xlab="varX",ylab="Residuos estandarizados") 
abline(h=0)
plot4(my_data2$x5,rstandard(modelo3),xlab="varX",ylab="Residuos estandarizados") 
abline(h=0)

# Normalidad

qqnorm(modelo3$residuals)
qqline(modelo3$residuals)

#Homocedasticidad

ggplot(my_data2, aes(modelo3$fitted.values, modelo3$residuals)) +
  geom_point() +
  geom_smooth(color = "firebrick", se = FALSE) +
  geom_hline(yintercept = 0) +
  theme_bw()























El primer paso a la hora de establecer un modelo lineal múltiple es estudiar la 
relación que existe entre variables. Esta información es crítica a la hora de 
identificar cuáles pueden ser los mejores predictores para el modelo, qué 
variables presentan relaciones de tipo no lineal (por lo que no pueden ser
                                                  incluidas) y para identificar colinialidad entre predictores. A modo 
complementario, es recomendable representar la distribución de cada variable 
mediante histogramas.

Las dos formas principales de hacerlo son mediante representaciones gráficas 
(gráficos de dispersión) y el cálculo del coeficiente de correlación de cada 
par de variables.

round(cor(x = my_data2, method = "pearson"), 3)

library(psych)
multi.hist(x = my_data2, dcol = c("blue", "red"), dlty = c("dotted", "solid"), main = "")

