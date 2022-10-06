#  EJERCICIO 31 ANALISIS DE REGRESION - GASES TOXICOS ----------------------------------------------------------------------------------


install.packages("tidyverse")
install.packages("readxl")
install.packages("psych")
install.packages("GGally")
install.packages("ggplot2")
install.packages("gridExtra")

library(readxl)

#Datos
X31<- c(3,	7,	11,	15,	18,	27,	29,	30,	30,	30,	31,	31,	32,	33,	33,	34,	36,	36,	36,	37,	33,	39,	39,	40,	41,	42,	42,	43,	44,	45,	46,	47,	50)
Y31<- c(5,	11,	21,	16,	16,	28,	27,	25,	36,	30,	40,	32,	34,	32,	34,	37,	38,	34,	36,	38,	37,	36,	45,	39,	41,	40,	44,	47,	44,	46,	46,	49,	51)

dat_31<-data.frame(Y31,X31)
str(dat_31)


# grafico de las variables
plot(Y31~X31,dat_31, main="Dispersión entre las variables",xlab="valores de reducción en los sólidos volátiles",ylab="porcentajes de reducción de gases tóxicos")

# AOV de no linealidad - hipótesis de no linealidad entre X e Y
anova <- aov(Y31~X31,dat_31)
summary(anova)
coefficients(anova)

#modelo de regresion
modelo31 <- lm(Y31~X31,dat_31)
summary(modelo31)
Corr <- cor(X31,Y31)


#gráfico de dispersión con el modelo
plot(Y31~X31,dat_31, main="Dispersión entre las variables",xlab="valores de reducción en los sólidos volátiles",ylab="porcentajes de reducción de gases tóxicos")
abline(modelo31, col="red", coef)
text(paste("Correlación:", round(Corr, 2)), x = 10, y = 45)


#analisis de correlacion (contraste de linealidad)
#cor.test(my_data$y,my_data$x)



#validacion: Normalidad - Los errores deben seguir una distribución normal
par(mfrow=c(1,3)) # divide la ventana en una fila y tres columnas 

res<-rstandard(modelo31) # residuos estándar modelo ajustado
hist(res) # histograma residuos estandar
boxplot(res) # diagrama de cajas residuos estandar 
qqnorm(res) # gráfico de cuantiles residuos estandar 
qqline(res) # Linea de la distribución teórica Normal


par(mfrow=c(1,1)) # devuelve la pantalla a su estado original

#  Varianza constante - La varianza de los errores  es constante
plot(fitted.values(modelo31),rstandard(modelo31), xlab="Valores ajustados", ylab="Residuos estandarizados")  #valores ajustados vs. residuos estandarizados 
abline(h=0) 


#Valores atípicos - La independencia de los errores
plot(dat_31$X31,rstandard(modelo31),xlab="varX",ylab="Residuos estandarizados") 
abline(h=0)

#prueba Durbin-Watson
library(car)
durbinWatsonTest(modelo31)



#  EJERCICIO 32 ANALISIS DE REGRESION - AUTOMOVILES ----------------------------------------------------------------------------------


# lectura del archivo de datos
archivo32 <- c("Personales/Javier/Academicos/UGR - Estadistica Aplicada/Materias/C2 Diseño experimental/Tema 3/Datos Ejercicio 3b.xlsx")
archivo32

dat_32 <- read_excel(archivo32)
head(dat_32)
str(dat_32)

# 1.Analizar la relación entre variables

library(GGally)
ggpairs(dat_32, lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")

#modelo de regresion
modelo32 <- lm(y~x1+x2+x3+x4+x5,dat_32)
summary(modelo32)

# seleccion de predictores
step(object = modelo32, direction = "both", trace = 1)


#modelo de regresion v2
modelo321 <- lm(y~x2+x3+x4+x5,dat_32)
summary(modelo321)
coefficients(modelo321)


# validacion

#linealidad

library(ggplot2)
library(gridExtra)

plot1 <- ggplot(data = dat_32, aes(x2, modelo321$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot2 <- ggplot(data = dat_32, aes(x3, modelo321$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot3 <- ggplot(data = dat_32, aes(x4, modelo321$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot4 <- ggplot(data = dat_32, aes(x5, modelo321$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
grid.arrange(plot1, plot2, plot3, plot4)



plot1(dat_32$x2,rstandard(modelo321),xlab="varX",ylab="Residuos estandarizados") 
abline(h=0)
plot2(dat_32$x3,rstandard(modelo321),xlab="varX",ylab="Residuos estandarizados") 
abline(h=0)
plot3(dat_32$x4,rstandard(modelo321),xlab="varX",ylab="Residuos estandarizados") 
abline(h=0)
plot4(dat_32$x5,rstandard(modelo321),xlab="varX",ylab="Residuos estandarizados") 
abline(h=0)


# Normalidad

qqnorm(modelo321$residuals)
qqline(modelo321$residuals)

#Homocedasticidad

ggplot(dat_32, aes(modelo321$fitted.values, modelo321$residuals)) +
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

