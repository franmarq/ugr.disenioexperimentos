
# EJERCICIO 2.1 ----------------------------------------------------------------------------------


### construccion de los datos
Resp21<- c(32,44,31,35,33,33,40,46,33,29,35,32,37,30,28,33,37,39)
Trat21<-c("A","A","A","A","A","A","B","B","B","B","B","B","C","C","C","C","C","C")

## el data frame
dat_21<-data.frame(Resp21,Trat21)
trat_21<-factor(Trat21)

#Determinacion de los factores
trat21f<-factor(trat_21)

#analisis los datos
require(ggplot2)
ggplot(data = dat_21, aes(x = Trat21, y = Resp21, color = Trat21)) +
  geom_boxplot() +
  theme_bw()

#supuesto 1: independencia
'Asumimos que los datos fueron recolectados en forma aleatoria'


#supuesto 2: distribucion normal
par(mfrow = c(1,3))
qqnorm(dat_21[dat_21$Trat21 == "A","Resp21"], main = "A")
qqline(dat_21[dat_21$Trat21 == "A","Resp21"])
qqnorm(dat_21[dat_21$Trat21 == "B","Resp21"], main = "B")
qqline(dat_21[dat_21$Trat21 == "B","Resp21"])
qqnorm(dat_21[dat_21$Trat21 == "C","Resp21"], main = "C")
qqline(dat_21[dat_21$Trat21 == "C","Resp21"])	

par(mfrow = c(1,1))

#test de normalidad (menos de 50 observaciones usamos el test Shapiro - Wilk)
#hist(dat_21$Resp21)
shapiro.test(dat_21$Resp21)

#supuesto 3: homocedasticidad o varianza constante entre grupos

bartlett.test(Resp21~Trat21,dat_21)



# **Para realizar el modelo en lenguaje R Y ANOVA**
modelo21 <- lm(Resp21~trat21f)
ANOVA21 <- aov(modelo21)
RESUMEN_ANOVA21 <- summary(ANOVA21)
RESUMEN_ANOVA21
summary(modelo21)


# validez del modelo
par(mfrow = c(2,2))
plot(modelo21)
par(mfrow = c(1,1))



# EJERCICIO 2.2 ----------------------------------------------------------------------------------

### construccion de los datos
Resp22<- c(32,45,68,29,41,37,78,54,23,33,35,11,30,23,41,42,31,37,22,45,61,34,38,39,28,29)
Trat22<-c("Em1","Em1","Em1","Em1","Em1","Em1","Em1","Em2","Em2","Em2","Em2","Em2","Em3",
          "Em3","Em3","Em3","Em3","Em3","Em3","Em3","Em4","Em4","Em4","Em4","Em4","Em4")

## el data frame
dat_22<-data.frame(Resp22,Trat22)
trat_22<-factor(Trat22)

#Determinacion de los factores
trat22f<-factor(trat_22)

#analisis los datos
require(ggplot2)
ggplot(data = dat_22, aes(x = Trat22, y = Resp22, color = Trat22)) +
  geom_boxplot() +
  theme_bw()

#supuesto 1: independencia
'Asumimos que los datos fueron recolectados en forma aleatoria'

#supuesto 2: distribucion normal
par(mar=c(1,1,1,1)) #cambio de margenes para que ajusten los 4 graficos
par(mfrow = c(2,2))
qqnorm(dat_22[dat_22$Trat22 == "Em1","Resp22"], main = "Em1")
qqline(dat_22[dat_22$Trat22 == "Em1","Resp22"])
qqnorm(dat_22[dat_22$Trat22 == "Em2","Resp22"], main = "Em2")
qqline(dat_22[dat_22$Trat22 == "Em2","Resp22"])
qqnorm(dat_22[dat_22$Trat22 == "Em3","Resp22"], main = "Em3")
qqline(dat_22[dat_22$Trat22 == "Em3","Resp22"])	
qqnorm(dat_22[dat_22$Trat22 == "Em4","Resp22"], main = "Em4")
qqline(dat_22[dat_22$Trat22 == "Em4","Resp22"])	

par(mfrow = c(1,1))

#test de normalidad (menos de 50 observaciones usamos el test Shapiro - Wilk)
#hist(dat_22$Resp22)
shapiro.test(dat_22$Resp22)

#supuesto 3: homocedasticidad o varianza constante entre grupos

# Dado que se encuentra en el límite para aceptar que se distribuye de forma normal, el test de Fisher y el de 
# Bartlett no son recomendables. En su lugar es mejor emplea un test basado en la mediana test de Levene o test 
# de Fligner-Killeen.

require(car)
leveneTest(Resp22~Trat22,dat_22,center = "median")
fligner.test(Resp22~Trat22,dat_22)


# **Para realizar el modelo en lenguaje R Y ANOVA**
modelo22 <- lm(Resp22~trat22f)
ANOVA22 <- aov(modelo22)
RESUMEN_ANOVA22 <- summary(ANOVA22)
RESUMEN_ANOVA22




# Validez del modelo

par(mfrow = c(2,2))
plot(modelo22)
par(mfrow = c(1,1))





# EJERCICIO 2.3 ----------------------------------------------------------------------------------

### construccion de los datos
Resp23<- c(98,97,99,96,91,90,93,92,96,95,97,95,95,96,99,98)
Trat23<-c("T1","T1","T1","T1","T2","T2","T2","T2","T3","T3","T3","T3","T4","T4","T4","T4")

## el data frame.
dat_23<-data.frame(Resp23,Trat23)
trat_23<-factor(Trat23)

#Determinacion de los factores
trat23f<-factor(trat_23)


#analisis los datos
require(ggplot2)
ggplot(data = dat_23, aes(x = Trat23, y = Resp23, color = Trat23)) +
  geom_boxplot() +
  theme_bw()

#supuesto 1: independencia
'Asumimos que los datos fueron recolectados en forma aleatoria'


#supuesto 2: distribucion normal

par(mar=c(1,1,1,1)) #cambio de margenes para que ajusten los 4 graficos
par(mfrow = c(2,2))
qqnorm(dat_23[dat_23$Trat23 == "T1","Resp23"], main = "T1")
qqline(dat_23[dat_23$Trat23 == "T1","Resp23"])
qqnorm(dat_23[dat_23$Trat23 == "T2","Resp23"], main = "T2")
qqline(dat_23[dat_23$Trat23 == "T2","Resp23"])
qqnorm(dat_23[dat_23$Trat23 == "T3","Resp23"], main = "T3")
qqline(dat_23[dat_23$Trat23 == "T3","Resp23"])	
qqnorm(dat_23[dat_23$Trat23 == "T4","Resp23"], main = "T4")
qqline(dat_23[dat_23$Trat23 == "T4","Resp23"])	

par(mfrow = c(1,1))

#test de normalidad (menos de 50 observaciones usamos el test Shapiro - Wilk)
#hist(dat_23$Resp23)
shapiro.test(dat_23$Resp23)

#supuesto 3: homocedasticidad o varianza constante entre grupos

bartlett.test(Resp23~Trat23,dat_23)


# Para realizar el modelo en lenguaje R Y ANOVA
modelo23 <- lm(Resp23~trat23f)
anova23 <- aov(modelo23)
summary(anova23)

#comparacion multiple
par(mar=c(3,3,3,3)) #cambio de margenes para que ajusten los 4 graficos
Tuke<-TukeyHSD(anova23)
plot(TukeyHSD(anova23))


# Validez del modelo

par(mfrow = c(2,2))
plot(modelo23)
par(mfrow = c(1,1))
