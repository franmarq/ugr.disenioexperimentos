

## Planteamiento del Problema**

# **Se realiza un experimento para analizar la influencia de la dosis de paracetamol ingerida sobre los niveles de toxicidad hepática bajo diferentes niveles de actividad renal. En la tabla siguiente se reflejan los niveles de toxicidad observados para diferentes niveles de insuficiencia renal, cuando se administran diferentes dosis de paracetamol**.

getwd()
install.packages("dplyr")
install.packages("kableExtra")
install.packages("ggplot2")
install.packages("ggbeeswarm")

?tidyverse()
remove.packages("rlang")
remove.packages("tidyverse")
remove.packages("plotly")
install.packages("rlang")
install.packages("tidyverse")
install.packages("plotly")

library(tidyverse)


library(dplyr)
library(reshape)
library(rlang)
library(kableExtra)
library(ggplot2)
library(ggbeeswarm)
library(plotly)


library(dplyr)
library(tidyverse)
library(kableExtra)
library(ggplot2)
library(ggbeeswarm)
library(plotly)



#Creando el Data Frame

Paracetamol<-c("0-25%","25-50%","50-75%","75-1%")
AR1<-c(7,12,14,19) #Actividad Renal nivel 1
AR2<-c(7,17,18,25) #Actividad Renal nivel 2
AR3<-c(15,12,18,22)#Actividad Renal nivel 3
AR4<-c(11,18,19,19)#Actividad Renal nivel 4
AR5<-c(18,19,23,11)#Actividad Renal nivel 5

i<-data.frame(Paracetamol,AR1,AR2,AR3,AR4,AR5) #los bloques representan la restriccion de la aleatoriedad
colnames(i)=c("PARACETAMOL","15","20","25","30","35")

#Variable respuesta: Niveles de Toxicidad hepática

Respuesta<-c(7,7,15,11,18,12,17,12,18,19,14,18,18,19,23,19,25,22,19,11)
Tratamiento<-c("0-25%","0-25%","0-25%","0-25%","0-25%","25-50%","25-50%","25-50%","25-50%","25-50%","50-75%","50-75%","50-75%","50-75%","50-75%","75-1%","75-1%","75-1%","75-1%","75-1%")
Bloques<-c("15","20","25","30","35","15","20","25","30","35","15","20","25","30","35","15","20","25","30","35")

i%>% 
  kbl %>% 
  kable_paper("hover",full_width = T)

#*Contrastar la significación de los bloques (niveles de actividad renal) y los tratamientos (dosis de paracetamol) en el estudio de los niveles de toxicidad hepática (α= 0;05).*
  
  ## **Modelo de Diseño por Bloques al Azar**
  
#  ***Para realizar el modelo en lenguaje R se considera estructurar un data frame y gráficos con la variable respuesta los tratamientos y bloques, cabe destacar que los tratamientos son los distintos niveles de paracetamol, los bloques la actividad renal en sus diversos niveles (los cuales hay que transformar a factor) y la variable respuesta de interes que es el nivel de toxicidad hépatica producto de la influencia de los niveles de paracetamol.***
  
  
  ## Procedimiento para el desarrollo de este trabajo:
  
#  ***Se cargaron las librerias correspondientes a fin de modificar el formato de los cuadros y gráficos***
  

## ***Se estructuro el data frame***.

datos_parac<-data.frame(Respuesta,Tratamiento,Bloques)
tratam_parac<-factor(Tratamiento)
bloque_act_ren<-factor(Bloques)

#Determinacion de los factores y bloques

tratamientof<-factor(tratam_parac)
bloquef<-factor(bloque_act_ren)

# **Para realizar el modelo en lenguaje R Y ANOVA**
  
modelo<-lm(Respuesta~tratamientof+bloquef)
ANOVA<-aov(modelo)
RESUMEN_ANOVA<-summary(ANOVA)

## Visualizacion y  analisis exploratorio

# ***En el gráfico 1 se observa el Efecto de dosis de paracetamol en la toxicidad hépatica, en lineas generales se observa  que a mayor dosis de paracetamol mayor es la toxicidad hépatica***
  
#  ***Sin embargo por el hecho de que cada unidad experimental tiene una diversa actividad renal se debe analizar el problema como un Diseño por Bloque Completo al Azar, se muestra el gráfico 2 donde se aprecia la relación entre el efecto en la toxicidad hépatica  por cada actividad renal una vez asignada la dosis de paracetamol según el nivel ***
  
## **Analisis Exploratorio**



  plot_ly(datos_parac, x = Tratamiento, y = Respuesta, type = "scatter", mode = "markers",
        color = Tratamiento, colors = "Set1") %>% 
  layout(title = "Gr�fico 1:Efecto de dosis de paracetamol en la toxicidad hepatica",
         xaxis = list(title = "Dosis de paracetamol"), 
         yaxis = list(title = "Efecto Toxicidad hepatica"))


##  **Respecto a los bloques**

plot_ly(datos_parac, x = Bloques, y = Respuesta, type = "scatter", mode = "markers",
        color = Bloques, colors = "Set1") %>% 
  layout(title = "Gr�fico2: Efecto de dosis de paracetamol seg�n la actividad renal en la toxicidad hepatica",
         xaxis = list(title = "Actividad Renal"), 
         yaxis = list(title = "Efecto Toxicidad hepatica"))


##  Resumen del modelo

RESUMEN_ANOVA

##  **Conclusión :**

# ***Se puede observar que los tratamientos y bloques no son significativos es decir el efecto promedio de tóxicidad hépatica  tanto para los niveles de paracetamol y actividad renal no difieren a un nivel de significación del 5%.***
  
#  ***Sin embargo con un nivel de significación del 10% se presenta una diferencia significativa entre los niveles de paracetamol y el efecto de toxicidad hépatica.***
  