
#### ABRIR LAS BASES DE DATOS
HOG <- read_sav("C:/Users/yheli/OneDrive - Universidad de Antioquia/BDEPECA/FINAL SPSS/hogaresFinal v.2.sav")
View(HOG)
IND <- read_sav("C:/Users/yheli/OneDrive - Universidad de Antioquia/BDEPECA/FINAL SPSS/individuosFinal v.2.sav")
View(IND)
####LIBRERÍAS
library(tidyverse)
library(haven)  # Lectura de datos
library(sjmisc) # Utilidades para encuestas con etiquetas
library(sjPlot) # Gráficos y tablas "bonitos" con etiquetas
library(broom)  # Convierte en tibbles los resultados de modelos
library(survey) # Análisis de encuestas complejas
library(srvyr) # Funciones survey a la tidyverse
library(knitr) # Tablas con kable
library(sandwich)
library(lmtest) # Varianzas-covarianzas
library(car)    # Inferencia de modelos
###TIPO DE VIVIENDA
prop.table(xtabs(HOG$ExpPoblacional~HOG$q_10))*100
###HOGARES CON SERVICIOS PÚBLICSO
prop.table(xtabs(HOG$ExpPoblacional~HOG$q_11))*100
prop.table(xtabs(HOG$ExpPoblacional~HOG$q_12))*100
prop.table(xtabs(HOG$ExpPoblacional~HOG$q_13))*100
prop.table(xtabs(HOG$ExpPoblacional~HOG$q_14))*100
prop.table(xtabs(HOG$ExpPoblacional~HOG$q_15))*100
prop.table(xtabs(HOG$ExpPoblacional~HOG$q_16))*100
prop.table(xtabs(HOG$ExpPoblacional~HOG$q_17))*100
prop.table(xtabs(HOG$ExpPoblacional~HOG$q_18))*100
prop.table(xtabs(HOG$ExpPoblacional~HOG$q_19))*100
prop.table(xtabs(HOG$ExpPoblacional~HOG$q_20))*100
prop.table(xtabs(HOG$ExpPoblacional~HOG$q_21))*100
##TENENCIA DE LA VIVIENDA
prop.table(xtabs(HOG$ExpPoblacional~HOG$q_28))*100
###ACTIVOS
prop.table(xtabs(HOG$ExpPoblacional~HOG$q_29))*100
prop.table(xtabs(HOG$ExpPoblacional~HOG$q_30))*100
prop.table(xtabs(HOG$ExpPoblacional~HOG$q_31))*100
prop.table(xtabs(HOG$ExpPoblacional~HOG$q_32))*100
prop.table(xtabs(HOG$ExpPoblacional~HOG$q_33))*100
prop.table(xtabs(HOG$ExpPoblacional~HOG$q_34))*100
prop.table(xtabs(HOG$ExpPoblacional~HOG$q_35))*100
prop.table(xtabs(HOG$ExpPoblacional~HOG$q_36))*100
prop.table(xtabs(HOG$ExpPoblacional~HOG$q_37))*100
prop.table(xtabs(HOG$ExpPoblacional~HOG$q_38))*100
prop.table(xtabs(HOG$ExpPoblacional~HOG$q_39))*100
prop.table(xtabs(HOG$ExpPoblacional~HOG$q_40))*100
#HOGARES QUE DEMANDAN TDCR
prop.table(xtabs(HOG$ExpPoblacional~HOG$q_41))*100
#HOGARES QUE DEMANDA TDCNR
prop.table(xtabs(HOG$ExpPoblacional~HOG$q_42))*100
prop.table(xtabs(HOG$ExpPoblacional~HOG$q_43))*100
#crear variable DTDNR
HOG$DTDNR<-ifelse(HOG$q_42==1|HOG$q_43==1|HOG$q_43==2|HOG$q_43==3,1,0)
prop.table(xtabs(HOG$ExpPoblacional~HOG$DTDNR))*100
#EXPORTAR LA VARIABLE
TEMP<-data.frame(HOG$keyhogar,HOG$DTDNR)
write.csv(TEMP,"TEMP.csv")
#CREAR VARIABLE JEFATURA FEMENINA
IND$JF<-ifelse(IND$q_46==1&IND$q_50==2,1,0)
#CREAR VARIABLE CABEZA DE HOGAR
IND$CH<-ifelse((IND$q_46==1&IND$q_49==4)|(IND$q_46==1&IND$q_49==5)|(IND$q_46==1&IND$q_49==6),1,0)
#CREAR VARIABLE JEFATURA FEMENINA CABEZA DE HOGAR
IND$JFCH<-ifelse(IND$JF==1&IND$CH==1,1,0)
#merge
TEMP<-IND %>% group_by(keyhogar) %>% summarise(JFCH=sum(JFCH))
MERG<-merge(HOG, TEMP, by = intersect(names(HOG), names(TEMP)), ALL=TRUE)
#se crea la variable (RF) persona que desea buscar trabajo o montar un negocio pero no puede por responsabilidad familiar
IND$RF<-ifelse(IND$q_68==3|IND$q_69==7,1,0)
IND$RF[is.na(IND$RF)]<-0
xtabs(IND$ExpPoblacional~IND$RF)
prop.table(xtabs(IND$ExpPoblacional~IND$RF))*100
#Se cruza con el género
xtabs(IND$ExpPoblacional~IND$RF+IND$q_50)
prop.table(xtabs(IND$ExpPoblacional~IND$RF+IND$q_50))*100
