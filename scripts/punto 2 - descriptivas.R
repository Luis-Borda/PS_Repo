
############################## PUNTO 1 ########################################


#Limpieza del área de trabajo 

rm(list = ls())

#CARGAMOS LOS PAQUETES

require(pacman)
p_load(rio,
       tidyverse,
       skimr,
       caret,
       readxl,
       rvest,
       stargazer,
       knitr,
       boot,
       data.table)

install.packages("ggplot2")
install.packages("openxlsx")
install.packages("xlsx")
install.packages("mice")
library(openxlsx)
library(ggplot2)
library(dplyr)
library(tidyr)
library (mice)
library(rvest)
library (skimr)
library(xlsx)

#CREAMOS UN DIRECTORIO

setwd("/Users/lordb/OneDrive - Universidad de los andes/2. GRUPOS/2. BIG DATA/PS_Repo_Taller1_G10")

###############################################################################
############### web scrapping para consolidar la base de datos ################
################################################################################

#Se crea un objeto con el link

link <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_", 1:10, ".html")

## Se hace un bucle que una las 10 bases
base <- data.frame()
for(i in link){
  print(i)
  GEIH<- read_html(i) %>% 
    html_table() %>%
    as.data.frame()
  base <- rbind(base, GEIH)
}

##############Filtro de la base de datos#######################################
###############################################################################

## Se filtra la base para personas ocupadas menores de 18 años
base_edad_ocu <- subset(base, base$age >= 18 & base$ocu == 1)

###Se seleccionan las variables a utilizar
base_filtrada <- select(base_edad_ocu, "estrato1",
                        "sex",
                        "age",
                        "p6240",
                        "p6426",
                        "ina",
                        "maxEducLevel",
                        "ocu",
                        "dsi",
                        "y_total_m_ha")

######### Estadísticas descriptivas ############################################
#1)tabla con estadísticas descriptivas
##Pasamos la base a tibble
base_tibble<- as_tibble(base_filtrada)
##generamos un reporte de las descriptivas
descriptivas <- skim(base_tibble) 
##se exporta
install.packages('openxlsx')
library(openxlsx)
write.xlsx(descriptivas, "stores/Descriptivas.xlsx")

##### Gráficos para interpretación

# Histograma edad

edad <- ggplot(data = base_filtrada,
               mapping = aes(x = age))  + 
  geom_histogram(aes(y =after_stat(density)),
                 bins = 5,
                 position = 'identity',
                 color="black", fill="gray" ) +
  stat_function(fun = dnorm, xlim = c(min(base_filtrada$age),max(base_filtrada$age)), colour="green", linewidth=1,
                args = list(mean = mean(base_filtrada$age), 
                            sd = sd(base_filtrada$age))) + 
  labs(title = 'Distribución de edad',
       x = 'Edad',
       y = 'Frecuencia') + 
  theme_bw()

edad

##Histograma ingreso

Ingreso <- ggplot(data = base_filtrada,
                  mapping = aes(x = y_total_m_ha))  + 
  geom_histogram(aes(y =after_stat(density)),
                 bins = 5,
                 position = 'identity',
                 color="black", fill="gray" ) +
  stat_function(fun = dnorm, xlim = c(min(base_filtrada$y_total_m_ha),max(base_filtrada$y_total_m_ha)), colour="green", linewidth=1,
                args = list(mean = mean(base_filtrada$y_total_m_ha), 
                            sd = sd(base_filtrada$y_total_m_ha))) + 
  labs(title = 'Distribución del ingreso',
       x = 'Ingreso',
       y = 'Frecuencia') + 
  theme_bw()

Ingreso

### relación ingreso edad

ggplot(data=base_filtrada)+ 
  geom_smooth(mapping = aes(x =age, y =y_total_m_ha))+
  scale_y_continuous (labels=function(n) {format(n, scientific = FALSE)})+
  labs(x="Edad", y="Ingreso")

ggplot(data=base_filtrada)+ 
  geom_point(mapping = aes(x =age, y =y_total_m_ha))+
  scale_y_continuous (labels=function(n) {format(n, scientific = FALSE)})+
  labs(x="Edad", y="Ingreso")

### relación ingreso estrato

#Grfico de dispesión con línea de tendencia
scatter.smooth(base_filtrada$estrato1, base_filtrada$y_total_m_ha, span = 2/3, degree = 1,
               family = "symmetric",
               xlab = "Estrato", ylab = "Ingreso",
               evaluation = 50, col = "green")  
title("Relación entre Estrato e Ingreso")

# gráfico de cajas y bigotes
boxplot(base_filtrada$y_total_m_ha ~ base_filtrada$estrato1, 
        xlab = "Estrato", ylab = "Ingreso",
        main = "Ingreso vs Estrato")


##### relación experiencia ingreso ################################
  
ggplot(data=base_filtrada)+ 
  geom_smooth(mapping = aes(x =p6426, y =y_total_m_ha))+
  scale_y_continuous (labels=function(n) {format(n, scientific = FALSE)})+
  labs(x="Experiencia", y="Ingreso")

ggplot(data=base_filtrada)+ 
  geom_point(mapping = aes(x =p6426, y =y_total_m_ha))+
  scale_y_continuous (labels=function(n) {format(n, scientific = FALSE)})+
  labs(x="Experiencia", y="Ingreso")