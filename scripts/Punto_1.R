################################################################################
##                                TALLER 1                                    ##
################################################################################

######## PUNTO 1 #######
#### CARGUE DE BASE ####
## Creamos un vector con los diferentes links ##

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
library(openxlsx)
library(ggplot2)
library(dplyr)
library(tidyr)

#CREAMOS UN DIRECTORIO

setwd("/Users/lordb/OneDrive - Universidad de los andes/2. GRUPOS/2. BIG DATA/PS_Repo_Taller1_G10")

#Hacemos el scraping

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

#### LIMPIEZA DE DATOS ####
summary(base)
str(base)
## Se realizo una tabla de datos vacios
vacios <- data.frame(apply(X = is.na(base), MARGIN =2, FUN = sum))
vacios$porcentaje_vacios <- round(vacios$apply.X...is.na.base...MARGIN...2..FUN...sum./32177,3)
colnames(vacios)[1]<- "Cantidad_vacios"

## Se seleccionan las varaibles de interés
base2 <- base %>%
  select(directorio,estrato1,sex,age,p6240,p6426,maxEducLevel,ocu,dsi,
         y_total_m,y_total_m_ha,ingtot,ingtotob,microEmpresa,cuentaPropia,formal, 
         oficio,p6210,p7090,hoursWorkUsual,relab,sizeFirm, college)

## Se filtra la base ##
base_filtrada <- subset(base2, base2$age >= 18 & base2$ocu == 1)

## Se dispone a eliminar todos los vacios

nombres <- colnames(base)
write.csv(nombres,"Nombres varaibles.csv")

#eliminamos las observaciones con NA de nuestra variable de interes, no se imputa
#Nos queda una observación con NA en maxEducLevel, la eliminamos
b_final <- base_filtrada %>% filter(y_total_m_ha != "NA")

b_final <- b_final %>% filter(maxEducLevel != "NA")
data.frame(apply(X = is.na(b_final), MARGIN =2, FUN = sum))
summary(base_final)

b_final <- b_final %>% 
  mutate(age_2 = age^2)

b_final <- b_final %>%
  mutate(log_Ingresos = log(y_total_m_ha))

b_final$female <- ifelse(b_final$sex == 0, 1, 0) %>% as.numeric()


write.csv(b_final,file = "stores/b_final.csv",row.names = FALSE)

#read_csv("stores/b_final.csv")
#View(b_final)

################################################################################
##ESTADISTICAS DESCRIPTIVAS 
################################################################################
library(stargazer)
library(knitr)
Descrip <- b_final %>%
  select(directorio,estrato1,sex,age,p6240,p6426,maxEducLevel,ocu,dsi,
         y_total_m,y_total_m_ha,ingtot,ingtotob,microEmpresa,cuentaPropia,formal, 
         oficio,p6210,p7090,hoursWorkUsual,relab,sizeFirm)

Descriptivas <- summary(Descrip)
tabla_descriptiva <-kable(Descriptivas, caption = "Estadísticas descriptivas")

write.csv(tabla_descriptiva, "stores/tabla_descriptiva.csv")


# 1. Crear el histograma
hist(b_final$estrato1)

hist(b_final$relab)
hist(b_final$sizeFirm)
hist(b_final$maxEducLevel)
hist(b_final$p6240)

#Se defninen las variables categoricas

Categoricas <- c("estrato1","relab","sizeFirm","maxEducLevel","p6240")

# Iterar a través de las columnas en el vector Categoricas
for (v in Categoricas) {
  # Seleccionar la columna v del dataframe b_final y convertirla en factor
  b_final[, v] <- as.factor(b_final[, v, drop = TRUE])
}


edad <- ggplot(data = b_final,
               mapping = aes(x = age))  + 
  geom_histogram(aes(y =after_stat(density)),
                 bins = 5,
                 position = 'identity',
                 color="black", fill="gray" ) +
  stat_function(fun = dnorm, xlim = c(min(b_final$age),max(b_final$age)), colour="green", linewidth=1,
                args = list(mean = mean(b_final$age), 
                            sd = sd(b_final$age))) + 
  labs(title = 'Distribución de edad',
       x = 'Edad',
       y = 'Frecuencia') + 
  theme_bw()

edad


Ingreso <- ggplot(data = b_final,
                  mapping = aes(x = y_total_m_ha))  + 
  geom_histogram(aes(y =after_stat(density)),
                 bins = 5,
                 position = 'identity',
                 color="black", fill="gray" ) +
  stat_function(fun = dnorm, xlim = c(min(b_final$y_total_m_ha),max(b_final$y_total_m_ha)), colour="green", linewidth=1,
                args = list(mean = mean(b_final$y_total_m_ha), 
                            sd = sd(b_final$y_total_m_ha))) + 
  labs(title = 'Distribución del ingreso',
       x = 'Edad',
       y = 'Frecuencia') + 
  theme_bw()
Ingreso

histogram(b_final$y_total_m_ha)

# Crear un gráfico de dispersión con una curva de suavizado en color
scatter.smooth(b_final$age, b_final$y_total_m_ha, span = 2/3, degree = 1,
               family = "symmetric",
               xlab = "Edad", ylab = "Ingreso",
               evaluation = 50, col = "blue") 
title("Relación entre Edad e Ingreso")

# Crear un gráfico de dispersión con una curva de suavizado en color
scatter.smooth(b_final$estrato1, b_final$y_total_m_ha, span = 2/3, degree = 1,
               family = "symmetric",
               xlab = "Estrato", ylab = "Ingreso",
               evaluation = 50, col = "green")  
title("Relación entre Estrato e Ingreso")

# Crear un gráfico de cajas y bigotes con estrato1 en el eje X y y_total_m_ha en el eje Y
boxplot(b_final$y_total_m_ha ~ b_final$estrato1, 
        xlab = "Estrato", ylab = "Ingreso",
        main = "Ingreso vs Estrato")

# Crear un gráfico de cajas y bigotes con relab en el eje X y y_total_m_ha en el eje Y
boxplot(b_final$y_total_m_ha ~ b_final$relab, 
        xlab = "Tipo de ocupación", ylab = "Ingreso",
        main = "Ingreso vs Tipo de ocupación")


scatter.smooth(b_final$hoursWorkUsual, b_final$y_total_m_ha, span = 2/3, degree = 1,
               family = "symmetric",
               xlab = "Horas trabajadas", ylab = "Ingreso",
               evaluation = 50, col = "green")  
title("Ingreso vs horas trabajadas")
