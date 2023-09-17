####################### TALLER 1 ##########################
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

hist(b_final$estrato1)
hist(b_final$relab)
hist(b_final$sizeFirm)
hist(b_final$oficio)
hist(b_final$maxEducLevel)
hist(b_final$p6240)

write.csv(b_final,file = "stores/base_final.csv",row.names = FALSE)



