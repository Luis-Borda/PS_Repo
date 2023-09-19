########### CUARTO PUNTO ##################
#### A
base_filtrada2$mujer <- if_else(base_filtrada2$sex ==1,0,1)
regresion_sexo <- lm(log_w~ mujer , data = base_filtrada2)
regresion2 <- stargazer(model5,
                        title = "Modelo Salario de acuerdo al sexo", type = "text")
### B
##### Se escogen las variables de control relacionadas con  trabajador similar y
##### características del puesto de trabajo varaible maximo nivel educativo y el
##### tiempo que lleva trabajando en la empresa

#### Se hace la regresion con los controles incluidos

regresion_sexo1 <- lm(log_w ~ mujer + base_filtrada2$maxEducLevel + base_filtrada2$p6426, 
                      data = base_filtrada2)

# Se regresa X1 en los controles y se toman residuales


paso1 <- base_filtrada2 %>% mutate (mujer_resid_f=lm(mujer ~ base_filtrada2$maxEducLevel 
                                                     +base_filtrada2$p6426, 
                                                     data=base_filtrada2)$residuals)
view(paso1)

# Se regresa variable "y" en controles y se toman residuales

rm(paso2)
paso2 <- base_filtrada2 %>% mutate (log_w_resid=lm(log_w~base_filtrada2$maxEducLevel +
                                                     base_filtrada2$p6426, 
                                                   data=base_filtrada2)$residuals)
view(paso2)

base_filtrada3 <- base_filtrada2%>% mutate(paso1,paso2)
view(base_filtrada3)

regresion_sexo2 <- lm(log_w_resid ~ mujer_resid_f, base_filtrada3)

# Se realiza stargazer para comparar coeficientes

stargazer(regresion_sexo1, regresion_sexo2, type = "text", digits = 7)

### FWL Bootstrap

fwl_boot<-function(data,index){
  
  b<-lm(log_w~ mujer + base_filtrada3$maxEducLevel + base_filtrada3$p6426, 
        data=base_filtrada3, subset = index)
  
  coefs<-b$coefficients
  
  betha1<-coefs[2]
  betha2<-coefs[3]
  
  mujer_b<-betha1+betha2
  
  
  return(mujer_b)
}

require(boot)
set.seed(123)
resultS <-  boot(base_filtrada3, fwl_boot, R = 1000)
resultS
