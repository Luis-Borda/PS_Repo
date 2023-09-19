########### TERCER PUNTO ##################
## Regresion
library(stargazer)
regresion_edad <- lm(log_w~ age + age_2, data = base_filtrada2)  
regresion <- stargazer(regresion_edad,
                       title = "Modelo Salario depende de Edad", type = "text")


## Predicción
base_filtrada2$yhat <- predict(regresion_edad)
head(base_filtrada2)
##Bootstrap
edad_fun<-function(data,index,
                   edad=median(base_filtrada2$age)){
  
  f<-lm(log_w~ age + age_2, data, subset = index)
  
  coefs<-f$coefficients
  
  b1<-coefs[2]
  b2<-coefs[3]
  
  edad_pico<-b1+b2*edad
  
  
  return(edad_pico)
}

require(boot)
set.seed(123)
resultados <-  boot(base_filtrada2, edad_fun, R = 1000)

## Intervalo de Confianza 
edad_punta <- (-regresion_edad$coefficients[2])/(2*regresion_edad$coefficients[3])
ee <- 0.001731169
ic_li <- (edad_punta - ee*1.96)
ic_ls <- (edad_punta + ee*1.96)

## Grafica
summ = base_filtrada2 %>%  
  group_by(
    age, age_2
  ) %>%  
  summarize(
    mean_y = mean(log_w),
    yhat_reg = mean(yhat), .groups="drop"
  ) 


ggplot(summ) + 
  geom_point(
    aes(x = age, y = mean_y),
    color = "blue", size = 2
  ) + 
  geom_line(
    aes(x = age, y = yhat_reg), 
    color = "green", linewidth = 1.5
  ) + 
  labs(
    title = "Relación Años de Educación y Salario por Horas",
    x = "Años de Educación",
    y = "ln Salario por horas"
  ) +
  theme_bw()
