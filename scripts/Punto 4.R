########### CUARTO PUNTO ##################
#### A
base_filtrada2$mujer <- if_else(base_filtrada2$sex ==1,0,1)
regresion_sexo <- lm(log_w~ mujer , data = base_filtrada2)
regresion2 <- stargazer(model5,
                        title = "Modelo Salario de acuerdo al sexo", type = "text")