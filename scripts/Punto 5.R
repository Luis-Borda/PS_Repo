########### QUINTO PUNTO ##################
### A
set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(base_filtrada2), replace=TRUE, prob=c(0.7,0.3))
head(sample)
train  <- base_filtrada2[sample, ] #train sample those that are TRUE in the sample index
test   <- base_filtrada2[!sample, ] #test sample those that are FALSE in the sample index
dim(train)

### B
## Especificacion edad
model_edad <- lm(log_w~ age + age_2, data = base_filtrada2)
test$model_edad<-predict(model_edad,newdata = test)
m_edad <- with(test,mean((log_w-model_edad)^2))

## Especificacion mujer 
model_mujer <- lm(log_w~ mujer , data = base_filtrada2)
test$model_mujer<-predict(model_mujer,newdata = test)
m_mujer <- with(test,mean((log_w-model_mujer)^2))

## Especificacón 1
model1<-lm(log_w~estrato1+mujer+age+p6426+p6240,data=train)
test$model1<-predict(model1,newdata = test)
m1 <- with(test,mean((log_w-model1)^2))

## Especificacion 2
model2<-lm(log_w~estrato1+mujer+age+p6426+p6240+age_2,data=train)
test$model2<-predict(model2,newdata = test)
m2 <- with(test,mean((log_w-model2)^2))

## Especificacion 3
model3<-lm(log_w~poly(mujer,1,raw=TRUE):poly(age,2,raw=TRUE)+
             poly(p6426,2,raw=TRUE):poly(estrato1,1,raw=TRUE)+poly(p6240,2,raw=TRUE),data=train)
test$model3<-predict(model3,newdata = test)
m3 <- with(test,mean((log_w-model3)^2))

## Especificacion 4
model4 <- lm(log_w~poly(mujer,1,raw=TRUE)+poly(age,2,raw=TRUE)+
               poly(p6426,2,raw=TRUE):poly(p6240,3,raw=TRUE)+poly(estrato1,1,raw=TRUE):poly(maxEducLevel),data=train)
test$model4<-predict(model4,newdata = test)
m4 <- with(test,mean((log_w-model4)^2))

## Especificacion 5
model5 <- lm(log_w~poly(mujer,1,raw=TRUE):poly(p6240,3,raw=TRUE)+poly(age,1,raw=TRUE)+
               poly(age,2,raw=TRUE):poly(p6426,2,raw=TRUE):poly(maxEducLevel)+poly(estrato1,1,raw=TRUE),data=train)
test$model5<-predict(model5,newdata = test)
m5 <- with(test,mean((log_w-model5)^2))

## Tabla
stargazer::stargazer(model1, model2,model3,model4, model5, type = "text")

tabla_mse <- data.frame("Modelo" = c("Modelo Edad", "Modelo Mujer", "Modelo 1",
                                     "Modelo 2", "Modelo 3", "Modelo 4", "Modelo 5"),
                        "Valor MSE" = c(m_edad,m_mujer,m1,m2,m3,m4,m5))
apa.reg.table(model_edad, filename= "regresion.doc")


### D
#Make this example reproducible
set.seed(123)

# Specify the number of folds for
# 5-fold cross-validation
K <- nrow(base_filtrada2)

#Split the data set into 5 folds
index <- split(1:nrow(base_filtrada2), 1: K)
#aplicar a la lista de folds 1,2,3,4,5
splt <- lapply(1:K, function(ind) base_filtrada2[index[[ind]], ])
install.packages("data.table")
library(data.table)

## Modelo 5
m1 <- lapply(1:K, function(ii) lm(log_w~poly(mujer,1,raw=TRUE):poly(p6240,3,raw=TRUE)+poly(age,1,raw=TRUE)+
                                    poly(age,2,raw=TRUE):poly(p6426,2,raw=TRUE):poly(maxEducLevel)+poly(estrato1,1,raw=TRUE), data = rbindlist(splt[-ii]))) 
p1 <- lapply(1:K, function(ii) data.frame(predict(m1[[ii]], newdata = rbindlist(splt[ii]))))
for (i in 1:K) {
  colnames(p1[[i]])<-"yhat" #change the name
  splt[[i]] <- cbind(splt[[i]], p1[[i]])
  
}
MSE2_k <- lapply(1:K, function(ii) mean((splt[[ii]]$log_w - splt[[ii]]$yhat)^2))

MSE2_k

## Modelo 4
m2 <- lapply(1:K, function(ii) lm(log_w~poly(mujer,1,raw=TRUE)+poly(age,2,raw=TRUE)+
                                    poly(p6426,2,raw=TRUE):poly(p6240,3,raw=TRUE)+poly(estrato1,1,raw=TRUE):poly(maxEducLevel) , data = rbindlist(splt[-ii]))) 
p2 <- lapply(1:K, function(ii) data.frame(predict(m2[[ii]], newdata = rbindlist(splt[ii]))))
for (i in 1:K) {
  colnames(p2[[i]])<-"yhat" #change the name
  splt[[i]] <- cbind(splt[[i]], p2[[i]])
  
}
MSE2_k2 <- lapply(1:K, function(ii) mean((splt[[ii]]$log_w - splt[[ii]]$yhat)^2))

MSE2_k2