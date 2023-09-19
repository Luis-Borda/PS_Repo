############ SEGUNDO PUNTO #################
### Imputar NA Salario
install.packages("mice")
library(mice)
columns <- colnames(base_filtrada)
base_filtrada2 <- mice(base_filtrada[,names(base_filtrada) %in% columns],m = 1,
                       maxit = 1, method = "norm.nob",seed = 2018,print=F)

base_filtrada2 <- mice::complete(base_filtrada2)
par(mfrow=c(1,1))
plot(density(base_filtrada$log_w,na.rm = T),col=2,main="Salario Por Hora")
lines(density(base_filtrada2$log_w),col=3)

