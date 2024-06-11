library("readxl")
library(caret)
library(fastDummies)
library(caret)
library(ROCR)
library(unbalanced)
library(pROC)

telco<-read_excel("C:\\Users\\danyg\\OneDrive\\Documents\\Javeriana Maestria\\Primer Semestre\\Metodo Analítica 1\\Taller 4\\retencion-en-telefonia-movil-2024-10\\traintelco.xlsx")
telco_test<-read_excel("C:\\Users\\danyg\\OneDrive\\Documents\\Javeriana Maestria\\Primer Semestre\\Metodo Analítica 1\\Taller 4\\retencion-en-telefonia-movil-2024-10\\testelco.xlsx")


#Cambio de variables
fecha_actual <- Sys.Date() # Obtener la fecha actual
telco$edad <- as.integer(difftime(fecha_actual, telco$"Fecha de nacimiento", units = "days") / 365.25) # Calcula la edad en años
telco$añosConServicio <- as.integer(difftime(fecha_actual, telco$"Fecha inicio contrato", units = "days") / 365.25) # Calcula la edad en años
telco <- dummy_cols(telco, select_columns = "tipo cliente")

telco_test$edad <- as.integer(difftime(fecha_actual, telco_test$"Fecha de nacimiento", units = "days") / 365.25) # Calcula la edad en años
telco_test$añosConServicio <- as.integer(difftime(fecha_actual, telco_test$"Fecha inicio contrato", units = "days") / 365.25) # Calcula la edad en años
telco_test <- dummy_cols(telco_test, select_columns = "tipo cliente")


#Eliminación de variables
id_test<-telco_test$id
id <- telco$id
telco <- telco[, !(names(telco) %in% c("id","Fecha de nacimiento", "Fecha inicio contrato", "tipo cliente", "tipo cliente_3"))]
telco_test <- telco_test[, !(names(telco_test) %in% c("id","Fecha de nacimiento", "Fecha inicio contrato", "tipo cliente", "tipo cliente_3"))]


#cell_log<-apply(telco[,c(4,6)],2,log1p)
#telco<-as.data.frame(cbind(cell_log,telco[,c(1:3,5,7:11)]))

#cell_log_test<-apply(telco_test[,c(4,6)],2,log1p)
#telco_test<-as.data.frame(cbind(cell_log_test,telco_test[,c(1:3,5,7:10)]))


set.seed(1545867) #
#aquí se define el tamaño de la muestra, en este caso entrenamiento tendrá el 75% de los casos
sample <- sample.int(nrow(telco), floor(0.8*nrow(telco)))
telco.tr <- telco[sample, ]
telco.te <- telco[-sample, ]

#intento 2 eliminacion variables no significativas
#telco <- telco[, !(names(telco) %in% c("Factura online","Plan de datos"))]
#telco_test <- telco_test[, !(names(telco_test) %in% c("Factura online","Plan de datos"))]

#"Antigüedad Equipo"


#Overbalancing
#balanceover<-ubOver(telco.tr[,c(1:6,8:11)],telco.tr$resultado)
#ver resultados
#table(balanceover$Y)

#Underbalancing
balanceover<-ubUnder(telco[,c(1:6,8:11)],telco$resultado)
#ver resultados
table(balanceover$Y)

modelo.logit<-glm(balanceover$Y~.,family=binomial,balanceover$X)
#trace=0 impide ver todos los detalles de la optimización stepwise
steplogit<-step(modelo.logit, direction="both", trace=0)
summary(steplogit)


#Prediccion

probtr<-predict(steplogit,newdata = balanceover$X,type='response')
prontr<-ifelse(probtr > 0.59,1,0)
conftest2<-confusionMatrix(as.factor(prontr),as.factor(balanceover$Y), positive = "1")
conftest2$table
conftest2$byClassS

probtest<-predict(steplogit,newdata = telco.te,type='response')
prontest<-ifelse(probtest > 0.59,1,0)

probtest_real<-predict(steplogit,newdata = telco_test,type='response')
prontest_real<-ifelse(probtest_real > 0.59,1,0)
conftest2<-confusionMatrix(as.factor(prontest),as.factor(telco.te$resultado), positive = "1")
conftest2$table
conftest2$byClass

roc_objeto <- roc(telco.te$resultado, probtest)
# Calcular el AUC
auc <- auc(roc_objeto)
# Graficar la curva ROC
plot(roc_objeto, main = "Curva ROC", col = "blue")
# Imprimir el AUC
print(auc)

prontest_real <- as.character(prontest_real)
prontest_real <- paste0(" ", prontest_real)
resultados_prediccion <- data.frame(id = id_test, resultado = prontest_real)
write.csv(resultados_prediccion, "C:\\Users\\danyg\\OneDrive\\Documents\\Javeriana Maestria\\Primer Semestre\\Metodo Analítica 1\\Taller 4\\retencion-en-telefonia-movil-2024-10\\prediccion.csv", row.names = FALSE,  quote = FALSE)

write.csv(telco, "C:\\Users\\danyg\\OneDrive\\Documents\\Javeriana Maestria\\Primer Semestre\\Metodo Analítica 1\\Taller 4\\retencion-en-telefonia-movil-2024-10\\graficos.csv", row.names = FALSE)



