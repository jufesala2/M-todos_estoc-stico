#1.La conductividad hidráulica en alguna ubicación no observada se modela con una distribución log-normal.
  #La media de Y = ln K es 2.0 y la varianza es 1.5. ¿Cuál es la media y la varianza de K?
mY<-2
vY<- 1.5
# Calcular la media de K
mK<- exp(mY + vY/2)
# Calcular la varianza de K
vK<- exp(2*mY + vY)*(exp(vY) - 1)
#####

#2. La conductividad hidráulica para un acuífero tiene una distribución lognormal con media 10 m/d y varianza 
#200 m²/d². ¿Cuál es la probabilidad de que en una ubicación no observada la conductividad sea mayor que 30 m/d?
library(stats) # Se importa la libreria stats para usar la funcion Probabilida de la disstribucio lognormal "plnorm"
Km <- 30 # COnductividad hidraulica del medio
mK <- 10 # Media de K
vK <- 200 # varianza de K
dsK <- sqrt(vK)# Desviación estandar
P <-(1- plnorm(30,mK,dsK,TRUE))*100 # funcion probabilidad de K<30m^2/día^2
#####

#3. Basado en un análisis geológico, se obtuvieron las siguientes probabilidades de clases de textura que
#ocurren en un acuífero: Pr[arena]=0.7, Pr[arcilla]=0.2, Pr[turba]=0.1. La siguiente tabla muestra las
#distribucion es de probabilidad de las clases de conductividad para las tres texturas
library(readxl)
valores <- read_excel("Maestria-recursos-hidrauilicos/3-Tercer-Semestre/Metodos_estocasticos/Trabajos/2-trabajo/valores.xlsx")
prob_K <- as.matrix(valores)
# Imprimir la matriz resultante
print(valores)
Vec_Pro <- c(0.7,0.2,0.1)
MatrizPro_acuifero=(prob_K*Vec_Pro) # Producto entre la probabilidad de la conductividad y la Probabilidad del tipo acuífero
prob_K <- colSums(MatrizPro_acuifero)
# Crea un gráfico de barras para representar la distribución de probabilidad de la conductividad
barplot(prob_K, main = "Distribución de probabilidad de K",xlab = "Conductividad m^2/día^2", ylab = "Probabilidad", 
        col = "skyblue", border = "black", ylim = c(0, 1))
print(prob_K)
#####
#4. Considerar dos variables aleatorias Z1 y Z2 con media 10 y 25 y varianzas 300 y 450 respectivamente. El
#coeficiente de correlación entre ambas variables es igual a 0.7
#a. Calcular la covarianza entre Z1 y Z2.
mZ1 <- 10
mZ2 <- 25
vZ1 <- 300
vZ2 <- 450
Coef_corr=0.7
# CoVarianza= Coe_Correlacion(Z1,Z2)/desviacion estandar (Z1+Z2)
CovarianzaZ1Z2=Coef_corr/sqrt(vZ1+vZ2)
print(CovarianzaZ1Z2)
#b. Calcular el valor esperado de Y = Z1 + Z2.
#El valor esperado corresponde a la suma del valor medio de cada variable aleatoria
#E(Y)=E(Z1)+E(Z2)
Y=mZ1+mZ2
print(Y)
#c. Calcular la varianza de Y = Z1 + Z2.
varY <- vZ1 + vZ2 + (2 * CovarianzaZ1Z2)
print(varY)
#5