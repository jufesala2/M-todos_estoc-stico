#1 es 2.0 y la varianza es 1.5
#¿Cuál es la media y la varianza de K?
mY<-2 # .La media de Y = ln K
vY<- 1.5
# Calcular la media de K
mK<- exp(mY + vY/2)
# Calcular la varianza de K
vK<- exp(2*mY + vY)*(exp(vY) - 1)
#####

#2. La conductividad hidr?ulica para un acu?fero tiene una distribuci?n lognormal con media 10 m/d y varianza 
#200 m?/d?. ?cual es la probabilidad de que en una ubicaci?n no observada la conductividad sea mayor que 30 m/d?
library(stats) # Se importa la libreria stats para usar la funcion Probabilida de la disstribucio lognormal "plnorm"
Km <- 30 # COnductividad hidraulica del medio
mK <- 10 # Media de K
vK <- 200 # varianza de K
dsK <- sqrt(vK)# Desviación estandar
P <-(1- plnorm(30,mK,dsK,TRUE))*100 # funcion probabilidad de K<30m^2/d^2
#####

#3. Pr[arena]=0.7, Pr[arcilla]=0.2, Pr[turba]=0.1.

library(readxl)
valores <- read_excel("Maestria-recursos-hidraulicos/3-Tercer-Semestre/Metodos_estocasticos/Trabajos/2-trabajo/valores.xlsx")
prob_K <- as.matrix(valores)
print(valores)
Vec_Pro <- c(0.7,0.2,0.1)

# Producto entre la probabilidad de la conductividad y la Probabilidad del tipo acuifero
MatrizPro_acuifero=(prob_K*Vec_Pro) 
prob_K <- colSums(MatrizPro_acuifero)
print(prob_K)

# Crea un gr?fico de barras para representar la distribuci?n de probabilidad de la conductividad
barplot(prob_K, main = "Distribucion de probabilidad de K",xlab = "Conductividad m^2/d^2", ylab = "Probabilidad", 
        col = "skyblue", border = "black", ylim = c(0, 1))
print(prob_K)

#####
#4. Considerar dos variables aleatorias Z1 y Z2

mZ1 <- 10 # media de Z1
mZ2 <- 25 # media de Z2
vZ1 <- 300 # Varianza de Z1
vZ2 <- 450 # Varianza de Z2
Coef_corr=0.7 # Coeficiente de Correlación

#a. Calcular la covarianza entre Z1 y Z2.
# Covarianza
CVZ1Z2=Coef_corr/sqrt(vZ1+vZ2)
print(CVZ1Z2)

#b. Calcular el valor esperado de Y = Z1 + Z2.
#El valor esperado corresponde a la suma del valor medio de cada variable aleatoria
Y=mZ1+mZ2
print(Y)

#c. Calcular la varianza de Y = Z1 + Z2.
varY <- vZ1 + vZ2 + (2 * CVZ1Z2)
print(varY)

#####
#5

# Parámetros de la distribución bivariada
mZ1 <- 10 # media de Z1
mZ2 <- 25 # media de Z2
vZ1 <- 300 # Varianza de Z1
vZ2 <- 450 # Varianza de Z2
DSZ1 <- sqrt(vZ1) # Desviación estandar de Z1
DSZ2 <- sqrt(vZ2) # Desviación estandar de Z2
Coef_corr <- 0.7 # Coeficiente de Correlación
Covarianza <- CVZ1Z2 # Covarianza de Z1 - Z2

# a. Calcular Pr[Z1 < 30]
prob_30<- pnorm(30, mean = mZ1, sd = DSZ1)
cat("Pr[Z1 < 30]:", prob_30, "\n")

# b. Calcular Pr[Z2 < 40]
prob_40 <- pnorm(40, mean = mZ2, sd = DSZ2)
cat("Pr[Z2 < 40]:", prob_40, "\n")

# c. Calcular la probabilidad Pr[Z1 + Z2 < 50]
mZ1Z2 <- mZ1 + mZ2
VZ1Z2 <- vZ1 + vZ2 + 2 * Coef_corr * sqrt(vZ1) * sqrt(vZ2)
DSZ1Z2 <- sqrt(VZ1Z2)

prob_50 <- pnorm(50, mean = mZ1Z2, sd = DSZ1Z2)
cat("Pr[Z1 + Z2 < 50]:", prob_50, "\n")

# d. Calcular la probabilidad Pr[Z1 < 30 ⋂ Z2 < 40]


prob_30_inter_40 <- prob_30*prob_40
cat("Pr[Z1 < 30 ⋂ Z2 < 40]:", prob_30_inter_40, "\n")

# e. Calcular la probabilidad Pr[Z1 < 30 ⋃ Z2 < 40]

prob_30_union_40 <- prob_30 + prob_40 - prob_30_inter_40
cat("Pr[Z1 < 30 ⋃ Z2 < 40]:", prob_30_union_40, "\n")

#####
#6



