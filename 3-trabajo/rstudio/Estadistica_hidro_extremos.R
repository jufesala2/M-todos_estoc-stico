library(ggplot2)
valores <- read_excel("~/Maestria-recursos-hidraulicos/3-Tercer-Semestre/Metodos_estocasticos/Trabajos/3-trabajo/datos.xlsx")
SerieQmax <- as.matrix(valores)
# 1. Pruebe si estos datos pueden ser modelados como variables estocasticas independientes
x <- SerieQmax$Qmax
       von_neumann_test <- function(x) {
         n <- length(x)
         x_lagged <- x[-n]
         x <- x[-1]
         r <- cor(x, x_lagged)
         statistic <- n * (r^2)
         critical_value <- qchisq(0.95, df = 1) # 0.95 de nivel de significancia
         
         result <- list(Statistic = statistic, 
                        Critical_Value = critical_value, 
                        Test_Result = ifelse(statistic > critical_value, "Rechazar H0 (Dependiente)", "No rechazar H0 (Independiente)"))
######
#2. Pruebe si puede detectar una tendencia
         
             rango <- function(x) sum(outer(x, x, ">"))
         
         # Calcula el estadístico de Mann-Kendall
         estadistico_MK <- function(x) {
           n <- length(x)
           S <- 0
           for (i in 1:(n-1)) {
             for (j in (i+1):n) {
               S <- S + sign(x[j] - x[i])
             }
           }
           return(S)
         }
         
         # Calcula la varianza del estadístico de Mann-Kendall
         varianza_MK <- function(x) {
           n <- length(x)
           sum_term <- 0
           for (i in 1:(n-1)) {
             for (j in (i+1):n) {
               sum_term <- sum_term + sign(x[j] - x[i])^2
             }
           }
           return((n * (n - 1) * (2 * n + 5) - sum_term) / 18)
         }
         
         # Calcula el test de Mann-Kendall
         test_MK <- function(x, alpha = 0.05) {
           n <- length(x)
           S <- estadistico_MK(x)
           V <- varianza_MK(x)
           
           # Estadístico de prueba Z
           Z <- ifelse(S > 0, (S - 1) / sqrt(V), ifelse(S < 0, (S + 1) / sqrt(V), 0))
           
           # Valor crítico para Z (distribución normal estándar)
           valor_critico <- qnorm(1 - alpha/2)
           
           # Determina si se rechaza la hipótesis nula
           if (abs(Z) > valor_critico) {
             conclusion <- "Se rechaza la hipótesis nula (hay tendencia)"
           } else {
             conclusion <- "No se rechaza la hipótesis nula (no hay tendencia)"
           }
           
           resultado <- list(Estadistico_MK = S,
                             Varianza_MK = V,
                             Estadistico_Z = Z,
                             Valor_Critico = valor_critico,
                             Conclusion = conclusion)
           
           return(resultado)
         }
         
         # Llama a la función test_MK con tu serie temporal
         resultado_test_MK <- test_MK(x)
         print(resultado_test_MK)
#####
#3. Grafique los datos en papel gumbel y determine los parametros de distribucin gumbel
        datos <- SerieQmax$Qmax
         # Instalar y cargar el paquete extRemes
        
         library(extRemes)
         
           # Graficar los datos en un papel de probabilidad Gumbel
         qqplot(datos, distribution = qgumbel, main = "Gráfico Q-Q Gumbel", xlab = "Cuantiles teóricos", ylab = "Cuantiles observados")
         
         # Ajustar la distribución Gumbel a los datos
         fit_gumbel <- gpd.fit(datos, threshold = 0, distribution = "gumbel")
         
         # Mostrar un resumen del ajuste para obtener los parámetros estimados de la distribución Gumbel
         summary(fit_gumbel)
         
         # Calcular la estimación de la inundación de 1000 años
         inundacion_1000_anios <- qgumbel(1 - 1/1000, location = fit_gumbel$par.ests[1], scale = fit_gumbel$par.ests[2])
         inundacion_1000_anios
         
         