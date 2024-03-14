library(tidyverse)
library(zoo)
Qdia <- read_excel("~/Maestria-recursos-hidraulicos/3-Tercer-Semestre/Metodos_estocasticos/Trabajos/5-trabajo/Rstudio/Qdia.xlsx")
Qdia$fecha <- as.Date(Qdia$fecha)
plot(Qdia,main="Caudale diarios Rio Lebrija", xlab="Fecha", ylab="Caudales")

serie_temporal <- zoo(Qdia$Q, order.by = Qdia$fecha)
#Caudales medios mensuales
Qmm <- aggregate(serie_temporal, as.yearmon, mean)
# Caudales medios anuales
#Qma <- aggregate(serie_temporal, as.yearmon, mean, FUN="mean")

#####
#2.
