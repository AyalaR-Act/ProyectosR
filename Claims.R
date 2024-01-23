install.packages("spuRs")
install.packages("readxl")
install.packages("modeest")
library(spuRs)
library(readxl)
library(modeest)
C1<-read_excel(file.choose(),sheet=1)
# C1 <- read_excel("D:/Claim.xlsx")
hist(C1$claim
     ,ylab = 'Frecuencia'
     ,xlab = 'Total de la Reclamación'
     ,main = 'Reclamación de pólizas de Seguro Médico'
     ,col = blues9 )
mean(C1$claim)
length(C1$claim)
median(C1$claim)
var(C1$claim)
sd(C1$claim)

#Ajuste a una distribución paramética
install.packages("fitdistrplus")
library(fitdistrplus)
descdist(data = C1$claim, graph = FALSE)
D1 <- fitdist(C1$claim, distr = "lnorm")
summary(D1)
plot(D1)

#Paqueteria para Graficar las distribuciones
install.packages("ggplot2")
library(ggplot2)

lnorm <- denscomp(
  list(D1),
  legendtext = c("log-normal"),
  xlab = "Total de las reclamaciones",
  #xlim = c(0, 250), 
  fitcol = c("red"),
  fitlty = 1, 
  xlegend = "topright",
  plotstyle = "ggplot",
  addlegend = FALSE
)

lngraph <- lnorm +
  ggplot2::ggtitle("Distribución de Reclamciones Seguro Médico") +
  theme_bw() +
  theme(legend.position = "bottom")

lngraph

#Comprobemos el ajuste gráficamente

Ajuslnorm <- cdfcomp(
  list(D1),
  legendtext = c("log-normal"),
  xlab = "serving sizes (g)",
  #xlim = c(0, 250), 
  fitcol = c("red"),
  fitlty = 1, 
  xlegend = "topright",
  plotstyle = "ggplot",
  addlegend = FALSE
)

Ajuslnormgraph<- Ajuslnorm +
  ggplot2::ggtitle("Distribución de Reclamaciones") +
  theme_bw() +
  theme(legend.position = "bottom")

Ajuslnormgraph

#------------------------------------------------------------------------------------------------------#

# Distribución log-nomral
dist_lnorm <- fitdist(C1$claim, distr = "lnorm")
dist_lnorm
# Distribución weibull
dist_weibull <- fitdist(C1$claim, distr = "weibull")
dist_weibull

comparacion <- gofstat(f = list(dist_lnorm, dist_weibull))
comparacion

Graphcompare <- denscomp(
  list(dist_lnorm, dist_weibull),
  legendtext = c("lognormal", "Weibull"),
  ylab = "Frecuencia",
  xlab = "Total de la reclamación",
  fitcol = c("red", "blue"),
  fitlty = 1, 
  xlegend = "topright",
  plotstyle = "ggplot",
  addlegend = FALSE)
Graphcompare <- Graphcompare +
  ggplot2::ggtitle("Distribución de las reclamaciones de Seguro Médico") +
  theme_bw() +
  theme(legend.position = "bottom")
Graphcompare

#-----------------------------------------------------------------------------------------#
#Defianamos una tercera funcion de distribución
dist_norm <- fitdist(C1$claim, distr = "norm")
dist_norm

Graphcompare2 <- denscomp(
  list(dist_lnorm, dist_weibull, dist_norm),
  legendtext = c("lognormal", "Weibull","Normal"),
  ylab = "Frecuencia",
  xlab = "Total de la reclamación",
  fitcol = c("red", "blue","orange"),
  fitlty = 1, 
  xlegend = "topright",
  plotstyle = "ggplot",
  addlegend = FALSE)
Graphcompare2 <- Graphcompare2 +
  ggplot2::ggtitle("Distribución de las reclamaciones de Seguro Médico") +
  theme_bw() +
  theme(legend.position = "bottom")
Graphcompare2
#*******************************#
#Pruebas de Bondad de ajuste#
par(mfrow = c(2, 2))
plot.legend <- c("Normal", "Weibull", "Log-Normal")
denscomp(list(dist_norm, dist_weibull, dist_lnorm), legendtext = plot.legend)
qqcomp(list(dist_norm, dist_weibull, dist_lnorm), legendtext = plot.legend)
cdfcomp(list(dist_norm, dist_weibull, dist_lnorm), legendtext = plot.legend)
ppcomp(list(dist_norm, dist_weibull, dist_lnorm), legendtext = plot.legend)
gofstat(list(dist_norm, dist_weibull, dist_lnorm), fitnames = c("Normal", "Weibull", "Log-Normal"))
#------------------------------------------------------------------------------------#
# Dado que notamos un mejor ajuste en la distribución Weibull veamos el ajuste gradicamente
G1 <- fitdist(C1$claim, distr = "weibull")
summary(G1)
plot(G1)

lwei <- denscomp(
  list(G1),
  legendtext = c("Weibull"),
  xlab = "Total de las reclamaciones",
  #xlim = c(0, 250), 
  fitcol = c("blue"),
  fitlty = 1, 
  xlegend = "topright",
  plotstyle = "ggplot",
  addlegend = FALSE
)

lweigraph <- lwei +
  ggplot2::ggtitle("Distribución de Reclamciones Seguro Médico") +
  theme_bw() +
  theme(legend.position = "bottom")

lweigraph

Ajuslwei <- cdfcomp(
  list(G1),
  legendtext = c("Weibull"),
  xlab = "Total de las reclamaciones",
  #xlim = c(0, 250), 
  fitcol = c("blue"),
  fitlty = 1, 
  xlegend = "topright",
  plotstyle = "ggplot",
  addlegend = FALSE
)

Ajuslwiegraph<- Ajuslwei +
  ggplot2::ggtitle("Distribución de Reclamaciones") +
  theme_bw() +
  theme(legend.position = "bottom")

Ajuslwiegraph

#-------********************************************************************#

#Elaboremos ahora la estimación no paramétrica

install.packages("KernSmooth")

library(KernSmooth)

ms <- c(C1$claim)

# Establece el valor de gamma o ajusta el ancho de banda según sea necesario
Ancho_banda <- 800  
#Prueba y error pero se estima grande por los datos
gridsize_valor <- 1500
# Utiliza la función bkde para estimar la densidad de probabilidad
densidad_estimada <- bkde(ms, bandwidth = Ancho_banda, gridsize = gridsize_valor)

# Gráfica  de los resultados
plot(density(ms), main = "Estimación de Densidad no paramétrica
     Kernel", col = "blue")
lines(densidad_estimada, col = "red")
legend("topright", legend = c("Sin Suavizado", "Con Suavizado"), col = c("blue", "red"), lty = 1)




