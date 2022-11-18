setwd("~/Workspace Universidad/Proyecto-Modelos")

library(ggplot2)
library(actuar)
library(survival)
library(MASS)
library(fitdistrplus)
library(moments)
library(nortest)
library(sandwich)
library(momentfit)
library(grid)
library(vcd)
library(fdth)
library(openxlsx)
library(kdensity)

db = read.csv("./db.csv")

db_clean = data.frame("Monto Pagado"=db$MONTO.PAGADO/10000)
db_clean = db_clean[c(1:100),]
print(paste("El número total de datos NA es: ",sum(is.na(db_clean))))
print(paste("El número total de datos iguales a 0 es: ",sum(db_clean==0)))
db_clean = data.frame("Monto Pagado"=db_clean)
db_clean = db_clean[db_clean>0,]
data = data.frame("Monto Pagado"=db_clean)

summary(data$Monto.Pagado)
skewness(data$Monto.Pagado)
kurtosis(data$Monto.Pagado)

plot(density(data$Monto.Pagado), main="Densidad de probabilidad")

#LogNormal
{
  fitlogn<-fitdist(data$Monto.Pagado, "lnorm", method = "mle")
  summary(fitlogn)
  confint(fitlogn)
  gofstat(fitlogn)
  
  mu<-fitlogn$estimate[1]
  sigmaln<-fitlogn$estimate[2]
  
  ks.test(data$Monto.Pagado, "plnorm", meanlog=mu, sdlog=sigmaln)
  ad.test(plnorm(data$Monto.Pagado, meanlog=mu, sdlog=sigmaln))
  
  cdfcomp(fitlogn)
}

#Pareto
{
  fitpar<-fitdist(data$Monto.Pagado, "pareto", method = "mle")
  
  summary(fitpar)
  confint(fitpar)
  gofstat(fitpar)
  
  alphha<-fitpar$estimate[1]
  sigmaln<-fitpar$estimate[2]
  
  ks.test(data$Monto.Pagado, "plnorm", meanlog=mu, sdlog=sigmaln)
  ad.test(plnorm(data$Monto.Pagado, meanlog=mu, sdlog=sigmaln))
  
  cdfcomp(fitpar)
}

#Weibull
{
  fitweibull<-fitdist(data$Monto.Pagado, "weibull", method="mle")
  summary(fitweibull)
  confint(fitweibull)
  gofstat(fitweibull)
  
  cdfcomp(fitweibull)
}

#LogGamma
{
  fitlgamma<-fitdist(data$Monto.Pagado, "lgamma", method="mle")
  summary(fitlgamma)
  confint(fitlgamma)
  gofstat(fitlgamma)
  
  cdfcomp(fitlgamma)
}

#LogLogistica
{
  fitllog<-fitdist(data$Monto.Pagado, "llogis", method="mle")
  summary(fitllog)
  confint(fitllog)
  gofstat(fitllog)
  
  cdfcomp(fitllog)
}


gofstat(list(fitlogn,fitpar,fitweibull,fitlgamma,fitllog), fitnames = c("Log-Normal", "Pareto","Weibull","Log-Gamma","Log-Logistica"))
gofstat(list(fitlogn,fitpar,fitweibull,fitlgamma,fitllog), fitnames = c("Log-Normal", "Pareto","Weibull","Log-Gamma","Log-Logistica"))[3]
gofstat(list(fitlogn,fitpar,fitweibull,fitlgamma,fitllog), fitnames = c("Log-Normal", "Pareto","Weibull","Log-Gamma","Log-Logistica"))[9]
gofstat(list(fitlogn,fitpar,fitweibull,fitlgamma,fitllog), fitnames = c("Log-Normal", "Pareto","Weibull","Log-Gamma","Log-Logistica"))[11]

#Kernel
kunif <- density(data$Monto.Pagado, kernel = "rectangular")
ktriang <- density(data$Monto.Pagado, kernel = "triangular")
kgamma <- kdensity(data$Monto.Pagado, kernel = "gamma")

#Graficamente

plot(density(data$Monto.Pagado), col = "green", lwd=4, main = "Monto Pagado")
lines(kunif, col = "blue", lwd=2)
lines(ktriang, col = "orange", lwd=2)
lines(kgamma, col = "red", lwd=2)
