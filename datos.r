


#-------------------------------------
# Base de datos
datos<-read.csv2("Insectos.csv",row.names=1)

#-------------------------------------
# LIBRERÍAS REQUERIDAS
library(lattice)
library(ellipse)
require(SciViews)
require(stats)

# Estructura de la base de datos
str(datos)
summary(datos[,2:8])

# 1. Gráfica por pares
pairs(datos[,2:8])
pairs(log10(datos[,2:8]))


# 2.Figura elipses 
plotcorr(cor(datos[,2:9]))


# 3. Figuras de pares
pairs ((datos[,c(2:9)]),panel=function(x,y)
  {abline(lsfit(x,y)$coef,lwd=2,col=3)
  lines(lowess(x,y),lty=2,lwd=2,col=2)
  points(x,y,cex=1)})

pairs ((datos[,c(2,6,7,9)]),panel=function(x,y)
  {abline(lsfit(x,y)$coef,lwd=2,col=3)
  lines(lowess(x,y),lty=2,lwd=2,col=2)
  points(x,y,col=datos$cuenca, cex=1.4)})

# 4. 
pairs(datos[, 2:9], diag.panel = panel.hist, 
  upper.panel = panel.smooth, lower.panel = panel.cor)


# 5. Figura con tres variables (Función: coplot)
with(datos,coplot(Efem~pH|temp))

with(datos, {
  coplot(Efem~pH|temp, number = 3,
 panel = function(x, y, ...) panel.smooth(x, y, span = .8, ...))
  coplot(Efem~pH|temp,
 panel = panel.smooth)
})


# 6. Coplot 
summary(datos[,2:8])

clasetemp<-cut(datos$temp,seq(15,20,1.2),include.lowest=T)
clasetemp
clasepH<-cut(datos$pH,seq(5,8,1,include.lowest=T))
clasepH

coplot(Efem~pH | clasetemp, pch=19,
   panel = panel.lm, data=datos)

# 7. 
panel.lm = function(x, y, ...) {
  tmp<-lm(y~x,na.action=na.omit)
  abline(tmp, lwd = 1.5, col= 2)
  points(x,y, ...)}

coplot(Efem ~ pH | clasetemp, pch=19,
   panel = panel.lm, data=datos)


# Categorizando variables continuas
splom(~datos[,4:8]|clasepH,pscales=0)   
splom(~datos[,4:8]|clasepH+clasetemp,pscales=0)

# Reación por niveles del factor Cuenca
# 
datos$cuenca<-factor(datos$cuenca, 
 levels=c("cuen3","cuen4","cuen1","cuen2"))

# 7. Figuras xyplot 
xyplot(Efem~pH|cuenca,data=datos,
   panel = panel.lm, data=datos)


# 8. Histogramas  
histogram	(~Ab,data=datos, ylab="Porcentaje del Total",
   xlab="Abundancia de insectos")
x11()
histogram	(~Ab|cuenca,data=datos, ylab="Porcentaje del Total",
   xlab="Abundancia de insectos")


# 8. Figuras de densidad
densityplot(~Ab,data=datos, ylab="Porcentaje del Total",
xlab="Abundancia de insectos")  
densityplot(~Ab|cuenca,data=datos, ylab="Porcentaje del Total",
xlab="Abundancia de insectos")


# 9. qqplot
panel<-par(mfrow=c(1,2), mar=c(4,3,3,2))

# figura con datos crudos
qqnorm (datos$Ab, main="Abundancia de Insectos",
ylab="Cuantiles de la muestra",
xlab="Cuantiles teóricos") 
qqline(datos$Ab)

# 10. figura con raíz log de abndancias
Ab.log <- log10(datos$Ab+1)
qqnorm (Ab.log, main="Log de Abundancia de Insectos",
ylab="Cuantiles de la muestra",
xlab="Cuantiles teóricos") 
qqline(Ab.log)
par(panel)
panel<-par(mfrow=c(1,1))



# 11. 
plot(Efem~Plec,col=as.integer(cuenca),data=datos,ylab="",
 xlab="Plecópteros") 
legend(0,27,legend=levels(datos$cuenca),pch=19,col=1:4,cex=0.8)
lines(abline(lm(datos$Efem~datos$Plec),lwd=2,col=2, lty=2))
par(panel)



# 14. Colocar colores al grafico por cuenca
xyplot(Efem~Plec,group=cuenca,auto.key=T,data=datos)


# 15. 
plot(Efem~Plec,col=as.integer(cuenca),data=datos) 
legend(0,25,legend=levels(datos$cuenca),pch=19,col=1:4,cex=0.8)
lines(abline(lm(datos$Efem~datos$Plec),lwd=2,col=2, lty=2))



# 16. Figuras de Cajas y cinturas
datos$cuenca<-factor(datos$cuenca, 
 levels=c("cuen1","cuen2","cuen3","cuen4"))

boxplot(Ab~cuenca,data=datos,notch=TRUE,
xlab="Cuencas",ylab="Abundancia",
col="lightgray", cex.lab=1.3)



