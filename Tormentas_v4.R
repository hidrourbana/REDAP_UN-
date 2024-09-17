# Lectura de datos

datos_estaciones <- read.csv("data/Datos_Finales.csv", header=TRUE)
x<-as.POSIXct(datos_estaciones$Fecha,format="%Y-%m-%d %H:%M",origin=min(datos_estaciones$Fecha))
datos_estaciones$Fecha<-x

library(lubridate)

# Modificación de fechas
x1<-as.POSIXct("2007-10-08 00:00:00")
x2<-as.POSIXct("2007-10-10 00:00:00")

limi=match(as.POSIXct(x1),datos_estaciones$Fecha)
lims=match(as.POSIXct(x2),datos_estaciones$Fecha)

prueba <- datos_estaciones[(limi):(lims),c(1,12)]

# Duracion
t<-20
d<-0.01
# Duración entre eventos
e<-100

# Devuelve en el vector index la posición de los elementos que satisfacen la condición
index <- which(prueba[,2] > d)
 
# Cuenta el número de elementos que cumple la condicion anterior (longitud del vector)
vindex <- length(index)

# Definición de variable que determina cuantas tormentas tienen una duración por encima de x tiempo (x=15)

matches<-c()
a<-0
# Conteo de la primera secuencia del vector index
if (vindex[1]!=0){
  a=a+1
  matches[a]<-c(1)
}

# Para cada reinicio de secuencia de numeros evalúa si los x siguientes numeros son una secuencia o no

for (i in (2:(vindex-t))){
  if ((index[i]>(index[i-1]+e))){
    a=a+1
    matches[a]<-c(i) 
  }
}

a2<-0
matches2<-c()
for (i in (1:(vindex-1))){
  if ((index[i]<(index[i+1]-e))){
    a2 = a2+1
    matches2[a2]<-c(i)
  }
}

if (index[vindex]==(index[vindex-t]+t)){
  matches2[length(matches2)+1]<-vindex
}

matches_index<-index[c(matches)]
matches2_index<-index[c(matches2)]
tormenta_inicio<-prueba[c(matches_index),]
tormenta_final<-prueba[c(matches2_index),]
vtormenta<-nrow(tormenta_inicio)
fechas<-prueba[c(matches_index),1]

tormenta<-as.POSIXct(c())

c2<-0
tormenta_cumple<-c()
for (i in 1:vtormenta) {
  if (matches2[i]-matches[i]>t){
    tormenta[matches_index[i]:matches2_index[i]]<-prueba[matches_index[i]:matches2_index[i],1]
    c2=c2+1
    tormenta_cumple[c2]<-c(i)
  }
}

tormenta<- na.omit(tormenta)

graf_tormentas<-prueba[c(match(tormenta,prueba$Fecha)),]

plot(prueba$Fecha,
     prueba[,2],
     type="h",
     xlab="Fecha",
     ylab="Precipitacion (mm)",
     col="cornflowerblue")
lines(graf_tormentas$Fecha,
      graf_tormentas[,2],
      type="h",
      col="red")
legend("topright",inset=0.05, legend=c("Precipitacion(mm)","Tormenta(mm)"),
       col=c("cornflowerblue", "red"), lty=1:1, cex=0.8,lwd=2,box.lty=0)

fechas<-fechas[c(tormenta_cumple)]

df_tormentas<-data.frame("No.Tormenta"=(1:length(fechas)),
                         "Dia"=as_date(fechas),
                         "Hora"=strftime(fechas, format="%H:%M"))

print(paste0("La cantidad de tormentas cuyo valor supera los ",d,"mm es de: ",c2," tormentas"))
