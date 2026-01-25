################################################################################
#2. Scale
################################################################################

#Instalar paquetes:

#tidyverse = Manejo de datos
#terra = Manejo de datos ráster
#sf = Manejo de simple features (shapes)
#landscapemetrics = Métricas de paisaje

#install.packages("tidyverse")
#install.packages("terra", "sf", "landscapemetrics")


#Instalar el paquete scalescape

#library(remotes)
#remotes::install_github("benjaminiuliano/scalescape")

library(tidyverse)
library(terra)
library(sf)
library(landscapemetrics)
library(scalescape)

#Crear un raster vacío y luego llenar sus celdas con valores generados de manera
#aleatoria con una distribución de Poisson

#Luego crear un raster de 6x6 donde se especifíquen las coordenadas mínimas y
#máximas

set.seed(16)
toy <- rast(ncol=6, nrow=6, xmin=1, xmax=6, ymin=1, ymax=6) #Crea el ráster y 
#especifica sus dimensiones, sin valores
values(toy) <- rpois(ncell(toy), lambda = 3) #Especifica los valores asumiendo
#una distribución de Poisson. Lambda es el valor de la media.
plot(toy)  #Gráfica del raster
text(toy, digits=2)


#Muestra los valores dentro de la gráfica
writeRaster(toy, "raster/toy.tif", overwrite=TRUE) #Exportar el raster

#Crear un nuevo raster (toy2) con una nueva organización de los valores 
#(De 1 a 36)


toy2 <- toy
values(toy2) <- 1:ncell(toy)
plot(toy2)
text(toy2, digits=2)

writeRaster(toy2, "raster/toy2.tif", overwrite=TRUE)

#Para modificar el tamaño del grano, se utiliza la función aggregate().
#Los valores se pueden agregar por la media de los valores de las celdas o por
#"regla de la mayoría".

#Cuando se especifica el factor de agregación = 2, agrupa las celdas originales
#de a 2 x 2 y obtiene su promedio (fun = "mean")

toy_mean_2 <- aggregate(toy, fact=2, fun="mean")
plot(toy_mean_2)
text(toy_mean_2, digits=2)

writeRaster(toy_mean_2, "raster/toy_mean_2.tif", overwrite=TRUE)

#Cuando se especifica el factor de agregación = 3, agrupa las celdas originales
#de a 3 x 3 y obtiene su promedio (fun = "mean")
toy_mean_3 <- aggregate(toy, fact=3, fun= "mean")
plot(toy_mean_3)
text(toy_mean_3, digits=2)

writeRaster(toy_mean_3, "raster/toy_mean_3.tif", overwrite=TRUE)


#Exportar imagen

png("images/agg_toy_mean.png", width = 20, height = 20, units="cm", res = 600)

par(mfrow=c(2,2))
plot(toy)  #Gráfica del raster
text(toy, digits=2)
plot(toy2)
text(toy2, digits=2)
plot(toy_mean_2)
text(toy_mean_2, digits=2)
plot(toy_mean_3)
text(toy_mean_3, digits=2)

dev.off() # Esto cierra y guarda el archivo
par(mfrow = c(1, 1))

#Regla de mayoría

#Agrupado por 2

toy_maj_2 <- aggregate(toy,fact=2, fun="modal")
plot(toy_maj_2)
text(toy_maj_2, digits=2)


#Agrupado por 3

toy_maj_3 <- aggregate(toy,fact=3, fun="modal")
plot(toy_maj_3)
text(toy_maj_3, digits=2)

png("images/agg_toy_maj.png", width = 20, height = 10, units="cm", res = 600 )
par(mfrow=c(1,2))
plot(toy_maj_2)
text(toy_maj_2, digits=2)
plot(toy_maj_3)
text(toy_maj_3, digits=2)
dev.off() # Esto cierra y guarda el archivo
par(mfrow = c(1, 1))

#Es posible evaluar estadísticas descriptivas del raster mediante la función
#global. En este caso, el raster original y el agregado tienen la misma media,
#pero la varianza disminuye cuando se aumenta el tamaño del grano (agregado)
global(toy,mean)
global(toy,var)

global(toy_mean_2,mean)
global(toy_mean_2,var)


#Desagregación

#Se utiliza la función disagg para desagregar las celdas del raster. Existen
#dos métodos. El primero por replicación (todos los valores originales se
#repiten) y el segundo por interpolación bilinear (se toman valores de la media
#ponderada por distancia en los ejes x y y). El primer método se utiliza para
#variables categóricas y el segundo para variables continuas.


#Raster original
plot(toy)
text(toy,digits=2)


#Replicación
toy_dis2 <- disagg(toy,fact=2)
plot(toy_dis2)
text(toy_dis2, digits=2)


#Método bilinear
#Nótese que se generan gradientes

toy_dis2_bilin <- disagg(toy, fact=2, method="bilinear")
plot(toy_dis2_bilin)
text(toy_dis2_bilin, digits=2)

#Exportar imagen de comparación
png("images/desag_toy.png", width = 30, height = 12, units="cm", res=300)
par(mfrow=c(1,3), mar=c(3,3,3,4))
plot(toy)
text(toy,digits=2, cex=0.8)
plot(toy_dis2)
text(toy_dis2, digits=2, cex=0.8)
plot(toy_dis2_bilin)
text(toy_dis2_bilin, digits=2, cex=0.8)
dev.off()
par(mfrow=c(1,1))

#Es posible disminuir y aumentar el tamaño del raster mediante las funciones
#crop y extent

#Raster original
plot(toy)
text(toy,digits=2)


e  <- ext(2,4,2,4) #Los argumentos son xmin,xmax,ymin,ymax. Primero se
#define la extensión que se va a recortar
toy_crop <- crop(toy,e) #Con la función crop, se especifica el raster original y 
#la extensión que se va a recortar
plot(toy_crop)
text(toy_crop, digits=2)


png("images/crop_toy.png", width = 30, height =10, units="cm", res=300)
par(mfrow=c(1,2))
plot(toy)
text(toy,digits=2)
plot(toy_crop)
text(toy_crop, digits=2)
dev.off()

