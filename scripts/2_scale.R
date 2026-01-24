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

set.seed(16) #C
toy <- rast(ncol=6, nrow=6, xmin=1, xmax=6, ymin=1, ymax=6) #Crea el ráster y 
#especifica sus dimensiones, sin valores
values(toy) <- rpois(ncell(toy), lambda = 3) #Especifica los valores asumiendo
#una distribución de Poisson. Lambda es el valor de la media.
plot(toy)#Gráfica del raster
text(toy, digits=2)#Muestra los valores dentro de la gráfica
writeRaster(toy, "raster/toy.tif", overwrite=TRUE) #Exportar el raster

#Crear un nuevo raster (toy2) con una nueva organización de los valores 
#(De 1 a 36)


toy2 <- toy
values(toy2) <- 1:ncell(toy)
plot(toy2)
text(toy2, digits=2)
writeRaster(toy2, "raster/toy2.tif", overwrite=TRUE)





