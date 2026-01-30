################################################################################
#2.3.4.1. Análisis multiescala de la cobertura forestal
################################################################################

#Instalar paquetes:

#tidyverse = Manejo de datos
#terra = Manejo de datos ráster
#sf = Manejo de simple features (shapes)
#landscapemetrics = Métricas de paisaje
#tidyterra = Graficar objetos spatraster (terra)


#install.packages("tidyverse")
#install.packages("terra", "sf", "landscapemetrics")
#install.packages("tidyterra")

#Instalar el paquete scalescape

#library(remotes)
#remotes::install_github("benjaminiuliano/scalescape")

library(tidyverse)
library(terra)
library(sf)
library(landscapemetrics)
library(scalescape)
library(ggthemes)
library(tidyterra)
################################################################################
#Cargar datos espaciales, definir proyección e inspeccionar

nlcd <- rast("data/nlcd2015SE.tif") #Cargar raster con la función rast
crs(nlcd, proj=T)#Identificar la proyección espacial (CRS)

#Definir la proyección como un nuevo elemento

nlcd_proj <- crs(nlcd, proj= T)

#Para obtener la resolución de un raster se utiliza la función res()

res(nlcd)

#Para obtener la extensión (coordenadas x y y), se utiliza la función ext()
ext(nlcd)

#Obtener número de celdas
ncell(nlcd)


#Convertir a factor. Esto se realiza para facilitar la visualización de datos
#como categorías. 

nlcd <- as.factor(nlcd)
levels(nlcd)
#View(nlcd)

#Bosque deciduo = 41
#Bosque siempreverde = 42
#Bosque mixto = 43

################################################################################
#Cargar datos de reptiles

sites <- read_sf("data/reptiledata/reptiledata.shp")
class(sites)
sites

#Definir proyección con st_crs(). En este caso, la misma proyección del 
#raster nlcd

st_crs(sites) <- nlcd_proj

head(sites,2)

#Filtrar las coberturas de la tierra de sites, exceptuando "corn"

sites <- filter(sites, management != "Corn")
#View(sites)

################################################################################
#Recortar el raster nlcd, a 10 km de los márgenes de los valores mínimos y 
#máximos de sites

#Definir marco

x_min <- min(sites$coords_x1) - 10000
x_max <- max(sites$coords_x1) + 10000
y_min <- min(sites$coords_x2) - 10000
y_max <- max(sites$coords_x2) + 10000

#Nueva extensión

extent_new <- ext(x_min, x_max, y_min, y_max)
nlcd <- crop(nlcd, extent_new)
writeRaster(nlcd, "raster/nlcd_crop.tif")


#Visualizar mapa

ggplot() +
  geom_spatraster(data= nlcd) + #Nótese que es importante escribir de
  #manera explícita data
  geom_sf(data= sites, color="black") + #Nótese que es importante escribir de
  #manera explícita data
  theme_bw() +
  theme(legend.position = "none") 

 ggsave("images/nlcd_sites.png", plot=get_last_plot())


#Hacer un reclassify

#Crear una nueva capa para los bosques. En este caso se creará un raster
#"Bosque y no bosque". Todos los valores de bosque tendrán valores de 1
#y lo no bosque será igual a 0

forest <- nlcd #Asignar la misma estructura del ráster nlcd
values(forest) <- 0 #Pero que los valores sean 0
#forest

forest[nlcd==41 | nlcd==42 | nlcd==43] <- 1 #Asignar 1 a los valores 41,42 y 43
#de forest

#forest

#Otra forma de clasificar y reclasificar usando la función classify().
#Esta se recomienda porque es más eficiente computacionalmente.

#Classify

#Identificar los niveles y su orden en el listado
levels(nlcd[[1]])

#Crear lista de valores para reclass.
#1.rep(0,6) = Asignar 0 a los primeros 6 valores
#2.rep(1,3) = Asignar 1 a los tres siguientes valores (41,42 y 43)
#3.rep(0,6) = Asignar 6 a los siguientes 6 valores
reclass <- c(rep(0,6), rep(1,3), rep(0,6))
reclass

#Crear matriz de reclasificación

#levels(nlcd)[[1]][,1] = Extrae el listado de los códigos de clasificación
#de nlcd

#reclass = Columna de 1 y 0

reclass_mat <- cbind(levels(nlcd)[[1]][,1], reclass) #Unir la primera columna
#de levels (nlcd), la cual contiene los códigos de las coberturas, y los nuevos
#valores de reclass
reclass_mat

#Clasificar el raster nlcd con los nuevos valores y asignarlos a forest

forest <- classify(nlcd, reclass_mat)
forest <- as.factor(forest)
writeRaster(forest, "raster/forest.tif")

ggplot() +
  geom_spatraster(data=forest) + #Nótese que es importante escribir de
  #manera explícita data
  scale_fill_manual(values = c("0"="firebrick" , "1"="green"),
                    #labels = c("No bosque", "Bosque"), #Esta sección para la
                    #leyenda
                    na.value="transparent") +
  theme_bw() +
  theme(legend.position = "none")

ggsave("images/forest.png", plot=get_last_plot())

################################################################################
#Crear buffer de 1 y 5 km

b_1km <- 1000
b_5km <- 5000

#Buffer para el primer sitio

#View(sites)
b_site1_1km <- st_buffer(sites[1, "geometry"], dist= b_1km)
b_site1_5km <- st_buffer(sites[1, "geometry"], dist= b_5km)


ggplot() +
  geom_sf(data=b_site1_1km, color="red", fill="transparent") +#Nótese que es
  #importante escribir de manera explícita data
  geom_sf(data=b_site1_5km, color="black", fill="transparent") +
  theme_bw()

ggsave("images/buffer.png", plot=get_last_plot())

#El paquete terra tiene una función para hacer zoom a capas de raster con
#polígonos sf


png("images/zoom_1km_5km.png", width = 10, height = 10,
    units="cm", res = 300)

zoom(nlcd, b_site1_5km)
#Nótese que para que se agreguen los siguientes plot al zoom, es necesario
#especificar el argumento add=T
plot(b_site1_1km, border = "red", lwd = 3, add = T) 
plot(b_site1_5km, border = "red", lwd = 3, add = T)
plot(sites[1,], pch=22, cex=2, add=T)
dev.off()

#Crop vs mask - Es recomendable usar primero crop y después mask

#La función crop hace un recorte espacial por extensión.
#Usa la extensión rectangular de un objeto (ráster o vector)
#No tiene en cuenta la forma del polígono sino su "caja"
#Reduce filas y columnas (hace el objeto más eficiente)

#Mask hace recorte por forma
#Aplica una máscara usando la geometría real del objeto
#Usa la forma exacta del polígono
#Los píxeles del polígono se convierten en NA
#No reduce el tamaño del ráster (a menos que antes se use crop())

b_forest1_1km <- crop(forest, b_site1_1km)  
b_forest1_1km <- mask(b_forest1_1km, b_site1_1km)
writeRaster(b_forest1_1km, "raster/b_forest_1km.tif")

#Imagen comparativa crop vs mask
png("images/crop_mask.png", width = 20, height = 10,
    units="cm", res=300)
par(mfrow=c(1,2))
b_forest1_1km <- crop(forest, b_site1_1km)  
plot(b_forest1_1km, main="Crop")
b_forest1_1km <- mask(b_forest1_1km, b_site1_1km)
plot(b_forest1_1km, main="Mask")
dev.off()

par(mfrow=c(1,1))


ggplot() +
  geom_spatraster(data=b_forest1_1km) + #Nótese que es importante escribir de
  #manera explícita data
  scale_fill_manual(values=c("0"="red", "1"="green"),
                    labels=c("No Forest", "Forest"),
                    name="Landcover", #El argumento name se utiliza para
                    #asignar el título de la leyenda. Si se desea omitir el 
                    #título de la leyenda se utiliza el argumento name= NULL
                    na.translate=F) + #Para que los valores de NA se omitan en
  #la leyenda se especifíca el argumento na.translate=FALSE
  geom_sf(data=sites[1,"geometry"], pch=22, cex=2, color="black") + #Nótese 
  #que es importante escribir de manera explícita data
  theme_bw()

ggsave("images/mask_landcover.png", plot=get_last_plot())
