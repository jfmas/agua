## Indices de calidad del agua con Sentinel 2 de Cointzio
## https://cran.r-project.org/web/packages/waterquality/vignettes/waterquality_vignette.html
## https://rdrr.io/github/RAJohansen/waterquality/src/data-raw/01_sentinel.R

# install.packages("waterquality", dependencies = TRUE)
setwd("/home/jf/pCloudDrive/proyectos/PAPIIT2023/Cointzio")

# library(devtools)
# install_github("bleutner/RStoolbox")


library(terra)
library(stringr)
#library(RStoolbox)
library(waterquality)
library(tmap)
library(tmaptools)
#library(raster)

mask <- vect("mask_agua.gpkg")
plot(mask)


rutaS2 <- "/home/jf/pCloudDrive/proyectos/PAPIIT2023/Cointzio/images_S2/BOA/"


imagen <- "S2A2A_20230409_112_Cointzio_BOA_10.tif"
# extrae fecha del nombre (caracteres 7 a 14)
fecha <- str_sub(imagen,7,14)
print(fecha)
r <- rast(paste0(rutaS2,imagen))
names(r)
crs(r)
r <- project(r, "EPSG:32614") # proyecta de UTM z13 a z14
plot(r)
plot(mask)

#### Enmascara las imagenes al polígono del cuerpo de agua
maskr <- rasterize(mask,r)
plot(maskr)
r <- mask(r, mask)
plot(r[[1]])


# Imagen con realce en el agua
mins <- as.numeric(zonal(r, mask, fun="min"))
maxs <- as.numeric(zonal(r, mask, fun="max"))
print(mins)
class(mins)

plot(r)
plot(r[[4]])

print("hace el strech")
rs <- terra::stretch(r, minv = 0, maxv = 255, smin = mins, smax = maxs)
plot(rs[[4]])

## Elabora un mapita con tmap
print("hace el mapa")
tm_shape(rs[[c(3,2,1)]]) + tm_rgb()
m <- tm_shape(rs[[c(3,2,1)]]) + tm_rgb()
print("salva el mapa")
tmap_save(m, "Figuras/S2_mapita.png",width = 15, height = 15, units = "cm")

###### AUTOMATIZACION ###################################################################
## Automatizando sobre un gran número de datos (proceso iterativo) usando
# list.files() y for(){}

lista <- list.files(path=rutaS2,include.dirs = T,
           recursive=T,pattern=".tif$")

i <- 2
for(i in 2:length(lista)){ 

fecha <- str_sub(lista[i],7,14)
print(fecha)
r <- rast(paste0(rutaS2,lista[i]))
r <- project(r, "EPSG:32614")

mins <- as.numeric(zonal(r, mask, fun="min"))
maxs <- as.numeric(zonal(r, mask, fun="max"))
class(mins)

plot(r)
print("hace el strech")
rs <- terra::stretch(r, minv = 0, maxv = 255, smin = mins, smax = maxs)
print("hace el mapa")
m <- tm_shape(rs[[c(3,2,1)]]) + tm_rgb()
print("salva el mapa")
tmap_save(m, paste0("/home/jf/pCloudDrive/proyectos/PAPIIT2023/Cointzio/Figuras/S2_",fecha,"_",i,".png"),width = 15, height = 15, units = "cm")
} # loop



######## Cálculo de los indices con paquete waterquality

## Pasar a raster
r
st <- raster::stack(r)
st <- st[[1:9]]
class(st)

#Clorofila: Al10SABI and Go04MCI,
#turbidity TurbMoore80Red
Al10SABI <- wq_calc(st, alg = "Al10SABI",sat = "sentinel2")
TurbMoore80Red  <- wq_calc(st, alg = "TurbMoore80Red",sat = "sentinel2")
## otra sintaxis
indicesQAagua  <- wq_calc(st, alg = c("Al10SABI","TurbMoore80Red"),sat = "sentinel2")
all  <- wq_calc(st, alg = "all" ,sat = "sentinel2")

plot(Al10SABI)
plot(TurbMoore80Red)
plot(indicesQAagua)
plot(all)
plot(all[[3]])


Al10SABI <- raster::raster(Al10SABI)
#writeRaster(Al10SABI, file=paste0("/home/jf/pCloudDrive/proyectos/PAPIIT2023/Cointzio/Figuras/Al10SABI",fecha,"_",i,".tif"), overwrite=TRUE, wopt= list(gdal=c("COMPRESS=NONE"), datatype='FLT4S'))
writeRaster(all, filename=paste0("/home/jf/pCloudDrive/proyectos/PAPIIT2023/Cointzio/Figuras/Am09KBBI.tif"), format="GTiff", overwrite=TRUE, datatype='FLT4S')


min <- as.numeric(zonal(rast(Al10SABI), mask, fun="min"))
max <- as.numeric(zonal(rast(Al10SABI), mask, fun="max"))
print("hace el strech")
rs <- terra::stretch(Al10SABI, minv = 0, maxv = 255, smin = min, smax = max)
print("hace el mapa")
m <- tm_shape(Al10SABI)+
  tm_raster(style= "quantile", n=7, palette=get_brewer_pal("Greys", n = 7, plot=FALSE))+
  tm_layout(legend.outside = TRUE)
print("salva el mapa")
tmap_save(m, paste0("/home/jf/pCloudDrive/proyectos/PAPIIT2023/Cointzio/Figuras/Al10SABI",fecha,".png"),width = 15, height = 15, units = "cm")
#print("salva el raster")
#writeRaster(r, file=paste0("corteS2/corteS2_",fecha,".tif"), overwrite=TRUE, wopt= list(gdal=c("COMPRESS=NONE"), datatype='INT2U'))




