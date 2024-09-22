## Indices de calidad del agua con Sentinel 2 de Gto
## https://cran.r-project.org/web/packages/waterquality/vignettes/waterquality_vignette.html
## https://rdrr.io/github/RAJohansen/waterquality/src/data-raw/01_sentinel.R

# install.packages("waterquality", dependencies = TRUE)
setwd("/home/jf/pCloudDrive/proyectos/PAPIIT2023/Gto")



library(terra)
library(stringr)
#library(RStoolbox)
library(waterquality)
library(tmap)
library(tmaptools)
library(sf)
#library(raster)

## unzip la imagen de interés en /home/jf/pCloudDrive/BaseDatosEspaciales/SentinelGto
mask <- vect("muestreo_061023/PRESA_ESPERANZA_SOLEDAD.shp")
plot(mask)


rutaS2 <- "/home/jf/pCloudDrive/proyectos/PAPIIT2023/Gto/imagenes011023/2023-10-01-00:00_2023-10-01-23:59_Sentinel-2_L2A_"
2023-10-01-00:00_2023-10-01-23:59_Sentinel-2_L2A_B01_(Raw).tiff
## bandas a 10 m
b2  <- rast(paste0(rutaS2,"B02_(Raw).tiff"))
plot(b2)
b3  <- rast(paste0(rutaS2,"B03_(Raw).tiff"))
b4  <- rast(paste0(rutaS2,"B04_(Raw).tiff"))
b8  <- rast(paste0(rutaS2,"B08_(Raw).tiff"))

#mascaracrop_nubes <- rast(paste0(rutaS2,"QI_DATA/MSK_CLDPRB_20m.jp2"))

## bandas a 20 m
b1  <- rast(paste0(rutaS2,"B01_(Raw).tiff"))
b5  <- rast(paste0(rutaS2,"B05_(Raw).tiff"))
b6  <- rast(paste0(rutaS2,"B06_(Raw).tiff"))
b7  <- rast(paste0(rutaS2,"B07_(Raw).tiff"))
b9  <- rast(paste0(rutaS2,"B8A_(Raw).tiff"))


stack <- c(b1,b2,b3,b4,b5,b6,b7,b8,b9)/10000        
print("salva el raster")
fecha <- "01-10-2023"
writeRaster(stack, file=paste0("corteS2_",fecha,".tif"), overwrite=TRUE, wopt= list(gdal=c("COMPRESS=NONE"), datatype='FLT4S'))


## Elabora un mapita con tmap
print("hace el mapa")
tm_shape(stack[[c(3,2,1)]]) + tm_rgb()
m <- tm_shape(rs[[c(3,2,1)]]) + tm_rgb()
print("salva el mapa")
tmap_save(m, "Figuras/S2_mapita.png",width = 15, height = 15, units = "cm")

######## Cálculo de los indices con paquete waterquality

fecha <- "01-10-2023"
stack <-rast(paste0("corteS2_",fecha,".tif"))
plot(stack)
st <- stack
## Pasar a raster
# r
# st <- raster::stack(stack)
# st <- st[[1:9]]
# class(st)

#Clorofila: Al10SABI and Go04MCI,
#turbidity TurbMoore80Red
Al10SABI <- wq_calc(st, alg = "Al10SABI",sat = "sentinel2")
TurbMoore80Red  <- wq_calc(st, alg = "TurbMoore80Red",sat = "sentinel2")
## otra sintaxis
indicesQAagua  <- wq_calc(st, alg = c("Al10SABI","TurbMoore80Red"),sat = "sentinel2")
all  <- wq_calc(st, alg = "all" ,sat = "sentinel2")
names(all)
plot(Al10SABI)
plot(TurbMoore80Red)
plot(indicesQAagua)
plot(all)
plot(all[[3]])


Al10SABI <- raster::raster(Al10SABI)
#writeRaster(Al10SABI, file=paste0("/home/jf/pCloudDrive/proyectos/PAPIIT2023/Cointzio/Figuras/Al10SABI",fecha,"_",i,".tif"), overwrite=TRUE, wopt= list(gdal=c("COMPRESS=NONE"), datatype='FLT4S'))
all <- rast(all)
writeRaster(all, filename=paste0("indices011023.tif"), format="GTiff", overwrite=TRUE, datatype='FLT4S')
writeRaster(all, file="indices011023.tif", overwrite=TRUE, wopt= list(gdal=c("COMPRESS=NONE"), datatype='FLT4S'))

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


st <- as(stack, "Raster")
st <- raster::stack(stack)
class(st)
Am092Bsub <- wq_calc(st, alg = "Am092Bsub",sat = "sentinel2")
plot(Am092Bsub)

all <- wq_calc(st, alg = "all", sat="sentinel2")
class(all)
summary(all)
plot(all)
all@file
name <- "all"
raster::writeRaster(all,"all.tif",datatype="FLT4S", format="GTiff", overwrite=TRUE)


################################
all <- rast("indices.tif")
plot(all)
names(all)
pts <- vect("muestreo_061023/Muestreos_061023_con_datos.shp")
plot(pts)
head(pts)
unique(pts$Presa)
pts$color[pts$Presa == "LE"] <- "green"
pts$color[pts$Presa == "LS"] <- "red"
pts$color[pts$Presa == "B"] <- "black"
pts$color[pts$Presa == "LP"] <- "purple"
#turb <- all[[14:22]]
pts <- project(pts,crs(all))
plot(turb[[1]]); plot(pts, add=T)
tab <- extract(all,pts)
nrow(tab) # 17
names(tab)
unique(tab$color)
head(pts)
tab$seki <- pts$Secchi_cms
tab$turb <- pts$Turbidez__
tab$SS <- pts$Solidos_su
tab$Clorofila <- pts$Clorofila
tab$presa <- pts$Presa
tab$color <- pts$color
names(tab)
summary(tab)
tab <- na.omit(tab)
tab <- tab[tab$seki != 0,]
for (j in 24:27){ # de 24 a 27 son los indices de laboratorio/campo
  print(paste("***********",names(tab)[j]))
for (i in 2:23){  # de 2 a 23 son los indices de la imagen
  print(paste0(names(tab[i]),": ",cor(tab[[j]],tab[[i]])))
}
}
names(tab)
cor(tab$Be16NDTIblue, tab$seki) #  -0.7433715
cor(tab$TurbFrohn09GreenPlusRedBothOverBlue, tab$turb) #  0.6649209
cor(tab$TurbFrohn09GreenPlusRedBothOverBlue, tab$SS) # 0.6478821
cor(tab$MM12NDCI, tab$Clorofila) # 0.6264665
unique(tab$presa)
png("plot011023seki.png", width = 15, height = 12, units="cm",res=300)
plot(tab$Be16NDTIblue, tab$seki, col=tab$color, xlab ="Índice Be16NDTIblue", ylab = "Secchi")
# Add a legend
legend("topright" , legend=c("La Soledad", "La Esperanza"),
       col=c("red","green"), pch=1,cex=0.8)
lm <- lm(seki ~ Be16NDTIblue, data=tab)
abline(lm)
dev.off()

png("plot011023turb.png", width = 15, height = 12, units="cm",res=300)
plot(tab$TurbFrohn09GreenPlusRedBothOverBlue, tab$turb, col=tab$color,
     xlab ="Índice TurbFrohn09GreenPlusRedBothOverBlue", ylab = "Turbidez")
# Add a legend 
legend("topleft" , legend=c("La Soledad", "La Esperanza"),
       col=c("red","green"), pch=1,cex=0.8)
lm <- lm(turb ~ TurbFrohn09GreenPlusRedBothOverBlue, data=tab)
abline(lm)
dev.off()

png("plot011023SS.png", width = 15, height = 12, units="cm",res=300)
plot(tab$TurbFrohn09GreenPlusRedBothOverBlue, tab$SS, col=tab$color,
     xlab ="Índice TurbFrohn09GreenPlusRedBothOverBlue", ylab = "Sólidos suspendidos")
# Add a legend 
legend("topleft" , legend=c("La Soledad", "La Esperanza"),
       col=c("red","green"), pch=1,cex=0.8)
lm <- lm(SS ~ TurbFrohn09GreenPlusRedBothOverBlue, data=tab)
abline(lm)
dev.off()

tab$MM12NDCI, tab$Clorofila
png("plot011023clorofila.png", width = 15, height = 12, units="cm",res=300)
plot(tab$MM12NDCI, tab$Clorofila, col=tab$color,
     xlab ="Índice MM12NDCI", ylab = "Clorofila")
# Add a legend 
legend("topleft" , legend=c("La Soledad", "La Esperanza"),
       col=c("red","green"), pch=1,cex=0.8)
lm <- lm(Clorofila ~ MM12NDCI, data=tab)
abline(lm)
dev.off()

## Sechi modelado con la regresion lineal
lm(seki ~ Be16NDTIblue, data=tab)

# Call:
#   lm(formula = seki ~ Be16NDTIblue, data = tab)
# 
# Coefficients:
#   (Intercept)  Be16NDTIblue  
# 85.01       -479.58  

secchi <- all["Be16NDTIblue"] * -479.58 + 85.01
plot(secchi)
writeRaster(secchi, file="secchi011023.tif", overwrite=TRUE, wopt= list(gdal=c("COMPRESS=NONE"), datatype='FLT4S'))



cor(tab$TurbBe16GreenPlusRedBothOverViolet, tab$seki)


plot(tab$TurbBe16GreenPlusRedBothOverViolet, tab$seki)
lm <- lm(seki ~ TurbBe16GreenPlusRedBothOverViolet, data=tab)
abline(lm)



zira_Al10SABI <- wq_calc(raster_stack = zira, 
                         alg = "Al10SABI",
                         sat = "sentinel2")


zira_Al10SABI[agua == 0] <- NA
plot(zira_Al10SABI)
writeRaster(zira_Al10SABI,"zira_Al10SABI.tif",datatype="FLT4S")
lista <- list.files(path = "images/BOA",pattern = ".tif$",full.names = T)
i <- 0
zira_Al10SABIs <- list()

for (imagen in lista){
  plot(i)
  i <- i + 1
  zira <- stack(imagen)
  zira_Al10SABI <- wq_calc(raster_stack = zira, 
                           alg = "Al10SABI",
                           sat = "sentinel2")
  zira_Al10SABI[agua == 0] <- NA
  zira_Al10SABIs[i] <- zira_Al10SABI
} 
zira_Al10SABIss <- stack(zira_Al10SABIs)
plot(zira_Al10SABIss)



