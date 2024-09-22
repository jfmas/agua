###############################################################
##### Determina la fecha de adquisición Sentinel y Landsat
##### para las presas de estudio
##### https://sentinels.copernicus.eu/web/sentinel/missions/sentinel-2/acquisition-plans
##### https://landsat.usgs.gov/landsat_acq
##### https://github.com/hevgyrt/harvest_sentinel_acquisition_plans
##### https://github.com/EPSCoR-blooms/sentinel_acquisition
###############################################################

# Librerias
library(dplyr)
library(sf)
library(data.table)
library(raster)

# presa	x	y
# Cointzio	19.6138	-101.2646
# Querendaro	19.8172	-100.8621
# LaEsperanza	21.0496	-101.2548
# LaSoledad	21.0455	-101.2772
# Malpais	21.0117	-101.2974
# Solis 20.1 -100.6


# your data (removed crs column)
DT <- data.table(
  place=c("Cointzio", "Querendaro", "LaEsperanza", "LaSoledad", "Malpais","Burrones","Presa Allende","Solis"),
  longitude=c(-101.2646, -100.8621, -101.2548, -101.2772, -101.2974, -101.2974, -100.8015, -100.6),
  latitude=c(19.6138, 19.8172, 21.0496, 21.0455, 21.0117, 21.0117, 20.8784, 20.1))
# st_as_sf() ######
# sf version 0.2-7
DT_sf <- st_as_sf(DT, coords = c("longitude", "latitude"),
                 crs = 4326, relation_to_geometry = "field")
# sf version 0.3-4, 0.4-0
DT_sf = st_as_sf(DT, coords = c("longitude", "latitude"),
                 crs = 4326, agr = "constant")
plot(DT_sf)
st_crs(DT_sf)
st_write(DT_sf,"/home/jf/pCloudDrive/BaseDatosEspaciales/SentinelGto/presas.gpkg")

DT_sf_z14 <- st_transform(DT_sf, crs = "+proj=utm +zone=14 +datum=WGS84 +units=m +no_defs ")
st_write(DT_sf_z14,"/home/jf/pCloudDrive/BaseDatosEspaciales/SentinelGto/presas_z14.gpkg")

# Bajar los kml actualizados de la página
# https://sentinels.copernicus.eu/web/sentinel/missions/sentinel-2/acquisition-plans
namekmlA <- "S2A_MP_ACQ__KML_20240905T120000_20240923T150000.kml"
namekmlB <- "S2B_MP_ACQ__KML_20240829T120000_20240916T150000.kml"

## Sentinel 2 A
## Ojo con windows que pone \ en vez de /
capas <- st_layers(paste0("/home/jf/Downloads/",namekmlA))
print(capas)
# Determina la capa con más features
capas$features == max(capas$features)
capas$name
capas$name[capas$features == max(capas$features)]
capaquetienemas <- capas$name[capas$features == max(capas$features)]

cobA <- st_read(paste0("/home/jf/Downloads/",namekmlA),capaquetienemas)
class(cobA)
head(cobA)
plot(cobA)
crs(cobA)

## Lo mismo para Sentinel 2 B
capas <- st_layers(paste0("/home/jf/Downloads/",namekmlB))
print(capas)
# Determina la capa con más features
capas$features == max(capas$features)
capas$name
capas$name[capas$features == max(capas$features)]
capaquetienemas <- capas$name[capas$features == max(capas$features)]
print(capaquetienemas)

cobB <- st_read(paste0("/home/jf/Downloads/",namekmlB),capaquetienemas)

A <- st_intersection(cobA, DT_sf)[,c("place","Name","ObservationTimeStart","OrbitAbsolute","OrbitRelative", "Scenes")]
print(A)
tail(A)

B <- st_intersection(cobB, DT_sf)[,c("place","Name","ObservationTimeStart","OrbitAbsolute","OrbitRelative", "Scenes")]
print(B)
tail(B)

##### Landsat checar las imágenes a continuación en
## https://landsat.usgs.gov/landsat_acq
# landsat
# Cointzio 28/46 27/46
# Querendaro 27/46
# Gto 28/45
# Solis 27/46
