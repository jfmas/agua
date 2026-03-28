## Proyecto PAPIIT Clave IN112823
## Título Azolvamiento y eutroficación en presas periurbanas de zonas templadas de México: 
## contribuciones para su evaluación y prospección

### BAJA SENTINEL
## https://stackoverflow.com/questions/78808743/downloading-sentinel-2-with-rsat-package-in-r-produces-error-argument-espa-ord/79041473#79041473

library(rsat)
library(terra)
library(sf)
library(tmap)
library(tmaptools)

set_credentials("KARENOFII.02@GMAIL.COM","yVJK#4nVLFR4nvL", "dataspace")
print_credentials()

### Función para corrigir Bug ##############################################
## https://stackoverflow.com/questions/78808743/downloading-sentinel-2-with-rsat-package-in-r-produces-error-argument-espa-ord/79041473#79041473
rsat_download2<-function(x, db_path, verbose = FALSE, parallel=FALSE, ...) {
  require(rsat)
  
  args <- list(...)
  
  if (missing(db_path)){
    db_path <- get_database(x)
    if(db_path==""){
      stop("db_path or global environment database needed for image downloading.")
    }
  }
  
  # filter records
  x<-records(x)
  dataspace <- x[get_api_name(x)%in%"dataspace"]
  usgs <- x[get_api_name(x)%in%"usgs"]
  lpdaac <- x[get_api_name(x)%in%"lpdaac"]
  
  # run download
  if(parallel){
    functions_list <- list(
      list(func = connection$getApi("lpdaac")$download_lpdaac_records,
           args = list(lpdaac_records=lpdaac,db_path=db_path,verbose=verbose,...)),
      list(func = rsat:::connection$getApi("dataspace")$dataspace_download_records,
           args = list(records=dataspace,db_path=db_path,verbose=verbose,...)),
      list(func = connection$getApi("usgs")$espa_order_and_download,
           args = list(usgs=usgs,db_path=db_path,verbose=verbose,...))
    )
    null.list <-mclapply(functions_list, function(entry) {
      do.call(entry$func, entry$args)
    }, mc.cores = 3)
  }else{
    functions_list <- list(
      list(func = rsat:::connection$getApi("usgs")$order_usgs_records,
           args = list(espa_orders=usgs,db_path=db_path,verbose=verbose,...)),
      list(func = rsat:::connection$getApi("lpdaac")$download_lpdaac_records,
           args = list(lpdaac_records=lpdaac,db_path=db_path,verbose=verbose,...)),
      list(func = rsat:::connection$getApi("dataspace")$dataspace_download_records,
           args = list(records=dataspace,db_path=db_path,verbose=verbose,...)),
      list(func = rsat:::connection$getApi("usgs")$download_espa_orders,
           args = list(espa.orders=usgs,db_path=db_path,verbose=verbose,...))
    )
    null.list <- lapply(functions_list, function(entry) {
      do.call(entry$func, entry$args)
    })
  }
}
######################################################################
## Area de interés capturar coodinadas extremas o leer mapa AOI
## Querendaro
xmin = -100.88755
xmax = -100.84190
ymin = 19.80071
ymax = 19.83478
AOI_ext<-ext(xmin, xmax, ymin, ymax)
AOI<-AOI_ext %>% st_bbox() %>% st_as_sfc() %>% st_as_sf(crs=4326) 


xmin<-538078.1
xmax<-866945.5  
ymin<-773531.3
ymax<-1436377.3
AOI_ext<-ext(xmin, xmax, ymin, ymax)
AOI<-AOI_ext %>% st_bbox() %>% st_as_sfc() %>% st_as_sf(crs=6557) 

### Alternativa get coordinates from map
### Crea box around cover
coverpath <- "/home/jf/pCloudDrive/congres/GISTAM2024/mapas/penjamo_z13.gpkg"
cover <- st_read(coverpath)
cover4326 <-  st_transform(cover, crs = 4326)
coord <- st_bbox(cover4326) # xmin       ymin       xmax       ymax 
xmin <- coord[1]
ymin <- coord[2]
xmax <- coord[3]
ymax <- coord[4]
AOI_ext<-ext(xmin, xmax, ymin, ymax)
AOI<-AOI_ext %>% st_bbox() %>% st_as_sfc() %>% st_as_sf(crs=4326) 


#### Vistazo al AOI
tmap_mode("view")

tm_shape(AOI)+
  tm_polygons(border.col="magenta", alpha=0, col=NA, lwd=2)+
  tm_basemap(server = providers$Esri.WorldImagery)

# Definir las fechas de interés, ACTUALIZAR a todo el 2024 cuando funcione el código
toi <- seq(as.Date("2024-09-02") , as.Date("2024-09-25"),1)

# Buscar imágenes sentinel2 1C en la región definida
rcd <- rsat_search(region = AOI,
                   product = c("S2MSI1C"),
                   dates = toi)
print(rcd)

#### 
db.path <- file.path(tempdir(),"database_test")
ds.path <- file.path(tempdir(),"datasets_test")

if(!dir.exists(db.path)){
  dir.create(db.path)
}

if(!dir.exists(ds.path)){
  dir.create(ds.path)
}

Querendaro<-new_rtoi(name="Querendaro_test", region=AOI, db_path=db.path, rtoi_path = ds.path)
rsat_search(region=Querendaro, product="S2MSI2A", dates=toi)
rsat_download2(Querendaro, db_path=db.path)
