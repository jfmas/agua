## Sentinel 2 Proyecto PAPIIT Agua
## Versión inutil hasta que sen2r se actualice  de de alta

#("sen2r", dependencies = TRUE)
setwd("/home/jf/pCloudDrive/proyectos/PAPIIT2023/Cointzio")
library(sen2r)
## Modo interactivo

sen2r()

## Fechas que se ven chidas
#fechas <- c("2023-02-23","2023-02-03","2022-12-05","2021-10-31","2020-11-25",
"2019-12-26","2019-01-10","2017-11-26","2017-11-16")            
fechas <- c("2023-04-19")
# Mode código
# Set paths
out_dir_1  <- "/home/jf/pCloudDrive/proyectos/PAPIIT2023/Cointzio/images_S2" # output folder
safe_dir1 <-  "/home/jf/pCloudDrive/proyectos/PAPIIT2023/Cointzio/images_S2"  # folder to store downloaded SAFE

#myextent_1 <- system.file("extdata/vector/barbellino.geojson", package = "sen2r") 

library(sen2r)
write_scihub_login("jfmas", "Claneta68")
is_scihub_configured()
check_scihub_login("jfmas", "Claneta68", service = "apihub")
check_scihub_connection(service = "apihub") 

### Sentinel 2 #######################################################
for (fecha in fechas){ 
out_paths_1 <- sen2r(
  gui = FALSE,
  step_atmcorr = "auto",
  sel_sensor = c("s2a", "s2b"),
  extent = "pol_cointzio.gpkg",
  clip_on_extent = TRUE,
  extent_name = "Cointzio",
  timewindow = c((as.Date(fecha)-15), as.Date(fecha)),
  list_prods = c("BOA"),
  #list_indices = c(),
  #list_rgb = c("RGB432B"),
  mask_type = "cloud_and_shadow",
  max_mask = 5, 
  path_l2a = safe_dir1,
  path_out = out_dir_1,
  online = TRUE,
  log ="log"
)
}



https://www.sentinel-hub.com/

sen2r("/home/jf/.sen2r/proc_par/s2proc_20230221_211243.json")
