## Proyecto PAPIIT Clave IN112823
## Título Azolvamiento y eutroficación en presas periurbanas de zonas templadas de México: 
## contribuciones para su evaluación y prospección


###############################################################
##### Baja imágenes Sentinel y Landsat usando rsat
##### para las presas de estudio
###############################################################

# https://github.com/ropensci/rsat



install.packages(c('usethis', 'pkgdown', 'rcmdcheck', 'rversions', 'urlchecker'))

# check and install devtools
if(!require("devtools")){
  install.packages("devtools")
}
# check and install rmarkdown
if(!require("rmarkdown")){
  install.packages("rmarkdown")
}
## si hay pex para instalar libmagick
# sudo apt-get install -y libmagick++-dev
# https://installati.one/install-libmagick++-dev-ubuntu-22-04/

devtools::install_github("spatialstatisticsupna/rsat", build_vignettes=TRUE)


library(rsat)
library(sf)
browseVignettes("rsat")

setwd("/home/jf/Downloads/")

# 28/46 y 47
# Landsat 5
# 30 3 2001
# 28 4 2000
# 26 4 1999
# 25 5 1998
# 3 3 1997

set_credentials("jfmas","EElaneta68##","earthdata")
set_credentials("jfmas@ciga.unam.mx","Claneta68$$$", "scihub")
set_credentials("rsat.package","UpnaSSG.2021")
print_credentials()


ip <- st_sf(st_as_sfc(st_bbox(c(
  xmin = -9.755859,
  xmax =  4.746094,
  ymin = 35.91557,
  ymax = 44.02201 
), crs = 4326)))
toi <- seq(as.Date("2021-01-10"),as.Date("2021-01-15"),1)

# The folders for the database and dataset can be created programmatically as follows:
  
db.path <- file.path(tempdir(),"database")
ds.path <- file.path(tempdir(),"datasets")
dir.create(db.path)
dir.create(ds.path)

# The minimum information to generate a new rtoi is the name, a polygon of the roi, and the paths to database and dataset:
  
  filomena <- new_rtoi(name = "filomena",
                       region = ip,
                       db_path = "/home/jf/Downloads/pruebarsat/db",
                       rtoi_path = "/home/jf/Downloads/pruebarsat/ds")

# To limit the amount of data and processing times, the assessment is conducted over MODIS imagery. A total number of 24
# images are found for the region over the 6-day period:
  
  rcd <- rsat_search(region = filomena, product = c("mod09ga"), dates = toi)
class(rcd)
summary(rcd)

rcd[12]
# Image acquisition
rcds <- records(filomena)
class(rcds)

plot(filomena,
     "preview",
     product = "mod09ga",
     dates = "2021-01-11")

plot(filomena, "dates")

rcd <- records(filomena)
records(filomena) <- subset(rcd, "product", "mod09ga")

rsat_download(filomena)

rsat_download(records(filomena), out.dir = get_database(filomena))


#################################################################

ip <- st_sf(st_as_sfc(st_bbox(c(
  xmin = -101.92726,
  xmax =  -101.5174,
  ymin = 19.26651,
  ymax = 19.57024 
), crs = 4326)))


toi <- c(as.Date("2001-03-30"), as.Date("2000-04-28"))

# The folders for the database and dataset can be created programmatically as follows:

db.path <- file.path(tempdir(),"database")
ds.path <- file.path(tempdir(),"datasets")
dir.create(db.path)
dir.create(ds.path)

# The minimum information to generate a new rtoi is the name, a polygon of the roi, and the paths to database and dataset:

mich <- new_rtoi(name = "mich",
                     region = ip,
                     db_path = "/home/jf/pCloudDrive/mich/pruebarsat/db",
                     rtoi_path = "/home/jf/pCloudDrive/mich/pruebarsat/ds")

# To limit the amount of data and processing times, the assessment is conducted over MODIS imagery. A total number of 24
# images are found for the region over the 6-day period:

rcd <- rsat_search(region = mich, product = c("LANDSAT_TM_C1"), dates = toi)
class(rcd)
summary(rcd)

rcd[12]
# Image acquisition
rcds <- records(filomena)
class(rcds)

plot(filomena,
     "preview",
     product = "mod09ga",
     dates = "2021-01-11")

plot(filomena, "dates")

rcd <- records(filomena)
records(filomena) <- subset(rcd, "product", "mod09ga")

rsat_download(filomena)

rsat_download(records(filomena), out.dir = get_database(filomena))
