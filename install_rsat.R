## Proyecto PAPIIT Clave IN112823
## Título Azolvamiento y eutroficación en presas periurbanas de zonas templadas de México: 
## contribuciones para su evaluación y prospección

### INSTALA RSAT
## https://stackoverflow.com/questions/78808743/downloading-sentinel-2-with-rsat-package-in-r-produces-error-argument-espa-ord/79041473#79041473

install.packages(c('usethis', 'pkgdown', 'rcmdcheck', 'rversions', 'urlchecker'))

# check and install devtools
if(!require("devtools")){
  install.packages("devtools")
}
# check and install rmarkdown
if(!require("rmarkdown")){
  install.packages("rmarkdown")
}

library(devtools)
devtools::install_github("spatialstatisticsupna/rsat", build_vignettes=TRUE)
