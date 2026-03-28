## Proyecto PAPIIT Clave IN112823
## Título Azolvamiento y eutroficación en presas periurbanas de zonas templadas de México: 
## contribuciones para su evaluación y prospección
## Procesa Indicadores_de_calidad_del_agua_2023_en_1056_sitios_de_monitoreo
## https://datos.conagua.gob.mx/views/index_datos_abiertos.html
## https://github.com/jfmas/agua

setwd("/home/jf/pCloudDrive/proyectos/PAPIIT2023/datosLibresCONAGUA/")
library(sf)
library(dplyr)
library(readxl)
library(janitor)
library(xtable)


ruta  <- "/home/jf/pCloudDrive/proyectos/PAPIIT2023/datosLibresCONAGUA/"
tab2021 <- read.csv(paste0(ruta, 
                           "Datos_de_calidad_del_agua_de_5000_sitios_de_monitoreo/Datos_de_calidad_del_agua_de_sitios_de_monitoreo_de_aguas_superficiales_2021.csv"), 
                    encoding = "latin1")
# Limpia y corrig errores
sort(unique(tab2021$SUBTIPO))
tab2021$SUBTIPO[tab2021$SUBTIPO=="PrEsa"] <- "PRESA"
tab2021$SUBTIPO[tab2021$SUBTIPO=="RÍO" ] <- "RIO"

tab2023 <- read.csv(paste0(ruta, 
                           "Indicadores_de_calidad_del_agua_2023_en_1056_sitios_de_monitoreo/Indicadores_de_la_calidad_del_agua_superficial_2023.csv"), 
                    encoding = "latin1")
# Limpia y corrige errores
sort(unique(tab2023$SUBTIPO))
tab2023$SUBTIPO[tab2023$SUBTIPO=="PRESA "] <- "PRESA"
tab2023$SUBTIPO[tab2023$SUBTIPO=="RÍO"] <- "RIO"


tab2024 <- data.frame(read_xlsx("Calidad_del_Agua_Superficial_a2024.xlsx"))
# Limpia y corrige errores
sort(unique(tab2024$SUBTIPO))
tab2024$SUBTIPO[tab2024$SUBTIPO=="RÍO"] <- "RIO"

tab2012a2024 <-  data.frame(read_xlsx("Calidad_del_Agua_Superficial_p2012-2024.xlsx"))
## cuantos sitios agua superficiales en cada base de datos
nrow(tab2021) # 788
nrow(tab2023) # 450
nrow(tab2024) # 636
nrow(tab2012a2024) # 4676

## Son los mismos sitios?
# 688 sitios 2021 que no están en 2023
tab2021$CLAVE[!tab2021$CLAVE %in% tab2023$CLAVE]
# 350 sitios 2023 que no están en 2021
tab2023$CLAVE[!tab2023$CLAVE %in% tab2021$CLAVE]
# 100 sitios comunes a 2021 y 2023
tab2023$CLAVE[tab2023$CLAVE %in% tab2021$CLAVE]

# 198 sitios 2023 que no están en 2024
tab2023$CLAVE[!tab2023$CLAVE %in% tab2024$CLAVE]
# 384 sitios 2024 que no están en 2023
tab2024$CLAVE[!tab2024$CLAVE %in% tab2023$CLAVE]
# 252 sitios comunes a 2023 y 2024
tab2024$CLAVE[tab2024$CLAVE %in% tab2023$CLAVE]

## con que tabla seguimos los análisis
#tab <- tab2021 tab <- tab2024
tab <- tab2012a2024

## Corrige unos errores 
sort(unique(tab$SUBTIPO))
table(tab$SUBTIPO)
tab$SUBTIPO[tab$SUBTIPO == "PRESA "] <- "PRESA"
tab$SUBTIPO[tab$SUBTIPO == "RIO"] <- "RÍO"
tab[tab == "-"] <- NA
tab[tab == "ND"] <- NA
summary(tab)
head(tab)
names(tab)
unique(tab$TIPO)
unique(tab$SUBTIPO)
tab$SEMAFORO
table(tab$SUBTIPO)

# Hay muchos subtipos, simplifica en SUBTIPO2
tab$SUBTIPO2 <- tab$SUBTIPO
frec <- table(tab$SUBTIPO)
pequenos <- names(frec)[frec < 20]
tab$SUBTIPO2[tab$SUBTIPO2 %in% pequenos] <- "OTROS"

head(tab)
table(tab$SUBTIPO2)

### Crea a sf object
# Load required libraries
library(sf)
library(dplyr)

# ---- 1. Example: your dataframe ----
# Replace 'df' with the name of your dataframe
# It must contain columns named LONGITUD and LATITUD

# Check structure (optional)
str(tab)

# ---- 2. Convert dataframe to sf object ----
sf_points <- st_as_sf(
  tab,
  coords = c("LONGITUD", "LATITUD"),
  crs = 4326,          # WGS84 geographic coordinates
  remove = FALSE       # keeps original LONGITUD/LATITUD columns
)

# ---- 3. Save as GeoPackage ----
st_write(
  sf_points,
  "points_CONAGUA_2012a24.gpkg",
  layer = "points_layer",
  delete_layer = TRUE   # overwrite if layer exists
)

# ---- 4. (Optional) Verify ----
print(sf_points)

############ Elaboración de mapas
# Load libraries
library(sf)
library(tmap)
library(dplyr)
library(ggplot2)

opcion <- "todomx"
opcion <- "centro"

# Ensure CRS
sf_points <- st_transform(sf_points, 4326)

# Ensure factors
sf_crop$SEMAFORO  <- factor(sf_crop$SEMAFORO,
                            levels = c("ROJO","AMARILLO","VERDE"))

sf_crop$SUBTIPO2  <- as.factor(sf_crop$SUBTIPO2)

# Define colors
semaforo_colors <- c(
  ROJO = "red",
  AMARILLO = "yellow",
  VERDE = "green"
)

## Recorte a la ventana espacial (todo México o centro)
if (opcion == "todomx"){
  UL_lon <- -117.08
  UL_lat <- 32.72
  BR_lon <- -86.71
  BR_lat <- 14.54
} else {
  UL_lon <- -103.7
  UL_lat <- 21.8
  BR_lon <- -98
  BR_lat <- 17.9
}

bbox_section <- st_bbox(
  c(xmin = UL_lon,
    xmax = BR_lon,
    ymin = BR_lat,
    ymax = UL_lat),
  crs = st_crs(4326)
)
sf_crop <- st_crop(sf_points, bbox_section)


# ---------------------------
# 1️⃣ MAP WITH TMAP
# ---------------------------

tmap_mode("plot")  # interactive map (use "plot" for static)

mapa <- tm_shape(sf_crop, bbox = bbox_section) +
  tm_basemap("OpenStreetMap") +
  
  # Graticule
  tm_graticules(
    lwd = 1,
    col = "gray60",
    alpha = 0.6,
    labels.size = 0.7
  ) +
  
  tm_symbols(
    fill = "SEMAFORO",
    fill.scale = tm_scale(values = semaforo_colors),
    fill.legend = tm_legend(title = "Semáforo"),
    
    shape = "SUBTIPO2",   # let tmap auto-assign shapes
    shape.legend = tm_legend(title = "Subtipo"),
    
    size = 0.3,           # make symbols clearly visible
    col = "black",
    lwd = 1
  ) +
  tm_layout(legend.outside = TRUE)

print(mapa)
tmap_save(mapa,paste0("mapita_conagua_",opcion,".png"), dpi = 300)

# ---------------------------
# 2️⃣ BARPLOT OF PROPORTIONS
# ---------------------------

head(tab)
nrow(tab)

# Cargar librerías necesarias
library(ggplot2)
library(dplyr)

# Definir el orden de los niveles de SEMAFORO
semaforo_order <- c("ROJO", "AMARILLO", "VERDE")
semaforo_colors <- c("ROJO" = "red", "AMARILLO" = "yellow", "VERDE" = "green")

# Crear un dataframe con los conteos por SUBTIPO2 y SEMAFORO
conteos <- tab %>%
  group_by(SUBTIPO2, SEMAFORO) %>%
  summarise(conteo = n(), .groups = 'drop') %>%
  # Asegurar que SEMAFORO sea un factor con el orden especificado
  mutate(SEMAFORO = factor(SEMAFORO, levels = semaforo_order))

# Verificar los conteos
print("Conteos por SUBTIPO2 y SEMAFORO:")
print(conteos)
# Opción 3: Versión más detallada con ggplot2 (incluyendo etiquetas)
ggplot(conteos, aes(x = SUBTIPO2, y = conteo, fill = SEMAFORO)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = conteo), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5,
            size = 3) +
  scale_fill_manual(values = semaforo_colors, 
                    breaks = semaforo_order) +
  labs(title = "Número de casos por SUBTIPO y SEMAFORO",
       subtitle = paste("Total de registros:", nrow(tab)),
       x = "SUBTIPO",
       y = "Número de casos",
       fill = "SEMÁFORO") +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom")

# Opción 4: Si quieres guardar el gráfico
ggsave("barplot_subtipo2_semaforo.png", 
       width = 10, 
       height = 6, 
       dpi = 300)

# Análisis adicional: Tabla resumen
tabla_resumen <- tab %>%
  group_by(SUBTIPO2, SEMAFORO) %>%
  summarise(conteo = n(), .groups = 'drop') %>%
  tidyr::pivot_wider(names_from = SEMAFORO, 
                     values_from = conteo, 
                     values_fill = 0) %>%
  select(SUBTIPO2, all_of(semaforo_order))

print("Tabla resumen:")
print(tabla_resumen)

# Calcular porcentajes
tabla_porcentajes <- tabla_resumen %>%
  mutate(Total = ROJO + AMARILLO + VERDE,
         Pct_ROJO = round(ROJO / Total * 100, 1),
         Pct_AMARILLO = round(AMARILLO / Total * 100, 1),
         Pct_VERDE = round(VERDE / Total * 100, 1))

print("Tabla con porcentajes:")
print(tabla_porcentajes)

zzz <- xtable(tabla_porcentajes, label = "tab:tabporcentaje", 
              caption = "Porcentaje de los diferentes tipos de cuerpo de agua 
en las tres categorías del semáforo (periodo 2012-
2024)", align = c("l","l","r","r","r","r","r","r","r"))
print(zzz)

## Análisis Presas
sort(unique(tab$SUBTIPO))
tab_presas <- tab[tab$SUBTIPO == "PRESA",]
table(tab_presas$SEMAFORO)
round(100*table(tab_presas$SEMAFORO)/sum(table(tab_presas$SEMAFORO)),2)
# AMARILLO     ROJO    VERDE (2012-2024)
# 59      173      478 (8.31    24.37    67.32 %)
59    +  173    +  478
8.31  +  24.37
### 2021

tab_presas2021 <- tab2021[tab2021$SUBTIPO == "PRESA",]
table(tab_presas2021$SEMAFORO)
round(100*table(tab_presas2021$SEMAFORO)/sum(table(tab_presas2021$SEMAFORO)),2)
# Amarillo     Rojo    Verde (2021) 
# 41       57       60 
# Amarillo     Rojo    Verde 
# 25.95    36.08    37.97 

### 2023
tab_presas2023 <- tab2023[tab2023$SUBTIPO == "PRESA",]
table(tab_presas2023$SEMAFORO)
round(100*table(tab_presas2023$SEMAFORO)/sum(table(tab_presas2023$SEMAFORO)),2)
# Amarillo     Rojo    Verde (2023)
#      9       33       18  
# AMARILLO     ROJO    VERDE (2023)
#      15       55       30 
100 - 30 # = 70%
9     +  33   +    18  # 60
### 2024
tab_presas2024 <- tab2024[tab2024$SUBTIPO == "PRESA",]
table(tab_presas2024$SEMAFORO)
round(100*table(tab_presas2024$SEMAFORO)/sum(table(tab_presas2024$SEMAFORO)),2)
# AMARILLO     ROJO    VERDE 
# 8       34       26 = 68
# 
# AMARILLO     ROJO    VERDE 
# 11.76    50.00    38.24 
sum(table(tab_presas2024$SEMAFORO))
100-38.24 

## Lerma
tab_presas_Lerma <- tab_presas[tab_presas$ORGANISMO_DE_CUENCA == "LERMA SANTIAGO PACÍFICO",]
table(tab_presas_Lerma$SEMAFORO)
round(100*table(tab_presas_Lerma$SEMAFORO)/sum(table(tab_presas_Lerma$SEMAFORO)),2)
# AMARILLO  ROJO    VERDE 
# 15        75       87   (8.47    42.37    49.15 % )

### Lerma 2023
unique(tab2023$ORGANISMO_DE_CUENCA)
tab_presasL2023 <- tab2023[tab2023$SUBTIPO == "PRESA" & tab2023$ORGANISMO_DE_CUENCA == "LERMA SANTIAGO PACIFICO",]
table(tab_presasL2023$SEMAFORO)
round(100*table(tab_presasL2023$SEMAFORO)/sum(table(tab_presasL2023$SEMAFORO)),2)
# Amarillo     Rojo    Verde (2023)
#      1       12       6  
# AMARILLO     ROJO    VERDE (2023)
#      5.26    63.16    31.58 
100 - 31.58 # = 68.4%
1   +    12    +   6   # 19

### 2024
unique(tab2024$ORGANISMO_DE_CUENCA)
tab_presasL2024 <- tab2024[tab2024$SUBTIPO == "PRESA" & tab2024$ORGANISMO_DE_CUENCA == "LERMA SANTIAGO PACÍFICO",]
table(tab_presasL2024$SEMAFORO)
round(100*table(tab_presasL2024$SEMAFORO)/sum(table(tab_presasL2024$SEMAFORO)),2)
# ROJO VERDE 
# 9     9 
# ROJO VERDE 
# 50    50

### Análisis DBO 2012-2024
sum(table(tab_presas$CALIDAD_DBO)) # 49
tab_presas$DBO_mg.L

table(tab_presas$CALIDAD_DQO)
sum(table(tab_presas$CALIDAD_DQO)) # 53

table(tab_presas$CALIDAD_SST)
sum(table(tab_presas$CALIDAD_SST)) # 50

indices <- c(
  "CALIDAD_DBO",
  "CALIDAD_DQO",
  "CALIDAD_SST",
  "CALIDAD_COLI_FEC",
  "CALIDAD_E_COLI",
  "CALIDAD_ENTEROC",
  "CALIDAD_OD_PORC",
  "CALIDAD_TOX_D_48")

niveles <- c("Fuertemente contaminada", "Contaminada", "Aceptable", "Buena calidad", "Excelente")              

indice <- indices[1]
for (indice in indices){ 
  column_num <- match(indice, names(tab_presas))
  niveles_encontrados <- unique(na.omit(tab_presas[,column_num]))
  total <- sum(table(tab_presas[,column_num]))
  print(paste0("*************", indice," (",total," sitios)"))
  if (min(niveles_encontrados %in% niveles) == 1) {
    print(table(factor(tab_presas[,column_num], 
                       levels = niveles, 
                       ordered = TRUE)))
  } else {print(paste("niveles:",niveles_encontrados))} # if
}
