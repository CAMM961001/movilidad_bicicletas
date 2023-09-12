# ---------------------------------------------------------- Carga de librerías


library(tidyverse)
library(geosphere)
library(leaflet)
library(rgdal)
library(sp)
library(sf)


# ------------------------------------------------------------------- Funciones


evaluar_haversine <- function(punto, spdf_){
  #' Función para evaluar distancias Haversine
  #' de un punto dado a todos los puntos de un 
  #' dataframe de puntos espaciales
  #'
  #' Parámetros
  #'    punto: Punto de coordenadas (lon,lat)
  #'    spdf_: Dataframe de coordenadas (lon, lat)
  #'
  #' Salidas
  #'    dist_vec: Vector con las distancias de 
  #'      un delito a todas las cámaras
  
  dist_vec <- distHaversine(
    p1=spdf_
    ,p2=c(punto[1], punto[2])
    ,r=6378137
  )
  
  return(dist_vec)
}

extraer_interseccion <- function(df_, r){
  #' Función para obtener el ID de los elementos
  #' que satisfacen una condición de radio de
  #' cobertura

  # Evaluar radio de cobertura
  flag <- (df_ <= r)
  
  # Número de elementos que satisfacen condición
  flag <- rowSums(flag)
  
  # ID de elementos que satisfacen condición al menos una vez
  flag <- which(flag >= 1)
  
  return(flag)
}


# ---------------------------------------------------- Ruta relativa a archivos


ROOT <- getwd()
CARTOG_PATH <- paste0(ROOT, '/datos/cartografia_shp/')
DELITOS_PATH <- paste0(ROOT, '/datos/delitos/')


# -------------------------------------------------------------- Carga de datos


stations_shp <- readOGR(paste0(CARTOG_PATH, 'ECOBICI_Cicloestaciones.shp'))

delitos_df <- read_csv(
    paste0(DELITOS_PATH, 'carpetas_2022-2023.csv')
    ,show_col_types = FALSE) |>
  filter(
    ao_hechos == 2023
    ,mes_hechos %in% c('Enero','Febrero','Marzo','Abril')
    ,alcaldia_hechos %in% c('CUAUHTEMOC', 'BENITO JUAREZ', 'MIGUEL HIDALGO'))

delitos_latlng <- SpatialPoints(
  delitos_df |> select(longitud, latitud)
  ,proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs")
)


# Matriz de distancias con dimensiones n_dels x n_estaciones
matriz_distancias <- apply(
    X=delitos_latlng@coords
    ,MARGIN=1
    ,FUN=evaluar_haversine
    ,spdf_=stations_shp@coords) |>
  t() |>
  as_tibble(.name_repair = 'minimal')

# Evaluar elementos en un radio de 100 mts.
ids_condicion <- extraer_interseccion(matriz_distancias, 100)

# Filtrar ids extraidos y almacenar archivo
write.csv(
  delitos_df[ids_condicion,]
  ,file = paste0(DELITOS_PATH, 'delitos_filtrados.csv')
  ,row.names = FALSE)

