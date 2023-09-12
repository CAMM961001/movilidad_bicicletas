# Carga de librerías
library(tidyverse)
library(geosphere)
library(rgdal)
library(sp)
library(leaflet)
library(ggvoronoi)


# ---------------------------------------------------- Ruta relativa a archivos


ROOT <- getwd()
CARTOG_PATH <- paste0(ROOT, '/datos/cartografia_shp/')
STCM_PATH <- paste0(ROOT, '/datos/stcmetro_shp/')
MB_PATH <- paste0(ROOT, '/datos/mb_shp/')


# -------------------------------------------------------------- Carga de datos


stations_shp <- readOGR(paste0(CARTOG_PATH, 'ECOBICI_Cicloestaciones.shp'))
metro_shp <- readOGR(paste0(STCM_PATH, 'STC_Metro_estaciones_utm14n.shp'))
mb_shp <- readOGR(paste0(MB_PATH, 'Metrobus_estaciones_utm14n.shp'))

target_proj <- stations_shp@proj4string@projargs
metro_shp <- spTransform(metro_shp, target_proj)
mb_shp <- spTransform(mb_shp, target_proj)


# ------------------------------------------------------------------- Funciones


evaluar_haversine <- function(coord_pair, spdf_){
  # Función para evaluar distancias Haversine
  # de un delito dado a todas las cámaras
  #
  # Parámetros
  #     delito: Punto de coordenadas (lon,lat)
  #
  # Salidas
  #     dist_vec: Vector con las distancias de 
  #     un delito a todas las cámaras
  # -------------------------------------------
  
  dist_vec <- distHaversine(
    p1=spdf_
    ,p2=c(coord_pair[1], coord_pair[2])
    ,r=6378137
  )
  
  return(dist_vec)
}


eficiencia_cobertura <- function(df_, r){
  # Función para evaluar la eficiencia de
  # cobertura del conjunto de cámaras
  #
  # Parámetros
  #     df_: Matriz distancias n_dels x n_cams
  #     r: Radio de cobertura p/cámara
  #
  # Salidas
  #     eficiencia: Eficiencia de cobertura
  # -------------------------------------------
  
  # Evaluar delitos dentro de r
  flag <- (df_ <= r)
  
  # Cámaras que captan cada delito (Sumar por fila)
  flag <- rowSums(flag)
  
  # Delitos captados por al menos una cámara
  flag <- sum(flag >= 1)
  
  # Métrica de eficiencia
  eficiencia <- flag / nrow(df_)
  
  return(eficiencia)
}


get_dist_id <- function(col, type='min'){
  # Función para extraer el ID del objeto con
  # la menor distancia de referencia
  #
  # Parámetros
  #     col: Columna de distancias
  #     type: Tipo de distancia a computar,
  #       default regresa distancia mínima
  #
  # Salidas
  #     id_min: ID del objeto con mínima dist
  # -------------------------------------------
  
  if (type == 'min'){mask <- min(col)}
  else if(type == 'max'){mask <- max(col)}
  
  idx_ <- which(col == mask)
  
  return(idx_)
}


# =============================================================================
# ======================= Distancias a otros servicios ========================
# =============================================================================


# ------------------------------------------------------ Matrices de distancias


# ¿Qué estación del metro queda a menor distancia de cada estación?
# Para resolverlo, se obtiene el ID de la estación de metro con min. dist.

# Matriz de distancias con dimensiones n_stcm x n_ecob
# n_stcm: Número de estaciones de metro
# n_ecob: Número de estaciones de ecobici
dist_stcm_ecob <- apply(
  X=metro_shp@coords
  ,MARGIN=1
  ,FUN=evaluar_haversine
  ,spdf_=stations_shp@coords
) |>
  t() |>
  as_tibble(.name_repair = 'minimal')

# Estaciones de metro más cercanas a una estación de ecobici
min_stcm <- unique(apply(dist_stcm_ecob, 2, get_dist_id, type='min'))

# Visualización de estaciones de metro
mask_stcm <- metro_shp[min_stcm,]

df_temp <- mask_stcm@coords |> 
  as_tibble() |>
  rename(lon=coords.x1, lat=coords.x2) |>
  cbind(mask_stcm@data)

# Estilo de marcadores
icon_subway <- makeAwesomeIcon(
  icon='subway'
  ,markerColor='purple'
  ,iconColor='black'
  ,library='fa')

# Visualización con leaflet
leaflet(data = df_temp) |>
  addAwesomeMarkers(
    df_temp$lon
    ,lat=df_temp$lat
    ,popup=paste(df_temp$LINEA, df_temp$NOMBRE)
    ,icon=icon_subway) |>
  addTiles() |>
  addProviderTiles(providers$CartoDB.VoyagerLabelsUnder)


# ¿Qué estación del metrobús queda a menor distancia de cada estación?
# Para resolverlo, se obtiene el ID de la estación de metro con min. dist.

# Matriz de distancias con dimensiones n_stcm x n_prk
# n_stcm: Número de estaciones de metro
# n_prk: Número de biciestacionamientos
dist_mb_ecob <- apply(
  X=mb_shp@coords
  ,MARGIN=1
  ,FUN=evaluar_haversine
  ,spdf_=stations_shp@coords
) |>
  t() |>
  as_tibble(.name_repair = 'minimal')

# Estaciones de MB más cercanas a una estación de ecobici
min_mb <- unique(apply(dist_mb_ecob, 2, get_dist_id, type='min'))

# Visualización de estaciones de metro
mask_mb <- mb_shp[min_mb,]

df_temp <- mask_mb@coords |> 
  as_tibble() |>
  rename(lon=coords.x1, lat=coords.x2) |>
  cbind(mask_mb@data)

# Estilo de marcadores
icon_bus <- makeAwesomeIcon(
  icon='bus'
  ,markerColor='red'
  ,iconColor='black'
  ,library='fa')

# Visualización con leaflet
leaflet(data = df_temp) |>
  addAwesomeMarkers(
    df_temp$lon
    ,lat=df_temp$lat
    ,popup=paste(df_temp$LINEA, df_temp$NOMBRE)
    ,icon=icon_bus) |>
  addTiles() |>
  addProviderTiles(providers$CartoDB.VoyagerLabelsUnder)


# ----------------------------------------------------- Eficiencia de cobertura


# ¿Cuál es la eficiencia de cobertura de las estaciones del metro a menos de 
# 500 mts. de una estación de ecobici considerando aquellas estaciones en la
# zona de cobertura de ecobici.

# Matriz de distancias con dimensiones n_stcm x n_ecob
# n_stcm: Número de estaciones de metro
# n_ecob: Número de estaciones de ecobici
dist_stcm_mask <- apply(
  X=mask_stcm@coords
  ,MARGIN=1
  ,FUN=evaluar_haversine
  ,spdf_=stations_shp@coords
) |>
  t() |>
  as_tibble(.name_repair = 'minimal')

# Eficiencia de cobertura de las estaciones del metro
radio_cobertura <- 480
eficiencia_stcm <- eficiencia_cobertura(dist_stcm_mask, radio_cobertura)


# ¿Cuál es la eficiencia de cobertura de las estaciones del metrobús a menos de 
# 500 mts. de una estación de ecobici considerando aquellas estaciones en la
# zona de cobertura de ecobici.

# Matriz de distancias con dimensiones n_stcm x n_ecob
# n_stcm: Número de estaciones de metro
# n_ecob: Número de estaciones de ecobici
dist_mb_mask <- apply(
  X=mask_mb@coords
  ,MARGIN=1
  ,FUN=evaluar_haversine
  ,spdf_=stations_shp@coords
) |>
  t() |>
  as_tibble(.name_repair = 'minimal')

# Eficiencia de cobertura de las estaciones del metro
eficiencia_mb <- eficiencia_cobertura(dist_mb_mask, radio_cobertura)

