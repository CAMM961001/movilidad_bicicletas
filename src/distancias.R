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
ARRIBO_PATH <- paste0(ROOT, '/datos/puntos_arribo_shp/')
STCM_PATH <- paste0(ROOT, '/datos/stcmetro_shp/')
MB_PATH <- paste0(ROOT, '/datos/mb_shp/')


# -------------------------------------------------------------- Carga de datos


stations_shp <- readOGR(paste0(CARTOG_PATH, 'ECOBICI_Cicloestaciones.shp'))
parking_shp <- readOGR(paste0(CARTOG_PATH, 'Biciestacionamientos.shp'))
roads_shp <- readOGR(paste0(CARTOG_PATH, '7ma_Infraestructura_Vial_Ciclista_CDMX_01_23.shp'))
arrival_shp <- readOGR(paste0(ARRIBO_PATH, 'puntos_de_arribo_sitis.shp'))
metro_shp <- readOGR(paste0(STCM_PATH, 'STC_Metro_estaciones_utm14n.shp'))
mb_shp <- readOGR(paste0(MB_PATH, 'Metrobus_estaciones_utm14n.shp'))

target_proj <- parking_shp@proj4string@projargs
stations_shp <- spTransform(stations_shp, target_proj)
roads_shp <- spTransform(roads_shp, target_proj)
arrival_shp <- spTransform(arrival_shp, target_proj)
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
# ========================= Biciestacionamientos ==============================
# =============================================================================


# ------------------------------------------------------ Matrices de distancias


# ¿Qué estación del metro queda a menor distancia de cada estacionamiento?
# Para resolverlo, se obtiene el ID de la estación de metro con min. dist.

# Matriz de distancias con dimensiones n_stcm x n_prk
# n_stcm: Número de estaciones de metro
# n_prk: Número de biciestacionamientos
dist_stcm_prk <- apply(
  X=metro_shp@coords
  ,MARGIN=1
  ,FUN=evaluar_haversine
  ,spdf_=parking_shp@coords
) |>
  t() |>
  as_tibble(.name_repair = 'minimal')

min_stcm <- unique(apply(dist_stcm_prk, 2, get_dist_id, type='min'))

# Visualización de estaciones de metro
mask_shp <- metro_shp[min_stcm,]

df_temp <- mask_shp@coords |> 
  as_tibble() |>
  rename(lon=coords.x1, lat=coords.x2) |>
  cbind(mask_shp@data)

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

# ¿Qué estación del metrobús queda a menor distancia de cada estacionamiento?
# Mismo enfoque

# Matriz de distancias con dimensiones n_stcm x n_prk
# n_stcm: Número de estaciones de metro
# n_prk: Número de biciestacionamientos
dist_mb_prk <- apply(
  X=mb_shp@coords
  ,MARGIN=1
  ,FUN=evaluar_haversine
  ,spdf_=parking_shp@coords
) |>
  t() |>
  as_tibble(.name_repair = 'minimal')

min_mb <- unique(apply(dist_mb_prk, 2, get_dist_id, type='min'))

# Visualización de estaciones de metro
mask_shp <- mb_shp[min_mb,]

df_temp <- mask_shp@coords |> 
  as_tibble() |>
  rename(lon=coords.x1, lat=coords.x2) |>
  cbind(mask_shp@data)

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


# ------------------------------------ Segmentación Voronoi de estacionamientos


esc_aux <- select(
    data.frame(parking_shp@coords)
    ,coords.x2
    ,coords.x1) |>
  unique()

#Partición Voronoi en ggplot() con las escuelas en la CDMX
ggplot(data = esc_aux, aes(coords.x2, coords.x1)) +
  stat_voronoi(geom="path") +
  geom_point()

#Nota que las líneas anteriores sólo son una gráfica en ggplot()

#Para generar un objeto espacial de Voronoi puedes utilizar una funcion de ggvoronoi()
vor_diag <- voronoi_polygon(
  esc_aux
  ,x="coords.x2"
  ,y="coords.x1")
vor_diag@proj4string@projargs <- target_proj


# ------------------------------------------ Intersección Voronoi con STCM y MB


