# Carga de librerías
library(tidyverse)
library(rgdal)

library(maps)
library(leaflet)

# Declaración de rutas y constantes
ROOT <- getwd()
CARTOG_PATH <- paste0(ROOT, '/datos/cartografia_shp/')
ARRIBO_PATH <- paste0(ROOT, '/datos/puntos_arribo_shp/')
STCM_PATH <- paste0(ROOT, '/datos/stcmetro_shp/')
MB_PATH <- paste0(ROOT, '/datos/mb_shp/')

# Carga de datos en memoria
stations_shp <- readOGR(paste0(CARTOG_PATH, 'ECOBICI_Cicloestaciones.shp'))
parking_shp <- readOGR(paste0(CARTOG_PATH, 'Biciestacionamientos.shp'))
roads_shp <- readOGR(paste0(CARTOG_PATH, '7ma_Infraestructura_Vial_Ciclista_CDMX_01_23.shp'))
arrival_shp <- readOGR(paste0(ARRIBO_PATH, 'puntos_de_arribo_sitis.shp'))
metro_shp <- readOGR(paste0(STCM_PATH, 'STC_Metro_estaciones_utm14n.shp'))
mb_shp <- readOGR(paste0(MB_PATH, 'Metrobus_estaciones_utm14n.shp'))

# Conciliación de proyección cartográfica entre arhicos
target_proj <- parking_shp@proj4string@projargs
stations_shp <- spTransform(stations_shp, target_proj)
roads_shp <- spTransform(roads_shp, target_proj)
arrival_shp <- spTransform(arrival_shp, target_proj)
metro_shp <- spTransform(metro_shp, target_proj)
mb_shp <- spTransform(mb_shp, target_proj)

# Visualización inicial de datos

# ---- Estaciones de ECOBICI ----

# Datos de visualización
df_temp <- stations_shp@coords |> 
  as_tibble() |>
  rename(lon=coords.x1, lat=coords.x2)

# Estilo de marcadores
icon_bicycle <- makeAwesomeIcon(
  icon='bicycle'
  ,markerColor='lightgreen'
  ,iconColor='black'
  ,library='fa')

# Visualización con leaflet
leaflet(data = df_temp) |>
  addAwesomeMarkers(
    df_temp$lon
    ,lat = df_temp$lat
    #,popup=rest$nom_estab
    ,icon = icon_bicycle) |>
  addTiles() |>
  addProviderTiles(providers$CartoDB.VoyagerLabelsUnder)

# ---- Estacionamientos ----

# Datos de visualización
df_temp <- parking_shp@coords |> 
  as_tibble() |>
  rename(lon=coords.x1, lat=coords.x2)

# Estilo de marcadores
icon_parking <- makeAwesomeIcon(
  icon='stop'
  ,markerColor='lightblue'
  ,iconColor='black'
  ,library='glyphicon')

# Visualización con leaflet
leaflet(data = df_temp) |>
  addAwesomeMarkers(
    df_temp$lon
    ,lat = df_temp$lat
    #,popup=rest$nom_estab
    ,icon = icon_parking) |>
  addTiles() |>
  addProviderTiles(providers$CartoDB.VoyagerLabelsUnder)

# ---- Puntos de arrivo ----

# Datos de visualización
df_temp <- arrival_shp@coords |> 
  as_tibble() |>
  rename(lon=coords.x1, lat=coords.x2)

# Estilo de marcadores
icon_arrival <- makeAwesomeIcon(
  icon='flag'
  ,markerColor='orange'
  ,iconColor='black'
  ,library='fa')

# Visualización con leaflet
leaflet(data = df_temp) |>
  addAwesomeMarkers(
    df_temp$lon
    ,lat = df_temp$lat
    #,popup=rest$nom_estab
    ,icon = icon_arrival) |>
  addTiles() |>
  addProviderTiles(providers$CartoDB.VoyagerLabelsUnder)

# ---- Sistema de Transporte Colectivo Metro ----

# Datos de visualización
df_temp <- metro_shp@coords |> 
  as_tibble() |>
  rename(lon=coords.x1, lat=coords.x2)

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
    ,lat = df_temp$lat
    #,popup=rest$nom_estab
    ,icon = icon_subway) |>
  addTiles() |>
  addProviderTiles(providers$CartoDB.VoyagerLabelsUnder)

# ---- Metrobús ----

# Datos de visualización
df_temp <- mb_shp@coords |> 
  as_tibble() |>
  rename(lon=coords.x1, lat=coords.x2)

# Estilo de marcadores
icon_bus <- makeAwesomeIcon(
  icon='bus'
  ,markerColor='yellow'
  ,iconColor='black'
  ,library='fa')

# Visualización con leaflet
leaflet(data = df_temp) |>
  addAwesomeMarkers(
    df_temp$lon
    ,lat = df_temp$lat
    #,popup=rest$nom_estab
    ,icon = icon_bus) |>
  addTiles() |>
  addProviderTiles(providers$CartoDB.VoyagerLabelsUnder)

# ---- Ciclovías ----
leaflet(roads_shp) |>
  addPolylines() |>
  addTiles() |>
  addProviderTiles(providers$CartoDB.VoyagerLabelsUnder)