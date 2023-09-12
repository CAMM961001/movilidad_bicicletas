# Carga de librerías
library(tidyverse)
library(rgdal)
library(maps)
library(leaflet)
library(leaflet.extras)
library(mapview)

# Declaración de rutas y constantes
ROOT <- getwd()
CARTOG_PATH <- paste0(ROOT, '/datos/cartografia_shp/')
CDMX_PATH <- paste0(ROOT, '/datos/alcaldias/')
STCM_PATH <- paste0(ROOT, '/datos/stcmetro_shp/')
MB_PATH <- paste0(ROOT, '/datos/mb_shp/')

# Carga de datos en memoria
stations_shp <- readOGR(paste0(CARTOG_PATH, 'ECOBICI_Cicloestaciones.shp'))
alcaldias_shp <- readOGR(paste0(CDMX_PATH, 'alcaldias.shp'), layer="alcaldias")
metro_shp <- readOGR(paste0(STCM_PATH, 'STC_Metro_estaciones_utm14n.shp'))
mb_shp <- readOGR(paste0(MB_PATH, 'Metrobus_estaciones_utm14n.shp'))


# Conciliación de proyección cartográfica entre arhicos
target_proj <- stations_shp@proj4string@projargs
alcaldias_shp <- spTransform(alcaldias_shp, target_proj)
metro_shp <- spTransform(metro_shp, target_proj)
mb_shp <- spTransform(mb_shp, target_proj)

# Visualización inicial de datos

# ---- Estaciones de ECOBICI ----

# Datos de visualización
df_ciclo <- stations_shp@coords |> 
  as_tibble() |>
  rename(lon=coords.x1, lat=coords.x2)

# Estilo de marcadores
icon_bicycle <- makeAwesomeIcon(
  icon='bicycle'
  ,markerColor='lightgreen'
  ,iconColor='black'
  ,library='fa')

# Visualización con leaflet
m1 <- leaflet(data = df_ciclo) |>
  setView(
    lng=-99.17094624232052
    ,lat=19.400519038362434
    ,zoom=15) |>
  addAwesomeMarkers(
    df_ciclo$lon
    ,lat = df_ciclo$lat
    #,popup=rest$nom_estab
    ,icon = icon_bicycle) |>
  addTiles() |>
  addProviderTiles(providers$CartoDB.Positron)

# ---- Mapa de la ciudad ----

leaflet(alcaldias_shp) |>
  addPolygons(
    color='#72b7b2'
    ,weight=2
    ,opacity=1
    ,fill=FALSE) |> 
  addTiles() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addCircles(
    ~df_ciclo$lon, ~df_ciclo$lat
    ,color='#54a24b'
    ,weight=1.5
    ,opacity=0.25
    ,fillOpacity=1)
  addTiles() |>
  addProviderTiles(providers$CartoDB.Positron)

# Como heatmap
m <- leaflet(alcaldias_shp) |>
  setView(
    lng=-99.16094624232052
    ,lat=19.340519038362434
    ,zoom=10.5) |>
  addPolygons(
    color='#009845'
    ,weight=2
    ,opacity=1
    ,fill=FALSE) |> 
  addTiles() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addHeatmap(lng=~df_ciclo$lon,lat=~df_ciclo$lat, max=8, radius=9, blur=10)

# Almacenar figura
mapshot(
  m
  ,file=paste0(ROOT, '/imagenes/concentracion_bicis.png')
  ,selfcontained=FALSE)


# ---- Sistema de Transporte Colectivo Metro ----

# Datos de visualización
df_metro <- metro_shp@coords |> 
  as_tibble() |>
  rename(lon=coords.x1, lat=coords.x2)

# Estilo de marcadores
icon_subway <- makeAwesomeIcon(
  icon='subway'
  ,markerColor='orange'
  ,iconColor='black'
  ,library='fa')

# Visualización con leaflet

m1 <- m1 |>
  addAwesomeMarkers(
    df_metro$lon
    ,lat = df_metro$lat
    ,icon = icon_subway) |>
  addTiles()

# ---- Metrobús ----

# Datos de visualización
df_mb <- mb_shp@coords |> 
  as_tibble() |>
  rename(lon=coords.x1, lat=coords.x2)

# Estilo de marcadores
icon_bus <- makeAwesomeIcon(
  icon='bus'
  ,markerColor='red'
  ,iconColor='black'
  ,library='fa')

# Visualización con leaflet
m1 <- m1 |>
  addAwesomeMarkers(
    df_mb$lon
    ,lat = df_mb$lat
    ,icon = icon_bus) |>
  addTiles() |>
  addProviderTiles(providers$CartoDB.Positron)

# Almacenar figura
mapshot(
  m1
  ,file=paste0(ROOT, '/imagenes/conectividad.png')
  ,selfcontained=FALSE)
