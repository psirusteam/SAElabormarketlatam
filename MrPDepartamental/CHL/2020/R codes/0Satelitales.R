#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################

### Cleaning R environment ###

rm(list = ls())

#################
### Libraries ###
#################
memory.limit(500000)

library(tidyverse)
library(sampling)
library(reticulate) # Conexión con Python
library(rgee) # Conexión con Google Earth Engine
library(sf) # Paquete para manejar datos geográficos
library(concaveman)
library(geojsonio)
library(magrittr)

####################################################
### Loading datasets: EH and Population census ###
####################################################
tasa_desocupacion <-
  readRDS("CHL/1.Ingreso/Data/tasa_desocupacion.rds")


#######################################
### configuración inicial de Python ###
#######################################

#rgee_environment_dir = "C://Users//agutierrez1//Anaconda3//envs//rgee_py//python.exe"
rgee_environment_dir = "C://Users//sguerrero//Anaconda3//envs//rgee_py//python.exe"
# Configurar python (Algunas veces no es detectado y se debe reiniciar R)
reticulate::use_python(rgee_environment_dir, required = T)
rgee::ee_install_set_pyenv(py_path = rgee_environment_dir, py_env = "rgee_py")
Sys.setenv(RETICULATE_PYTHON = rgee_environment_dir)
Sys.setenv(EARTHENGINE_PYTHON = rgee_environment_dir)
rgee::ee_Initialize(drive = T)

###################################################
### Arreglar la shape CHL                       ###
### Aplicar el CONVEX HULL a los multipolígonos ###
###################################################

## revisando CHLentina
CHL <- read_sf("CHL/ShapeDeptoCHL/Regional.shp")
CHL %<>% mutate(depto = str_pad(codregion, pad = "0", width = 2)) %>%
  filter(depto != "00")

## Region  10 
mls <- CHL["depto"] %>% filter(depto == "10") 

plot(mls)

mls.envelope <- mls %>%
  st_combine() %>%
  st_cast("POINT") %>%
  st_sf() %>%
  concaveman()

plot(mls.envelope)

mls.envelope %<>% 
  mutate(depto = "10",
         nombre = "Región de Los Lagos",
         pais = "Chile" ) %>% 
  rename(geometry = polygons)

CHL <- bind_rows(mls.envelope,
                 CHL  %>% filter(depto != "10")  )

## Region  11 
mls <- CHL["depto"] %>% filter(depto == "11") 

plot(mls)

mls.envelope <- mls %>%
  st_combine() %>%
  st_cast("POINT") %>%
  st_sf() %>%
  concaveman()

plot(mls.envelope)

mls.envelope %<>% 
  mutate(depto = "11",
         nombre = "Región de Aysén del Gral.Ibañez del Campo Chile",
         pais = "Chile" ) %>% 
  rename(geometry = polygons)

CHL <- bind_rows(mls.envelope,
                 CHL  %>% filter(depto != "11")  )


## Region  12 
mls <- CHL["depto"] %>% filter(depto == "12") 

plot(mls)

mls.envelope <- mls %>%
  st_combine() %>%
  st_cast("POINT") %>%
  st_sf() %>%
  concaveman()

plot(mls.envelope)

mls.envelope %<>% 
  mutate(depto = "12",
         nombre = "Región de Magallanes y Antártica Chilena  Chile",
         pais = "Chile" ) %>% 
  rename(geometry = polygons)

CHL <- bind_rows(mls.envelope,
                 CHL  %>% filter(depto != "12")  )


## Region  05 
mls <- CHL["depto"] %>% filter(depto == "05") 

plot(mls)

mls.envelope <- mls %>%
  st_combine() %>%
  st_cast("POINT") %>%
  st_sf() %>%
  concaveman()

plot(mls.envelope)

mls.envelope %<>% 
  mutate(depto = "05",
         nombre = "Región de Valparaíso",
         pais = "Chile" ) %>% 
  rename(geometry = polygons)

CHL <- bind_rows(mls.envelope,
                 CHL  %>% filter(depto != "05")  )



plot(CHL["depto"])


###################
### Luminosidad ###
###################

luces = ee$ImageCollection("NOAA/DMSP-OLS/NIGHTTIME_LIGHTS") %>%
  ee$ImageCollection$filterDate("2013-01-01", "2014-01-01") %>%
  ee$ImageCollection$map(function(x) x$select("stable_lights")) %>%
  ee$ImageCollection$toBands()
ee_print(luces)

CHL_luces <- map(unique(CHL$depto),
                 ~tryCatch(ee_extract(
                   x = luces,
                   y = CHL["depto"] %>% filter(depto == .x),
                   ee$Reducer$mean(),
                   sf = FALSE
                 ) %>% mutate(depto = .x),  error = function(e)data.frame(depto = .x)))

CHL_luces %<>% bind_rows()

#################
### Urbanismo ###
#################

tiposuelo = ee$ImageCollection("COPERNICUS/Landcover/100m/Proba-V-C3/Global") %>%
  ee$ImageCollection$filterDate("2016-01-01", "2016-12-31") %>%
  ee$ImageCollection$map(function(x) x$select("urban-coverfraction", "crops-coverfraction")) %>% 
  ee$ImageCollection$toBands()
ee_print(tiposuelo)

CHL_urbano_cultivo <- map(unique(CHL$depto),
                 ~tryCatch(ee_extract(
                   x = tiposuelo,
                   y = CHL["depto"] %>% filter(depto == .x),
                   ee$Reducer$mean(),
                   sf = FALSE
                 ) %>% mutate(depto = .x),  error = function(e)data.frame(depto = .x)))

CHL_urbano_cultivo %<>% bind_rows()

###############
### Guardar ###
###############

tasa_desocupacion %<>% 
  full_join(CHL_luces) %>% 
  full_join(CHL_urbano_cultivo)


cor(tasa_desocupacion[, -1 ])
saveRDS(tasa_desocupacion, "CHL/1.Ingreso/Data/tasa_desocupacion.rds")
