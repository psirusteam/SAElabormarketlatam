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
  readRDS("BRA/1.Ingreso/Data/tasa_desocupacion.rds")


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
### Arreglar la shape BRA                       ###
### Aplicar el CONVEX HULL a los multipolígonos ###
###################################################

## revisando BRAentina
BRA <- read_sf("BRA/ShapeDeptoBRA/UFEBRASIL.shp")
BRA %<>% mutate(depto = str_pad(CD_GEOCODU, pad = "0", width = 2))

###################
### Luminosidad ###
###################
luces = ee$ImageCollection("NOAA/DMSP-OLS/NIGHTTIME_LIGHTS") %>%
  ee$ImageCollection$filterDate("2013-01-01", "2014-01-01") %>%
  ee$ImageCollection$map(function(x) x$select("stable_lights")) %>%
  ee$ImageCollection$toBands()
ee_print(luces)

BRA_luces <- map(unique(BRA$depto),
                 ~tryCatch(ee_extract(
                   x = luces,
                   y = BRA["depto"] %>% filter(depto == .x),
                   ee$Reducer$mean(),
                   sf = FALSE
                 ) %>% mutate(depto = .x),  error = function(e)data.frame(depto = .x)))

BRA_luces %<>% bind_rows()

#################
### Urbanismo ###
#################

tiposuelo = ee$ImageCollection("COPERNICUS/Landcover/100m/Proba-V-C3/Global") %>%
  ee$ImageCollection$filterDate("2016-01-01", "2016-12-31") %>%
  ee$ImageCollection$map(function(x) x$select("urban-coverfraction", "crops-coverfraction")) %>% 
  ee$ImageCollection$toBands()
ee_print(tiposuelo)

BRA_urbano_cultivo <- map(unique(BRA$depto),
                 ~tryCatch(ee_extract(
                   x = tiposuelo,
                   y = BRA["depto"] %>% filter(depto == .x),
                   ee$Reducer$mean(),
                   sf = FALSE
                 ) %>% mutate(depto = .x), 
                 error = function(e)data.frame(depto = .x)))

BRA_urbano_cultivo %<>% bind_rows()

###############
### Guardar ###
###############

tasa_desocupacion %<>% 
  full_join(BRA_luces) %>% 
  full_join(BRA_urbano_cultivo)


cor(tasa_desocupacion[, -1 ])
saveRDS(tasa_desocupacion, "BRA/1.Ingreso/Data/tasa_desocupacion.rds")
