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
  readRDS("MrPDepartamental/ARG/2020/1.Ingreso/Data/tasa_desocupacion.rds")


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
### Arreglar la shape ARG                       ###
### Aplicar el CONVEX HULL a los multipolígonos ###
###################################################

## revisando Argentina
ARG <- read_sf("MrPDepartamental/ARG/2020/ShapeDeptoARG/provincia.shp") %>% 
  mutate(depto = in1, nombre = fna) %>% 
  select(depto)
## provincia 06 
mls <- ARG["depto"] %>% filter(depto == "06") 
plot(mls)

mls.envelope <- mls %>%
  st_combine() %>%
  st_cast("POINT") %>%
  st_sf() %>%
  concaveman()
plot(mls.envelope)

mls.envelope %<>% 
  mutate(depto = "06",
         nombre = "Provincia de Buenos Aires") %>% 
  rename(geometry = polygons)

ARG <- bind_rows(mls.envelope,
                 ARG  %>% filter(depto != "06")  )

## provincia 94 
mls <- ARG["depto"] %>% filter(depto == "94") 
plot(mls)

mls.envelope <- mls %>%
  st_combine() %>%
  st_cast("POINT") %>%
  st_sf() %>%
  concaveman()

plot(mls.envelope)

mls.envelope %<>% 
  mutate(depto = "94",
         nombre = "Antártida e Islas del Atlántico Sur") %>% 
  rename(geometry = polygons)

ARG <- bind_rows(mls.envelope,
                 ARG  %>% filter(depto != "94")  )

plot(ARG["depto"])

###################
### Luminosidad ###
###################

luces = ee$ImageCollection("NOAA/DMSP-OLS/NIGHTTIME_LIGHTS") %>%
  ee$ImageCollection$filterDate("2013-01-01", "2014-01-01") %>%
  ee$ImageCollection$map(function(x) x$select("stable_lights")) %>%
  ee$ImageCollection$toBands()
ee_print(luces)

ARG_luces <- map(unique(ARG$depto),
                 ~tryCatch(ee_extract(
                   x = luces,
                   y = ARG["depto"] %>% filter(depto == .x),
                   ee$Reducer$mean(),
                   sf = FALSE
                 ) %>% mutate(depto = .x),  error = function(e)data.frame(depto = .x)))

ARG_luces %<>% bind_rows()

#################
### Urbanismo ###
#################

tiposuelo = ee$ImageCollection("COPERNICUS/Landcover/100m/Proba-V-C3/Global") %>%
  ee$ImageCollection$filterDate("2016-01-01", "2016-12-31") %>%
  ee$ImageCollection$map(function(x) x$select("urban-coverfraction", "crops-coverfraction")) %>% 
  ee$ImageCollection$toBands()
ee_print(tiposuelo)

ARG_urbano_cultivo <- map(unique(ARG$depto),
                 ~tryCatch(ee_extract(
                   x = tiposuelo,
                   y = ARG["depto"] %>% filter(depto == .x),
                   ee$Reducer$mean(),
                   sf = FALSE
                 ) %>% mutate(depto = .x),  error = function(e)data.frame(depto = .x)))

ARG_urbano_cultivo %<>% bind_rows()

###############
### Guardar ###
###############

tasa_desocupacion %<>% 
  full_join(ARG_luces) %>% 
  full_join(ARG_urbano_cultivo)


cor(tasa_desocupacion[, -1 ])
saveRDS(tasa_desocupacion, "ARG/2020/1.Ingreso/Data/tasa_desocupacion.rds")
