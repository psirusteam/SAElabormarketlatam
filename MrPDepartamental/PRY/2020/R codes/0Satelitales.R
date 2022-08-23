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
  readRDS("PRY/2020/1.Ingreso/Data/tasa_desocupacion.rds") %>% 
  select(depto, tasa_desocupacion) %>% unique()


#######################################
### configuración inicial de Python ###
#######################################

rgee_environment_dir = "C://Users//sguerrero//Anaconda3//envs//rgee_py//python.exe"
#rgee_environment_dir =  "C:/Users/agutierrez1/Anaconda3/envs/rgee_py/python.exe"
# Configurar python (Algunas veces no es detectado y se debe reiniciar R)
reticulate::use_python(rgee_environment_dir, required = T)
rgee::ee_install_set_pyenv(py_path = rgee_environment_dir, py_env = "rgee_py")
Sys.setenv(RETICULATE_PYTHON = rgee_environment_dir)
Sys.setenv(EARTHENGINE_PYTHON = rgee_environment_dir)
rgee::ee_Initialize(drive = T)
###################################################
### Arreglar la shape                           ###
### Aplicar el CONVEX HULL a los multipolígonos ###
###################################################

PRY <- read_sf("PRY/2020/ShapeDeptoPRY/PRY_adm1.shp")

PRY %<>% mutate(depto = str_pad(ID_1, pad = "0", width = 2))

PRY %<>% mutate(depto = case_when(depto == "05" ~ "00",
                                       depto == "18" ~ "02",
                                       depto == "07" ~ "05",
                                       depto == "14" ~ "07",
                                       depto == "08" ~ "06",
                                       depto == "03" ~ "10",
                                       depto == "10" ~ "11",
                                       TRUE ~ "20"))
## Agrupando la región 20 
mls <- PRY["depto"] %>% filter(depto == "20") 

plot(mls)

mls.envelope <- mls %>%
  st_combine() %>%
  st_cast("POINT") %>%
  st_sf() %>%
  concaveman()

plot(mls.envelope)

mls.envelope %<>% 
  mutate(depto = "20",
         nombre = "Resto",
         pais = "Paraguay" ) %>% 
  rename(geometry = polygons)

PRY <- bind_rows(mls.envelope,
                 PRY  %>% filter(depto != "20")  )



plot(PRY["depto"])


###################
### Luminosidad ###
###################

luces = ee$ImageCollection("NOAA/DMSP-OLS/NIGHTTIME_LIGHTS") %>%
  ee$ImageCollection$filterDate("2013-01-01", "2014-01-01") %>%
  ee$ImageCollection$map(function(x) x$select("stable_lights")) %>%
  ee$ImageCollection$toBands()
ee_print(luces)

PRY_luces <- ee_extract(
  x = luces,
  y = PRY["depto"],
  ee$Reducer$mean(),
  sf = FALSE
)

#################
### Urbanismo ###
#################

tiposuelo = ee$ImageCollection("COPERNICUS/Landcover/100m/Proba-V-C3/Global") %>%
  ee$ImageCollection$filterDate("2016-01-01", "2016-12-31") %>%
  ee$ImageCollection$map(function(x) x$select("urban-coverfraction", "crops-coverfraction")) %>% 
  ee$ImageCollection$toBands()
ee_print(tiposuelo)

PRY_urbano_cultivo <- ee_extract(
                   x = tiposuelo,
                   y = PRY["depto"],
                   ee$Reducer$mean(),
                   sf = FALSE
                 ) 
###############
### Guardar ###
###############
tasa_desocupacion %<>%
  full_join(
  full_join(PRY_luces, PRY_urbano_cultivo, by = "depto"),
  by = "depto")

saveRDS(tasa_desocupacion, "PRY/2020/1.Ingreso/Data/tasa_desocupacion.rds")
saveRDS(tasa_desocupacion, "PRY/2020/2.Pobreza/Data/tasa_desocupacion.rds")
saveRDS(tasa_desocupacion, "PRY/2020/3.PobrezaExtrema/Data/tasa_desocupacion.rds")
