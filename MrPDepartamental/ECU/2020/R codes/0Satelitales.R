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
  readRDS("ECU/2020/1.Ingreso/Data/tasa_desocupacion.rds") %>% 
  filter(depto != "90") %>% 
  select(depto, tasa_desocupacion)


#######################################
### configuración inicial de Python ###
#######################################

rgee_environment_dir = "C://Users//sguerrero//Anaconda3//envs//rgee_py//python.exe"
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

## revisando ECUivia
ECU <- read_sf("ECU/ShapeDeptoECU/ECU_adm1.shp")
ECU %<>% mutate(depto = str_pad(ID_1, pad = "0", width = 2))
ECU %<>% mutate(depto = case_when(depto == "06" ~ "05",
                                       depto == "05" ~ "06",
                                       depto == "09" ~ "20",
                                       depto == "10" ~ "09",
                                       depto == "11" ~ "10",
                                       depto == "12" ~ "11",
                                       depto == "13" ~ "12",
                                       depto == "14" ~ "13",
                                       depto == "15" ~ "14",
                                       depto == "16" ~ "15",
                                       depto == "18" ~ "16",
                                       depto == "19" ~ "17",
                                       depto == "23" ~ "18",
                                       depto == "24" ~ "19",
                                       depto == "22" ~ "21",
                                       depto == "17" ~ "22",
                                       depto == "21" ~ "23",
                                       depto == "20" ~ "24",
                                       TRUE ~ depto ) ) 
###################
### Luminosidad ###
###################

luces = ee$ImageCollection("NOAA/DMSP-OLS/NIGHTTIME_LIGHTS") %>%
  ee$ImageCollection$filterDate("2013-01-01", "2014-01-01") %>%
  ee$ImageCollection$map(function(x) x$select("stable_lights")) %>%
  ee$ImageCollection$toBands()
ee_print(luces)

ECU_luces <- ee_extract(
  x = luces,
  y = ECU["depto"],
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

ECU_urbano_cultivo <- ee_extract(
                   x = tiposuelo,
                   y = ECU["depto"],
                   ee$Reducer$mean(),
                   sf = FALSE
                 ) 
###############
### Guardar ###
###############

tasa_desocupacion %<>% 
  full_join(ECU_luces) %>% 
  full_join(ECU_urbano_cultivo)

cor(tasa_desocupacion[, -1 ])

saveRDS(tasa_desocupacion, "ECU/2020/1.Ingreso/Data/tasa_desocupacion.rds")

