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
  readRDS("MrPDepartamental/NIC/2014/1.Ingreso/Data/tasa_desocupacion.rds")


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

## revisando NICivia
NIC <- read_sf("NIC/ShapeDeptoNIC/NIC_adm1.shp")
NIC %<>% mutate(depto = str_pad(ID_1, pad = "0",width = 2))
NIC %<>% mutate(depto = case_when(depto == "03" ~ "50",
                                       depto == "04" ~ "75",
                                       depto == "08" ~ "70",
                                       depto == "11" ~ "35",
                                       depto == "12" ~ "20",
                                       depto == "13" ~ "55",
                                       depto == "17" ~ "85",
                                       depto == "14" ~ "60",
                                       depto == "18" ~ "80",
                                       depto == "07" ~ "25",
                                       depto == "05" ~ "30",
                                       depto == "06" ~ "65",
                                       depto == "01" ~ "91",
                                       depto == "02" ~ "93",
                                       depto == "15" ~ "40",
                                       depto == "16" ~ "05",
                                       depto == "09" ~ "10",
                                       TRUE ~ "00")) %>%
  filter(depto != "00")

###################
### Luminosidad ###
###################

luces = ee$ImageCollection("NOAA/DMSP-OLS/NIGHTTIME_LIGHTS") %>%
  ee$ImageCollection$filterDate("2013-01-01", "2014-01-01") %>%
  ee$ImageCollection$map(function(x) x$select("stable_lights")) %>%
  ee$ImageCollection$toBands()
ee_print(luces)

NIC_luces <- ee_extract(
  x = luces,
  y = NIC["depto"],
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

NIC_urbano_cultivo <- ee_extract(
                   x = tiposuelo,
                   y = NIC["depto"],
                   ee$Reducer$mean(),
                   sf = FALSE
                 ) 
###############
### Guardar ###
###############

tasa_desocupacion %<>% 
  full_join(NIC_luces) %>% 
  full_join(NIC_urbano_cultivo)

cor(tasa_desocupacion[, -1 ])

saveRDS(tasa_desocupacion, "MrPDepartamental/NIC/2014/1.Ingreso/Data/tasa_desocupacion.rds")

