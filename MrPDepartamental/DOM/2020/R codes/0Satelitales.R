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
  readRDS("DOM/2020/1.Ingreso/Data/tasa_desocupacion.rds")


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

## revisando DOMivia
DOM <- read_sf("DOM/ShapeDeptoDOM/DOM_adm1.shp")

DOM %<>% mutate(depto = str_pad(ID_1, pad = "0", width = 2))

DOM %<>% mutate(depto = case_when(depto == "01" ~ "02",
                                       depto == "02" ~ "03",
                                       depto == "03" ~ "04",
                                       depto == "04" ~ "05",
                                       depto == "05" ~ "01",
                                       depto == "07" ~ "08",
                                       depto == "08" ~ "09",
                                       depto == "13" ~ "12",
                                       depto == "14" ~ "13",
                                       depto == "15" ~ "14",
                                       depto == "17" ~ "15",
                                       depto == "19" ~ "16",
                                       depto == "20" ~ "17",
                                       depto == "21" ~ "18",
                                       depto == "24" ~ "20",
                                       depto == "25" ~ "21",
                                       depto == "27" ~ "22",
                                       depto == "28" ~ "23",
                                       depto == "22" ~ "24",
                                       depto == "30" ~ "25",
                                       depto == "29" ~ "26",
                                       depto == "32" ~ "27",
                                       depto == "16" ~ "28",
                                       depto == "18" ~ "29",
                                       depto == "09" ~ "30",
                                       depto == "26" ~ "31",
                                       depto == "31" ~ "32",
                                       depto == "06" ~ "06",
                                       depto == "10" ~ "10",
                                       depto == "11" ~ "11",
                                       depto == "23" ~ "19",
                                       depto == "12" ~ "07",
                                       TRUE ~ "00"))

###################
### Luminosidad ###
###################

luces = ee$ImageCollection("NOAA/DMSP-OLS/NIGHTTIME_LIGHTS") %>%
  ee$ImageCollection$filterDate("2013-01-01", "2014-01-01") %>%
  ee$ImageCollection$map(function(x) x$select("stable_lights")) %>%
  ee$ImageCollection$toBands()
ee_print(luces)

DOM_luces <- ee_extract(
  x = luces,
  y = DOM["depto"],
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

DOM_urbano_cultivo <- ee_extract(
                   x = tiposuelo,
                   y = DOM["depto"],
                   ee$Reducer$mean(),
                   sf = FALSE
                 ) 
###############
### Guardar ###
###############

tasa_desocupacion %<>% 
  full_join(DOM_luces) %>% 
  full_join(DOM_urbano_cultivo)

cor(tasa_desocupacion[, -1 ])

saveRDS(tasa_desocupacion, "DOM/2020/1.Ingreso/Data/tasa_desocupacion.rds")

