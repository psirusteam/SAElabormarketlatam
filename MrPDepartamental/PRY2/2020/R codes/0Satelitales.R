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
  readRDS("MrPDepartamental/PRY2/2020/Data/tasa_desocupacion.rds")

#######################################
### configuración inicial de Python ###
#######################################

#rgee_environment_dir = "C://Users//sguerrero//Anaconda3//envs//rgee_py//python.exe"
rgee_environment_dir =  "C:/Users/agutierrez1/Anaconda3/envs/rgee_py/python.exe"
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

PRY <- read_sf("MrPDepartamental/PRY2/2020/ShapeDeptoPRY/PRY_adm1.shp")

PRY %<>% mutate(depto = str_pad(ID_1, pad = "0", width = 2))
PRY2 <- PRY

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

PRY_luces2 <- ee_extract(
  x = luces,
  y = PRY2["depto"],
  ee$Reducer$mean(),
  sf = FALSE
)

names(PRY_luces2) <- paste0(names(PRY_luces2),"2")

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
PRY_urbano_cultivo2 <- ee_extract(
  x = tiposuelo,
  y = PRY2["depto"],
  ee$Reducer$mean(),
  sf = FALSE
) 
names(PRY_urbano_cultivo2) <- paste0(names(PRY_urbano_cultivo2),"2")
###############
### Guardar ###
###############
tasa_desocupacion <- full_join(PRY_luces2,PRY_urbano_cultivo2, by = "depto2") %>% 
  mutate(depto = case_when(depto2 == "05" ~ "00",
                           depto2 == "18" ~ "02",
                           depto2 == "07" ~ "05",
                           depto2 == "14" ~ "07",
                           depto2 == "08" ~ "06",
                           depto2 == "03" ~ "10",
                           depto2 == "10" ~ "11",
                           depto2 == "01" ~ "12",
                           depto2 == "02" ~ "17",
                           depto2 == "04" ~ "13",
                           depto2 == "06" ~ "16",
                           depto2 == "09" ~ "14",
                           depto2 == "11" ~ "01",
                           depto2 == "12" ~ "03",
                           depto2 == "13" ~ "04",
                           depto2 == "15" ~ "08",
                           depto2 == "16" ~ "09",
                           depto2 == "17" ~ "15"),
         depto2 = NULL) %>% 
  full_join(tasa_desocupacion,
            by = c("depto" = "depto"))



tasa_desocupacion %<>%
  full_join(
  full_join(PRY_luces, PRY_urbano_cultivo),
  by = c("depto2" = "depto"))

cor(tasa_desocupacion[, -1 ])

saveRDS(tasa_desocupacion, "MrPDepartamental/PRY2/2020/Data/tasa_desocupacion.rds")
