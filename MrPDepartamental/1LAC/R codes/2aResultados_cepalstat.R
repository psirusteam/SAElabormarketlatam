#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################

### Cleaning R environment ###

rm(list = ls())

# Loading required libraries ----------------------------------------------

library(tidyverse)
library(magrittr)
library(patchwork)
library(sp)
library(sf)
library(tmap)
library(RColorBrewer)
library(maptools)
library(tmaptools)


source("MrPDepartamental//0Funciones/Funciones_empleo.R", encoding = "UTF-8")
# Loading data ------------------------------------------------------------
poststrat_df <- readRDS("MrPDepartamental/1LAC/Data/poststrat_LAC_25082022.RDS") %>% 
  mutate(n = n*gk)

# Puntos de corte para el mapa
brks_TO <- c(0 ,20, 40, 60, 80,100)
brks_TD <- c(0,5, 10, 15, 20, 100)
brks_TP <- c(0 ,20, 40, 60, 80,100)
  
load("MrPDepartamental/1LAC/ShapeDepto/ShapeSAE.Rdata") 
## Actualizando PRY
PRY_shape <- ShapeSAE %>% filter(pais == "Paraguay") %>% select(-depto) 

PRY_temp <- data.frame(
  depto = c("17","10", "13", "00", "16", "05", "06", "14",
            "11", "01", "03", "04", "07", "08", "12", "09", 
            "15", "02"),
 nombre = c("Alto Paraguay" ,"Alto Paraná" ,"Amambay" ,"Asunción" ,
            "Boquerón" ,"Caaguazú" ,"Caazapá" ,"Canindeyú" ,"Central" ,
            "Concepción" ,"Cordillera" ,"Guairá" ,"Itapúa" ,"Misiones" ,
            "Ñeembucú" ,"Paraguarí" ,"Presidente Hayes" ,"San Pedro" )
)

PRY_shape <- inner_join(PRY_shape,PRY_temp,  by = "nombre")

ShapeSAE %<>% filter(pais != "Paraguay") %>% 
  bind_rows(PRY_shape) %>% arrange(pais)

## codigos ISO para los paises 
cod_ISO <- c(
  "Argentina" = "ARG",
  "Bolivia" = "BOL",
  "Brasil" = "BRA",
  "Chile" = "CHL",
  "Colombia" = "COL",
  "Costa Rica" = "CRI",
  "Ecuador" = "ECU",
  "El salvador" = "SLV",
  "Guatemala" = "GTM",
  "Honduras" = "HND",
  "Mexico" = "MEX",
  "Nicaragua" = "NIC",
  "Panama" = "PAN",
  "Paraguay" = "PRY2",
  "Peru" = "PER",
  "Republica dominicana" = "DOM",
  "Uruguay" = "URY"
)

ShapeSAE[["pais"]] <-
  dplyr::recode(as.character(ShapeSAE[["pais"]]) ,!!!cod_ISO)

setdata <- Indicadores_censo(poststrat_df, unique(c("pais", "depto", "Año" )))

P1_tasa <- tm_shape(ShapeSAE %>%
                      left_join(setdata,  by = c("depto", "pais")))

Mapa_TD <-
  P1_tasa + tm_polygons(
    "TD",
    breaks = brks_TD,
    title = "Tasa de desocupación",
    palette = "YlOrRd", 
    colorNA = "white"
  ) + tm_layout(
    legend.only = FALSE,
    legend.height = -0.4,
    legend.width = -0.4,
    legend.position = c(0, 0),
    asp = 0,
    legend.title.size = 4,
    legend.text.size =  4
  )

tmap_save(
  Mapa_TD,
  paste0("MrPDepartamental/1LAC/Output/Tasa de desocupación.pdf"),
  width = 6920,
  height = 4080,
  asp = 0
)

Mapa_TO <-
  P1_tasa + tm_polygons(
    "TO",
    breaks = brks_TO,
    title = "Tasa de ocupación",
    palette = "-YlOrRd",
    colorNA = "white"
  ) + tm_layout(
    legend.only = FALSE,
    legend.height = -0.4,
    legend.width = -0.4,
    legend.position = c(0,0),
    asp = 0,
    legend.title.size = 4,
    legend.text.size =  4
  )

tmap_save(
  Mapa_TO,
  paste0("MrPDepartamental/1LAC/Output/Tasa de ocupación.pdf"),
  width = 6920,
  height = 4080,
  asp = 0
)


Mapa_TP <-
  P1_tasa + tm_polygons(
    "TP",
    breaks = brks_TP,
    title = "Tasa de Participación",
    palette = "-YlOrRd",
    colorNA = "white"
  ) + tm_layout(
    legend.only = FALSE,
    legend.height = -0.4,
    legend.width = -0.4,
    legend.position = c(0,0),
    asp = 0,
    legend.title.size = 4,
    legend.text.size =  4
  )

tmap_save(
  Mapa_TP,
  paste0("MrPDepartamental/1LAC/Output/Tasa de Participación.pdf"),
  width = 6920,
  height = 4080,
  asp = 0
)

x11()
tmap_arrange(list(Mapa_TO, Mapa_TD, Mapa_TP),ncol = 3,nrow = 1)