#########################################################
# Proyecto MRP - Left No One Behind                     #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################

rm(list = ls())

# Loading required libraries ----------------------------------------------
library(sp)
library(sf)
library(tidyverse)
library(magrittr)
library(openxlsx)


#########################################################
## Trayendo los resultados por departamento de los países
#########################################################
file_poststrat_df <-
  data.frame(file =
               list_state <- list.files(
                  "MrPDepartamental",
                 full.names = TRUE,
                 recursive = TRUE,
                 pattern = "poststrat_df.RDS"
               )) %>%
  separate(
    col = "file",
    into = c("temp", "pais", "Año", "carpeta"),
    sep = "\\/",
    remove = FALSE
  ) %>% filter(pais != "PRY")
#--------
# Lista de scrips de mapas  de los paises
states_LAC <- file_poststrat_df %>% as_tibble() %>% 
  mutate(poststrat = map(file,~readRDS(file =.))) %>% 
  select(-temp, -file)

states_LAC[15,]$poststrat[[1]] %<>% mutate(depto = depto2, depto2 = NULL)

saveRDS(states_LAC %>% unnest(cols = "poststrat"),
        "MrPDepartamental/1LAC/Data/poststrat_LAC_25082022.RDS")


