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
library(DataExplorer)
library(PerformanceAnalytics)


source("0Funciones/funciones_mrp.R", encoding = "UTF-8")
# Loading data ------------------------------------------------------------
poststrat_df <- readRDS("1LAC/Data/poststrat_LAC_OK.RDS") 

#########################################################
## Trayendo las covariables satelitales y de censo
#########################################################

list_state <- list.files(full.names = TRUE, recursive = TRUE,
                         pattern = "tasa_desocupacion.rds")
#--------
# Lista de scrips de mapas  de los paises
pais <-  gsub(pattern = "^.\\/(\\w{3}).*",
              x = list_state, "\\1")

variable <-  gsub(pattern = "^.\\/.*\\d{1}\\.(.*)\\/Data.*",
                  x = list_state, "\\1")

list_state <- list_state[variable == "Ingreso"]
pais <- pais[variable == "Ingreso"]

tasa_desocupacion_LAC <- map2(list_state, pais, function(x, y) {
  read_rds(x) %>% mutate(pais = y)
})

names(tasa_desocupacion_LAC) <- pais
saveRDS(tasa_desocupacion_LAC,
        "1LAC/Data/tasa_desocupacion_LAC.rds")

#########################################################
## Estimando indicadores por estado y país
#########################################################

covariablesLAC <- 
map_df(.x = names(tasa_desocupacion_LAC),
    function(bypais) {
      xpais_I <- poststrat_df$ingreso %>% filter(pais == bypais) %>%
        select_if(function(x) {
          !all(is.na(x))
        })
      xpais_P <- poststrat_df$pobreza %>% filter(pais == bypais) %>%
        select_if(function(x) {
          !all(is.na(x))
        })
      xpais_E <- poststrat_df$extrema %>% filter(pais == bypais) %>%
        select_if(function(x) {
          !all(is.na(x))
        })
      
      
      dat_df_I = xpais_I %>% group_by_at(vars("depto")) %>%
        summarise(Ingreso = sum(n * pobreza2) / sum(n), .groups = "drop") %>%
        ungroup()
      
      dat_df_P = xpais_P %>% group_by_at(vars("depto")) %>%
        summarise(Pobreza = sum(n * pobreza2) / sum(n), .groups = "drop") %>%
        ungroup()
      
      dat_df_E = xpais_E %>% group_by_at(vars("depto")) %>%
        summarise(P_Extrema = sum(n * pobreza2) / sum(n), .groups = "drop") %>%
        ungroup()
      
      covariables <- dat_df_I %>%
        full_join(dat_df_P) %>%
        full_join(dat_df_E) %>%
        full_join(tasa_desocupacion_LAC[[bypais]])
      
      ggsave(plot_correlation( na.omit(covariables[, -c(1, 9)]) ), 
             filename = paste0("1LAC/Output/corplot_", bypais, ".jpeg"))
      
      return(covariables)
      
    })





ECU <- covariablesLAC %>% 
  filter(pais == "ECU") 
  s
plot_correlation(ECU[, -c(1, 9)])
chart.Correlation(ECU[, -c(1, 9)])


plot_correlation(na.omit(covariablesLAC[, -c(1, 9)]) )
chart.Correlation(na.omit(covariablesLAC[, -c(1, 9)]))
