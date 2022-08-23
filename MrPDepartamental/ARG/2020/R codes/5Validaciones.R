#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################

rm(list =ls())
# Creado directorio para out
if(!dir.exists("MrPDepartamental/ARG/2020/Output/Tasa desocupados/")){
dir.create("MrPDepartamental/ARG/2020/Output/Tasa desocupados/")
}
if(!dir.exists("MrPDepartamental/ARG/2020/Output/Tasa Ocupados/")){
  dir.create("MrPDepartamental/ARG/2020/Output/Tasa Ocupados/")
}
if(!dir.exists("MrPDepartamental/ARG/2020/Output/Tasa participacion/")){
  dir.create("MrPDepartamental/ARG/2020/Output/Tasa participacion/")
}

outParticipacion <- "MrPDepartamental/ARG/2020/Output/Tasa participacion/"
outOcupado <- "MrPDepartamental/ARG/2020/Output/Tasa Ocupados/"
outDesocupado <- "MrPDepartamental/ARG/2020/Output/Tasa desocupados/"
# Librerías

library(tidyverse)
library(patchwork)
library(survey)
library(srvyr)

source("MrPDepartamental/0Funciones/funciones_mrp.R", encoding = "UTF-8")
source("MrPDepartamental/0Funciones/Funciones_empleo.R", encoding = "UTF-8")
# Loading data ------------------------------------------------------------
encuesta_mrp <- readRDS("MrPDepartamental/ARG/2020/Data/encuesta_mrp.rds") %>% 
  filter(edad != "1")
poststrat_df <- readRDS("MrPDepartamental/ARG/2020/Data/poststrat_df.RDS")%>% 
  filter(edad != "1")

# Revisión de NAs ---------------------------------------------------------

sum(complete.cases(poststrat_df)) == nrow(poststrat_df)

###########################################
###########################################
### Validaciones por subgrupo completo  ###
###########################################
###########################################
bynames <-
  grep(
    pattern =  "^(theta|^n|pobreza|ingreso|tasa_desocupacion|epred_mat|gk|yk|lp|X|F)",
    x = names(poststrat_df),
    invert = TRUE,
    value = TRUE
  )

## Tasa de desocupacion 
plot_uni <- map(
  .x = setNames(bynames, bynames),
  ~ plot_compare_Ind(
    sample = encuesta_mrp,
    poststrat = poststrat_df,
    by1 = .x, 
    Ind = "TD"
  )
)

(plot_uni <- plot_uni$depto$Plot  / 
  ( plot_uni$sexo$Plot + plot_uni$anoest$Plot +
      plot_uni$edad$Plot+ plot_uni$etnia$Plot + 
      plot_uni$area$Plot + plot_uni$Discapacidad$Plot)
)
ggsave(plot = plot_uni, 
       filename = paste0(outDesocupado,"plot_uni.jpeg"),scale = 4)

## Tasa de Ocupacion 
plot_uni <- map(
  .x = setNames(bynames, bynames),
  ~ plot_compare_Ind(
    sample = encuesta_mrp,
    poststrat = poststrat_df,
    by1 = .x, 
    Ind = "TO"
  )
)

(plot_uni <- plot_uni$depto$Plot  / 
  ( plot_uni$sexo$Plot + plot_uni$anoest$Plot +
      plot_uni$edad$Plot+ plot_uni$etnia$Plot + 
      plot_uni$area$Plot + plot_uni$Discapacidad$Plot)
)
ggsave(plot = plot_uni, 
       filename = paste0(outOcupado,"plot_uni.jpeg"),scale = 4)

## Tasa de participación 
plot_uni <- map(
  .x = setNames(bynames, bynames),
  ~ plot_compare_Ind(
    sample = encuesta_mrp,
    poststrat = poststrat_df,
    by1 = .x, 
    Ind = "TP"
  )
)

(plot_uni <- plot_uni$depto$Plot  / 
  ( plot_uni$sexo$Plot + plot_uni$anoest$Plot +
      plot_uni$edad$Plot+ plot_uni$etnia$Plot + 
      plot_uni$area$Plot + plot_uni$Discapacidad$Plot)
)
ggsave(plot = plot_uni, 
       filename = paste0(outParticipacion,"plot_uni.jpeg"),scale = 4)


###########################################
###########################################
### Validaciones por pares de subgrupos ###
###########################################
###########################################

# 1. Dpto-sexo -----------------------------------------------------------------
# 
# bynames2 <- t(combn(bynames, 2))
# 
# map(
#   1:nrow(bynames2),
#   ~ plot_compare2(
#     sample_diseno = diseno,
#     poststrat = poststrat_df,
#     by1 = bynames2[.x, ]
#   )$Plot %>%
#     ggsave(
#       paste0(
#         "MrPDepartamental/ARG/2020/Output/plot_",
#         paste0(bynames2[.x, ], collapse = "_"),
#         ".jpeg"
#       ),
#       plot = .,
#       scale = 4
#     )
# )
# 
# ###########################################
# ###########################################
# ### Validaciones por triplas            ###
# ###########################################
# ###########################################
# bynames3 <- t(combn(bynames, 3))
# 
# # 1. Dpto -----------------------------------------------------------------
# map(
#   1:nrow(bynames3),
#   ~ plot_compare2(
#     sample_diseno = diseno,
#     poststrat = poststrat_df,
#     by1 = bynames3[.x, ]
#   )$Plot %>%
#     ggsave(
#       paste0(
#         "MrPDepartamental/ARG/2020/Output/plot_",
#         paste0(bynames3[.x, ], collapse = "_"),
#         ".jpeg"
#       ),
#       plot = .,
#       scale = 4
#     )
# )
# 
