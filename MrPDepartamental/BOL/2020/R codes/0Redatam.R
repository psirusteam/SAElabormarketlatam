#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################
### Cleaning R environment ###

rm(list =ls())

library(Rcpp)
library(RcppProgress)
library(redatam)
library(dplyr)
library(tidyverse)
library(haven)
library(DataExplorer)
library(magrittr)

## leer base desde el repositorio CEPAL

Bolivia <- redatam.open("MrPDepartamental/BOL/2020/1.Ingreso/Data/cpv2012bol-cde.dicX")

redatam.entities( Bolivia)
redatam.variables(Bolivia, "PERSONA")

CONTEOS <- redatam.query(Bolivia, "freq depto.Ndepto
                           by VIVIENDA.URBRUR
                           by PERSONA.SEXO
                           by PERSONA.P25
                           by PERSONA.P29A
                           by PERSONA.anoest
                           ",  tot.omit = FALSE)

saveRDS(object = CONTEOS, "MrPDepartamental/BOL/2020/1.Ingreso/Data/CONTEOS.RDS")
rm("$table1")
CONTEOS <- readRDS("MrPDepartamental/BOL/2020/1.Ingreso/Data/CONTEOS.RDS")

#   revisando valores unicos.
map(grep(pattern = "_value", x = names(CONTEOS),value = TRUE),
    function(by){
      unique(CONTEOS[[by]])
    })
# Eliminando totales de la tabla
CONTEOS2 <- CONTEOS %>% filter_at(vars(matches("_label")), all_vars(. !=  "__tot__"))

## sumas por variables de agregación, coincidir con el total nacional.
map(grep(pattern = "_label", x = names(CONTEOS2),value = TRUE),
    function(by){
      CONTEOS2 %>% group_by_at(by) %>%
        summarise(n = sum(value)) %>%
        mutate(Prop = n / sum(n), N = sum(n))
    })

map(grep(pattern = "_value", x = names(CONTEOS2),value = TRUE),
    function(by){
      CONTEOS2 %>% group_by_at(by) %>%
        summarise(n = sum(value)) %>%
        mutate(Prop = n / sum(n), N = sum(n))
    })

##

censo_mrp <- CONTEOS2 %>%
  filter(P254_value > 6) %>% 
  transmute(
    depto = str_pad(
      string = NDEPTO1_value,
      width = 2,
      pad = "0"
    ),
    area = case_when(URBRUR2_value == 1 ~ "1", # 1 = Urbana
                     TRUE ~ "0"),    # 0 = Rural
    sexo = as.character(SEXO3_value),

    edad = case_when(
      P254_value %in% 0:14 ~ "1",       # 5 a 14
      P254_value %in% 15:29 ~ "2",      # 15 a 29
      P254_value %in% 30:44 ~ "3",      # 30 a 44
      P254_value %in% 45:65 ~ "4",      # 45 a 64
      TRUE ~ "5"    ),                  # 65 o mas

    etnia = case_when(P29A5_value %in% 1 ~ "1", #indigena
                      TRUE ~ "3"), # No indigena

    anoest = case_when(
      P254_value < 5 | is.na(anoest6_value) ~ "98",     # No aplica
      anoest6_value %in% c(78,80,102) ~ "99", #NS/NR
      anoest6_value %in% 0 ~ "1",  # Sin educacion
      anoest6_value %in% c(1:6) ~ "2",  # 1-6
      anoest6_value %in% c(7:12) ~ "3",  # 7-12
      anoest6_value > 12 ~ "4" ,  # 12 o mas
      TRUE ~ "Error"
    ),
    
    value
  ) %>%
  group_by(depto, area, sexo, edad, etnia, anoest) %>%
  summarise(n = sum(value)) %>%
  mutate(
    depto = case_when(
      depto == "Chuquisaca" ~ "01",
      depto == "La Paz" ~ "02",
      depto == "Cochabamba" ~ "03",
      depto == "Oruro" ~ "04",
      depto == "Potosi" ~ "05",
      depto == "Tarija" ~ "06",
      depto == "Santa Cruz" ~ "07",
      depto == "Beni" ~ "08",
      depto ==  "Pando" ~ "09"
    )
  )

# Suma del total nacional
sum(censo_mrp$n)

# agregados por nuevas variables
map(c("depto", "area", "etnia", "sexo", "edad", "anoest"),
    function(x) {
      censo_mrp %>% group_by_at(x) %>%
        summarise(n = sum(n)) %>%
        mutate(Prop = n / sum(n), N = sum(n))
    })

plot_intro(censo_mrp)
plot_missing(censo_mrp)
plot_bar(censo_mrp, with = "n")


saveRDS(censo_mrp, 
        "MrPDepartamental/BOL/2020/1.Ingreso/Data/censo_mrp.rds")

# Tasa de desocupados
OCUPACION_1 <- redatam.query(Bolivia, "freq depto.Ndepto
                           by PERSONA.P39") %>% filter(P392_value == 1)


DESOCUPACION <- redatam.query(Bolivia,
                              "freq depto.Ndepto
                           by PERSONA.P40
                            by PERSONA.P41")

OCUPACION = OCUPACION_1 %>% bind_rows(DESOCUPACION)



OCUPACION <- OCUPACION %>% transmute(
  depto = NDEPTO1_label ,
  ocupados = ifelse(P392_value  %in% c(1) |
                      P402_value %in% c(1, 2, 3, 4), 1, 0),
  desocupados = ifelse(P413_value %in% c(6, 7), 1, 0),
  value
) %>% group_by(depto, ocupados, desocupados) %>%
  summarise(value = sum(value))

tabla <-
  pivot_wider(
    OCUPACION,
    names_from = c("ocupados", "desocupados"),
    values_from = value,
    names_prefix = c("ocupados")
  )

tasa_desocupacion = tabla %>% transmute(depto,
                                        tasa_desocupacion = ocupados0_1 / sum(ocupados0_1 + ocupados1_0)) %>%
  mutate(depto = case_when(depto == "Chuquisaca" ~ "01",
                           depto == "La Paz" ~ "02",
                           depto == "Cochabamba" ~ "03",
                           depto == "Oruro" ~ "04",
                           depto == "Potosi" ~ "05",
                           depto == "Tarija" ~ "06",
                           depto == "Santa Cruz" ~ "07",
                           depto == "Beni" ~ "08",
                           depto ==  "Pando" ~ "09"
                           ))


saveRDS(tasa_desocupacion, "MrPDepartamental/BOL/2020/1.Ingreso/Data/tasa_desocupacion.rds")
saveRDS(tasa_desocupacion, "MrPDepartamental/BOL/2020/2.Pobreza/Data/tasa_desocupacion.rds")
saveRDS(tasa_desocupacion, "MrPDepartamental/BOL/2020/3.PobrezaExtrema/Data/tasa_desocupacion.rds")

## Leer encuesta
# encuesta <- read_dta("Z:/BG/bol20n/bol20n.dta")
encuesta <- read_dta("Z:/BC/BOL_2020N.dta")
## Guardar encuesta
saveRDS(encuesta, "MrPDepartamental/BOL/2020/1.Ingreso/Data/encuestaBOL20n.rds")
