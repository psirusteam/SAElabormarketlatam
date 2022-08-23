#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################

### Cleaning R environment ###

rm(list =ls())
cat("\f")

library(Rcpp)
library(RcppProgress)
library(redatam)
library(dplyr)
library(tidyverse)
library(haven)
library(DataExplorer)


Nicaragua <- redatam.open("MrPDepartamental/NIC/2014/1.Ingreso/Data/cpv2005nic-cde.dicx")
redatam.variables(Nicaragua, "PERSONA")

CONTEOS <- redatam.query(Nicaragua, "freq DEPTO.CODDEP
                      by VIVIENDA.I07AREA
                      by PERSONA.P02
                      by PERSONA.P03
                      by PERSONA.P06
                      by PERSONA.ANOEST", tot.omit = FALSE)

saveRDS(CONTEOS, file = "MrPDepartamental/NIC/2014/1.Ingreso/Data/CONTEOS.RDS")
rm("$table1")
# CONTEOS <- readRDS(file = "MrPDepartamental/NIC/2014/1.Ingreso/Data/CONTEOS.RDS")

#   revisando valores unicos.
map(grep(pattern = "_value", x = names(CONTEOS),value = TRUE),
    function(by){
      unique(CONTEOS[[by]])
    })
# Eliminando totales de la tabla
CONTEOS2 <- CONTEOS %>% filter_at(vars(matches("_label")),
                                  all_vars(. !=  "__tot__"))


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
CONTEOS2 %>% group_by(ANOEST6_value, ANOEST6_label) %>%
  summarise(n = sum(value))  %>% mutate(N = sum(n)) %>% data.frame()


# Se filtra para mayores de 4 años por las variables NAS en etnia
censo_mrp <- CONTEOS2 %>% 
  filter(P034_value>9) %>%
  transmute(depto = str_pad(
              string = CODDEP1_value,
              width = 2,
              pad = "0"
            ),
            area = case_when(I07AREA2_value == 1 ~ "1", # 1 = Urbana
                             TRUE ~ "0"),
            sexo = as.character(P023_value),

            edad = case_when(
              P034_value  %in% 0:14 ~ "1", # 5 a 14
              P034_value  %in% 15:29 ~ "2", # 15 a 29
              P034_value  %in% 30:44 ~ "3", # 30 a 44
              P034_value  %in% 45:64~ "4", # 45 a 64
              TRUE ~ "5"), # 65 o mas
             etnia = case_when(
               P065_value %in% c(1) ~ "1", # Indigena
               TRUE ~ "3"), # Otro
            anoest = case_when(
              is.na(ANOEST6_value) | P034_value < 6 ~ "98",     # No aplica
              ANOEST6_value == 99 ~ "99", #NS/NR
              ANOEST6_value %in% 0 ~ "1",  # Sin educacion
              ANOEST6_value %in% c(1:6) ~ "2",  # 1-6
              ANOEST6_value %in% c(7:12) ~ "3",  # 7-12
              ANOEST6_value > 12 ~ "4" ,  # 12 o mas
              TRUE ~ "Error"
            ),

          value) %>% group_by(depto, area, sexo, edad, etnia,
                                anoest, ) %>%
  summarise(n = sum(value), .groups = "drop")

# Suma del total nacional
sum(censo_mrp$n)

# agregados por nuevas variables
map(c("depto", "area", "etnia", "sexo", "edad", "anoest"),
    function(x){
      censo_mrp %>% group_by_at(x) %>%
        summarise(n = sum(n)) %>%
        mutate(Prop = n / sum(n), N = sum(n))
    })


plot_intro(censo_mrp)
plot_missing(censo_mrp)
plot_bar(censo_mrp, with = "n")

saveRDS(censo_mrp, "MrPDepartamental/NIC/2014/1.Ingreso/Data/censo_mrp.rds")

# tasa de desocupacion
OCUPACION <- redatam.query(Nicaragua, "freq DEPTO.CODDEP by PERSONA.ECONOMIC",
                           tot.omit = FALSE)

OCUPACION2 <- OCUPACION %>% filter_at(vars(matches("_label")),
                                      all_vars(!. %in%   c("__mv__", "__tot__", "No especificado", "__na__")))

group_by(OCUPACION2,ECONOMIC2_value, ECONOMIC2_label) %>% summarise(n = sum(value))

sum(OCUPACION2$value)

OCUPACION2 <- OCUPACION2 %>% transmute(
  depto = str_pad(
    string = CODDEP1_value,
    width = 2,
    pad = "0"
  ),
  ocupados = ifelse(ECONOMIC2_value %in% c(11,12),1,0),
  desocupados = ifelse(ECONOMIC2_value %in% c(21,22,31,32,33),1,0), # verificar categorias 32 32 33
  value
) %>% group_by(depto, ocupados, desocupados) %>%
  summarise(value = sum(value), .groups = "drop")

tabla <-
  pivot_wider(
    OCUPACION2,
    names_from = c("ocupados", "desocupados"),
    values_from = value,
    names_prefix = c("ocupados")
  )


tasa_desocupacion <- tabla %>%
  transmute(depto,
            tasa_desocupacion = ocupados0_1 / sum(ocupados0_1 + ocupados1_0))

saveRDS(tasa_desocupacion, "MrPDepartamental/NIC/2014/1.Ingreso/Data/tasa_desocupacion.rds")
saveRDS(tasa_desocupacion, "NIC/2.Pobreza/Data/tasa_desocupacion.rds")
saveRDS(tasa_desocupacion, "NIC/3.PobrezaExtrema/Data/tasa_desocupacion.rds")
## Leer encuesta
# encuesta <- read_dta("Z:/BG/nic14n/nic14n.dta")
encuesta <- read_dta("Z:/BC/NIC_2014N.dta")
## Guardar encuesta
saveRDS(encuesta, "MrPDepartamental/NIC/2014/1.Ingreso/Data/encuestaNIC14N.rds")
