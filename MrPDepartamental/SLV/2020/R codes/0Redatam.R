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
salvador <- redatam.open("MrPDepartamental/SLV/2020/1.Ingreso/Data/cpv2007slv-cde.dicx")

CONTEOS <- redatam.query(salvador, "freq DEPTO.R1
                         by SEGMENTO.R6
                         by PERSONA.P02
                         by PERSONA.P03A
                         by PERSONA.P06A
                         by PERSONA.ANOEST", tot.omit = FALSE)

saveRDS(CONTEOS, file = "MrPDepartamental/SLV/2020/1.Ingreso/Data/CONTEOS.RDS")
rm("$table1")
#CONTEOS<- readRDS(file = "MrPDepartamental/SLV/2020/1.Ingreso/Data/CONTEOS.RDS")
#   revisando valores unicos.
map(grep(pattern = "_value", x = names(CONTEOS),value = TRUE),
    function(by){
      unique(CONTEOS[[by]])
    })
# Eliminando totales de la tabla
CONTEOS2 <-
  CONTEOS %>% filter_at(vars(matches("_label")), all_vars(. !=  "__tot__"))

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

group_by(CONTEOS2, ANOEST6_value, ANOEST6_label) %>%
  summarise(n = sum(value)) %>%
  mutate(Prop = n / sum(n), N = sum(n)) %>%
  data.frame()
# Se filtra para mayores de 4 años por las variables NAS en etnia
censo_mrp <- CONTEOS2 %>% 
  filter(P03A4_value > 4 ) %>%
  transmute(depto = str_pad(
              string = R11_value,
              width = 2,
              pad = "0"
            ),
            area = case_when(R62_value == 1 ~ "1", # 1 = Urbana
                             TRUE ~ "0"),
            sexo = as.character(P023_value),
            edad = case_when(
              P03A4_value  %in% 0:14 ~ "1", # 5 a 14
              P03A4_value  %in% 15:29 ~ "2", # 15 a 29
              P03A4_value  %in% 30:44 ~ "3", # 30 a 44
              P03A4_value  %in% 45:64 ~ "4", # 45 a 64
              TRUE ~ "5"), # 65 o mas
             etnia = case_when(
               P06A5_value %in% c(3) ~ "1", # Indigena
               P06A5_value %in% c(4)  ~ "2",# Negro
               TRUE ~ "3" # Blanco, mestizo, otro
               ),

            anoest = case_when(
              P03A4_value < 7 | is.na(ANOEST6_value) ~ "98",     # No aplica
              ANOEST6_value == 99 ~ "99", #NS/NR
              ANOEST6_value %in% 0 ~ "1",  # Sin educacion
              ANOEST6_value %in% c(1:6) ~ "2",  # 1-6
              ANOEST6_value %in% c(7:12) ~ "3",  # 7-12
              ANOEST6_value > 12 ~ "4" ,  # 12 o mas
              TRUE ~ "Error"
            ),

            value) %>% group_by(depto, area, sexo, edad, etnia,
                                anoest) %>%
  summarise(n = sum(value))


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



saveRDS(censo_mrp, "MrPDepartamental/SLV/2020/1.Ingreso/Data/censo_mrp.rds")

## tasa de desocupación
OCUPACION <- redatam.query(salvador, "freq DEPTO.R1 by  PERSONA.CONDACT",
                           tot.omit = FALSE)

OCUPACION2 <- OCUPACION %>% filter_at(vars(matches("_label")),
                                      all_vars(!. %in%   c(
                                        "__mv__", "__tot__", "No especificado", "__na__"
                                      )))


group_by(OCUPACION2, CONDACT2_value, CONDACT2_label) %>%
  summarise(n = sum(value))

sum(OCUPACION2$value)

OCUPACION <- OCUPACION2 %>% transmute(
  depto  = str_pad(
    string = R11_value,
    width = 2,
    pad = "0"
  ),
  ocupados = ifelse(CONDACT2_value %in% c(1), 1, 0),
  desocupados = ifelse(CONDACT2_value %in% c(2, 3), 1, 0),
  value
) %>% group_by(depto, ocupados, desocupados) %>%
  summarise(value = sum(value))

tabla <- pivot_wider(OCUPACION,
                     names_from = c("ocupados", "desocupados"),
                     values_from = value,
                     names_prefix = c("ocupados"))

tasa_desocupacion <- tabla %>%
  transmute(depto,
            tasa_desocupacion = ocupados0_1 / sum(ocupados0_1 + ocupados1_0))

saveRDS(tasa_desocupacion, "MrPDepartamental/SLV/2020/1.Ingreso/Data/tasa_desocupacion.rds")
saveRDS(tasa_desocupacion, "SLV/2020/2.Pobreza/Data/tasa_desocupacion.rds")
saveRDS(tasa_desocupacion, "SLV/2020/3.PobrezaExtrema/Data/tasa_desocupacion.rds")

## Leer encuesta
encuesta <- read_dta("Z:/BC/SLV_2020N.dta")
#encuesta <- read_dta("Z:/BG/slv20n/slv20n.dta")
## Guardar encuesta
saveRDS(encuesta, "MrPDepartamental/SLV/2020/1.Ingreso/Data/encuestaSLV20n.rds")
