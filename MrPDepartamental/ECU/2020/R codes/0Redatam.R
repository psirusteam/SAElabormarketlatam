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
ecuador <- redatam.open("MrPDepartamental/ECU/2020/1.Ingreso/Data/CE11.dicX")

CONTEOS <- redatam.query(ecuador, "freq PROVIN.PROVIN
                      by VIVIENDA.UR
                      by PERSONA.P01
                      by PERSONA.P03
                      by PERSONA.P16
                      by PERSONA.GRAESC
                      by PERSONA.DISCAP
                      ", tot.omit = FALSE)
saveRDS(CONTEOS, "MrPDepartamental/ECU/2020/1.Ingreso/Data/CONTEOS.RDS")
rm("$table1")
#CONTEOS <-  readRDS("MrPDepartamental/ECU/2020/1.Ingreso/Data/CONTEOS.RDS")
#   revisando valores únicos.
map(grep(pattern = "_value", x = names(CONTEOS),value = TRUE),
    function(by){
      unique(CONTEOS[[by]])
    })
# Eliminando totales de la tabla
CONTEOS2 <- CONTEOS %>% filter_at(vars(matches("_label")),
                                  all_vars(. !=  "__tot__"))

## sumas por variables de agregación, coincidir con el total nacional.
map(grep(pattern = "_value", x = names(CONTEOS2),value = TRUE),
    function(by){
      CONTEOS2 %>% group_by_at(by) %>%
        summarise(n = sum(value)) %>%
        mutate(Prop = n / sum(n), N = sum(n))
    })

map(grep(pattern = "_label", x = names(CONTEOS2),value = TRUE),
    function(by){
      CONTEOS2 %>% group_by_at(by) %>%
        summarise(n = sum(value)) %>%
        mutate(Prop = n / sum(n), N = sum(n))
    })

group_by(CONTEOS2, GRAESC6_value, GRAESC6_label) %>%
  summarise(n = sum(value)) %>% data.frame()

censo_mrp <- CONTEOS2 %>%
  filter(P034_value > 4) %>% 
  transmute(
    depto = str_pad(
      string = PROVIN1_value,
      width = 2,
      pad = "0"
    ),
    area = case_when(UR2_value == 1 ~ "1", # 1 = Urbana
                     TRUE ~ "0"),    # 0 = Rural
    sexo = as.character(P013_value),

    edad = case_when(
      P034_value %in% 0:14 ~ "1",       # 5 a 14
      P034_value %in% 15:29 ~ "2",      # 15 a 29
      P034_value %in% 30:44 ~ "3",      # 30 a 44
      P034_value %in% 45:64 ~ "4",      # 45 a 64
      TRUE ~ "5"
    ),     # 65 o mas

    anoest = case_when(
      is.na(GRAESC6_value) | P034_value < 6 ~ "98",     # No aplica
      GRAESC6_value == 99 ~ "99", #NS/NR
      GRAESC6_value %in% 0 ~ "1",  # Sin educacion
      GRAESC6_value %in% c(1:6) ~ "2",  # 1-6
      GRAESC6_value %in% c(7:12) ~ "3",  # 7-12
      GRAESC6_value > 12 ~ "4" ,  # 12 o mas
      TRUE ~ "Error"
    ),

    etnia = case_when(
      P165_value %in% 1 ~ "1", #indigena
      P165_value %in% 2:4 ~ "2", #afro negro mulato
      TRUE ~ "3" ),# Otro
    discapacidad = case_when(
      DISCAP7_value %in% 1:6 ~ "1", # discapacitado
      TRUE ~ "0"), # No discapacitado
            value) %>% group_by(depto, area, sexo, edad, etnia, anoest, discapacidad) %>%
  summarise(n = sum(value))


# Suma del total nacional
sum(censo_mrp$n)

# agregados por nuevas variables
map(c(
  "depto",
  "area",
  "etnia",
  "sexo",
  "edad",
  "anoest",
  "discapacidad"
),
function(x) {
  censo_mrp %>% group_by_at(x) %>%
    summarise(n = sum(n)) %>%
    mutate(Prop = n / sum(n), N = sum(n))
})


plot_intro(censo_mrp)
plot_missing(censo_mrp)
plot_bar(censo_mrp, with = "n")


saveRDS(censo_mrp, "MrPDepartamental/ECU/2020/1.Ingreso/Data/censo_mrp.rds")


OCUPACION <-
  redatam.query(ecuador, "freq PROVIN.PROVIN by PERSONA.TIPOACT",
                tot.omit = FALSE)

OCUPACION2 <- OCUPACION %>% filter_at(vars(matches("_label")),
                                      all_vars(!. %in%   c(
                                        "__mv__", "__tot__", "No especificado", "__na__"
                                      )))

group_by(OCUPACION2,TIPOACT2_value, TIPOACT2_label) %>% summarise(n = sum(value))

OCUPACION2 <- OCUPACION2 %>% transmute(
  depto = str_pad(
    string = PROVIN1_value,
    width = 2,
    pad = "0"
  ),
  ocupados = ifelse(TIPOACT2_value %in% c(1:5),1,0),
  desocupados = ifelse(TIPOACT2_value %in% c(6,7),1,0),
  value
) %>% group_by(depto, ocupados, desocupados) %>%
  summarise(value = sum(value))


tabla <- pivot_wider(
  OCUPACION2,
  names_from = c("ocupados", "desocupados"),
  values_from = value,
  names_prefix = c("ocupados")
)

tasa_desocupacion <- tabla %>%
  transmute(depto,
            tasa_desocupacion = ocupados0_1 / sum(ocupados0_1 + ocupados1_0))

saveRDS(tasa_desocupacion, "MrPDepartamental/ECU/2020/1.Ingreso/Data/tasa_desocupacion.rds")
saveRDS(tasa_desocupacion, "MrPDepartamental/ECU/2020/2.Pobreza/Data/tasa_desocupacion.rds")
saveRDS(tasa_desocupacion, "MrPDepartamental/ECU/2020/3.PobrezaExtrema/Data/tasa_desocupacion.rds")
## Leer encuesta
upm <- read_dta("Z:/BG/ecu20n/ecu20n.dta") %>% select(upm)
encuesta <- read_dta("Z:/BC/ECU_2020N.dta") %>% 
  mutate(upm = upm$upm)
## Guardar encuesta
saveRDS(encuesta, "MrPDepartamental/ECU/2020/1.Ingreso/Data/encuestaECU20N.rds")

