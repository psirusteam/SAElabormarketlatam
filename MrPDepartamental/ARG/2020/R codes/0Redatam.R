#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de empleo CEPAL                                #
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

## leer base desde el repositorio CEPAL
#argentina <- redatam.open("W:/CP2010/CP2010ARG/BASE_BAS_SEG/CPV2010Basico.dicx")
argentina <- redatam.open("MrPDepartamental/ARG/2020/1.Ingreso/Data/cpv2010ampliado.dicX")

redatam.entities(argentina)
redatam.variables(argentina, "PERSONA")

CONTEOS <- redatam.query(argentina, "freq PROV.PROV
                      by VIVIENDA.URP
                      by PERSONA.P02
                      by PERSONA.P03
                      by PERSONA.ANOEST
                      by PERSONA.MARCA_INDI
                      by PERSONA.MARCA_AFRO
                      TALLY VIVIENDA.EXPV
                      ", tot.omit = FALSE)
saveRDS(CONTEOS, file = "MrPDepartamental/ARG/2020/1.Ingreso/Data/CONTEOS.RDS")
rm("$table1")
CONTEOS <- readRDS(file = "MrPDepartamental/ARG/2020/1.Ingreso/Data/CONTEOS.RDS")
#   revisando valores unicos.
map(grep(pattern = "_value", x = names(CONTEOS),value = TRUE),
    function(by){
      unique(CONTEOS[[by]])
    })
# Eliminando totales de la tabla
CONTEOS2 <- CONTEOS %>% filter_at(vars(matches("_label")), 
                                  all_vars(. !=  "__tot__"))


sum(CONTEOS2$value)
sum(CONTEOS2$weight_tally)

## sumas por variables de agregación, coincidir con el total nacional.
map(grep(pattern = "_label", x = names(CONTEOS2),value = TRUE),
    function(by){
      CONTEOS2 %>% group_by_at(by) %>%
        summarise(n = ceiling(sum(weight_tally))) %>%
        mutate(Prop = n / sum(n), N = sum(n))
    })

map(grep(pattern = "_value", x = names(CONTEOS2),value = TRUE),
    function(by){
      CONTEOS2 %>% group_by_at(by) %>%
        summarise(n = sum(weight_tally)) %>%
        mutate(Prop = n / sum(n), N = sum(n))
    })

CONTEOS2 %>% group_by(ANOEST5_value, ANOEST5_label ) %>%
  summarise(n = sum(weight_tally)) %>% data.frame() %>%
  mutate(Prop = n / sum(n), N = sum(n)) %>%
  data.frame()


CONTEOS2 %>% group_by(MARCA_INDI6_value, MARCA_AFRO7_value ) %>%
  summarise(n = sum(weight_tally)) %>%
  mutate(Prop = n / sum(n), N = sum(n))

# SOLO PARA URBANO PUES LA ENCUESTA ES URBANA

censo_mrp <- CONTEOS2 %>%
  filter(URP2_value == 1,
         P034_value > 9) %>%
  transmute(
    depto = str_pad(
      string = PROV1_value,
      width = 2,
      pad = "0"
    ),
    sexo = as.character(P023_value),
    edad = case_when(
      P034_value %in% 0:14 ~ "1",       # 10 a 14
      P034_value %in% 15:29 ~ "2",      # 15 a 29
      P034_value %in% 30:44 ~ "3",      # 30 a 44
      P034_value %in% 45:64 ~ "4",      # 45 a 64
      TRUE ~ "5"
    ),
      anoest = case_when(
      P034_value < 5| ANOEST5_value == 100 ~ "98",     # No aplica
      ANOEST5_value == 99 ~ "99", #NS/NR
      ANOEST5_value %in% 0 ~ "1",  # Sin educacion
      ANOEST5_value %in% c(1:6) ~ "2",  # 1-6
      ANOEST5_value %in% c(7:12) ~ "3",  # 7-12
      ANOEST5_value > 12 ~ "4" ,  # 12 o mas
      TRUE ~ "Error"
      ),

    etnia = case_when(
      MARCA_INDI6_value == 1  ~ "1", #Indigena
      MARCA_INDI6_value == 2 & MARCA_AFRO7_value == 1 ~ "2", # Afro
      TRUE ~ "3", # Otro
    ),
    weight_tally
  ) %>% group_by(depto, sexo, edad, anoest, etnia) %>%
  summarise(n = sum(weight_tally))

######################
######################
### MUY IMPORTANTE ###
######################
######################

sum(censo_mrp$n)

# agregados por nuevas variables
map(c("depto", "sexo", "edad", "anoest", "etnia"),
    function(x) {
      censo_mrp %>% group_by_at(x) %>%
        summarise(n = sum(n)) %>%
        mutate(Prop = n / sum(n), N = sum(n))
    })

plot_missing(censo_mrp)
plot_bar(censo_mrp, with = "n")

saveRDS(censo_mrp, "MrPDepartamental/ARG/2020/1.Ingreso/Data/censo_mrp.rds")


# Tasa de desocupacion ----------------------------------------------------


OCUPACION <- redatam.query(argentina,
                           "freq PROV.PROV 
                           by VIVIENDA.URP 
                           by PERSONA.CONDACT
                           TALLY VIVIENDA.EXPV")

OCUPACION <- OCUPACION %>% filter(URP2_value == 1) %>%
  transmute(
    depto = str_pad(
      string = PROV1_value,
      width = 2,
      pad = "0"
    ),
    ocupados = ifelse(CONDACT3_value  %in% c(1), 1, 0),
    desocupados = ifelse(CONDACT3_value  %in% c(2), 1, 0),
    weight_tally
  ) %>% group_by(depto, ocupados, desocupados) %>%
  summarise(value = sum(weight_tally))


tabla <-
  pivot_wider(
    OCUPACION,
    names_from = c("ocupados", "desocupados"),
    values_from = value,
    names_prefix = c("ocupados")
  )

tasa_desocupacion <- tabla %>%
  transmute(depto,
            tasa_desocupacion = ocupados0_1 / sum(ocupados0_1 + ocupados1_0))

saveRDS(tasa_desocupacion, "MrPDepartamental/ARG/2020/1.Ingreso/Data/tasa_desocupacion.rds")
## leer encuesta
#encuesta <- read_dta("Z:/BG/arg19n/arg19n.dta")
encuesta <- read_dta("Z:/BC/ARG_2020N.dta")

saveRDS(encuesta, "MrPDepartamental/ARG/2020/1.Ingreso/Data/encuestaARG20n.rds")

