#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################

rm(list = ls())
cat("\f")

library(Rcpp)
library(RcppProgress)
library(redatam)
library(dplyr)
library(tidyverse)
library(haven)
library(DataExplorer)
# Se guarda una copia del diccionario (cpv2013hnd-cde.dicX) dado que
# el original presentas problemas para compilar desde R.
Honduras <- redatam.open("MrPDepartamental/HND/2019/1.Ingreso/Data/cpv2013hnd-cde.dicX")

redatam.entities(Honduras)
redatam.variables(Honduras, "VIVIENDA")

CONTEOS <- redatam.query(Honduras, "freq DEPTO.REDCODEN
                      by VIVIENDA.AREA
                      by PERSONA.P02
                      by PERSONA.P03
                      by PERSONA.P05
                      by PERSONA.ANOSEST
                      weight VIVIENDA.FACTORVI", tot.omit = FALSE)


saveRDS(CONTEOS, file = "MrPDepartamental/HND/2019/1.Ingreso/Data/CONTEOS.RDS")
rm("$table1")
# CONTEOS <- readRDS(file = "MrPDepartamental/HND/2019/1.Ingreso/Data/CONTEOS.RDS")

#   revisando valores unicos.
map(grep(pattern = "_value", x = names(CONTEOS),value = TRUE),
    function(by){
      unique(CONTEOS[[by]])
    })
# Eliminando totales de la tabla
CONTEOS2 <-
  CONTEOS %>% filter_at(vars(matches("_label")), all_vars(. !=  "__tot__"))

sum(CONTEOS2$value)
sum(CONTEOS2$weight_tally)
CONTEOS2$value <- CONTEOS2$weight_tally
## sumas por variables de agregación, coincidir con el total nacional.
map(grep(pattern = "_label", x = names(CONTEOS2),value = TRUE),
    function(by){
      CONTEOS2 %>% group_by_at(by) %>%
        summarise(n = sum(value)) %>%
        mutate(Prop = n / sum(n), N = sum(n)) %>%
        data.frame()
    })
map(grep(pattern = "_value", x = names(CONTEOS2),value = TRUE),
    function(by){
      CONTEOS2 %>% group_by_at(by) %>%
        summarise(n = sum(value)) %>%
        mutate(Prop = n / sum(n), N = sum(n)) %>%
        data.frame()
    })

CONTEOS2 %>% group_by(P034_value, P034_label) %>%
  summarise(n = sum(value)) %>% data.frame()

CONTEOS2 %>% group_by(P055_value, P055_label) %>%
  summarise(n = sum(value)) %>% data.frame()

CONTEOS2 %>% group_by(ANOSEST6_value, ANOSEST6_label) %>%
  summarise(n = sum(value)) %>% data.frame()


# Se filtra para mayores de 4 años por las variables NAS en idioma
censo_mrp <- CONTEOS2 %>%
  filter(P034_value > 4) %>% 
          transmute(
            depto = str_pad(
              string = REDCODEN1_value,
              width = 2,
              pad = "0"
            ),
            area = case_when(AREA2_value == 1 ~ "1", # 1 = Urbana
                             TRUE ~ "0"),
            sexo = as.character(P023_value),

            edad = case_when(
              P034_value  %in% 0:14 ~ "1", # 0 a 14
              P034_value  %in% 15:29 ~ "2", # 15 a 29
              P034_value  %in% 30:44 ~ "3", # 30 a 44
              P034_value  %in% 45:64 ~ "4", # 45 a 64
              TRUE ~ "5"), # 65 o mas

             etnia = case_when(
               P055_value %in% c(1) ~ "1", # Indigena
               P055_value %in% c(2,3)~ "2",# Negro-Afro hondureno
               TRUE ~ "3"), # Otro

            anoest = case_when(
              is.na(ANOSEST6_value) | P034_value < 6 ~ "98",     # No aplica
              ANOSEST6_value == 99 ~ "99", #NS/NR
              ANOSEST6_value %in% 0 ~ "1",  # Sin educacion
              ANOSEST6_value %in% c(1:6) ~ "2",  # 1-6
              ANOSEST6_value %in% c(7:12) ~ "3",  # 7-12
              ANOSEST6_value > 12 ~ "4" ,  # 12 o mas
              TRUE ~ "Error"
            ),
            value) %>%
    group_by(depto, area, sexo, edad, etnia, anoest) %>%
  summarise(n = sum(value), .groups = "drop")


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

saveRDS(censo_mrp, "MrPDepartamental/HND/2019/1.Ingreso/Data/censo_mrp.rds")

## tasa de desocupación
OCUPACION <-
  redatam.query(Honduras, "freq DEPTO.REDCODEN by PERSONA.PEA", tot.omit = FALSE)

OCUPACION2 <- OCUPACION %>%
  filter_at(vars(matches("_label")),
            all_vars(!. %in%   c(
              "__mv__", "__tot__", "No especificado", "__na__"
            )))

group_by(OCUPACION2,PEA2_value, PEA2_label) %>%
  summarise(n = sum(value))

sum(OCUPACION2$value)


OCUPACION <- OCUPACION2 %>% transmute(
  depto = str_pad(
    string = REDCODEN1_value,
    width = 2,
    pad = "0"
  ),
  ocupados = ifelse(PEA2_value %in% c(1), 1, 0),
  desocupados = ifelse(PEA2_value %in% c(2), 1, 0),
  value
) %>% group_by(depto, ocupados, desocupados) %>%
  summarise(value = sum(value), .groups = "drop")

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

saveRDS(tasa_desocupacion, "MrPDepartamental/HND/2019/1.Ingreso/Data/tasa_desocupacion.rds")
saveRDS(tasa_desocupacion, "MrPDepartamental/HND/2019/2.Pobreza/Data/tasa_desocupacion.rds")
saveRDS(tasa_desocupacion, "MrPDepartamental/HND/2019/3.PobrezaExtrema/Data/tasa_desocupacion.rds")

## Leer encuesta
encuesta <- read_dta("Z:/BC/HND_2019N.dta")
# encuesta <- read_dta("Z:/BG/hnd19n/hnd19n.dta")
## Guardar encuesta
saveRDS(encuesta, "MrPDepartamental/HND/2019/1.Ingreso/Data/encuestaHND19n.rds")

