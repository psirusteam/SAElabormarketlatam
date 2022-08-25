#########################################################
# Proyecto MRP - Left No One Behind                     #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################

### Cleaning R environment ###
rm(list = ls())
cat("\f")

library(Rcpp)
library(RcppProgress)
library(redatam)
library(dplyr)
library(tidyverse)
library(haven)
library(DataExplorer)
library(magrittr)

## leer base desde el repositorio CEPAL
colombia <- redatam.open("MrPMunicipal/COL/2018/data/cpv2018col-cde.dicX")
redatam.entities(colombia)
redatam.variables(colombia, entName = "MUPIO")
redatam.variables(colombia, entName = "PERSONA")


CONTEOS <- redatam.query(colombia,
                      "freq MUPIO.REDCODEN
                      by CLASE.AREA
                      by PERSONA.P_SEXO
                      by PERSONA.EDAD5
                      by PERSONA.PBLOPER
                      by PERSONA.EDUCA",
                      tot.omit = FALSE)

saveRDS(CONTEOS, file = "MrPMunicipal/COL/2018/data/CONTEOS.rds")
rm(list = "$table1")
#CONTEOS <-  readRDS(file = "MrPMunicipal/COL/2018/data/CONTEOS.rds")

#   revisando valores unicos.
map(grep(pattern = "_value", x = names(CONTEOS),value = TRUE),
    function(by){
      unique(CONTEOS[[by]])
    })
# Eliminando totales de la tabla
CONTEOS2 <- CONTEOS %>%
  filter_at(vars(matches("_label")),all_vars(. !=  "__tot__"))


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


# Se filtra para mayores de 4 años por las variables NAS en idioma
censo_mrp <- CONTEOS2 %>%
  filter(EDAD54_value > 1) %>% 
  transmute(mpio =str_pad(string = REDCODEN1_value, width = 5, pad = "0"),
            depto = str_sub(string = mpio,start = 1,end = 2 ),
            area = case_when(AREA2_value == 1 ~ "1", # 1 = Urbana
                             TRUE ~ "0"),
            sexo = as.character(P_SEXO3_value),
            edad = EDAD54_label,
            etnia = case_when(
               PBLOPER5_value %in% c(1) ~ "1", # Indigena
               PBLOPER5_value %in% c(2)~ "2",  # Afro
               TRUE ~ "3"), # Otro

            anoest = EDUCA6_label,
            value) %>%
  group_by(depto,mpio, area, sexo, edad, etnia, anoest) %>%
  summarise(n = sum(value), .groups = "drop")

# Suma del total nacional
sum(censo_mrp$n)

# agregados por nuevas variables
map(c("depto", "mpio", "area", "etnia", "sexo", "edad", "anoest"),
    function(x){
      censo_mrp %>% group_by_at(x) %>%
        summarise(n = sum(n)) %>%
        mutate(Prop = n / sum(n), N = sum(n))
    })

plot_intro(censo_mrp)
plot_missing(censo_mrp)
plot_bar(censo_mrp, with = "n")

saveRDS(censo_mrp, "MrPMunicipal/COL/2018/Data/censo_mrp.rds")

OCUPACION <- redatam.query(colombia,
                           "freq MUPIO.REDCODEN by PERSONA.PET",
                           tot.omit = FALSE)

OCUPACION2 <- OCUPACION %>%
  filter(!PET2_label %in% c("__tot__", "No especificado", "__na__") &
         !is.na(REDCODEN1_value) )


group_by(OCUPACION2, PET2_value, PET2_label) %>% summarise(n = sum(value))


OCUPACION2 <- OCUPACION2 %>% transmute(
  mpio = str_pad(
    string = REDCODEN1_value,
    width = 5,
    pad = "0"
  ),
  depto = str_sub(string = mpio,start = 1,end = 2 ),
  ocupados = ifelse(PET2_value %in% c(1), 1, 0),
  desocupados = ifelse(PET2_value %in% c(2), 1, 0),
  value
) %>% group_by(depto, mpio, ocupados, desocupados) %>%
  summarise(value = sum(value), .groups = "drop")


tabla <-
  pivot_wider(
    OCUPACION2,
    names_from = c("ocupados", "desocupados"),
    values_from = value,
    names_prefix = c("ocupados"),
    values_fill = 0
  )

tasa_desocupacion <- tabla %>%
  transmute(depto,mpio,
            tasa_desocupacion = ocupados0_1/sum(ocupados0_1 + ocupados1_0 ))

saveRDS(tasa_desocupacion, "MrPMunicipal/COL/2018/Data/tasa_desocupacion.rds")

## Leer encuesta
encuesta <- read_dta("Z:/BG/col18n1/col18n1.dta")
encuesta %<>% mutate(orden_temp = str_pad(
  string = 1:n(),
  width = 7,
  pad = "0"
)) 

upms <- sas7bdat::read.sas7bdat("MrPMunicipal/COL/2018/Data/upm_dpto_2018.sas7bdat")

encuesta %<>% left_join(upms,
                        by = c("directorio" = "DIRECTORIO",
                               "secuencia_p" = "SECUENCIA_P",
                               "orden"))

encuesta$mpio <- substr(encuesta$segmento,8,12)

encuesta %<>% arrange(orden_temp)  
encuesta2 <- read_dta("Z:/BC/COL_2018N1.dta")
encuesta2$segmento <- encuesta$segmento
encuesta2$mpio <- encuesta$mpio
## Guardar encuesta
saveRDS(encuesta2, "MrPMunicipal/COL/2018/Data/encuestaCOL18N1.rds")

