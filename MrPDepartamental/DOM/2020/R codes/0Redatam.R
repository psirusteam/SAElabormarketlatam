#########################################################
# Proyecto MRP - Left No One Behind                     #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Diego Lemus & Andrés Gutiérrez#
#        & Felipe Molina                                #
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

# Pedir ayuda para crear la variable discapacitado.
RepDoma <- redatam.open("MrPDepartamental/DOM/2020/1.Ingreso/Data/cpv2010dom-cde.dicX")

redatam.entities(RepDoma)
redatam.variables(RepDoma, "PERSONA")

CONTEOS <- redatam.query(RepDoma,
                         "freq PROVIC.IDPROVI
                         by VIVIENDA.ZONA
                         by PERSONA.P27
                         by PERSONA.P29
                         by PERSONA.ANEST",
                         tot.omit = FALSE)

saveRDS(CONTEOS, file = "MrPDepartamental/DOM/2020/1.Ingreso/Data/CONTEOS.RDS")
rm("$table1")
# CONTEOS <- readRDS(file = "MrPDepartamental/DOM/2020/1.Ingreso/Data/CONTEOS.RDS")

#   revisando valores unicos.
map(grep(pattern = "_value", x = names(CONTEOS),value = TRUE),
    function(by){
      unique(CONTEOS[[by]])
    })
# Eliminando totales de la tabla
CONTEOS2 <-
  CONTEOS %>% filter_at(vars(matches("_label")), all_vars(. !=  "__tot__"))


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




CONTEOS2 %>% group_by( ANEST5_label,  ANEST5_value) %>%
   summarise(n = sum(value))  %>% arrange(ANEST5_value) %>%
  ungroup() %>%
  mutate(N = sum(n),
         Prop = ( round( n / N,3)),
         Acum = cumsum(Prop)) %>% data.frame()

# Se filtra para mayores de 4 años por las variables NAS en idioma
censo_mrp <- CONTEOS2 %>%
  filter(P294_value > 9)  %>% 
  transmute(depto = str_pad(
              string = IDPROVI1_value,
              width = 2,
              pad = "0"
            ),
            area = case_when(ZONA2_value == 1 ~ "1", # 1 = Urbana
                             TRUE ~ "0"),
            sexo = as.character(P273_value),
            edad = case_when(
              P294_value  %in% 0:14 ~ "1", # 0 a 14
              P294_value  %in% 15:29 ~ "2", # 15 a 29
              P294_value  %in% 30:44 ~ "3", # 30 a 44
              P294_value  %in% 45:64 ~ "4", # 45 a 64
              TRUE ~ "5"), # 65 o mas

            anoest = case_when(
              P294_value < 5| is.na(ANEST5_value) ~ "98",     # No aplica
              ANEST5_value == 99 ~ "99", #NS/NR
              ANEST5_value %in% 0 ~ "1",  # Sin educacion
              ANEST5_value %in% c(1:6) ~ "2",  # 1-6
              ANEST5_value %in% c(7:12) ~ "3",  # 7-12
              ANEST5_value > 12 ~ "4" ,  # 12 o mas
              TRUE ~ "Error"
            ),
            value) %>%
  group_by(depto, area, sexo, edad,anoest) %>%
  summarise(n = sum(value), .groups = "drop")

sum(censo_mrp$n)

# agregados por nuevas variables
map(c("depto", "area",  "sexo", "edad", "anoest"),
    function(x){
      censo_mrp %>% group_by_at(x) %>%
        summarise(n = sum(n)) %>%
        mutate(Prop = n / sum(n), N = sum(n))
    })

plot_intro(censo_mrp)
plot_missing(censo_mrp)
plot_bar(censo_mrp, with = "n")

saveRDS(censo_mrp, "MrPDepartamental/DOM/2020/1.Ingreso/Data/censo_mrp.rds")


OCUPACION <- redatam.query(RepDoma,
                           "freq PROVIC.IDPROVI
                           by PERSONA.PET", tot.omit = FALSE)

OCUPACION2 <- OCUPACION %>%
  filter_at(vars(matches("_label")),
            all_vars(!. %in%   c(
              "__mv__", "__tot__",
              "No espcificado", "__na__"
            )))

group_by(OCUPACION2, PET2_value, PET2_label) %>% summarise(n = sum(value))

sum(OCUPACION2$value)


OCUPACION2 <- OCUPACION2 %>%
  transmute(
    depto = str_pad(
      string = IDPROVI1_value,
      width = 2,
      pad = "0"
    ),
    ocupados = ifelse(PET2_value  %in% c(1), 1, 0),
    desocupados = ifelse(PET2_value  %in% c(2), 1, 0),
    value
  ) %>% group_by(depto, ocupados, desocupados) %>%
  summarise(value = sum(value))


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


saveRDS(tasa_desocupacion, "MrPDepartamental/DOM/2020/1.Ingreso/Data/tasa_desocupacion.rds")
saveRDS(tasa_desocupacion, "MrPDepartamental/DOM/2020/2.Pobreza/Data/tasa_desocupacion.rds")
saveRDS(tasa_desocupacion, "MrPDepartamental/DOM/2020/3.PobrezaExtrema/Data/tasa_desocupacion.rds")

##leer base de la encuesta
# encuesta <- read_dta("Z:/BG/dom20n1/dom20n1.dta")
encuesta <- read_dta("Z:/BC/DOM_2020N1.dta", encoding = "LATIN1")
## Guardar encuesta
saveRDS(encuesta, "MrPDepartamental/DOM/2020/1.Ingreso/Data/encuestaDOM20N1.rds")
