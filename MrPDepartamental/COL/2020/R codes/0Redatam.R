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
library(haven)
library(tidyverse)
library(DataExplorer)

### leer diccionario de CELADE

Colombia <- redatam.open("MrPDepartamental/COL/2020/1.Ingreso/Data/cpv2018col-cde.dicx")


CONTEOS <- redatam.query(Colombia,
                      "freq DEPTO.REDCODEN
                      by CLASE.AREA
                      by PERSONA.P_SEXO
                      by PERSONA.P_EDAD
                      by PERSONA.PBLOPER
                      by PERSONA.ANEST",
                      tot.omit = FALSE)
saveRDS(CONTEOS, file = "MrPDepartamental/COL/2020/1.Ingreso/Data/CONTEOS.RDS")
rm("$table1")
#CONTEOS <- readRDS( file = "MrPDepartamental/COL/2020/1.Ingreso/Data/CONTEOS.RDS")
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
  filter((P_EDAD4_value > 9 & AREA2_value == 2)|
           (P_EDAD4_value > 12 & AREA2_value == 1)) %>% 
  transmute(depto =str_pad(string = REDCODEN1_value, width = 2, pad = "0"),
            area = case_when(AREA2_value == 1 ~ "1", # 1 = Urbana
                             TRUE ~ "0"),
            sexo = as.character(P_SEXO3_value),
            edad = case_when(
              P_EDAD4_value %in% 0:14 ~ "1", # 0 a 14
              P_EDAD4_value %in% 15:29 ~ "2", # 15 a 29
              P_EDAD4_value %in% 30:44 ~ "3", # 30 a 44
              P_EDAD4_value %in% 45:64~ "4", # 45 a 64
              TRUE ~ "5"), # 65 o mas
            etnia = case_when(
               PBLOPER5_value %in% c(1) ~ "1", # Indigena
               PBLOPER5_value %in% c(2)~ "2",  # Afro
               TRUE ~ "3"), # Otro

            anoest = case_when(
              P_EDAD4_value < 6 | is.na(ANEST6_value) ~ "98",     # No aplica
              ANEST6_value == 99 ~ "99", #NS/NR
              ANEST6_value %in% 0 ~ "1",  # Sin educacion
              ANEST6_value %in% c(1:6) ~ "2",  # 1-6
              ANEST6_value %in% c(7:12) ~ "3",  # 7-12
              ANEST6_value > 12 ~ "4" ,  # 12 o mas
              TRUE ~ "Error"
            ),
            value) %>%
  group_by(depto, area, sexo, edad, etnia, anoest, .groups = "drop") %>%
  summarise(n = sum(value))

censo_mrp %>% group_by(area,edad) %>% 
  summarise(n = sum(n))

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

saveRDS(censo_mrp, "MrPDepartamental/COL/2020/1.Ingreso/Data/censo_mrp.rds")

OCUPACION <- redatam.query(Colombia,
                           "freq DEPTO.REDCODEN by PERSONA.PET",
                           tot.omit = FALSE)

OCUPACION2 <- OCUPACION %>%
  filter(!PET2_label %in% c("__tot__", "No especificado", "__na__") &
         !is.na(REDCODEN1_value) )


group_by(OCUPACION2, PET2_value, PET2_label) %>% summarise(n = sum(value))


OCUPACION2 <- OCUPACION2 %>% transmute(
  depto = str_pad(
    string = REDCODEN1_value,
    width = 2,
    pad = "0"
  ),
  ocupados = ifelse(PET2_value %in% c(1), 1, 0),
  desocupados = ifelse(PET2_value %in% c(2), 1, 0),
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
            tasa_desocupacion = ocupados0_1/sum(ocupados0_1 + ocupados1_0 ))

saveRDS(tasa_desocupacion, "MrPDepartamental/COL/2020/1.Ingreso/Data/tasa_desocupacion.rds")
saveRDS(tasa_desocupacion, "MrPDepartamental/COL/2020/2.Pobreza/Data/tasa_desocupacion.rds")
saveRDS(tasa_desocupacion, "MrPDepartamental/COL/2020/3.PobrezaExtrema/Data/tasa_desocupacion.rds")


## Leer encuesta
depto <- read_dta("Z:/BG/col20n1/col20n1.dta") %>% select(depto)
encuesta <- read_dta("Z:/BC/COL_2020N1.dta") %>%
  mutate(depto =depto$depto)
table(encuesta$depto, useNA = "a")
## Guardar encuesta
saveRDS(encuesta, "MrPDepartamental/COL/2020/1.Ingreso/Data/encuestaCOL20N1.rds")
