#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################

rm(list = ls())
library(Rcpp)
library(RcppProgress)
library(redatam)
library(dplyr)
library(tidyverse)
library(haven)
library(DataExplorer)

#peru <- redatam.open("W:/CP2010/CP2017PER/celade/cpv2017per-cde.dicx")
peru <- redatam.open("MrPDepartamental/PER/2020/1.Ingreso/Data/cpv2017per-cde.dicx")

redatam.entities(peru)
redatam.variables(peru, "VIVIENDA")
redatam.variables(peru, "PERSONA")

CONTEOS <- redatam.query(peru, "freq DEPARTAM.CCDD
                                  by VIVIENDA.VAREA
                                  by PERSONA.C5P041
                                  by PERSONA.C5P02
                                  by PERSONA.ANEST
                                  by PERSONA.P09DISC
                                  by PERSONA.PBLOPER
                         TALLY PERSONA.FACTORPOND",
                         tot.omit = FALSE)
saveRDS(CONTEOS, file = "MrPDepartamental/PER/2020/1.Ingreso/Data/CONTEOS.RDS")
rm("$table1")
CONTEOS <- readRDS(file = "MrPDepartamental/PER/2020/1.Ingreso/Data/CONTEOS.RDS")

#   revisando valores unicos.
map(grep(pattern = "_value", x = names(CONTEOS),value = TRUE),
    function(by){
      unique(CONTEOS[[by]])
    })
# Eliminando totales de la tabla
CONTEOS2 <- CONTEOS %>% filter_at(vars(matches("_label")),all_vars(. !=  "__tot__"))

sum(CONTEOS2$value)
sum(CONTEOS2$weight_tally)
CONTEOS2$value <- CONTEOS2$weight_tally
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


CONTEOS2 %>% group_by(ANEST5_label, ANEST5_value) %>%
  summarise(n = sum(value))  %>%
  mutate(N = sum(n)) %>%
  data.frame()

CONTEOS2 %>% group_by(PBLOPER7_label, PBLOPER7_value) %>%
  summarise(n = sum(value))  %>%
  mutate(N = sum(n)) %>%
  data.frame()


censo_mrp <- CONTEOS2 %>%
  filter(C5P0413_value > 13) %>% 
  transmute(depto = str_pad(
              string = CCDD1_value,
              width = 2,
              pad = "0"
            ),
            area = case_when(VAREA2_value == 1 ~ "1", # 1 = Urbana
                             TRUE ~ "0"),    # 0 = Rural
            sexo = as.character(C5P024_value),

          edad = case_when(
              C5P0413_value  %in% 0:14 ~ "1", # 5 a 14
              C5P0413_value  %in% 15:29 ~ "2", # 15 a 29
              C5P0413_value  %in% 30:44 ~ "3", # 30 a 44
              C5P0413_value  %in% 45:64 ~ "4", # 45 a 64
              TRUE ~ "5"), # 65 o mas

          anoest = case_when(
            C5P0413_value < 4| is.na(ANEST5_value) ~ "98",     # No aplica
            ANEST5_value == 99 ~ "99", #NS/NR
            ANEST5_value %in% 0 ~ "1",  # Sin educacion
            ANEST5_value %in% c(1:6) ~ "2",  # 1-6
            ANEST5_value %in% c(7:11) ~ "3",  # 7-12 (caso particular  de perú)
            ANEST5_value > 11 ~ "4" ,  # 12 o mas
            TRUE ~ "Error"
          ),
          etnia = case_when(
            PBLOPER7_value == 1 ~ "1", # Indigena
            PBLOPER7_value == 2 ~ "2", # Afro
            TRUE ~ "3"), # Otro

          discapacidad = case_when(
            P09DISC6_value == 63 ~ "0", # No discapacitado
            TRUE ~ "1"), # Discapacitado
            value) %>%
  group_by(depto, area, sexo, edad, etnia, discapacidad, anoest) %>%
  summarise(n = sum(value), .groups = "drop")

# Suma del total nacional
sum(censo_mrp$n)

# agregados por nuevas variables
map(c(
  "depto",
  "area",
  "discapacidad",
  "sexo",
  "edad",
  "etnia",
  "anoest"
),
function(x) {
  censo_mrp %>% group_by_at(x) %>%
    summarise(n = sum(n)) %>%
    mutate(Prop = n / sum(n), N = sum(n))
})

plot_intro(censo_mrp)
plot_missing(censo_mrp)
plot_bar(censo_mrp, with = "n")

saveRDS(censo_mrp, "MrPDepartamental/PER/2020/1.Ingreso/Data/censo_mrp.rds")
####################################################
OCUPACION <- redatam.query(peru, "freq DEPARTAM.CCDD
                           by PERSONA.PET
                           TALLY PERSONA.FACTORPOND", tot.omit = FALSE)

OCUPACION2 <- OCUPACION %>%
  filter_at(vars(matches("_label")),
            all_vars(!. %in%   c(
              "__mv__", "__tot__", "No especificado", "__na__"
            )))
OCUPACION2$value <- OCUPACION2$weight_tally
group_by(OCUPACION2, PET2_label, PET2_value) %>% summarise(n = sum(value))

sum(OCUPACION2$value)


OCUPACION2 <- OCUPACION2 %>%
  transmute(
    depto = str_pad(
      string = CCDD1_value,
      width = 2,
      pad = "0"
    ),
    ocupados = ifelse(PET2_value  %in% c(1), 1, 0),
    desocupados = ifelse(PET2_value  %in% c(2), 1, 0),
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

saveRDS(tasa_desocupacion, "MrPDepartamental/PER/2020/1.Ingreso/Data/tasa_desocupacion.rds")
saveRDS(tasa_desocupacion, "PER/2020/2.Pobreza/Data/tasa_desocupacion.rds")
saveRDS(tasa_desocupacion, "PER/2020/3.PobrezaExtrema/Data/tasa_desocupacion.rds")

## Leer encuesta
encuesta <- read_dta("Z:/BG/per20n/per20n.dta")
#encuesta <- readRDS("MrPDepartamental/PER/2020/1.Ingreso/Data/temp_encu.rds")
fep <- read_dta("Z:/BC/PER_2020N.dta", encoding = "LATIN1") %>% select(`_fep`)
encuesta$fep <- fep[["_fep"]]
## Guardar encuesta
saveRDS(encuesta, "MrPDepartamental/PER/2020/1.Ingreso/Data/encuestaPER20n.rds")

