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

## leer base desde el repositorio CEPAL
brasil <- redatam.open("MrPDepartamental/BRA/2020/1.Ingreso/Data/cpv2010bra-cde.dicX")

redatam.entities(brasil)
redatam.variables(brasil, "DOMICIL")

CONTEOS <- redatam.query(brasil, "freq   UF.UF
                                  by     DOMICIL.AREA
                                  by     PESSOA.SEXO
                                  by     PESSOA.IDADE
                                  by     PESSOA.pbloper
                                  by     PESSOA.ANEST
                                TALLY DOMICIL.PESO", tot.omit = FALSE)

saveRDS(CONTEOS, file = "MrPDepartamental/BRA/1.Ingreso/Data/CONTEOS.RDS")
rm("$table1")
CONTEOS <- readRDS(file = "MrPDepartamental/BRA/2020/1.Ingreso/Data/CONTEOS.RDS")
#   revisando valores unicos.
map(grep(pattern = "_value", x = names(CONTEOS),value = TRUE),
    function(by){
      unique(CONTEOS[[by]])
    })
# Eliminando totales de la tabla
CONTEOS2 <- CONTEOS %>%
  filter_at(vars(matches("_label")),all_vars(. !=  "__tot__"))

sum(CONTEOS2$value)
sum(CONTEOS2$weight_tally)

## sumas por variables de agregación, coincidir con el total nacional.
map(grep(pattern = "_value", x = names(CONTEOS2),value = TRUE),
    function(by){
      CONTEOS2 %>% group_by_at(by) %>%
        summarise(n = sum(weight_tally)) %>%
        mutate(Prop = n / sum(n), N = sum(n))
    })

## sumas por variables de agregación, coincidir con el total nacional.
map(grep(pattern = "_label", x = names(CONTEOS2),value = TRUE),
    function(by){
      CONTEOS2 %>% group_by_at(by) %>%
        summarise(n = sum(weight_tally)) %>%
        mutate(Prop = n / sum(n), N = sum(n))
    })

##
CONTEOS2 %>% group_by(pbloper5_value, pbloper5_label) %>%
  summarise(n = sum(weight_tally), .groups = "drop")  %>% 
  mutate(N = sum(n)) %>% data.frame()

CONTEOS2 %>% group_by(ANEST6_value, ANEST6_label) %>%
  summarise(n = sum(weight_tally))  %>%
  mutate(N = sum(n)) %>% data.frame()



censo_mrp <- CONTEOS2 %>%
  filter(IDADE4_value > 13) %>% 
  transmute(
  depto = str_pad(
    string = UF1_value,
    width = 2,
    pad = "0"
  ),
  area = case_when(AREA2_value == 1 ~ "1", # 1 = Urbana
                   TRUE ~ "0"),    # 0 = Rural
  sexo = as.character(SEXO3_value),

  edad = case_when(
    IDADE4_value %in% 0:14 ~ "1",       # 5 a 14
    IDADE4_value %in% 15:29 ~ "2",      # 15 a 29
    IDADE4_value %in% 30:44 ~ "3",      # 30 a 44
    IDADE4_value %in% 45:64 ~ "4",      # 45 a 64
    TRUE ~ "5"
  ),     # 65 o mas

  anoest = case_when(
    IDADE4_value < 5 ~ "98",       # No aplica 
    ANEST6_value == 4 ~ "1",       # Sin eduacion
    ANEST6_value == 9 ~ "2",       # Primaria
    ANEST6_value == 12 ~ "3",      # Secundaria
    ANEST6_value == 16 ~ "4",      # Superior
    TRUE ~ "99"  ),                # No respondio

  etnia = case_when(
    pbloper5_value == 2 ~ "2",    # Afro
    pbloper5_value == 1 ~ "1", # Indigena,
    TRUE ~ "3" # Otro
  ),
  value = weight_tally
) %>% group_by(depto, area, etnia, sexo, edad, anoest) %>%
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

saveRDS(censo_mrp, "MrPDepartamental/BRA/2020/1.Ingreso/Data/censo_mrp.rds")


## tasa de desocupación
OCUPACION <- redatam.query(brasil, "freq UF.UF by  PESSOA.PET",
                           tot.omit = FALSE)

OCUPACION2 <- OCUPACION %>% filter_at(vars(matches("_label")),
                                      all_vars(!. %in%   c("__mv__", "__tot__", "No especificado", "__na__")))

group_by(OCUPACION2,PET2_value, PET2_label) %>% summarise(n = sum(value))

sum(OCUPACION2$value)

OCUPACION2 %<>%
  transmute(
    depto = str_pad(
      string = UF1_value,
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

tasa_desocupacion <- tabla %>% transmute(depto,
                                         tasa_desocupacion = ocupados0_1 / sum(ocupados0_1 + ocupados1_0))

saveRDS(tasa_desocupacion, "BRA/2020/1.Ingreso/Data/tasa_desocupacion.rds")
saveRDS(tasa_desocupacion, "BRA/2020/2.Pobreza/Data/tasa_desocupacion.rds")
saveRDS(tasa_desocupacion, "BRA/2020/3.PobrezaExtrema/Data/tasa_desocupacion.rds")


## Leer encuesta
encuesta <- read_dta("Z:/BG/bra20n1/bra20n1.dta")

## Guardar encuesta
saveRDS(encuesta, "BRA/2020/1.Ingreso/Data/encuestaBRA20N1.rds")
