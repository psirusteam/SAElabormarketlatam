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

Paraguay <- redatam.open("MrPDepartamental/PRY/2020/1.Ingreso/Data/cpv2002pry-cde.dicx")

redatam.entities(Paraguay)
redatam.variables(Paraguay, "AREAC")

CONTEOS <- redatam.query(
  Paraguay,
  "freq DPTO.IDPTO
  by AREAC.AREARECO
  by PERSONA.P03
  by PERSONA.P04
  by PERSONA.ESTUDIOS",
  tot.omit = FALSE
)

saveRDS(CONTEOS, file = "MrPDepartamental/PRY/2020/1.Ingreso/Data/CONTEOS.RDS")
rm("$table1")
# CONTEOS <- readRDS(file = "MrPDepartamental/PRY/2020/1.Ingreso/Data/CONTEOS.RDS")

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


CONTEOS2 %>% group_by(IDPTO1_label, IDPTO1_value) %>%
  summarise(n = sum(value))  %>% arrange(IDPTO1_value) %>% data.frame()


########################


censo_mrp <- CONTEOS2 %>%
  filter(P044_value > 9) %>% 
  transmute(depto = str_pad(string = IDPTO1_value,
                            width = 2,
                            pad = "0"),
            area = case_when(AREARECO2_value == 1 ~ "1", # 1 = Urbana
                             TRUE ~ "0"),
            sexo = as.character(P033_value),

            edad = case_when(
              P044_value  %in% 0:14 ~ "1",  # 5 a 14
              P044_value  %in% 15:29 ~ "2", # 15 a 29
              P044_value  %in% 30:44 ~ "3", # 30 a 44
              P044_value  %in% 45:64 ~ "4", # 45 a 64
              TRUE ~ "5" ),                 # 65 o mas

            anoest = case_when(
              P044_value < 7 | is.na(ESTUDIOS5_value) ~ "98",     # No aplica
              ESTUDIOS5_value == 19 ~ "99", #NS/NR
              ESTUDIOS5_value %in% 0 ~ "1",  # Sin educacion
              ESTUDIOS5_value %in% c(1:6) ~ "2",  # 1-6
              ESTUDIOS5_value %in% c(7:12) ~ "3",  # 7-12
              ESTUDIOS5_value > 12 ~ "4" ,  # 12 o mas
              TRUE ~ "Error"
            ),
            value) %>% 
  mutate(depto = case_when(
    depto %in% c("00", "02", "05", "06", "07", "10", "11") ~ depto,
    TRUE ~ "20"
  )) %>% 
  group_by(depto, area, sexo, edad, anoest) %>%
  summarise(n = sum(value), .groups = "drop")

# Suma del total nacional
sum(censo_mrp$n)

# agregados por nuevas variables
map(c("depto",  "area",  "sexo", "edad", "anoest"),
    function(x){
      censo_mrp %>% group_by_at(x) %>%
        summarise(n = sum(n)) %>%
        mutate(Prop = n / sum(n), N = sum(n))
    })


plot_intro(censo_mrp)
plot_missing(censo_mrp)
plot_bar(censo_mrp, with = "n")


saveRDS(censo_mrp, "MrPDepartamental/PRY/2020/1.Ingreso/Data/censo_mrp.rds")

# Tasa de desocupcacion
OCUPACION <- redatam.query(Paraguay, "freq DPTO.IDPTO by PERSONA.PEAA",tot.omit = FALSE)

OCUPACION2 <- OCUPACION %>% filter_at(vars(matches("_label")),
                                      all_vars(!. %in%   c("__mv__", "NR", "__tot__", "No especificado", "__na__")))

group_by(OCUPACION2,PEAA2_value, PEAA2_label) %>% summarise(n = sum(value))

OCUPACION2 <- OCUPACION2 %>% transmute(
  depto = str_pad(
    string = IDPTO1_value,
    width = 2,
    pad = "0"
  ),
  ocupados = ifelse(PEAA2_value %in% c(1),1,0),
  desocupados = ifelse(PEAA2_value %in% c(2),1,0),
  value
) %>%
  mutate(depto = case_when(
    depto %in% c("00", "02", "05", "06", "07", "10", "11") ~ depto,
    TRUE ~ "20"
  )) %>%   
  group_by(depto, ocupados, desocupados) %>%
  summarise(value = sum(value))

tabla <-
  pivot_wider(
    OCUPACION2,
    names_from = c("ocupados", "desocupados"),
    values_from = value,
    names_prefix = c("ocupados")
  )

tasa_desocupacion = tabla %>%
  transmute(depto, depto,
            tasa_desocupacion = ocupados0_1 / sum(ocupados0_1 + ocupados1_0))

saveRDS(tasa_desocupacion, "MrPDepartamental/PRY/2020/1.Ingreso/Data/tasa_desocupacion.rds")
## Leer encuesta
# encuesta <- read_dta("Z:/BG/pry20n/pry20n.dta", encoding = "latin1")
encuesta <- read_dta("Z:/BC/PRY_2020N.dta", encoding = "latin1")
## Guardar encuesta
saveRDS(encuesta, "MrPDepartamental/PRY/2020/1.Ingreso/Data/encuestaPRY20n.rds")
