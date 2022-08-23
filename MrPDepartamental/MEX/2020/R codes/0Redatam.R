#########################################################
# Proyecto MRP - Leave no one behind                    #
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
mexico <- redatam.open("MrPDepartamental/MEX/2020/1.Ingreso/Data/cpv2020mex-cde.dicX")

redatam.entities(mexico)
redatam.variables(mexico, "VIVIENDA")


CONTEOS <- redatam.query(
  mexico,
  "     freq   ENT.REDCODEN
       by     VIVIENDA.area
       by   PERSONA.sexo
       by   PERSONA.edad
       by   PERSONA.pbloper
       by   PERSONA.ESCOACUM
       by   PERSONA.disres
       TALLY VIVIENDA.FACTOR",
  tot.omit = FALSE
)

saveRDS(CONTEOS, file = "MrPDepartamental/MEX/2020/1.Ingreso/Data/CONTEOS.RDS")
rm("$table1")
# CONTEOS <- readRDS(file = "MrPDepartamental/MEX/2020/1.Ingreso/Data/CONTEOS.RDS")


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


##
CONTEOS2 %>% group_by(ESCOACUM6_value, ESCOACUM6_label) %>%
  summarise(n = sum(value))  %>% mutate(N = sum(n)) %>% data.frame()

CONTEOS2 %>% group_by(pbloper5_label, pbloper5_value) %>%
  summarise(n = sum(value))  %>% mutate(N = sum(n)) %>% data.frame()

CONTEOS2 %>% group_by(disres7_label, disres7_value) %>%
  summarise(n = sum(value))  %>% mutate(N = sum(n)) %>% data.frame()


censo_mrp <- CONTEOS2 %>% 
  filter(edad4_value > 11) %>%
  transmute(
  depto = str_pad(
    string = REDCODEN1_value,
    width = 2,
    pad = "0"
  ),
  area = case_when(area2_value == 1 ~ "1", # 1 = Urbana
                   TRUE ~ "0"),    # 0 = Rural
  sexo = as.character(sexo3_value),
  edad = case_when(
    edad4_value %in% 0:14 ~ "1",       # 5 a 14
    edad4_value %in% 15:29 ~ "2",      # 15 a 29
    edad4_value %in% 30:44 ~ "3",      # 30 a 44
    edad4_value %in% 45:64 ~ "4",      # 45 a 64
    TRUE ~ "5"
  ),     # 65 o mas


  anoest = case_when(
    edad4_value < 2| is.na(ESCOACUM6_value) ~ "98",     # No aplica
    ESCOACUM6_value == 99 ~ "99", #NS/NR
    ESCOACUM6_value %in% 0 ~ "1",  # Sin educacion
    ESCOACUM6_value %in% c(1:6) ~ "2",  # 1-6
    ESCOACUM6_value %in% c(7:12) ~ "3",  # 7-12
    ESCOACUM6_value > 12 ~ "4" ,  # 12 o mas
    TRUE ~ "Error"
  ),


  etnia = case_when(
    pbloper5_value == 1 ~ "1",    # Indigena
    pbloper5_value %in% c(5, 2)  ~ "2", # Afro,
    TRUE ~ "3" # Otro
  ),
  discapacidad = case_when(
    disres7_value == 8 ~ "0",    # No discapacitado
    TRUE ~ "1" # Discapacitado
  ),
  value
) %>% group_by(depto, area, etnia, sexo, edad, anoest, discapacidad) %>%
  summarise(n = sum(value), .groups = "drop")

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


saveRDS(censo_mrp, "MrPDepartamental/MEX/2020/1.Ingreso/Data/censo_mrp.rds")


## tasa de desocupación
OCUPACION <- redatam.query(mexico, "freq ENT.REDCODEN 
                                    by  PERSONA.pet
                                     TALLY VIVIENDA.FACTOR",
                           tot.omit = FALSE)


OCUPACION2 <- OCUPACION %>%
  filter_at(vars(matches("_label")),
            all_vars(!. %in%   c(
              "__mv__", "__tot__", "No especificado", "__na__"
            )))

group_by(OCUPACION2,pet2_value, pet2_label) %>%
  summarise(n = sum(value),
            N = sum(weight_tally))

sum(OCUPACION2$value)
sum(OCUPACION2$weight_tally)

OCUPACION2 %<>%
  transmute(
    depto = str_pad(
      string = REDCODEN1_value,
      width = 2,
      pad = "0"
    ),
    ocupados = ifelse(pet2_value  %in% c(1), 1, 0),
    desocupados = ifelse(pet2_value  %in% c(2), 1, 0),
    weight_tally
  ) %>% group_by(depto, ocupados, desocupados) %>%
  summarise(value = sum(weight_tally))


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

saveRDS(tasa_desocupacion, "MrPDepartamental/MEX/2020/1.Ingreso/Data/tasa_desocupacion.rds")
saveRDS(tasa_desocupacion, "MEX/2020/2.Pobreza/Data/tasa_desocupacion.rds")
saveRDS(tasa_desocupacion, "MEX/2020/3.PobrezaExtrema/Data/tasa_desocupacion.rds")
## Leer encuesta
#encuesta <- read_dta("Z:/BC/MEX_2018N1.dta")
entifede <- read_dta("Z:/BC/MEX_2020N1.dta") %>% select(entifede)
encuesta <- read_dta("Z:/BG/mex20n1/mex20n1.dta") %>%
   mutate(entifede = entifede$entifede)
## Guardar encuesta
saveRDS(encuesta, "MrPDepartamental/MEX/2020/1.Ingreso/Data/encuestaMEX20N1.rds")

