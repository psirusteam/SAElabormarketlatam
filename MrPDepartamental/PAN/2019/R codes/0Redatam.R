#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################

### Cleaning R environment ###

rm(list = ls())
library(Rcpp)
library(RcppProgress)
library(redatam)
library(magrittr)
library(tidyverse)
library(haven)
library(DataExplorer)

Panama <- redatam.open("MrPDepartamental/PAN/2019/Data/PAN2010.dicx")

CONTEOS <- redatam.query(Panama, "freq PROVIN.PROV
                                  by PERSONA.P02SEXO
                                  by PERSONA.P03EDAD
                                  by PERSONA.P07DISCA
                                  by PERSONA.RP14GRADO
                                  by  LUGPOB.AREA
                                  by PERSONA.AAPROB", tot.omit = FALSE)

saveRDS(CONTEOS, file = "MrPDepartamental/PAN/2019/Data/CONTEOS.RDS")
rm("$table1")
# CONTEOS <- readRDS(file = "MrPDepartamental/PAN/2019/Data/CONTEOS.RDS")

#   revisando valores unicos.
map(grep(pattern = "_value", x = names(CONTEOS),value = TRUE),
    function(by){
      unique(CONTEOS[[by]])
    })
# Eliminando totales de la tabla
CONTEOS2 <- CONTEOS %>%
  filter_at(vars(matches("_label")),all_vars(. !=  "__tot__"))


## sumas por variables de agregación, coincidir con el total nacional.
map(grep(pattern = "_label", x = names(CONTEOS2),value = TRUE),
    function(by){
      CONTEOS2 %>% group_by_at(by) %>%
        summarise(n = sum(value)) %>%
        mutate(Prop = n / sum(n), N = sum(n))
    })
map(grep(pattern = "_value", x = names(CONTEOS2),value = TRUE),
    function(by){
      CONTEOS2 %>% group_by_at(by) %>%
        summarise(n = sum(value)) %>%
        mutate(Prop = n / sum(n), N = sum(n))
    })

group_by(CONTEOS2, AAPROB7_value, AAPROB7_label) %>%
  summarise(n = sum(value)) %>%
  mutate(Prop = n / sum(n), N = sum(n)) %>%
  data.frame()

censo_mrp <- CONTEOS2 %>%
  filter(P03EDAD3_value > 9) %>% 
  transmute(depto = str_pad(
              string = PROV1_label,
              width = 2,
              pad = "0"
            ),
            area = case_when(AREA6_value == 1 ~ "1", # 1 = Urbana
                             TRUE ~ "0"),    # 0 = Rural
            sexo = as.character(P02SEXO2_value),
             discapacidad = case_when(
                P07DISCA4_value %in% 1:6 ~ "1", # discapacitado
               TRUE ~ "0"), # No discapacitado
            edad = case_when(
              P03EDAD3_value  %in% 0:14 ~ "1", # 5 a 14
              P03EDAD3_value  %in% 15:29 ~ "2", # 15 a 29
              P03EDAD3_value  %in% 30:44 ~ "3", # 30 a 44
              P03EDAD3_value  %in% 45:64 ~ "4", # 45 a 64
              TRUE ~ "5"), # 65 o mas
            anoest = case_when(
              is.na(AAPROB7_value) | P03EDAD3_value < 6 ~ "98",     # No aplica
              AAPROB7_value == 99 ~ "99", #NS/NR
              AAPROB7_value %in% 0 ~ "1",  # Sin educacion
              AAPROB7_value %in% c(1:6) ~ "2",  # 1-6
              AAPROB7_value %in% c(7:12) ~ "3",  # 7-12
              AAPROB7_value > 12 ~ "4" ,  # 12 o mas
              TRUE ~ "Error"
            ),
            value) %>%
  group_by(depto, area, sexo, edad, discapacidad, anoest) %>%
  summarise(n = sum(value), .groups = "drop")

# Suma del total nacional
sum(censo_mrp$n)

# agregados por nuevas variables
map(c("depto", "area", "discapacidad", "sexo", "edad", "anoest"),
    function(x){
      censo_mrp %>% group_by_at(x) %>%
        summarise(n = sum(n)) %>%
        mutate(Prop = n / sum(n), N = sum(n))
    })

plot_intro(censo_mrp)
plot_missing(censo_mrp)
plot_bar(censo_mrp, with = "n")

saveRDS(censo_mrp, "MrPDepartamental/PAN/2019/Data/Censo_mrp.rds")

OCUPACION <- redatam.query(Panama, "freq PROVIN.PROV by PERSONA.RP17TRAB", tot.omit = FALSE)

OCUPACION2 <- OCUPACION %>%
  filter_at(vars(matches("_label")),
            all_vars(!. %in%   c(
              "__mv__", "__tot__", "No especificado", "__na__"
            )))

group_by(OCUPACION2,RP17TRAB2_label, RP17TRAB2_value) %>% summarise(n = sum(value))

sum(OCUPACION2$value)


OCUPACION2 <- OCUPACION2 %>% transmute(
  depto = str_pad(
    string = PROV1_value,
    width = 2,
    pad = "0"
  ),
  ocupados = ifelse(RP17TRAB2_value %in% c(1110,1120),1,0),
  desocupados = ifelse(RP17TRAB2_value %in% c(1211:1224),1,0),
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

saveRDS(tasa_desocupacion, "MrPDepartamental/PAN/2019/Data/tasa_desocupacion.rds")
saveRDS(tasa_desocupacion, "MrPDepartamental/PAN/2019/2.Pobreza/Data/tasa_desocupacion.rds")
saveRDS(tasa_desocupacion, "MrPDepartamental/PAN/2019/3.PobrezaExtrema/Data/tasa_desocupacion.rds")
## Leer encuesta
#prov <- read_dta("Z:/BG/pan19n1/pan19n1.dta") %>% select(prov)
encuesta <- read_dta("Z:/BC/PAN_2019N1.dta", encoding = "LATIN1") 

## Guardar encuesta
saveRDS(encuesta, "MrPDepartamental/PAN/2019//Data/encuestaPAN19N.rds")
