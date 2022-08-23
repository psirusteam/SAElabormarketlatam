#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################

### Cleaning R environment ###

rm(list =ls())
cat("\f")

library(Rcpp)
library(RcppProgress)
library(redatam)
library(dplyr)
library(tidyverse)
library(haven)
library(DataExplorer)

## leer base desde el repositorio CEPAL
guatemala <- redatam.open("MrPDepartamental/GTM/2014/1.Ingreso/Data/cpv2018gtm-cde.dicx")

CONTEOS <- redatam.query(
  guatemala,
  "freq DEPTO.IDEPTO
             by VIVIENDA.PLG11
             by  PERSONA.PCP7
             by PERSONA.PCP6
             by PERSONA.ANEDUCA
             by PERSONA.PBLOPER
  ",
  tot.omit = FALSE
)
saveRDS(CONTEOS,
        "MrPDepartamental/GTM/2014/1.Ingreso/Data/CONTEOS.RDS")
rm("$table1")
# CONTEOS <- readRDS(file =  "MrPDepartamental/GTM/2014/1.Ingreso/Data/CONTEOS.RDS")
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

##
CONTEOS2 %>% group_by(ANEDUCA5_value, ANEDUCA5_label) %>%
  summarise(n = sum(value))  %>%
  mutate(N = sum(n)) %>% data.frame()


censo_mrp <- CONTEOS2 %>% 
  filter(PCP73_value>6) %>% 
  transmute(
  depto = str_pad(
    string = IDEPTO1_value,
    width = 2,
    pad = "0"
  ),
  area = case_when(PLG112_value == 1 ~ "1", # 1 = Urbana
                   TRUE ~ "0"),
  # 0 = Rural
  sexo = as.character(PCP64_value),

  edad = case_when(
    PCP73_value %in% 0:14 ~ "1",       # 5 a 14
    PCP73_value %in% 15:29 ~ "2",      # 15 a 29
    PCP73_value %in% 30:44 ~ "3",      # 30 a 44
    PCP73_value %in% 45:64 ~ "4",      # 45 a 64
    TRUE ~ "5"
  ),     # 65 o mas

  anoest = case_when(

    is.na(ANEDUCA5_value) | PCP73_value < 7 ~ "98",     # No aplica
    ANEDUCA5_value == 99 ~ "99", #NS/NR
    ANEDUCA5_value %in% 0 ~ "1",  # Sin educacion
    ANEDUCA5_value %in% c(1:6) ~ "2",  # 1-6
    ANEDUCA5_value %in% c(7:12) ~ "3",  # 7-12
    ANEDUCA5_value > 12 ~ "4" ,  # 12 o mas
    TRUE ~ "Error"
  ),    
  etnia = case_when(
    PBLOPER6_value %in% 2:3 ~ "2",    # Afro
    PBLOPER6_value == 1  ~ "1", # Indigena,
    TRUE ~ "3" # Otro
  ),
  value
) %>% group_by(depto, area, etnia, sexo, edad, anoest) %>%
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

saveRDS(censo_mrp, "MrPDepartamental/GTM/2014/1.Ingreso/Data/censo_mrp.rds")

## tasa de desocupación
OCUPACION <-
  redatam.query(guatemala, "freq  DEPTO.IDEPTO by  PERSONA.PET",
                tot.omit = FALSE)
OCUPACION2 <- OCUPACION %>%
  filter(!PET2_label %in% c("__tot__", "No especificado", "__na__"))

group_by(OCUPACION2, PET2_value, PET2_label) %>% summarise(n = sum(value))

sum(OCUPACION2$value)


OCUPACION2 <- OCUPACION2 %>%
  transmute(
    depto = str_pad(
      string = IDEPTO1_value,
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

saveRDS(tasa_desocupacion, "MrPDepartamental/GTM/2014/1.Ingreso/Data/tasa_desocupacion.rds")
saveRDS(tasa_desocupacion, "GTM/2.Pobreza/Data/tasa_desocupacion.rds")
saveRDS(tasa_desocupacion, "GTM/3.PobrezaExtrema/Data/tasa_desocupacion.rds")

#Leer encuesta
depto <- read_dta("Z:/BG/gtm14n/gtm14n.dta") %>% select(depto)
encuesta <- read_dta("Z:/BC/GTM_2014N.dta") %>% 
  mutate(depto = depto$depto)
# Guardar encuesta
saveRDS(encuesta, "MrPDepartamental/GTM/2014/1.Ingreso/Data/encuestaGTM14n.rds")
