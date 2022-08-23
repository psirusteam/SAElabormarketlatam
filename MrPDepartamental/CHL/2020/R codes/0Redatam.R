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
  CHL <- redatam.open("MrPDepartamental/CHL/2020/1.Ingreso/Data/cpv2017chl-cde.dicx")


  CONTEOS <- redatam.query(
    CHL,
  "freq REGION.IDREGION
  by AREAUR.URBRUR
  by PERSONA.P09
  by PERSONA.ANEST
  by PERSONA.P08
  by PERSONA.PBLOPER",  tot.omit = FALSE
)

saveRDS(CONTEOS, "MrPDepartamental/CHL/2020/1.Ingreso/Data/CONTEO.RDS")
rm("$table1")
#CONTEOS <- readRDS("MrPDepartamental/CHL/2020/1.Ingreso/Data/CONTEO.RDS")

#   revisando valores unicos.
map(grep(pattern = "_value", x = names(CONTEOS),value = TRUE),
    function(by){
      unique(CONTEOS[[by]])
    })
# Eliminando totales de la tabla
CONTEOS2 <- CONTEOS %>% filter_at(vars(matches("_label")),all_vars(. !=  "__tot__"))


## sumas por variables de agregación, coincidir con el total nacional.
map(grep(pattern = "_value", x = names(CONTEOS2),value = TRUE),
    function(by){
      CONTEOS2 %>% group_by_at(by) %>%
        summarise(n = sum(value)) %>%
        mutate(Prop = n / sum(n), N = sum(n))
    })

## sumas por variables de agregación, coincidir con el total nacional.
map(grep(pattern = "_label", x = names(CONTEOS2),value = TRUE),
    function(by){
      CONTEOS2 %>% group_by_at(by) %>%
        summarise(n = sum(value)) %>%
        mutate(Prop = n / sum(n), N = sum(n))
    })

CONTEOS2 %>% group_by(ANEST4_value, ANEST4_label) %>%
  summarise(n = sum(value)) %>%
  mutate(Prop = n / sum(n), N = sum(n)) %>%
  data.frame()

CONTEOS2 %>% group_by(PBLOPER6_label,PBLOPER6_value) %>%
  summarise(n = sum(value)) %>%
  mutate(Prop = n / sum(n), N = sum(n)) %>%
  data.frame()


censo_mrp <- CONTEOS2 %>% 
  filter(P093_value > 14) %>% 
  transmute(
    depto = str_pad(
      string = IDREGION1_value,
      width = 2,
      pad = "0"
    ),
    area = case_when(URBRUR2_value == 1 ~ "1", # 1 = Urbana
                     TRUE ~ "0"), # 0 = Rural

    sexo = as.character(P085_value),
    edad = case_when(
      P093_value %in% 0:14 ~ "1",       # 5 a 14
      P093_value %in% 15:29 ~ "2",      # 15 a 29
      P093_value %in% 30:44 ~ "3",      # 30 a 44
      P093_value %in% 45:64 ~ "4",      # 45 a 64
      TRUE ~ "5"
    ),     # 65 o mas

    anoest = case_when(
      P093_value < 6~ "98", #No aplica 
      ANEST4_value == 99 ~ "99", #NS/NR
      ANEST4_value == 0  ~ "1", # Sin educacion
      ANEST4_value %in% c(1:6) ~ "2",       # 1 - 6
      ANEST4_value %in% c(7:12) ~ "3",      # 7 - 12
      ANEST4_value > 12 ~ "4",      # mas de 12
      TRUE ~ "Error"  ),

    etnia = case_when(
      PBLOPER6_value == 1 ~ "1", # Indigenas
      PBLOPER6_value == 2 ~ "2", # Afro
      TRUE ~ "3" # Otro
    ),
    value
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

saveRDS(censo_mrp, "MrPDepartamental/CHL/2020/1.Ingreso/Data/censo_mrp.rds")

## tasa de desocupación
OCUPACION <- redatam.query(CHL, "freq REGION.IDREGION by  PERSONA.PET",
                           tot.omit = FALSE)

OCUPACION2 <- OCUPACION %>% filter_at(vars(matches("_label")),
                        all_vars(!. %in%   c("__mv__", "__tot__", "No especificado", "__na__")))

  group_by(OCUPACION2,PET2_value, PET2_label) %>% summarise(n = sum(value))

sum(OCUPACION2$value)


OCUPACION2 <- OCUPACION2 %>%
  transmute(
    depto = str_pad(
      string = IDREGION1_value,
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

saveRDS(tasa_desocupacion, "MrPDepartamental/CHL/2020/1.Ingreso/Data/tasa_desocupacion.rds")
saveRDS(tasa_desocupacion, "MrPDepartamental/CHL/2020/2.Pobreza/Data/tasa_desocupacion.rds")
saveRDS(tasa_desocupacion, "MrPDepartamental/CHL/2020/3.PobrezaExtrema/Data/tasa_desocupacion.rds")

## Leer encuesta
encuesta <- read_dta("Z:/BC/CHL_2020N.dta")
## Guardar encuesta
saveRDS(encuesta, "MrPDepartamental/CHL/2020/1.Ingreso/Data/encuestaCHL20n.rds")


