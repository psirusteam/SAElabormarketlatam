#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################

rm(list = ls())
memory.limit(100000000000)

library(dplyr)
library(tidyverse)
library(readstata13)
library(haven)
library(DataExplorer)
library(redatam)

Costa_Rica <- redatam.open("MrPDepartamental/CRI/2020/1.Ingreso/Data/cpv2011cri-cde.dicx")

CONTEOS <- redatam.query(Costa_Rica,
"freq  DISTRITO.REDCODEN
 by    VIVIENDA.URBRUR
 by    PERSONA.SEXO
 by    PERSONA.EDAD
 by    PERSONA.ANEST
 by    PERSONA.DISCAPA
 by    PERSONA.ETNIA", tot.omit = FALSE)

saveRDS(CONTEOS, file = "MrPDepartamental/CRI/2020/1.Ingreso/Data/CONTEOS.RDS")
rm("$table1")
# CONTEOS <- readRDS( file = "MrPDepartamental/CRI/2020/1.Ingreso/Data/CONTEOS.RDS")
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
CONTEOS2 %>% group_by(ETNIA7_label , ETNIA7_value) %>%
  summarise(n = sum(value))  %>% mutate(N = sum(n)) %>% data.frame()

##
CONTEOS2 %>% group_by(ANEST5_label , ANEST5_value) %>%
  summarise(n = sum(value))  %>% mutate(N = sum(n)) %>% data.frame()



censo_mrp <- CONTEOS2 %>% 
  filter(EDAD4_value > 14) %>% 
  transmute(
  distrito = str_pad(
    string = REDCODEN1_value,
    width = 5,
    pad = "0"
  ),
  area = case_when(URBRUR2_value == 1 ~ "1", # 1 = Urbana
                   TRUE ~ "0"),   # 0 = Rural
  sexo = as.character(SEXO3_value),

  edad = case_when(
    EDAD4_value %in% 0:14 ~ "1",       # 5 a 14
    EDAD4_value %in% 15:29 ~ "2",      # 15 a 29
    EDAD4_value %in% 30:44 ~ "3",      # 30 a 44
    EDAD4_value %in% 45:64 ~ "4",      # 45 a 64
    TRUE ~ "5"   ),                    # 65 o mas

  anoest = case_when(
    is.na(ANEST5_value) | EDAD4_value < 7 ~ "98",     # No aplica
    ANEST5_value == 99 ~ "99", #NS/NR
    ANEST5_value %in% 0 ~ "1",  # Sin educacion
    ANEST5_value %in% c(1:6) ~ "2",  # 1-6
    ANEST5_value %in% c(7:12) ~ "3",  # 7-12
    ANEST5_value > 12 ~ "4" ,  # 12 o mas
    TRUE ~ "Error"
  ),

  etnia = case_when(
    ETNIA7_value == 1 ~ "2",    # Afro
    TRUE ~ "3" # Indigena
  ),
  discapacidad = case_when(DISCAPA6_value == 0 ~ "0", TRUE ~ "1"),
  value
) %>% group_by(distrito, area, etnia, sexo, edad, anoest, discapacidad) %>%
  summarise(n = sum(value))

cod5 <- c("70101", "70102", "70103", "70104", "70201", "70202",
          "70203", "70204", "70205", "70206", "70207", "70301", "70302",
          "70303", "70304", "70305", "70306", "70401", "70402", "70403",
          "70404", "70501", "70502", "70503", "70601", "70602", "70603",
          "70604", "70605")

cod3 <- c("20401", "20402", "20403", "20404", "20901", "20902",
          "20903", "20904", "20905", "60101", "60102", "60103", "60104",
          "60105", "60106", "60107", "60108", "60109", "60111", "60112",
          "60113", "60114", "60115", "60116", "60201", "60202", "60203",
          "60204", "60205", "60206", "60401", "60402", "60403", "60601",
          "60602", "60603", "60901", "61101", "61102")

cod6 <- c( "20114", "20213", "20306", "21001", "21002", "21003",
           "21004", "21005", "21006", "21007", "21008", "21009", "21010",
           "21011", "21012", "21013", "21301", "21302", "21303", "21304",
           "21305", "21306", "21307", "21308", "21401", "21402", "21403",
           "21404", "21501", "21502", "21503", "21504", "41001", "41002",
           "41003", "41004", "41005")

cod4 <- c( "11901", "11902", "11903", "11904", "11905", "11906",
           "11907", "11908", "11909", "11910", "11911", "60301", "60302",
           "60303", "60304", "60305", "60306", "60307", "60308", "60309",
           "60501", "60502", "60503", "60504", "60505", "60506", "60701",
           "60702", "60703", "60704", "60801", "60802", "60803", "60804",
           "60805", "60806", "61001", "61002", "61003", "61004"  )

cod2 <- c( "50101", "50102", "50103", "50104", "50105",
           "50201", "50202", "50203", "50204", "50205", "50206",
           "50207", "50301", "50302", "50303", "50304", "50305",
           "50306", "50307", "50308", "50309", "50401", "50402",
           "50403", "50404", "50501", "50502", "50503", "50504",
           "50601", "50602", "50603", "50604", "50605", "50701",
           "50702", "50703", "50704", "50801", "50802", "50803",
           "50804", "50805", "50806", "50807", "50901", "50902",
           "50903", "50904", "50905", "50906", "51001", "51002",
           "51003", "51004", "51101", "51102", "51103", "51104" )

cod1 <- c( "10101", "10102", "10103", "10104", "10105",
           "10106", "10107", "10108", "10109", "10110", "10111",
           "10201", "10202", "10203", "10301", "10302", "10303",
           "10304", "10305", "10306", "10307", "10308", "10309",
           "10310", "10311", "10312", "10313", "10401", "10402",
           "10403", "10404", "10405", "10406", "10407", "10408",
           "10409", "10501", "10502", "10503", "10601", "10602",
           "10603", "10604", "10605", "10606", "10607", "10701",
           "10702", "10703", "10704", "10705", "10706", "10801",
           "10802", "10803", "10804", "10805", "10806", "10807",
           "10901", "10902", "10903", "10904", "10905", "10906",
           "11001", "11002", "11003", "11004", "11005", "11101",
           "11102", "11103", "11104", "11105", "11201", "11202",
           "11203", "11204", "11205", "11301", "11302", "11303",
           "11304", "11305", "11401", "11402", "11403", "11501",
           "11502", "11503", "11504", "11601", "11602", "11603",
           "11604", "11605", "11701", "11702", "11703", "11801",
           "11802", "11803", "11804", "12001", "12002", "12003",
           "12004", "12005", "12006", "20101", "20102", "20103",
           "20104", "20105", "20106", "20107", "20108", "20109",
           "20110", "20111", "20112", "20113", "20201", "20202",
           "20203", "20204", "20205", "20206", "20207", "20208",
           "20209", "20210", "20211", "20212", "20301", "20302",
           "20303", "20304", "20305", "20307", "20308", "20501",
           "20502", "20503", "20504", "20505", "20506", "20507",
           "20508", "20601", "20602", "20603", "20604", "20605",
           "20606", "20607", "20608", "20701", "20702", "20703",
           "20704", "20705", "20706", "20707", "20801", "20802",
           "20803", "20804", "20805", "21101", "21102", "21103",
           "21104", "21105", "21106", "21107", "21201", "21202",
           "21203", "21204", "21205", "30101", "30102", "30103",
           "30104", "30105", "30106", "30107", "30108", "30109",
           "30110", "30111", "30201", "30202", "30203", "30204",
           "30205", "30301", "30302", "30303", "30304", "30305",
           "30306", "30307", "30308", "30401", "30402", "30403",
           "30501", "30502", "30503", "30504", "30505", "30506",
           "30507", "30508", "30509", "30510", "30511", "30512",
           "30601", "30602", "30603", "30701", "30702", "30703",
           "30704", "30705", "30801", "30802", "30803", "30804",
           "40101", "40102", "40103", "40104", "40105", "40201",
           "40202", "40203", "40204", "40205", "40206", "40301",
           "40302", "40303", "40304", "40305", "40306", "40307",
           "40308", "40401", "40402", "40403", "40404", "40405",
           "40406", "40501", "40502", "40503", "40504", "40505",
           "40601", "40602", "40603", "40604", "40701", "40702",
           "40703", "40801", "40802", "40803", "40901", "40902" )

censo1 <- censo_mrp %>%
  mutate(
    depto =
    case_when(
      distrito %in% cod1 ~ 1 ,
      distrito %in% cod2 ~ 2 ,
      distrito %in% cod3 ~ 3 ,
      distrito %in% cod4 ~ 4 ,
      distrito %in% cod5 ~ 5 ,
      distrito %in% cod6 ~ 6))

censo_mrp2 <- censo1 %>% mutate(depto = str_pad(
  string = depto,
  width = 2,
  pad = "0"
)) %>% group_by(depto, area, etnia, sexo, edad, anoest, discapacidad) %>%
  summarise(n = sum(n), .groups = "drop") %>% arrange(desc(n))

# Suma del total nacional
sum(censo_mrp2$n)

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
  censo_mrp2 %>% group_by_at(x) %>%
    summarise(n = sum(n)) %>%
    mutate(Prop = n / sum(n), N = sum(n))
})

plot_intro(censo_mrp2)
plot_missing(censo_mrp2)
plot_bar(censo_mrp2, with = "n")

saveRDS(censo_mrp2, "MrPDepartamental/CRI/2020/1.Ingreso/Data/censo_mrp.rds")

## tasa de desocupación
OCUPACION <-
  redatam.query(Costa_Rica, "freq DISTRITO.REDCODEN by  PERSONA.PET",
                tot.omit = FALSE)

OCUPACION2 <- OCUPACION %>% filter_at(vars(matches("_label")),
                                      all_vars(!. %in%   c("__tot__", "No especificado", "__na__")))


group_by(OCUPACION2,PET2_value, PET2_label) %>% summarise(n = sum(value))

sum(OCUPACION2$value)



OCUPACION2 <- OCUPACION2 %>%
  transmute(
    depto =
      case_when(
        REDCODEN1_value %in% cod1 ~ 1 ,
        REDCODEN1_value %in% cod2 ~ 2 ,
        REDCODEN1_value %in% cod3 ~ 3 ,
        REDCODEN1_value %in% cod4 ~ 4 ,
        REDCODEN1_value %in% cod5 ~ 5 ,
        REDCODEN1_value %in% cod6 ~ 6),
    depto = str_pad(
      string = depto,
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

saveRDS(tasa_desocupacion, "MrPDepartamental/CRI/2020/1.Ingreso/Data/tasa_desocupacion.rds")
saveRDS(tasa_desocupacion, "MrPDepartamental/CRI/2020/2.Pobreza/Data/tasa_desocupacion.rds")
saveRDS(tasa_desocupacion, "MrPDepartamental/CRI/2020/3.PobrezaExtrema/Data/tasa_desocupacion.rds")
## Leer encuesta
encuesta <- read_dta("Z:/BG/cri20n1/cri20n1.dta",  encoding = "LATIN1")
#encuesta <- read_dta("Z:/BC/CRI_2020N1.dta", encoding = "LATIN1")
## Guardar encuesta
saveRDS(encuesta, "MrPDepartamental/CRI/2020/1.Ingreso/Data/encuestaCRI20n1.rds")
