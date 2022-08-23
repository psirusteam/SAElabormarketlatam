#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Modelo Multinivel Bayesiano                           #
# Autor: Stalyn Guerrero  &  Andrés Gutiérrez           #
#########################################################
rm(list = ls())

outOcupado <- "MrPDepartamental/DOM/2020/Output/Tasa Ocupados/"
outDesocupado <- "MrPDepartamental/DOM/2020/Output/Tasa desocupados/"
outParticipacion <- "MrPDepartamental/DOM/2020/Output/Tasa participacion/"

# Loading required libraries ----------------------------------------------

library(tidyverse)
library(magrittr)
library(patchwork)
library(sp)
library(sf)
library(tmap)
library(RColorBrewer)
library(maptools)
library(DescTools)

source("MrPDepartamental/0Funciones/funciones_mrp.R", encoding = "UTF-8")
source("MrPDepartamental/0Funciones/Funciones_empleo.R", encoding = "UTF-8")

# Loading data ------------------------------------------------------------
poststrat_df <- readRDS("MrPDepartamental/DOM/2020/Data/poststrat_df.RDS") %>% 
  mutate(n = n*gk)

# Puntos de corte para el mapa
brks_TO <- c(0 ,20, 40, 60, 80,100)
brks_TD <- c(0,5, 10, 15, 20, 100)
brks_TP <- c(0 ,20, 40, 60, 80,100)

### 


bynames <-
  grep(
    pattern =  "^(theta|n|pobreza|ingreso|tasa_desocupacion|epred_mat|gk|depto|lp|X|F)",
    x = names(poststrat_df),
    invert = TRUE,
    value = TRUE
  )

bynames <-   t(combn(bynames, 2)) 
bynames <- rbind(c("depto","depto"),bynames)

dat_df = map(1:nrow(bynames),
             ~Indicadores_censo(poststrat_df, unique(c("depto", bynames[.x,]) )))

bynames <-   as.tibble(bynames) %>% 
  mutate(dat_df = dat_df)

## Leer Shape del pais
#leyendo shape

ShapeSAE <- read_sf("MrPDepartamental/DOM/2020/ShapeDeptoDOM/DOM_adm1.shp")

ShapeSAE %<>% mutate(depto = str_pad(ID_1, pad = "0", width = 2))


## Ajustando codigo de departamento

### cod de departamento en la shape
cod_shape <- ShapeSAE %>% as.data.frame() %>% select(depto, NAME_1) %>%
  arrange(depto)

cod_censo <- c(
  "01" =  "Distrito Nacional",
  "02" =  "Azua",
  "03" =  "Baoruco",
  "04" =  "Barahona",
  "05" =  "Dajabon",
  "06" =  "Duarte",
  "07" =  "Elías Piña",
  "08" =  "El Seibo",
  "09" =  "Espaillat",
  "10" =  "Independencia",
  "11" =  "La Altagracia",
  "12" =  "La Romana",
  "13" =  "La Vega",
  "14" =  "María Trinidad Sánchez",
  "15" =  "Monte Cristi",
  "16" =  "Pedernales",
  "17" =  "Peravia",
  "18" =  "Puerto Plata",
  "19" =  "Hermanas Mirabal",
  "20" =  "SamanÃ¡",
  "21" =  "San Cristobal",
  "22" =  "San Juan",
  "23" =  "San Pedro de Macorís",
  "24" =  "SÃ¡nchez Ramírez",
  "25" =  "Santiago",
  "26" =  "Santiago Rodríguez",
  "27" =  "Valverde",
  "28" =  "Monseñor Nouel",
  "29" =  "Monte Plata",
  "30" =  "Hato Mayor",
  "31" =  "San José de Ocoa",
  "32" =  "Santo Domingo"
) %>%
  data.frame(nombre = .) %>% rownames_to_column("depto") %>%
  mutate(nombre = toupper(nombre))

full_join(cod_censo, cod_shape, by = "depto")

ShapeSAE %<>% mutate(depto = case_when(depto == "01" ~ "02",
                                       depto == "02" ~ "03",
                                       depto == "03" ~ "04",
                                       depto == "04" ~ "05",
                                       depto == "05" ~ "01",
                                       depto == "07" ~ "08",
                                       depto == "08" ~ "09",
                                       depto == "13" ~ "12",
                                       depto == "14" ~ "13",
                                       depto == "15" ~ "14",
                                       depto == "17" ~ "15",
                                       depto == "19" ~ "16",
                                       depto == "20" ~ "17",
                                       depto == "21" ~ "18",
                                       depto == "24" ~ "20",
                                       depto == "25" ~ "21",
                                       depto == "27" ~ "22",
                                       depto == "28" ~ "23",
                                       depto == "22" ~ "24",
                                       depto == "30" ~ "25",
                                       depto == "29" ~ "26",
                                       depto == "32" ~ "27",
                                       depto == "16" ~ "28",
                                       depto == "18" ~ "29",
                                       depto == "09" ~ "30",
                                       depto == "26" ~ "31",
                                       depto == "31" ~ "32",
                                       depto == "06" ~ "06",
                                       depto == "10" ~ "10",
                                       depto == "11" ~ "11",
                                       depto == "23" ~ "19",
                                       depto == "12" ~ "07",
                                       TRUE ~ "00"))
# Republica dominicana
P1_tasa <- tm_shape(ShapeSAE %>%
                      left_join(bynames$dat_df[[1]],  by = "depto"))

Mapa_TD <-
  P1_tasa + tm_polygons(
    "TD",
    breaks = brks_TD,
    title = "Tasa de desocupación",
    palette = "YlOrRd"
  ) + tm_layout(
    legend.only = FALSE,
    legend.height = -0.4,
    legend.width = -0.4,
    legend.position = c(0, 0),
    asp = 0,
    legend.title.size = 4,
    legend.text.size =  4
  )

tmap_save(
  Mapa_TD,
  paste0(outDesocupado,"Estados.pdf"),
  width = 6920,
  height = 4080,
  asp = 0
)

Mapa_TO <-
  P1_tasa + tm_polygons(
    "TO",
    breaks = brks_TO,
    title = "Tasa de ocupación",
    palette = "-YlOrRd"
  ) + tm_layout(
    legend.only = FALSE,
    legend.height = -0.4,
    legend.width = -0.4,
    legend.position = c(0,0),
    asp = 0,
    legend.title.size = 4,
    legend.text.size =  4
  )

tmap_save(
  Mapa_TO,
  paste0(outOcupado,"Estados.pdf"),
  width = 6920,
  height = 4080,
  asp = 0
)


Mapa_TP <-
  P1_tasa + tm_polygons(
    "TP",
    breaks = brks_TP,
    title = "Tasa de Participación",
    palette = "-YlOrRd"
  ) + tm_layout(
    legend.only = FALSE,
    legend.height = -0.4,
    legend.width = -0.4,
    legend.position = c(0,0),
    asp = 0,
    legend.title.size = 4,
    legend.text.size =  4
  )

tmap_save(
  Mapa_TP,
  paste0(outParticipacion,"Estados.pdf"),
  width = 6920,
  height = 4080,
  asp = 0
)

tmap_arrange(list(Mapa_TO,Mapa_TD,Mapa_TP),
             ncol = 3, norw = 1)

bynames %<>% rename(fnames = V1, cnames = V2)
### Tasa de desocupacion

temp_TD <- bynames %>% mutate(dat_df = dat_df %>%
                                map(~.x %>% rename(Benchmarking_estimate = TD)))
temp_TD %<>% mutate(outPaht = paste0(outDesocupado, fnames, "_", cnames, ".pdf"))

temp_TD <- pmap(temp_TD[-1,] , function(...) {
  Aux_Maps(Shape = ShapeSAE,
           color_p = "YlOrRd" ,
           brks = brks_TD,
           ...)
})

### Tasa de ocupacion
temp_TO <- bynames %>% mutate(dat_df = dat_df %>%
                                map(~.x %>% rename(Benchmarking_estimate = TO)))
temp_TO %<>% mutate(outPaht = paste0(outOcupado, fnames, "_", cnames, ".pdf"))

temp_TO <- pmap(temp_TO[-1,] , function(...) {
  Aux_Maps(Shape = ShapeSAE,
           color_p = "-YlOrRd" ,
           brks = brks_TO,
           ...)
})


### Tasa de Participación
temp_TP <- bynames %>% mutate(dat_df = dat_df %>%
                                map(~.x %>% rename(Benchmarking_estimate = TO)))
temp_TP %<>% mutate(outPaht = paste0(outParticipacion, fnames, "_", cnames, ".pdf"))

temp_TP <- pmap(temp_TP[-1,] , function(...) {
  Aux_Maps(Shape = ShapeSAE,
           color_p = "YlOrRd" ,
           brks = brks_TP,
           ...)
})



#########################################################
## Exportando tablas 
cod_shape <- ShapeSAE %>% as.data.frame() %>% select(depto, NAME_1) %>%
  arrange(depto)

nom_tabs <- c("depto",paste0(bynames$fnames[-1],"_", bynames$cnames[-1]))
names(dat_df) <- nom_tabs

dat_df <- map(dat_df, ~full_join(y = .x, cod_shape, by = "depto")) %>% 
  map(~.x %>% rename("Tasa de desocupación" = TD,
                     "Tasa de ocupación" = TO,
                     "Tasa de participación" = TP))

openxlsx::write.xlsx(dat_df, 
                     file = "MrPDepartamental/DOM/2020/Output/tablas.xlsx", 
                     overwrite = TRUE)
openxlsx::openXL("MrPDepartamental/DOM/2020/Output/tablas.xlsx")

