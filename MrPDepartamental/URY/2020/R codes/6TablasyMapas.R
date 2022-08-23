#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Modelo Multinivel Bayesiano                           #
# Autor: Stalyn Guerrero  &  Andrés Gutiérrez           #
#########################################################

rm(list = ls())

outOcupado <- "MrPDepartamental/URY/2020/Output/Tasa Ocupados/"
outDesocupado <- "MrPDepartamental/URY/2020/Output/Tasa desocupados/"
outParticipacion <- "MrPDepartamental/URY/2020/Output/Tasa participacion/"

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
poststrat_df <- readRDS("MrPDepartamental/URY/2020/Data/poststrat_df.RDS") %>% 
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

bynames <-   as_tibble(bynames) %>% 
  mutate(dat_df = dat_df)


## Leer Shape del pais
ShapeSAE <- read_sf("MrPDepartamental/URY/2020/ShapeDeptoURY/URY_adm1.shp")
ShapeSAE %<>% mutate(depto = str_pad(ID_1, pad = "0", width = 2),
                     nombre = toupper(NAME_1),
                     depto = case_when(depto == "01" ~ "02",
                                       depto == "02" ~ "03",
                                       depto == "03" ~ "04",
                                       depto == "04" ~ "05",
                                       depto == "05" ~ "06",
                                       depto == "06" ~ "07",
                                       depto == "07" ~ "08",
                                       depto == "08" ~ "09",
                                       depto == "09" ~ "10",
                                       depto == "10" ~ "01",
                                       TRUE ~ depto))

## Ajustando codigo de departamento

### cod de departamento en la shape
cod_shape <- ShapeSAE %>% as.data.frame() %>% select(depto, nombre)

### cod de departamento en la encuesta

cod_encusta <- c(
  "01" =  "Montevideo",
  "02" =  "Artigas",
  "03" =  "Canelones",
  "04" =  "Cerro Largo",
  "05" =  "Colonia",
  "06" =  "Durazno",
  "07" =  "Flores",
  "08" =  "Florida",
  "09" =  "Lavalleja",
  "10" =  "Maldonado",
  "11" =  "Paysandú",
  "12" =  "Río Negro",
  "13" =  "Rivera",
  "14" =  "Rocha",
  "15" =  "Salto",
  "16" =  "San José",
  "17" =  "Soriano",
  "18" =  "Tacuarembó",
  "19" =  "Treinta y Tres"
) %>%
  data.frame(nombre = .) %>% rownames_to_column("depto") %>%
  mutate(nombre = toupper(nombre))
### cod de departamento en el censo
cod_censo <-
  c(
    "01" = "Montevideo",
    "02" = "Artigas",
    "03" = "Canelones",
    "04" = "Cerro Largo",
    "05" = "Colonia",
    "06" = "Durazno",
    "07" = "Flores",
    "08" = "Florida",
    "09" = "Lavalleja",
    "10" = "Maldonado",
    "11" = "PaysandÃº",
    "12" = "Rio Negro",
    "13" = "Rivera",
    "14" = "Rocha",
    "15" = "Salto",
    "16" = "San JosÃ©",
    "17" = "Soriano",
    "18" = "TacuarembÃ³",
    "19" = "Treinta y Tres"
  ) %>%
  data.frame(nombre = .) %>% rownames_to_column("depto") %>%
  mutate(nombre = toupper(nombre))



full_join(cod_encusta, cod_censo, by = "depto") %>%
  full_join(cod_shape, by = "depto") %>% arrange(depto)

# Uruguays
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
cod_shape <- ShapeSAE %>% as.data.frame() %>% select(depto, nombre)

nom_tabs <- c("depto",paste0(bynames$fnames[-1],"_", bynames$cnames[-1]))
names(dat_df) <- nom_tabs

dat_df <- map(dat_df, ~full_join(y = .x, cod_shape, by = "depto")) %>% 
  map(~.x %>% rename("Tasa de desocupación" = TD,
                     "Tasa de ocupación" = TO,
                     "Tasa de participación" = TP))

openxlsx::write.xlsx(dat_df, 
                     file = "MrPDepartamental/URY/2020/Output/tablas.xlsx", 
                     overwrite = TRUE)
openxlsx::openXL("MrPDepartamental/URY/2020/Output/tablas.xlsx")

