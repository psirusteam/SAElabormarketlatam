#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Modelo Multinivel Bayesiano                           #
# Autor: Stalyn Guerrero  &  Andrés Gutiérrez           #
#########################################################

rm(list = ls())

# Loading required libraries ----------------------------------------------

library(tidyverse)
library(magrittr)
library(patchwork)
library(sp)
library(sf)
library(tmap)
library(RColorBrewer)
library(maptools)
library(tmaptools)

source("0Funciones/funciones_mrp.R", encoding = "UTF-8")
# Loading data ------------------------------------------------------------
poststrat_df <- readRDS("COL/1.Ingreso/Data/poststrat_df.RDS") %>% 
  mutate(.groups = NULL) %>% filter(anoest != "99")

bynames <-
  grep(
    pattern =  "^(n|pobreza|ingreso|tasa_desocupacion|epred_mat|gk|depto|mpio|lp)",
    x = names(poststrat_df),
    invert = TRUE,
    value = TRUE
  )

bynames <- t(combn(bynames, 2)) 

 
dat_df = map(1:nrow(bynames),
    ~poststrat_df %>% group_by_at(vars("mpio", bynames[.x,])) %>%
  summarise(Benchmarking_estimate = sum(n * pobreza2) / sum(n), .groups = "drop") %>% 
  ungroup())

bynames <-   as.tibble(bynames) %>% 
  mutate(dat_df = dat_df)


bynames <- mutate(
  bynames,
  outPaht = paste0("COL/1.Ingreso/Output/IngresoM_", paste0(V1, "_", V2),
                   ".pdf")
)

states_df <- poststrat_df %>% group_by_at("depto") %>%
  summarise(Benchmarking_estimate = sum(n * pobreza2) / sum(n), .groups = "drop") %>% 
  ungroup() 

mpio_df <- poststrat_df %>% group_by_at("mpio") %>%
  summarise(Benchmarking_estimate = sum(n * pobreza2) / sum(n), .groups = "drop") %>% 
  ungroup() 


## Leer Shape del pais
ShapeSAE <-
  read_sf("COL/ShapeDeptoCOL/dv_Municipio.shp") %>%
  rename(depto = COD_DEPTO, mpio = ID_ESPACIA) %>%
  mutate(depto = str_pad(depto, pad = "0", width = 2),
         depto_nombre = NOM_DEPART, 
         mpio_nombre = NOM_MUNICI)

###############################################################################
# Colombia
P1_ingresolp <- tm_shape(ShapeSAE %>%
                           left_join(states_df,  by = "depto"))

brks_ing <- c(0, 1, 1.5, 2, 3, 5)
tmap_options(check.and.fix = TRUE)
Mapa_ing <-
  P1_ingresolp + tm_polygons(
    "Benchmarking_estimate",
    breaks = brks_ing,
    title = "Ingreso medio",
    palette = "-YlOrRd"
  ) + tm_layout(asp = 0)

tmap_save(
  Mapa_ing,
  "COL/1.Ingreso/Output/depto.pdf",
  width = 6920,
  height = 4080,
  asp = 0
)

# Colombia
P1_ingresolp <- tm_shape(ShapeSAE %>%
                           left_join(mpio_df,  by = "mpio"))

Mapa_ing <-
  P1_ingresolp + tm_polygons(
    "Benchmarking_estimate",
    breaks = brks_ing,
    title = "Ingreso medio",
    palette = "-YlOrRd"
  ) + tm_layout(asp = 0)

tmap_save(
  Mapa_ing,
  "COL/1.Ingreso/Output/mpio.pdf",
  width = 6920,
  height = 4080,
  asp = 0
)


bynames %<>% rename(fnames = V1, cnames = V2)

bynames$Maps = pmap(bynames, function(...) {
  Aux_Maps(Shape = ShapeSAE,
           color_p = "-YlOrRd" ,
           brks = brks_ing,
           ...)
})

#########################################################
## Exportando tablas 
cod_shape <- ShapeSAE %>% as.data.frame() %>%
  select(depto, depto_nombre, mpio, mpio_nombre) 

nom_tabs <- c("depto", "mpio", paste0(bynames$fnames, "_", bynames$cnames))
dat_df <- c(list(states_df, mpio_df), bynames$dat_df)
names(dat_df) <- nom_tabs

dat_df <-
  map(dat_df, ~ full_join(y = .x, cod_shape)) %>%
  map( ~ .x %>% rename(Benchmarking = Benchmarking_estimate))

openxlsx::write.xlsx(dat_df, file = "COL/1.Ingreso/Output/tablas.xlsx", 
                     overwrite = TRUE)
