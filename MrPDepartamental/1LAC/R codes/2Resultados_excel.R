#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################

### Cleaning R environment ###

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
poststrat_df <- readRDS("1LAC/Data/poststrat_LAC_OK.RDS") 
poststrat_df_CME <- readRDS("1LAC/Data/poststrat_CME_07042022.RDS") %>% 
  unnest("poststrat") %>% unnest("poststrat")
load("1LAC/ShapeDepto/ShapeSAE.Rdata") 
## Actualizando PRY
PRY_shape <- ShapeSAE %>% filter(pais == "Paraguay") %>% select(-depto) 

PRY_temp <- data.frame(
  depto = c("17","10", "13", "00", "16", "05", "06", "14",
            "11", "01", "03", "04", "07", "08", "12", "09", 
            "15", "02"),
 nombre = c("Alto Paraguay" ,"Alto Paraná" ,"Amambay" ,"Asunción" ,
            "Boquerón" ,"Caaguazú" ,"Caazapá" ,"Canindeyú" ,"Central" ,
            "Concepción" ,"Cordillera" ,"Guairá" ,"Itapúa" ,"Misiones" ,
            "Ñeembucú" ,"Paraguarí" ,"Presidente Hayes" ,"San Pedro" )
)

PRY_shape <- inner_join(PRY_shape,PRY_temp,  by = "nombre")

ShapeSAE %<>% filter(pais != "Paraguay") %>% 
  bind_rows(PRY_shape) %>% arrange(pais)


bynames <- c("sexo", "edad" , "anoest", "etnia", "area", "discapacidad")

DAM <- readxl::read_xlsx("1LAC/CepalStat/DAM.xlsx") %>% 
  mutate(cod_DAM = paste0(pais, "_", depto))

DAM_anios <- readxl::read_xlsx("1LAC/CepalStat/DAM.xlsx", sheet = "anios")

ShapeSAE %<>% mutate( geometry =NULL)

ShapeSAE <- rbind(
  data.frame(pais = c("Argentina", "Ecuador"), 
            depto = c("94", "20"),
  nombre = NA
                 ), 
      ShapeSAE)

ShapeSAE %<>% mutate(cod_DAM = paste0(pais, "_", depto))

ShapeSAE <- full_join(DAM,ShapeSAE, by = c("pais", "depto", "cod_DAM"))

## codigos ISO para los paises 
cod_ISO <- c(
  "Argentina" = "ARG",
  "Bolivia" = "BOL",
  "Brasil" = "BRA",
  "Chile" = "CHL",
  "Colombia" = "COL",
  "Costa Rica" = "CRI",
  "Ecuador" = "ECU",
  "El salvador" = "SLV",
  "Guatemala" = "GTM",
  "Honduras" = "HND",
  "Mexico" = "MEX",
  "Nicaragua" = "NIC",
  "Panama" = "PAN",
  "Paraguay" = "PRY",
  "Peru" = "PER",
  "Republica dominicana" = "DOM",
  "Uruguay" = "URY"
)

ShapeSAE[["pais"]] <-
  dplyr::recode(as.character(ShapeSAE[["pais"]]) ,!!!cod_ISO)
bypais <- "PRY"
for(bypais in cod_ISO){
  xpais_I <- poststrat_df$ingreso %>% filter(pais == bypais) %>%
    select_if(function(x) {
      !all(is.na(x))
    })
  xpais_P <- poststrat_df$pobreza %>% filter(pais == bypais) %>%
    select_if(function(x) {
      !all(is.na(x))
    })
  xpais_E <- poststrat_df$extrema %>% filter(pais == bypais) %>%
    select_if(function(x) {
      !all(is.na(x))
    })
  
  xbynames <- c(intersect(bynames, names(xpais_I)))
  
  xbynames <-   t(combn(xbynames, 2)) 
  xbynames <- rbind(c("depto", "depto"),xbynames)
  
  dat_df_I = map(1:nrow(xbynames),
                 ~xpais_I %>% group_by_at(vars("depto", xbynames[.x,])) %>%
                   summarise(Ingreso = sum(n * pobreza2) / sum(n), .groups = "drop") %>% 
                   ungroup() )
  
  dat_df_P = map(1:nrow(xbynames),
                 ~xpais_P %>% group_by_at(vars("depto", xbynames[.x,])) %>%
                   summarise(Pobreza = sum(n * pobreza2) / sum(n), .groups = "drop") %>% 
                   ungroup() )
  
  dat_df_E = map(1:nrow(xbynames),
                 ~xpais_E %>% group_by_at(vars("depto", xbynames[.x,])) %>%
                   summarise(P_Extrema = sum(n * pobreza2) / sum(n), .groups = "drop") %>% 
                   ungroup() )
  
  nom_depto <- ShapeSAE %>% filter(pais == bypais) %>% 
    data.frame() %>% select(depto, nombre)
  
  dat_df <- map2(dat_df_I,dat_df_P,inner_join) %>% 
    map2(dat_df_E, inner_join) %>% 
    map(~.x %>% full_join(x = nom_depto, by = "depto"))
  
  names(dat_df) <- c("depto", apply(xbynames[-1,],MARGIN = 1,paste0,collapse = "_"))
  
  
  alerta <-
    dat_df %>% map_df( ~ .x %>% filter(Pobreza < P_Extrema)) %>% nrow()
  
  if(alerta>0){
    dat_df %<>% map( 
      ~ .x %>% mutate(P_Extrema = ifelse(Pobreza < P_Extrema,Pobreza, P_Extrema)))  
    cat("País =", bypais, "\n","Corregidos =", alerta, "\n")
  }
  
  openxlsx::write.xlsx(dat_df, 
                       file = paste0("1LAC/Data/Excel/",bypais, ".xlsx"),
                       overwrite = TRUE)  
  
}

##########################################################33
## codigos ISO para los paises 
ShapeSAE %<>% mutate(cod_DAM = paste0(pais, "_", depto))
cod_DAM <- setNames( ShapeSAE$DAM,ShapeSAE$cod_DAM)
cod_DAM_anios <- setNames( DAM_anios$DAM,DAM_anios$anios)

cod_API <- c(
  "ARG" = "216",
  "BOL" = "221",
  "BRA" = "222",
  "CHL" = "46573",
  "COL" = "225",
  "CRI" = "226",
  "ECU" = "229",
  "SLV" = "230",
  "GTM" = "235",
  "HND" = "239",
  "MEX" = "233",
  "NIC" = "240",
  "PAN" = "241",
  "PRY" = "242",
  "PER" = "244",
  "DOM" = "228",
  "URY" = "258"
)

 Area = c("0" = "331", "1" = "330")
 sexo = c("1" = "265", "2" = "266" )
 etnia = c("2" = "43044", "1" = "77477", "3" = "43043")
 edad = c("1" = "88214", "2" = "88215", "3" = "88216", "4" = "88217",
          "5" = "88218")
 anoest = c("1" = "88208", "2" = "88209", "3" = "88210", "4" = "88211")
 discapacitado = c("0" = "88205", "1" = "88204" )



xbynames <- c(intersect(bynames, names(poststrat_df$ingreso)))

xbynames <-   t(combn(xbynames, 2)) 
xbynames <- rbind(c("depto", "depto"),xbynames)

poststrat_df$ingreso %<>% 
  mutate(cod_DAM = paste0(pais,"_",depto))

poststrat_df$ingreso$depto <-
  dplyr::recode(poststrat_df$ingreso$cod_DAM ,!!!cod_DAM)
poststrat_df$ingreso$Año <-
  dplyr::recode(poststrat_df$ingreso$Año ,!!!cod_DAM_anios)


poststrat_df$ingreso$pais <-
  dplyr::recode(poststrat_df$ingreso$pais ,!!!cod_API)
poststrat_df$ingreso$area <-
  dplyr::recode(poststrat_df$ingreso$area ,!!!Area)
poststrat_df$ingreso$sexo <-
  dplyr::recode(poststrat_df$ingreso$sexo ,!!!sexo)
poststrat_df$ingreso$edad <-
  dplyr::recode(poststrat_df$ingreso$edad ,!!!edad)
poststrat_df$ingreso$etnia <-
  dplyr::recode(poststrat_df$ingreso$etnia ,!!!etnia)
poststrat_df$ingreso$anoest <-
  dplyr::recode(poststrat_df$ingreso$anoest ,!!!anoest)
poststrat_df$ingreso$discapacidad <-
  dplyr::recode(poststrat_df$ingreso$discapacidad ,!!!discapacitado)
poststrat_df$ingreso$cod_indicador <- "00001"

poststrat_df$pobreza %<>% 
  mutate(cod_DAM = paste0(pais,"_",depto))

poststrat_df$pobreza$depto <-
  dplyr::recode(poststrat_df$pobreza$cod_DAM ,!!!cod_DAM)
poststrat_df$pobreza$Año <-
  dplyr::recode(poststrat_df$pobreza$Año ,!!!cod_DAM_anios)
poststrat_df$pobreza$pais <-
  dplyr::recode(poststrat_df$pobreza$pais ,!!!cod_API)
poststrat_df$pobreza$area <-
  dplyr::recode(poststrat_df$pobreza$area ,!!!Area)
poststrat_df$pobreza$sexo <-
  dplyr::recode(poststrat_df$pobreza$sexo ,!!!sexo)
poststrat_df$pobreza$edad <-
  dplyr::recode(poststrat_df$pobreza$edad ,!!!edad)
poststrat_df$pobreza$etnia <-
  dplyr::recode(poststrat_df$pobreza$etnia ,!!!etnia)
poststrat_df$pobreza$anoest <-
  dplyr::recode(poststrat_df$pobreza$anoest ,!!!anoest)
poststrat_df$pobreza$discapacidad <-
  dplyr::recode(poststrat_df$pobreza$discapacidad ,!!!discapacitado)



poststrat_df$extrema %<>% 
  mutate(cod_DAM = paste0(pais,"_",depto))

poststrat_df$extrema$depto <-
  dplyr::recode(poststrat_df$extrema$cod_DAM ,!!!cod_DAM)
poststrat_df$extrema$Año <-
  dplyr::recode(poststrat_df$extrema$Año ,!!!cod_DAM_anios)
poststrat_df$extrema$pais <-
  dplyr::recode(poststrat_df$extrema$pais ,!!!cod_API)
poststrat_df$extrema$area <-
  dplyr::recode(poststrat_df$extrema$area ,!!!Area)
poststrat_df$extrema$sexo <-
  dplyr::recode(poststrat_df$extrema$sexo ,!!!sexo)
poststrat_df$extrema$edad <-
  dplyr::recode(poststrat_df$extrema$edad ,!!!edad)
poststrat_df$extrema$etnia <-
  dplyr::recode(poststrat_df$extrema$etnia ,!!!etnia)
poststrat_df$extrema$anoest <-
  dplyr::recode(poststrat_df$extrema$anoest ,!!!anoest)
poststrat_df$extrema$discapacidad <-
  dplyr::recode(poststrat_df$extrema$discapacidad ,!!!discapacitado)
poststrat_df$extrema$cod_indicador <- "00003"

## Pobreza sexo-anoest-depto
cod_indicador <- "4669"
poststrat_df$pobreza %>% filter(!anoest %in% c("99", "98")) %>%
  group_by_at(vars("pais",  "Año", "depto", "sexo", "anoest")) %>%
  summarise(value = sum(n * pobreza2) / sum(n), .groups = "drop") %>% 
  ungroup() %>% na.omit() %>% 
  mutate(record_id = paste0("s", 1:n()), 
         indicator_id = cod_indicador,
         source_id = "6470",
         footnotes_id = "",
         members_id = apply(.[,c("Año","depto", "sexo", "anoest")], 1, paste0, 
                            collapse = ","),
         value = format(value*100,  big.mark=",", scientific = FALSE)) %>% 
  select(record_id:members_id, value ) %>% 
  openxlsx::write.xlsx(x = .,
                       file = paste0("1LAC/CepalStat/Pobreza/sexo_anoest_depto_Pobreza.xlsx"),
                       overwrite = TRUE)



# 
# dat_df_I = map(1:nrow(xbynames),
#                  ~poststrat_df$ingreso %>% filter(!anoest %in% c("99", "98")) %>% 
#                    group_by_at(vars("pais", "Año", "cod_indicador", "depto", xbynames[.x,])) %>%
#                    summarise(value = sum(n * pobreza2) / sum(n), .groups = "drop") %>% 
#                    ungroup() %>% na.omit() %>% 
#                    mutate(record_id = paste0("s", 1:n()), 
#                         indicator_id = cod_indicador,
#                         source_id = "6470",
#                         footnotes_id = "",
#                         members_id = apply(.[,c("depto", xbynames[.x,])], 1, paste0, 
#                                            collapse = ",")) %>% 
#                        select(record_id:members_id, value ))
#   
# dat_df_P = map(1:nrow(xbynames),
#                  ~poststrat_df$pobreza %>% filter(!anoest %in% c("99", "98")) %>%
#                    group_by_at(vars("pais",  "Año", "cod_indicador", "depto", xbynames[.x,])) %>%
#                    summarise(value = sum(n * pobreza2) / sum(n), .groups = "drop") %>% 
#                    ungroup() %>% na.omit() %>% 
#                  mutate(record_id = paste0("s", 1:n()), 
#                         indicator_id = cod_indicador,
#                         source_id = "6470",
#                         footnotes_id = "",
#                         members_id = apply(.[,c("depto", xbynames[.x,])], 1, paste0, 
#                                            collapse = ",")) %>% 
#                  select(record_id:members_id, value ))
#   
# dat_df_E = map(1:nrow(xbynames),
#                  ~ poststrat_df$extrema %>% filter(!anoest %in% c("99", "98")) %>% 
#                    group_by_at(vars("pais",  "Año", "cod_indicador", "depto", xbynames[.x,])) %>%
#                    summarise(value = sum(n * pobreza2) / sum(n), .groups = "drop") %>% 
#                    ungroup() %>% na.omit() %>% 
#                  mutate(record_id = paste0("s", 1:n()), 
#                         indicator_id = cod_indicador,
#                         source_id = "6470",
#                         footnotes_id = "",
#                         members_id = apply(.[,c("depto", xbynames[.x,])], 1, paste0, 
#                                            collapse = ",")) %>% 
#                  select(record_id:members_id, value ))
# 
# names(dat_df_I) <- c("depto", apply(xbynames[-1,],
#                                     MARGIN = 1,
#                                     paste0,collapse = "_"))
# openxlsx::write.xlsx() 
# options(openxlsx.numFmt = "#.#0,00")
# 
# names(dat_df_I) %>% map(
#   ~ openxlsx::write.xlsx(x = dat_df_I[[.x]] %>% 
#                            mutate(value = format(value,  big.mark=",")),
#              file = paste0("1LAC/CepalStat/Ingreso/",.x,"_ingreso.xlsx"), overwrite = TRUE))
# 
# names(dat_df_P) <-  names(dat_df_I)
# names(dat_df_E)<-  names(dat_df_I)
# 
# 
# names(dat_df_P) %>% map(
#   ~ openxlsx::write.xlsx(x = dat_df_P[[.x]] %>% 
#                            mutate(value = format(value, big.mark=",")),
#                          file = paste0("1LAC/CepalStat/Pobreza/",.x,"_Pobreza.xlsx"),
#                          overwrite = TRUE))
# 
# names(dat_df_E) %>% map(
#   ~ openxlsx::write.xlsx(x = dat_df_P[[.x]] %>% 
#                            mutate(value = format(value, big.mark=",")),
#                          file = paste0("1LAC/CepalStat/Pobreza Extrema/",.x,"_Extrema.xlsx"),
#                          overwrite = TRUE))
# 
# 




names(dat_df_P) %>% map(
  ~write.csv2(x = dat_df_P[[.x]],append = FALSE, quote = FALSE,row.names = FALSE,
             file = paste0("1LAC/CepalStat/Pobreza/",.x,"_Pobreza.txt"),dec = "."
  ))
names(dat_df_I) %>% map(
  ~write.csv2(x = dat_df_E[[.x]],append = FALSE, quote = FALSE,row.names = FALSE,
             file = paste0("1LAC/CepalStat/Pobreza Extrema/",.x,"_Extrema.txt"),dec = "."
  ))


########################
poststrat_df_CME %<>% 
  mutate(cod_DAM = paste0(pais,"_",depto))

poststrat_df_CME$depto <-
  dplyr::recode(poststrat_df_CME$cod_DAM ,!!!cod_DAM)

  poststrat_df_CME$pais <-
    dplyr::recode(poststrat_df_CME$pais ,!!!cod_API)
  poststrat_df_CME$area <-
    dplyr::recode(poststrat_df_CME$area ,!!!Area)
  poststrat_df_CME$sexo <-
    dplyr::recode(poststrat_df_CME$sexo ,!!!sexo)
  poststrat_df_CME$edad <-
    dplyr::recode(poststrat_df_CME$edad ,!!!edad)
  poststrat_df_CME$etnia <-
    dplyr::recode(poststrat_df_CME$etnia ,!!!etnia)
  poststrat_df_CME$anoest <-
    dplyr::recode(poststrat_df_CME$anoest ,!!!anoest)
  poststrat_df_CME$discapacidad <-
    dplyr::recode(poststrat_df_CME$discapacidad ,!!!discapacitado)
  
  poststrat_df_CME %<>%
    mutate(cod_indicador = case_when(carpeta == "1.Ingreso" ~ "00004",
                                     carpeta ==  "2.Pobreza" ~ "00005",
                                     carpeta ==  "3.PobrezaExtrema" ~ "00006")) 

  
  dat_df_I = map(1:nrow(xbynames),
                 ~poststrat_df_CME %>% filter(cod_indicador == "00004") %>% 
                   select("pais", "Año", "cod_indicador", "depto", 
                          xbynames[.x,], "mrp_cv" ) %>% 
                   rename("Indicador" = mrp_cv) %>% 
                   na.omit())
  
  dat_df_P =  map(1:nrow(xbynames),
                  ~poststrat_df_CME %>% filter(cod_indicador == "00005") %>% 
                    select("pais", "cod_indicador", "depto",
                           xbynames[.x,], "mrp_cv") %>% 
                    rename("valor" = mrp_cv) %>% 
                    na.omit())
  
  dat_df_E =  map(1:nrow(xbynames),
                  ~poststrat_df_CME %>% filter(cod_indicador == "00006") %>% 
                    select("pais", "cod_indicador", "depto",
                           xbynames[.x,], "mrp_cv") %>% 
                    rename("valor" = mrp_cv) %>% 
                    na.omit())
  
  names(dat_df_I) <- c("depto", apply(xbynames[-1,],
                                      MARGIN = 1,
                                      paste0,collapse = "_"))
  names(dat_df_P) <-  names(dat_df_I)
  names(dat_df_E)<-  names(dat_df_I)
  
  openxlsx::write.xlsx(dat_df_I, 
                       file = paste0("1LAC/Data/Excel/Tablas_Ingreso_CME.xlsx"),
                       overwrite = TRUE)  
  openxlsx::write.xlsx(dat_df_P, 
                       file = paste0("1LAC/Data/Excel/Tablas_Pobreza_CME.xlsx"),
                       overwrite = TRUE)  
  openxlsx::write.xlsx(dat_df_E, 
                       file = paste0("1LAC/Data/Excel/Tablas_Pobreza_Extrema_CME.xlsx"),
                       overwrite = TRUE)  
                       

  # Mapping the estimates  --------------------------------------------------

dat_df_I = poststrat_df$ingreso %>% group_by_at(vars("pais", "depto")) %>%
  summarise(Ingreso = sum(n * pobreza2) / sum(n), .groups = "drop") %>%
  ungroup()

dat_df_P = poststrat_df$pobreza %>% group_by_at(vars("pais", "depto")) %>%
  summarise(pobreza = sum(n * pobreza2) / sum(n), .groups = "drop") %>%
  ungroup()

dat_df_E = poststrat_df$extrema %>% group_by_at(vars("pais", "depto")) %>%
  summarise(extrema = sum(n * pobreza2) / sum(n), .groups = "drop") %>%
  ungroup()

#Ingreso medios
############################################################################
brks_ing <- c(0, 1, 1.5, 2, 3, 6)
tmap_options(check.and.fix = TRUE)
map_ing <- tm_shape(
  ShapeSAE %>% left_join(dat_df_I,  by = c("depto", "pais"))) +
  tm_polygons("Ingreso",
              breaks = brks_ing,
              title = "Ingresos medios",
              palette = "-YlOrRd") + tm_layout(asp = 0)

tmap_save(
  map_ing,
  "1LAC/Output/Ingresos medios.pdf",
  width = 3*6920,
  height =3*4080,
  asp = 0
)

# Pobreza
############################################################################
brks_lp <- c(0, 0.2, 0.4, 0.6,0.8, 1)

map_P <- tm_shape( ShapeSAE %>% left_join(dat_df_P,  by = c("depto", "pais"))) +
  tm_polygons("pobreza",
              breaks = brks_lp,
              title = "Pobreza",
              palette = "YlOrRd") + tm_layout(asp = 0)

tmap_save(
  map_P,
  "1LAC/Output/Pobreza.pdf",
  width = 3*6920,
  height =3*4080,
  asp = 0
)

# Pobreza extrema 
############################################################################
brks_li <- c(0, 0.05, 0.1, 0.15, 0.2, 1)

map_E <- tm_shape( ShapeSAE %>% left_join(dat_df_E,  by = c("depto", "pais"))) +
  tm_polygons("extrema",
              breaks = brks_li,
              title = "Pobreza\nextrema",
              palette = "YlOrRd") + tm_layout(asp = 0)

tmap_save(
  map_E,
  "1LAC/Output/Pobreza extrema.pdf",
  width = 3*6920,
  height =3*4080,
  asp = 0
)

# Creando archivo conjunto
############################################################################
Maps_LAC <-  tmap_arrange(list(map_ing, map_P, map_E), ncol = 3, nrow = 1)


tmap_save(
  Maps_LAC,
  "1LAC/Output/LAC.pdf",
  width = 6*6920,
  height = 6*4080,
  asp = 0
)


