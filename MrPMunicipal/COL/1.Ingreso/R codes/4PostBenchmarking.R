#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################

rm(list =ls())
cat("\f")
# Loading required libraries ----------------------------------------------

library(scales)
library(patchwork)
library(srvyr)
library(survey)
library(haven)
library(sampling)
library(tidyverse)
library(magrittr)
source("0Funciones/funciones_mrp.R", encoding = "UTF-8")

# Loading data ------------------------------------------------------------
encuesta_mrp <- readRDS("COL/1.Ingreso/Data/encuesta_mrp.rds")
censo_mrp <- readRDS("COL/1.Ingreso/Data/censo_mrp.rds")
tasa_desocupados <- readRDS("COL/1.Ingreso/Data/tasa_desocupacion.rds")

fit <- readRDS("COL/1.Ingreso/Data/fit_mrp_logshift.rds")

fit <- fit[[1]]

# Poststratification at the National Level --------------------------------

statelevel_predictors_df <- tasa_desocupados


byAgrega <-
  grep(
    pattern =  "^(n|pobreza|ingreso)",
    x = names(censo_mrp),
    invert = TRUE,
    value = TRUE
  )

poststrat_df <- censo_mrp %>%  group_by_at(byAgrega) %>%
  summarise(n = sum(n), .groups = "drop")

# Expand state level predictors to the individual level

poststrat_df <- left_join(poststrat_df, statelevel_predictors_df,
                          by = c("depto","mpio"))

# Posterior_epred returns the posterior estimates for the different subgroups stored in the
# poststrat_df dataframe.


epred_mat <- predict(fit, newdata = poststrat_df, 
                     allow.new.levels = TRUE)

length(epred_mat[which(epred_mat < 0)])
poststrat_df[which(epred_mat < 0),]
epred_mat[which(epred_mat < 0)] = 1

summary(epred_mat)
hist(as.numeric(epred_mat))


poststrat_df$epred_mat <- epred_mat

# Resultados nacionales ---------------------------------------------------
## lineas de pobreza por área
## "0" = "Rural", "1" = "Urbana"

(lp <- encuesta_mrp %>% select(area,lp) %>% unique())
poststrat_df %<>% inner_join(lp, by = "area")


# Calculo de la pobreza Cepal.  -------------------------------------------
encuesta_mrp %<>% mutate(pobreza = ingreso/lp)
poststrat_df %<>% mutate(pobreza = epred_mat/lp)

# definiendo diseno muestral

diseno <- encuesta_mrp %>%
  as_survey_design(weights = fep)

###########################################
###########################################
###           Benchmarking              ###
###     (Gutiérrez - Guerrero, 2022)    ###
###########################################
###########################################
names_cov <-
  grep(
    pattern =  "^(n|pobreza|ingreso)",
    x = names(censo_mrp),
    invert = TRUE,
    value = TRUE
  )

names_cov <- names_cov[names_cov %in% names(encuesta_mrp)]

num_cat_censo <- apply(poststrat_df[names_cov], MARGIN = 2, function(x)
  length(unique(x)))

num_cat_sample <- apply(encuesta_mrp[names_cov], MARGIN = 2, function(x)
  length(unique(x)))

names_cov <- names_cov[num_cat_censo==num_cat_sample]
#names_cov <- c("area",   "sexo",   "edad",   "anoest", "depto")

poststrat_df %<>%
  fastDummies::dummy_cols(select_columns = names_cov,
                          remove_selected_columns = FALSE)


estimaciones <-
  map(names_cov ,~ poststrat_df %>% group_by_at(all_of(.x)) %>%
        summarise(
          medias = weighted.mean(pobreza, n),
          Nhat = sum(n),
          t_pobreza = sum(pobreza *n)
        ))

poststrat_df %<>% 
  mutate_at(vars(matches("\\d$")) ,~.*poststrat_df$pobreza)


### total
totales <- map(names_cov, ~encuesta_mrp %>% group_by_at(all_of(.x)) %>% 
                 summarise(
                   medias = weighted.mean(pobreza,fep),
                   Nhat = sum(fep),
                   t_pobreza = sum(pobreza*fep)
                 ))

paso <- sapply(names_cov, function(byi){
  encuesta_mrp %>% group_by_at(all_of(byi)) %>% 
    summarise(t_pobreza = sum(pobreza*fep))
})

unlist(paso["t_pobreza",])


poststrat_df$gk <- calib(Xs = poststrat_df %>% select(matches("\\d$")), 
                         d = poststrat_df$n,
                         total = unlist(paso["t_pobreza",]),
                         method="logit") 

checkcalibration(Xs = poststrat_df %>% select(matches("\\d$")), 
                 d = poststrat_df$n,
                 total = unlist(paso["t_pobreza",]),
                 g = poststrat_df$gk)

summary(poststrat_df$gk)

map(names_cov ,~ poststrat_df %>% group_by_at(all_of(.x)) %>%
      summarise(
        Nhat = sum(n),
        t_pobreza = sum(pobreza *n*gk)
      ) %>% 
      mutate(medias = t_pobreza/Nhat))

poststrat_df %<>%
  mutate(pobreza2 = pobreza *gk)  

temp <- map(names_cov ,~ poststrat_df %>% group_by_at(all_of(.x)) %>%
              summarise(
                Nhat = sum(n),
                t_pobreza = sum(n*pobreza2)
              ) %>% 
              mutate(medias = t_pobreza/Nhat)) 

totales[[1]]$t_pobreza-temp[[1]]$t_pobreza
totales[[2]]$t_pobreza-temp[[2]]$t_pobreza
totales[[3]]$t_pobreza-temp[[3]]$t_pobreza
totales[[4]]$t_pobreza-temp[[4]]$t_pobreza
totales[[5]]$t_pobreza-temp[[5]]$t_pobreza

jpeg(file = "COL/1.Ingreso/Output/Plot_Bench_Pobreza.jpeg")
hist(poststrat_df$gk)
dev.off()

saveRDS(poststrat_df %>% select(!matches("_\\d{,2}$")), 
        "COL/1.Ingreso/Data/poststrat_df.RDS")
saveRDS(encuesta_mrp, "COL/1.Ingreso/Data/encuesta_mrp.RDS")


