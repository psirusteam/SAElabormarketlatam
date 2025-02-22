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
library(rstan)
library(rstantools)
library(cmdstanr)
library(posterior)
library(tidybayes)

source("MrPDepartamental/0Funciones/funciones_mrp.R", encoding = "UTF-8")

# Loading data ------------------------------------------------------------
encuesta_mrp <- readRDS("MrPDepartamental/PRY2/2020/Data/encuesta_mrp.rds") %>% 
  filter(edad != "1") %>% rename(depto = depto2)
censo_mrp <- readRDS("MrPDepartamental/PRY2/2020/Data/censo_mrp.rds") %>% 
  filter(edad != "1") %>% select(-depto2) 
tasa_desocupados <- readRDS("MrPDepartamental/PRY2/2020/Data/tasa_desocupacion.rds")

fit <- readRDS(file = "MrPDepartamental/PRY2/2020/Data/fit_multinomial.rds")

# Poststratification at the National Level --------------------------------
# temp1 <- tasa_desocupados %>% 
#   select(ends_with("2"), depto, -depto2) %>% 
#   distinct() %>% rename(depto2 = depto) %>% 
#   rename_all(~gsub(pattern = "2$", replacement = "", x = .))
# 
# statelevel_predictors_df <- tasa_desocupados %>% 
#   select(-ends_with("2"), -depto, depto2) %>% 
#   distinct() %>% rename(depto = depto2)


byAgrega <-
  grep(
    pattern =  "^(n|pobreza|ingreso|lp|li|fep)",
    x = names(encuesta_mrp),
    invert = TRUE,
    value = TRUE
  )
#############################################
## Creando la variable multinomial (encuesta)
#############################################
draws <- as_draws_df(fit$draws())
ncat <- length(unique(encuesta_mrp$empleo))

encuesta_df_agg <-
  encuesta_mrp %>%
  group_by_at(all_of(byAgrega)) %>%
  summarise(n = n(),
            .groups = "drop") 

encuesta_df_agg %<>%
  spread(key = "empleo",
         value = "n", sep = "_" ,fill = 0) %>% 
  arrange((empleo_1))


theta_fit <- draws %>% select(matches("theta\\[")) %>% 
  colMeans() %>% 
  matrix(., nrow = nrow(encuesta_df_agg),
         ncol = ncat, byrow = FALSE,
         dimnames = list(1:nrow(encuesta_df_agg),paste0("theta_",1:ncat)) 
  ) %>% 
  data.frame()

theta_fit <- cbind(encuesta_df_agg %>% select(-matches("empleo")),
      theta_fit)

#############################################
## Creando la variable multinomial (censo)
#############################################
censo_df <- censo_mrp %>% filter(!anoest %in% c("99"))

theta_censo <- draws %>% select(matches("theta_p\\[")) %>% 
  colMeans() %>% 
  matrix(., nrow = nrow(censo_df),
         ncol = ncat, byrow = FALSE,
         dimnames = list(1:nrow(censo_df),paste0("theta_",1:ncat)) 
  ) %>% 
  data.frame()

poststrat_df <- cbind(censo_df , theta_censo)

###########################################
###########################################
###           Benchmarking              ###
###     (Gutiérrez - Guerrero, 2022)    ###
###########################################
###########################################
new_depto <- tasa_desocupados %>% distinct(depto, depto2) 
poststrat_df <- left_join(new_depto,poststrat_df,by = "depto")
depto <- poststrat_df$depto
poststrat_df %<>% select(-depto) %>%  rename(depto = depto2)

names_cov <- c("depto", "area", "sexo")
poststrat_df %<>%
  fastDummies::dummy_cols(select_columns = names_cov,
                          remove_selected_columns = FALSE)
estimaciones <-
  map(names_cov ,~ poststrat_df %>% group_by_at(all_of(.x)) %>%
        summarise(
          medias_1 = weighted.mean(theta_1, n),
          medias_2 = weighted.mean(theta_2, n),
          medias_3 = weighted.mean(theta_3, n),
          Nhat = sum(n),
          empleo_1 = sum(theta_1 *n),
          empleo_2 = sum(theta_2 *n),
          empleo_3 = sum(theta_3 *n)
        ))

Xdommy <- poststrat_df %>% select((-matches("theta_(1|2|3)$"))) %>% 
  mutate_at(vars(matches("_\\d")) ,
            list(theta_1 = function(x) x*poststrat_df$theta_1,
                 theta_2 = function(x) x*poststrat_df$theta_2,
                 theta_3 = function(x) x*poststrat_df$theta_3)) %>% 
  select((matches("theta_(1|2|3)$"))) 


### total
totales <- map(names_cov, ~encuesta_mrp %>% group_by_at(all_of(.x)) %>% 
                 summarise(
                   medias_1 = weighted.mean(empleo==1,fep),
                   medias_2 = weighted.mean(empleo==2,fep),
                   medias_3 = weighted.mean(empleo==3,fep),
                   empleo_1 = sum((empleo==1)*fep),
                   empleo_2 = sum((empleo==2)*fep),
                   empleo_3 = sum((empleo==3)*fep),
                   Nhat = sum(fep)
                 ))

Total <- encuesta_mrp[,names_cov] %>% 
  fastDummies::dummy_cols(select_columns = names_cov,
                        remove_selected_columns = TRUE) %>% 
  mutate_all(list(theta_1 = function(x) x*(encuesta_mrp$empleo==1)*encuesta_mrp$fep,
                  theta_2 = function(x) x*(encuesta_mrp$empleo==2)*encuesta_mrp$fep,
                  theta_3 = function(x) x*(encuesta_mrp$empleo==3)*encuesta_mrp$fep)) %>% 
  select((matches("theta_(1|2|3)$"))) %>%  colSums()


if(any(names(Total) != names(Xdommy))){
  setdiff(names(Total) , names(Xdommy))
  setdiff(names(Xdommy), names(Total) )
}else{
  data.frame(Poblacion = colSums(Xdommy * poststrat_df$n),
             Muestra = Total) %>% 
    mutate(diff = (Poblacion - Muestra)) %>%
    arrange(diff)
}

gk <- calib(Xs = Xdommy, 
                         d = poststrat_df$n,
                         total = Total,
                         method="logit", max_iter = 2000) 

checkcalibration(Xs = Xdommy, 
                 d = poststrat_df$n,
                 total = Total,
                 g = gk)

summary(gk)

data.frame(Poblacion = colSums(Xdommy*poststrat_df$n*gk),
           Muestra = Total) %>% mutate(diff = round(abs(Poblacion- Muestra),3)) %>% 
  arrange(diff)


jpeg(file = "MrPDepartamental/PRY2/2020/Output/Plot_Bench_gk.jpeg")
hist(gk)
dev.off()
poststrat_df$gk <- gk
poststrat_df$depto2 <- depto
saveRDS(poststrat_df %>% select(!matches("_\\d{,2}$"), matches("theta_")), 
        "MrPDepartamental/PRY2/2020/Data/poststrat_df.RDS")
