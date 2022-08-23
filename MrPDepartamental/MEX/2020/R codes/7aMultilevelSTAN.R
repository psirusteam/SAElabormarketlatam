#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################
rm(list =ls())


# Loading required libraries ----------------------------------------------

library(rstan)
library(rstanarm)
library(data.table)
library(dplyr)
library(forcats)
library(tidyr)
library(reshape2)
library(stringr)
library(ggplot2)
library(scales)
library(bayesplot)
library(gridExtra)
library(ggalt)
library(usmap)
library(gridExtra)
library(scales)
library(kableExtra)
library(formatR)
library(patchwork)

theme_set(bayesplot::theme_default())
library(tidyverse)


# Loading data ------------------------------------------------------------
memory.limit(10000000000000)
encuesta_mrp <- readRDS("MEX/2020/1.Ingreso/Data/encuesta_mrp.rds")
tasa_desocupados <- readRDS("MEX/2020/1.Ingreso/Data/tasa_desocupacion.rds")


# Bayesian Multilevel Modelling -------------------------------------------
summary(encuesta_mrp$ingreso)
encuesta_mrp$ingreso <- encuesta_mrp$ingreso + 1

statelevel_predictors_df <- tasa_desocupados

byAgrega <-
  grep(
    pattern =  "^(n|pobreza|ingreso|lp|li|fep)",
    x = names(encuesta_mrp),
    invert = TRUE,
    value = TRUE
  )

encuesta_df_agg <-
  encuesta_mrp %>%
  group_by_at(all_of(byAgrega)) %>%
  summarise(n = n(),
            ingreso = mean(ingreso),
            .groups = "drop") 


encuesta_df_agg <- inner_join(encuesta_df_agg, statelevel_predictors_df, by = "depto")


s <- system.time(
#--- Fit in stan_glmer ---#
  fit <- stan_lmer(
    ingreso ~ (1 | depto) +
      (1 | edad) +
      (1 | area) +
      (1 | anoest) +
      (1 | etnia) +
      (1 | depto:area) +
      (1 | depto:etnia) +
      (1 | depto:sexo) +
      (1 | depto:edad) +
      (1 | depto:anoest) +
      (1 | area:etnia) +
      (1 | area:sexo) +
      (1 | area:edad) +
      (1 | area:anoest) +
      (1 | etnia:sexo) +
      (1 | etnia:edad) +
      (1 | etnia:anoest) +
      (1 | sexo:edad) +
      (1 | sexo:anoest) +
      (1 | edad:anoest) +
      (1 | discapacidad) +
      sexo  + tasa_desocupacion +
        F182013_stable_lights + 
  X2016_crops.coverfraction +
  X2016_urban.coverfraction ,
                  weights = n,
                  data = encuesta_df_agg,
                  verbose = TRUE,
                 cores = 7,
                 chains = 4,
                 iter = 200  )
)


saveRDS(fit, file = "MEX/2020/1.Ingreso/Data/fit_bayes.rds")
# Assessment of the model -------------------------------------------------
#shinystan::launch_shinystan(fit)
############################################################
## Comparando los medelos Freq y Bayes
############################################################
# fit_freq <- readRDS("MEX/2020/1.Ingreso/Data/fit_mrp_logshift.rds")[[1]]
# fit_Bayes <- readRDS("MEX/2020/1.Ingreso/Data/fit_bayes.rds")

# # Graphical posterior predictive checks -----------------------------------
# new_encuesta <- encuesta_mrp %>% inner_join(statelevel_predictors_df, by = "depto")
# y_sam<- as.numeric(new_encuesta$ingreso)
# y_pred <- predict(fit_freq, newdata = new_encuesta)
# y_pred_B <- posterior_epred(fit_Bayes, newdata = new_encuesta)

# rowsrandom <- sample(nrow(y_pred_B), 100)
# y_pred2 <- y_pred_B[rowsrandom, ]

# color_scheme_set("brightblue")
# #
# dim(y_pred2)
# length(y_sam)
# #ppc_dens_overlay(y = y, y_pred)
# ppc_dens_overlay(y = as.numeric(y_sam), y_pred2)
# ppc_dens_overlay(y = y_sam, y_pred2) + xlim(c(0, 20500))

# colMeans(y_pred_B)
# ggplot(data.frame(datos = c(y_sam, y_pred, colMeans(y_pred_B)),
#                   repe = gl(3, length(y_sam), 
#                             labels = c("muestra", "Freq", "Bayes"))), 
#        aes(x = datos, fill = repe, alpha = 0.1)) +
#   geom_density()  + xlim(c(0, 20500))


# #ppc_hist(y = y, y_pred2)
# ppc_ecdf_overlay(y_sam, y_pred2)
# # Prueba de hipótesis posterior
# prop_gzero <- function(x) mean(x < 0)
# prop_gzero(y_sam)
# ppc_stat(y_sam, y_pred2, stat = "prop_gzero")

# y_lp <- as.numeric(y_sam/new_encuesta$lp)
# y_pred_lp <- t(t(y_pred_B)/new_encuesta$lp)

# prop_glp <- function(x) mean(x)
# prop_glp(y_lp)
# ppc_stat(y_lp, y_pred_lp, stat = "prop_glp")

