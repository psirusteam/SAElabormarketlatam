#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################
rm(list =ls())

cat("\f")
# Loading required libraries ----------------------------------------------

library(rstan)
library(rstanarm)
library(data.table)
library(dplyr)
library(magrittr)
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

source("0Funciones/funciones_mrp.R", encoding = "UTF-8")

# Loading data ------------------------------------------------------------

encuesta_mrp <- readRDS("URY/2020/1.Ingreso/Data/encuesta_mrp.rds")
poststrat_df <- readRDS("URY/2020/1.Ingreso/Data/poststrat_df.RDS") %>% 
  filter(anoest != "99")
tasa_desocupados <- readRDS("URY/2020/1.Ingreso/Data/tasa_desocupacion.rds")
fit <- readRDS("URY/2020/1.Ingreso/Data/fit_bayes.rds")

# Poststratification at the National Level --------------------------------

statelevel_predictors_df <- tasa_desocupados

byAgrega <-
  grep(
    pattern =  "^(X|F|n|pobreza|ingreso|tasa_desocupacion|epred_mat|gk|depto|lp)",
    x = names(poststrat_df),
    invert = TRUE,
    value = TRUE
  )


# Posterior_epred returns the posterior estimates for the different subgroups stored in the
# poststrat_df dataframe.

epred_mat <- posterior_epred(fit, newdata = poststrat_df)

## validacion de los valores posteriores
summary(rowMeans(epred_mat))
summary(colMeans(epred_mat))

summary(as.numeric(epred_mat))
hist(as.numeric(epred_mat))

length(epred_mat[which(epred_mat < 0)])

prop_gzero <- function(x) mean(x < 0)
prop_gzero(epred_mat)

epred_mat[which(epred_mat < 0)] = 1

# Resultados nacionales ---------------------------------------------------
## lineas de pobreza por área
## "0" = "Rural", "1" = "Urbana"

(lp <- encuesta_mrp %>% select(area, lp) %>% unique())

lp <-   poststrat_df %>% mutate(orden = 1:n()) %>%
        select(orden, area) %>%
        left_join(lp) %>%
        arrange(orden, desc = FALSE) %>%
        select(lp)
lp <- lp$lp
### Ingresos
summary(colMeans(t(epred_mat)/lp))
summary(rowMeans(t(t(epred_mat)/lp)))

epred_mat_ingreso <- t(t(epred_mat)/lp)
dim(epred_mat)
dim(epred_mat_ingreso)

(mrp_estimate_Ingresolp <-
  Aux_Agregado(poststrat = poststrat_df,
             epredmat = epred_mat_ingreso,
             byMap = NULL)
)
# Poststratification at the State level -----------------------------------

byAgrega <-
  grep(
    pattern = "depto",
    x = byAgrega,
    value = TRUE,
    invert = TRUE
  )

byAgrega <- t(combn(byAgrega, 2))
byAgrega <- rbind(c("depto","depto" ), byAgrega)
# resultados para ingreso medio

mrp_estimate = map(1:nrow(byAgrega),
             ~poststrat_df %>% group_by_at(vars("depto", byAgrega[.x,])) %>%
               summarise(Benchmarking_estimate = sum(n * pobreza2) / sum(n), .groups = "drop") %>% 
               ungroup())


mrp_ECM <- map(1:nrow(byAgrega), function(i) {
     Aux_Agregado(poststrat_df,
                 epredmat = epred_mat_ingreso,
                 byMap = c("depto", byAgrega[i, ])) 

  })

tablas <- map2(mrp_estimate, mrp_ECM, inner_join) %>% 
  map(~.x %>%
        mutate(mrp_cv = mrp_estimate_se/Benchmarking_estimate*100))

nom_tabs <- c("depto", apply(byAgrega[-1,],MARGIN = 1,paste0, collapse = "_"))
names(tablas) <- nom_tabs


tablas %>% map(~.x %>% summarise(max = max(mrp_cv)))
tablas %>% map(~.x %>% filter(mrp_cv > 50))


openxlsx::write.xlsx(tablas, file = "URY/2020/1.Ingreso/Output/tablas_ECM.xlsx", 
                     overwrite = TRUE)
