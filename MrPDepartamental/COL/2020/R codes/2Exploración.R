#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero  Andrés Gutiérrez              #
#########################################################

rm(list = ls())

# Loading required libraries ----------------------------------------------

library(tidyverse)
library(reshape2)
library(stringr)
library(ggalt)
library(gridExtra)
library(scales)
library(formatR)
library(patchwork)

theme_set(bayesplot::theme_default())

source(file = "0Funciones/funciones_mrp.R", encoding = "UTF-8")
# Loading data ------------------------------------------------------------

encuesta_mrp <- readRDS("COL/2020/1.Ingreso/Data/encuesta_mrp.rds")
censo_mrp <- readRDS("COL/2020/1.Ingreso//Data/censo_mrp.rds")

# Exploratory data analysis -----------------------------------------------

theme_set(theme_bw())

### AGE ###
age_plot <-
  Plot_Compare(dat_censo = censo_mrp,
               dat_encuesta = encuesta_mrp,
               by = "edad")
### Sex ###
sex_plot <-
  Plot_Compare(dat_censo = censo_mrp,
               dat_encuesta = encuesta_mrp,
               by = "sexo")
### Level of schooling (LoS) ###
escolar_plot <-
  Plot_Compare(dat_censo = censo_mrp,
               dat_encuesta = encuesta_mrp,
               by = "anoest")

### States ###
depto_plot <-
  Plot_Compare(dat_censo = censo_mrp,
               dat_encuesta = encuesta_mrp,
               by = "depto")

#--- Patchwork in action ---#
(age_plot | sex_plot | escolar_plot) / ( depto_plot)


# Interaction effects  ----------------------------------------------------

theme_set(theme_bw())

### AGE x SEX ###
encuesta_mrp$pobreza <- encuesta_mrp$ingreso
#--- Percentage of people in poverty by AGE x SEX ---#
p_sex_age <-
  plot_interaction(dat_encuesta = encuesta_mrp,
                   by = "sexo",
                   by2 = "edad")

### Level of schooling (LoS) x SEX ###
p_sex_escolar <-
  plot_interaction(dat_encuesta = encuesta_mrp,
                   by = "sexo",
                   by2 = "anoest")

### State x SEX ###
p_sex_depto <-
  plot_interaction(dat_encuesta = encuesta_mrp,
                   by = "sexo",
                   by2 = "depto")

#--- Patchwork in action ---#
(p_sex_age + p_sex_escolar) / p_sex_depto

### Level of schooling (LoS) x AGE ###
p_escolar_edad <-
  plot_interaction(dat_encuesta = encuesta_mrp,
                   by = "anoest",
                   by2 = "edad") +
  theme(legend.position = "bottom") + labs(colour = "anoest")

### State x AGE ###
p_depto_edad <-
  plot_interaction(dat_encuesta = encuesta_mrp,
                   by = "edad",
                   by2 = "depto") +
  theme(legend.position = "bottom") + labs(colour = "Edad")

p_escolar_edad / p_depto_edad

### Level of schooling (LoS) x State ###
p_depto_escolar <-
  plot_interaction(dat_encuesta = encuesta_mrp,
                   by = "anoest",
                   by2 = "depto") +
  theme(legend.position = "bottom") + labs(colour = "anoest")

p_depto_escolar
