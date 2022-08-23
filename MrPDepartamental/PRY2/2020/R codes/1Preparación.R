#########################################################
# Proyecto MRP - Left No One Behind                     #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Diego Lemus & Andrés Gutiérrez#
#        & Felipe Molina                                #
#########################################################

### Cleaning R environment ###

rm(list =ls())
cat("\f")
#################
### Libraries ###
#################

library(tidyverse)
library(sampling)

####################################################
### Loading datasets: EPHC and Population census ###
####################################################
encuesta <- readRDS("MrPDepartamental/PRY2/2020/Data/encuestaPRY20n.rds" )
censo_mrp <- readRDS( "MrPDepartamental/PRY2/2020/Data/censo_mrp.rds")
##############################
### Exploratory statistics ###
##############################

### EPHC: Percentage of people living in poverty ##
  encuesta %>% group_by(dptorep, condact3) %>%
  summarise(prop = sum(`_fep`)  ) %>% 
  group_by(dptorep) %>% 
  mutate(prop = prop/sum(prop)) %>% 
  spread(key = condact3, value = "prop", fill = 0)


### EPHC: Searching for NA in the response and pre post-stratification ###
###                             variables                              ###
table(encuesta$condact3, useNA = "always")
labelled::generate_dictionary(encuesta %>% select(condact3))

table(encuesta$edad, encuesta$condact3, useNA = "always")
# 
# table(encuesta$pobreza, useNA = "always")
# labelled::generate_dictionary(encuesta %>% select(pobreza))
# 
# table(encuesta$sexo, useNA = "always")
# labelled::generate_dictionary(encuesta %>% select(sexo))
# 
# table(encuesta$edad, useNA = "always")
# labelled::generate_dictionary(encuesta %>% select(edad))
# 
# table(encuesta$anoest, useNA = "always")
# labelled::generate_dictionary(encuesta %>% select(anoest))
# 
# table(encuesta$anoest, encuesta$edad, useNA = "always")

### EPHC: Creating the post-stratification variables: Age, Schooling, Ethnic ###

encuesta_mrp <- encuesta %>% 
  filter(edad > 9, condact3 %in% 1:3)  %>%
  transmute(
  depto2 = str_pad(
    string = dptorep,
    width = 2,
    pad = "0"
  ),
  ingreso = ingcorte,   
  empleo = condact3,
  pobreza = ifelse(pobreza != 3, 1, 0),
  area = case_when(areageo2 == 1 ~ "1", TRUE ~ "0"),
  sexo = as.character(sexo),

  anoest = case_when(
    edad < 5 | is.na(anoest)   ~ "98"  , #No aplica
    anoest == 99 ~ "99", #NS/NR
    anoest == 0  ~ "1", # Sin educacion
    anoest %in% c(1:6) ~ "2",       # 1 - 6
    anoest %in% c(7:12) ~ "3",      # 7 - 12
    anoest > 12 ~ "4",      # mas de 12
    TRUE ~ "Error"  ),

  edad = case_when(
    edad < 15 ~ "1",
    edad < 30 ~ "2",
    edad < 45 ~ "3",
    edad < 65 ~ "4",
    TRUE ~ "5"),
fep =  `_fep`
  )

########################################
plot_intro(encuesta_mrp)
plot_missing(encuesta_mrp)
plot_bar(encuesta_mrp)

saveRDS(encuesta_mrp, file = "MrPDepartamental/PRY2/2020/Data/encuesta_mrp.rds")

# Actualización de tabla censal- IPFP -------------------------------------
names_cov <-
  grep(
    pattern =  "^(n|pobreza|ingreso)",
    x = names(censo_mrp),
    invert = TRUE,
    value = TRUE
  )

names_cov <- names_cov[names_cov %in% names(encuesta_mrp)]

num_cat_censo <- apply(censo_mrp[names_cov], MARGIN  = 2, function(x)
  length(unique(x)))

num_cat_sample <- apply(encuesta_mrp[names_cov], MARGIN  = 2, function(x)
  length(unique(x)))

names_cov <- names_cov[num_cat_censo==num_cat_sample]
# names_cov <- c("depto",  "area",   "sexo","edad","etnia")
# MatrizCalibrada creada únicamente para los niveles completos 
# IMPORTANTE: Excluir las covariables que tengan niveles incompletos

encuesta_mrp %>% group_by(anoest) %>% 
  summarise(n = sum(fep),
            n1 = n(),
            .groups = "drop") %>% 
  mutate(prop = n1/sum(n1))

censo_mrp %>% group_by(anoest) %>% 
  summarise(n = sum(n),
            .groups = "drop") %>% 
  mutate(prop = n/sum(n))


auxSuma <- function(dat, col, ni){
  dat %>% ungroup() %>% select(all_of(col))  %>%
    fastDummies::dummy_cols(remove_selected_columns = TRUE) %>% 
    mutate_all(~.*ni) %>% colSums()  
}

N.g <- map(names_cov,
           ~ auxSuma(encuesta_mrp, col = .x, ni = encuesta_mrp$fep)) %>%
  unlist()

N_censo.g <- map(names_cov,
                 ~ auxSuma(censo_mrp, col = .x, ni = censo_mrp$n)) %>%
  unlist()

names_xk <- intersect(names(N.g),names(N_censo.g))

N.g <- N.g[names_xk]
N_censo.g <- N_censo.g[names_xk]
data.frame(N.g, N_censo.g)

Xk <- censo_mrp %>% ungroup() %>% select(all_of(names_cov)) %>%
  fastDummies::dummy_cols(remove_selected_columns = TRUE) %>% 
  select(all_of(names_xk))


gk <- calib(Xs = Xk, 
            d = censo_mrp$n,
            total = N.g,
            method="logit") # linear primera opcion  

checkcalibration(Xs = Xk, 
                 d = censo_mrp$n,
                 total = N.g,
                 g = gk)


hist(gk)
summary(gk)
length(table(gk))



n1 <- ceiling(censo_mrp$n*gk)
summary(n1)
summary(censo_mrp$n)

jpeg(filename = "MrPDepartamental/PRY2/2020/Output/plot_actualizacion_censo1.jpeg",
     width = 2000, height = 2000)
plot(censo_mrp$n, n1)
dev.off()

sum(round(censo_mrp$n))
sum(n1) 
sum(encuesta_mrp$fep)

jpeg(filename = "MrPDepartamental/PRY2/2020/Output/plot_actualizacion_censo2.jpeg",
     width = 2000, height = 2000)
par(mfrow = c(2,2))
hist(censo_mrp$n)
boxplot(censo_mrp$n)
hist(n1)
boxplot(n1)
dev.off()

censo_mrp$n <- n1

saveRDS(censo_mrp, "MrPDepartamental/PRY2/2020/Data/censo_mrp.rds")










