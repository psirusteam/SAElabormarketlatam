#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################

### Cleaning R environment ###

rm(list =ls())

#################
### Libraries ###
#################
memory.limit(500000)

library(tidyverse)
library(sampling)

####################################################
### Loading datasets: CASEN and Population census ###
####################################################

encuesta <- readRDS("MrPDepartamental/CHL/2020/1.Ingreso/Data/encuestaCHL20n.rds")
censo_mrp <- readRDS( "MrPDepartamental/CHL/2020/1.Ingreso/Data/censo_mrp.rds")

##############################
### Exploratory statistics ###
##############################

### CASEN: Percentage of people living in poverty ##

encuesta %>% group_by(region, condact3) %>%
  summarise(prop = sum(`_fep`)  ) %>% 
  group_by(region) %>% 
  mutate(prop = prop/sum(prop)) %>% 
  spread(key = condact3, value = "prop", fill = 0)
### CASEN: Searching for NA in the response and pre post-stratification ###
###                             variables                              ###

 table(encuesta$condact3, useNA = "always")
 
 table(encuesta$edad, encuesta$condact3, useNA = "always")
 labelled::generate_dictionary(encuesta %>% select(sexo))
# 
# table(encuesta$edad, useNA = "always")
# 
# table(encuesta$region, useNA = "always")
# labelled::generate_dictionary(encuesta %>% select(region))
# 
# table(encuesta$areageo2, useNA = "always")
# labelled::generate_dictionary(encuesta %>% select(areageo2))
# 
# 
# table(encuesta$etnia_ee, useNA = "always")
# labelled::generate_dictionary(encuesta %>% select(etnia_ee))
# 
# table(encuesta$anoest, useNA = "always")
# labelled::generate_dictionary(encuesta %>% select(niveduc_ee))
# 
# table(encuesta$anoest, encuesta$edad, useNA = "always")

### CASEN: Creating the post-stratification variables: Age, Schooling, Ethnic ###
# mayores a 14
 encuesta_mrp <- encuesta %>%
   filter(edad > 14, condact3 %in% 1:3) %>% 
   transmute(
  depto = str_pad(
    string = region,
    width = 2,
    pad = "0"
  ),
  ingreso = ingcorte,
  empleo = condact3,
  area = case_when(areageo2 == 1 ~ "1", TRUE ~ "0"),
  sexo = as.character(sexo),

  anoest = case_when(
    edad < 6 | is.na(anoest)   ~ "98"  , #No aplica
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

  etnia = case_when(
    etnia_ee == 1 ~ "1", # Indigena
    etnia_ee == 2 ~ "2",
     TRUE ~ "3"), # Otro
 fep = `_feh`
  )

########################################
plot_missing(encuesta_mrp)
plot_bar(encuesta_mrp)
plot_histogram(encuesta_mrp)

saveRDS(encuesta_mrp, file = "MrPDepartamental/CHL/2020/1.Ingreso/Data/encuesta_mrp.rds")

# Actualización de tabla censal- IPFP -------------------------------------
names_cov <-
  grep(
    pattern =  "^(n|pobreza|ingreso)",
    x = names(censo_mrp),
    invert = TRUE,
    value = TRUE
  )

names_cov <- names_cov[names_cov %in% names(encuesta_mrp)]

num_cat_censo <- apply(censo_mrp[names_cov], MARGIN =  2, function(x)
  length(unique(x)))

num_cat_sample <- apply(encuesta_mrp[names_cov], MARGIN =  2, function(x)
  length(unique(x)))

names_cov <- names_cov[num_cat_censo==num_cat_sample]


# MatrizCalibrada creada únicamente para los niveles completos 
# IMPORTANTE: Excluir las covariables que tengan niveles incompletos

encuesta_mrp %>% group_by(etnia) %>% 
  summarise(n = sum(fep),
            n1 = n(),
            .groups = "drop") %>% 
  mutate(prop = n1/sum(n1))

censo_mrp %>% group_by(etnia) %>% 
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

length(N.g)


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

jpeg(filename = "MrPDepartamental/CHL/2020/1.Ingreso/Output/plot_actualizacion_censo1.jpeg",
     width = 2000, height = 2000)
plot(censo_mrp$n, n1)
dev.off()

sum(round(censo_mrp$n))
sum(n1)
sum(encuesta_mrp$fep)

jpeg(filename = "MrPDepartamental/CHL/2020/1.Ingreso/Output/plot_actualizacion_censo2.jpeg",
     width = 2000, height = 2000)
par(mfrow = c(2,2))
hist(censo_mrp$n)
boxplot(censo_mrp$n)
hist(n1)
boxplot(n1)
dev.off()

censo_mrp$n <- n1

saveRDS(censo_mrp, "MrPDepartamental/CHL/2020/1.Ingreso/Data/censo_mrp.rds")
