#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################
rm(list =ls())

# Loading required libraries ----------------------------------------------

library(patchwork)
library(trafo)
library(nortest)
library(lme4)
library(tidyverse)
library(rstan)
library(rstantools)
library(cmdstanr)
library(posterior)
library(tidybayes)
# Loading data ------------------------------------------------------------
memory.limit(10000000000000)
encuesta_mrp <- readRDS("MrPDepartamental/ARG/2020/Data/encuesta_mrp.rds") %>% 
  filter(edad != "1")
censo_mrp <- readRDS("MrPDepartamental/ARG/2020/Data/censo_mrp.rds") %>% 
  filter(edad != "1")
tasa_desocupados <- readRDS("MrPDepartamental/ARG/2020/Data/tasa_desocupacion.rds")
statelevel_predictors_df <- tasa_desocupados
###################################################################
# Leyendo el modelo 
fit <-
  cmdstan_model(
    stan_file = "MrPDepartamental/0Funciones/Multinivel8.stan",
    compile = TRUE)


#############################################
## Creando la variable multinomial (encuesta)
#############################################
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
            .groups = "drop") 

encuesta_df_agg %<>%
  spread(key = "empleo",
         value = "n", sep = "_" ,fill = 0) %>% 
  arrange((empleo_1))


encuesta_df_agg <- inner_join(encuesta_df_agg, statelevel_predictors_df, by = "depto")
#############################################
## Creando la variable multinomial (censo)
#############################################
censo_df <- censo_mrp %>% filter(!anoest %in% c("99"))

censo_df <- inner_join(censo_df, 
                       statelevel_predictors_df, by = "depto") %>% 
  ungroup()

##################################################
## Parámetros del modelo
##################################################
X <- encuesta_df_agg %>% select(-matches("depto|empleo|tasa|^F|^X")) %>%
  model.matrix( ~ -1+ ., data = .)  %>%
  bind_cols(encuesta_df_agg %>% select(matches("tasa|^F|^X"))) 

Y <- encuesta_df_agg %>% select(matches("empleo")) %>%
     as.matrix(.)

Z <- encuesta_df_agg %>% select(matches("depto")) %>%
  model.matrix( ~ -1+ ., data = .)%>%
  as.matrix(.)


Xp <- censo_df %>% select(-matches("^n|depto|empleo|tasa|^F|^X")) %>%
  model.matrix( ~-1+ ., data = .)  %>%
  bind_cols(censo_df %>% select(matches("tasa|F|^X"))) 

Zp <- censo_df %>% select(matches("depto")) %>%
  model.matrix( ~ -1+ ., data = .)%>%
  as.matrix(.)

## Validando X y Xp 
setdiff(names(X) ,names(Xp))
setdiff(names(Xp) ,names(X))
## Validando Z y Zp 
if(length(setdiff(colnames(Z) ,colnames(Zp)))>0){
  agregarZp  <- setdiff(colnames(Z) ,colnames(Zp))
  temp <- matrix(0, nrow = nrow(Zp),
                 ncol = length(agregarZp),
                 dimnames = list(1:nrow(Zp), agregarZp))
  
  Zp <- cbind(Zp, temp)  
}
if(length(setdiff(colnames(Zp) ,colnames(Z)))>0){
  agregarZ  <- setdiff(colnames(Zp) ,colnames(Z))
  temp <- matrix(0,nrow = nrow(Z),
                 ncol = length(agregarZ),
                 dimnames = list(1:nrow(Z), agregarZ))
  
  Z <- cbind(Z, temp)
  
}

xnames <-  intersect(names(Xp) ,names(X))
Znames <-  intersect(colnames(Zp) ,colnames(Z))

sample_data <- list(D = nrow(encuesta_df_agg),
                    P = ncol(Y),
                    K = ncol(X[,xnames]),
                    D1 = nrow(Xp),
                    Kz = ncol(Z),
                    Z = Z[,Znames],
                    Zp = Zp[,Znames],
                    y = Y,
                    X = X[,xnames]%>% as.matrix(),
                    Xp = Xp[,xnames] %>% as.matrix()
                    )
# STAN fit ----------------------------------------------------------------

#' # Draw from posterior distribution
#+ results='hide'


# fit <- stan("2. multiparamétricos/Multinomial7a.stan",            
#             data = sample_data)

fit_mcmc <- fit$sample(
  data = sample_data,
  seed = 123,
  chains = 4,
  parallel_chains = 4
)


#--- Exporting Bayesian Multilevel Model Results ---#

fit_mcmc$save_object( 
        file = "MrPDepartamental/ARG/2020/Data/fit_multinomial.rds")



draws <- fit_mcmc$draws()
draws <- as_draws_df(draws)

betas <- draws %>% 
  select(matches("beta")) %>% 
  colMeans() %>% 
  matrix(., ncol = ncol(X[,xnames]),
         nrow = ncol(Y)-1,
         byrow = FALSE)

efecto_aleatorio <- draws %>% 
  select(matches("^u")) %>% 
  colMeans() %>% 
  matrix(., ncol = ncol(Z),
         nrow = ncol(Y)-1,
         byrow = FALSE) %>% t()




theta_fit <- draws %>% select(matches("theta\\[")) %>% 
  colMeans() %>% 
  matrix(., nrow = nrow(encuesta_df_agg),
         ncol = ncol(Y), byrow = FALSE,
         dimnames = list(1:nrow(Y),paste0("theta_",1:ncol(Y))) 
           ) %>% 
  data.frame()

summary(rowSums(theta_fit))

cbind(encuesta_df_agg,theta_fit) %>% View()

temp <- cbind(encuesta_df_agg,theta_fit) %>% 
  select(matches("theta|empleo"))

temp %>% mutate_at(vars(matches("theta")),
                    ~cut(., seq(0, 1, len = 50),
                         include.lowest = TRUE)) %>%
  group_by(e1) %>% summarise(tn = sum(empleo_1)) %>% 
ggplot(data = .,aes(x = e1, y=tn/sum(tn))) + 
  geom_point()





# extract the posterior draws
draws <- as_draws_df(fit22.1)

# wrangle
draws %>% 
  pivot_longer(starts_with("b_")) %>% 
  mutate(name = str_remove(name, "b_")) %>% 
  mutate(lambda    = str_extract(name, "[2-4]+") %>% str_c("lambda==", .),
         parameter = case_when(str_detect(name, "Intercept") ~ "beta[0]",
                               str_detect(name, "X1")        ~ "beta[1]",
                               str_detect(name, "X2")        ~ "beta[2]")) %>% 
  
  # plot
  ggplot(aes(x = value, y = 0)) +
  stat_histinterval(point_interval = mode_hdi, .width = .95,
                    fill = pl[4], slab_ARGor = pl[3], ARGor = pl[1], point_ARGor = pl[2],
                    normalize = "panels") +
  scale_y_continuous(NULL, breaks = NULL) +
  xlab("marginal posterior") +
  facet_grid(lambda ~ parameter, labeller = label_parsed, scales = "free_x")





# Assessment of the model -------------------------------------------------

# Graphical posterior predictive checks -----------------------------------
## Regresando a la escala original los ingresos

# new_encuesta <- encuesta_mrp %>% inner_join(statelevel_predictors_df, by = "depto")
# y_sam<- new_encuesta$ingreso
# y_pred <- predict(fit, newdata = new_encuesta)

# new_encuesta %<>% mutate(y_pred = y_pred,
#                          ingresolp = y_sam)

# names_cov <-
#   grep(
#     pattern =  "^(n|pobreza|ingreso|li|lp|fep|y_pred)",
#     x = names(new_encuesta),
#     invert = TRUE,
#     value = TRUE
#   )


# new_encuesta %>% 
#   summarise(
#     media_ingresolp = mean(ingresolp),
#     media_ingreso_pred = mean(y_pred),
#     dif = (media_ingresolp-media_ingreso_pred)/media_ingresolp
#   )

# ### total
# map(names_cov, ~new_encuesta %>% group_by_at(all_of(.x)) %>% 
#       summarise(
#         media_ingresolp = mean(ingresolp),
#         media_ingreso_pred = mean(y_pred),
#         dif = (media_ingresolp-media_ingreso_pred)/media_ingresolp
#       ))

# ggplot(data.frame(datos = c(y_sam, y_pred),
#                   repe = gl(2, length(y_sam), 
#                             labels = c("muestra", "predicción"))), 
#        aes(x = datos, fill = repe, alpha = 0.1)) +
#   geom_density() 



