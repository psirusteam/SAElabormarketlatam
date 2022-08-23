source("MrPDepartamental/ARG/2020/R codes/6TablasyMapas.R")
source("MrPDepartamental/BOL/2020/R codes/6TablasyMapas.R")
source("MrPDepartamental/BRA/2020/R codes/6TablasyMapas.R")
#source("MrPDepartamental/CHL/2020/R codes/6TablasyMapas.R")*
#source("MrPDepartamental/COL/2020/R codes/6TablasyMapas.R")*
source("MrPDepartamental/ECU/2020/R codes/6TablasyMapas.R")
source("MrPDepartamental/CRI/2020/R codes/6TablasyMapas.R")
source("MrPDepartamental/DOM/2020/R codes/6TablasyMapas.R")
source("MrPDepartamental/MEX/2020/R codes/6TablasyMapas.R")
source("MrPDepartamental/PAN/2019/R codes/6TablasyMapas.R")
source("MrPDepartamental/PRY/2020/R codes/6TablasyMapas.R")
source("MrPDepartamental/PRY2/2020/R codes/6TablasyMapas.R")
source("MrPDepartamental/SLV/2020/R codes/6TablasyMapas.R")
source("MrPDepartamental/URY/2020/R codes/6TablasyMapas.R")
source("MrPDepartamental/NIC/2014/R codes/6TablasyMapas.R")

# Andres 
source("MrPDepartamental/CHL/2020/R codes/3MultilevelARM_Multinomial.R")
source("MrPDepartamental/COL/2020/R codes/3MultilevelARM_Multinomial.R")

# source("MrPDepartamental/CRI/2020/R codes/3MultilevelARM_Multinomial.R")
source("MrPDepartamental/PER/2020/R codes/3MultilevelARM_Multinomial.R")

# source("MrPDepartamental/DOM/2020/R codes/3MultilevelARM_Multinomial.R")
 source("MrPDepartamental/ECU/2020/R codes/3MultilevelARM_Multinomial.R")
# Stalyn 
#source("MrPDepartamental/GTM/2014/R codes/3MultilevelARM_Multinomial.R")
#source("MrPDepartamental/HND/2019/R codes/3MultilevelARM_Multinomial.R")
#source("MrPDepartamental/PAN/2019/R codes/3MultilevelARM_Multinomial.R")
#source("MrPDepartamental/PRY/2020/R codes/3MultilevelARM_Multinomial.R")
# Gabriel
#source("MrPDepartamental/PRY2/2020/R codes/3MultilevelARM_Multinomial.R")
#source("MrPDepartamental/SLV/2020/R codes/3MultilevelARM_Multinomial.R")
#source("MrPDepartamental/URY/2020/R codes/3MultilevelARM_Multinomial.R")
#source("MrPDepartamental/MEX/2020/R codes/3MultilevelARM_Multinomial.R")
## Validando X y Xp 
setdiff(names(X) ,names(Xp))
setdiff(names(Xp) ,names(X))

fit_mcmc$save_object(
  file = "MrPDepartamental/COL/2020/Data/fit_multinomial.rds")
