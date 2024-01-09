###################################
#      Archivo de Financiera      #
###################################

options(scipen=999)

###############
#  Librerías  #
###############

library(jfa)
library(readxl)
library(dplyr)

################
#  Directorio  #
################

setwd("C:/Users/Oscar Centeno/Desktop/Oscar/CGR/2023/Muestreo Auditoría/Otro material")

#####################
#  Impportar datos  #
#####################

data <- read_excel("Proto_MM.xlsx")

###################################
# Stage 1: Planning               #
###################################

# Planning function
# https://koenderks.github.io/jfa/articles/audit-sampling.html

# planning(
#  materiality = NULL,
#  min.precision = NULL,
#  expected = 0,
#  likelihood = c("poisson", "binomial", "hypergeometric"),
#  conf.level = 0.95,
#  N.units = NULL,
#  by = 1,
#  max = 5000,
#  prior = FALSE
#  )

stage1 <- planning(materiality = 0.05, 
                   expected = 0.01,
                   likelihood = "poisson", 
                   conf.level = 0.95
)


summary(stage1)
stage1$n
class(stage1$n)
class(4)


sample_size <- data.frame(`Muestra` = stage1$n)
sample_size

###################################
#       Stage 2: Selection        #
###################################

# selection(
#   data,
#   size,
#   units = c("items", "values"),
#   method = c("interval", "cell", "random", "sieve"),
#   values = NULL,
#   order = NULL,
#   decreasing = FALSE,
#   randomize = FALSE,
#   replace = FALSE,
#   start = 1
# )

stage2 <- selection(
  data = data, 
  size = stage1,      #### Stage1 previous defined 
  units = "values", 
  values = "Contra presupuesto p.importe verific.en...6",   #### Column from  data
  method = "random", start = 2
)
summary(stage2)

# sample <- stage2$sample


###################################
#       Stage 3: Execution        #
###################################

sample <- stage2[["sample"]]  # With the sample, it is request the documents review

sample1 <- sample %>% dplyr::select(
  "Número de documento precedente",
  "Contra presupuesto p.importe verific.en...6",
  "Contra presupuesto p.importe verific.en...7"
  
)


#####################################
#       # Stage 4: Evaluation       #
#####################################


stage4 <- evaluation(
                      materiality = 0.03, 
                      method = "stringer",
                      conf.level = 0.95, 
                      data = sample1,
                      values = "Contra presupuesto p.importe verific.en...6", 
                      values.audit = "Contra presupuesto p.importe verific.en...7"
)

summary(stage4)



