######################################################
#     Muestreo por categoría en Auditoria ---- jfa   #
######################################################

#################################
#       Test te muestreo        #
#################################

options(scipen=999)


#################
#   Libraries   #
#################

library(jfa)


#######################
#   Generar la data   #
#######################

set.seed(123456) # Para asegurar reproducibilidad

# Generamos un DataFrame con 1000 filas y las columnas especificadas
df <- data.frame(
  Id = 1:1000,
  Monto = rgamma(1000, shape = 2, scale = 100), # Distribución con asimetría
  País = sample(c("País A", "País B", "País C", "País D", "País E"), 1000, replace = TRUE),
  Resultado = sample(c("Adecuado", "Rechazado"), 1000, replace = TRUE)
)

# Visualizamos las primeras filas del DataFrame
head(df,20)

##################################
#  Calculo de tamaño de muestra  #
##################################

Muestra_atri <- planning(materiality = 0.2,
                      min.precision = NULL,
                      expected = 0.1,
                      likelihood = c("poisson"),
                      conf.level = 0.95,
                      N.units = NULL,
                      by = 1,
                      max = 5000,
                      prior = FALSE
                      
)

Muestra <- data.frame(`Muestra` = Muestra_atri$n)
Muestra
