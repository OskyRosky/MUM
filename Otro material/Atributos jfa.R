######################################################
#     Muestreo por categoría en Auditoria ---- jfa   #
######################################################

install.packages("openxlsx")

#################################
#       Test te muestreo        #
#################################

options(scipen=999)


#################
#   Libraries   #
#################

library(jfa)
library(openxlsx)

#######################
#   Generar la data   #
#######################

set.seed(123) # Para asegurar reproducibilidad

# Generamos un DataFrame con 10,000 filas y las columnas especificadas
n <- 10000
df <- data.frame(
  Id = 1:n,
  Monto = rgamma(n, shape = 2, scale = 100), # Distribución con asimetría
  País = rep(c("País A", "País B", "País C", "País D", "País E"), times = c(5000, 1500, 1500, 1500, 500)),
  Resultado = sample(c(rep("Adecuado", 7000), rep("Rechazado", 3000)), n, replace = FALSE)
)

# Visualizamos las primeras filas del DataFrame
head(df,20)
tail(df,20)

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

# Exportamos el DataFrame a un archivo .xlsx

setwd("C:/Users/Oscar Centeno/Desktop/Oscar/CGR/2024/MUM/App/data")
write.xlsx(df, file = "Atributos.xlsx")
