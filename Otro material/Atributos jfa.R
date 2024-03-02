######################################################
#     Muestreo por categoría en Auditoria ---- jfa   #
######################################################



library(highcharter)
#################################
#       Test te muestreo        #
#################################

options(scipen=999)


#################
#   Libraries   #
#################

library(dplyr)
library(jfa)
library(openxlsx)
library(ggplot2)

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

df <- as.data.frame(df)

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


#################################################################
#################################################################

set.seed(123) # Para asegurar reproducibilidad

# Generamos un DataFrame con 10,000 filas y las columnas especificadas
n <- 10000

df <- data.frame(
  Id = 1:n,
  Monto = rgamma(n, shape = 2, scale = 100), # Distribución con asimetría
  País = rep(c("País A", "País B", "País C", "País D", "País E"), times = c(5000, 1500, 1500, 1500, 500)),
  Resultado = sample(c(rep("Adecuado", 7000), rep("Rechazado", 3000)), n, replace = FALSE)
)

class(df)

# Visualizar los datos

frecuencias <- df %>%
  count(Resultado) %>%
  mutate(Porcentaje = n / sum(n) * 100) %>% as_tibble()


class(frecuencias)


hc <- hchart(frecuencias, type = "bar", hcaes(x = Resultado, y = Porcentaje))
hc

################################################################################################################
################################################################################################################
# install.packages("palmerpenguins")
x <- c(rnorm(10000), rnorm(1000, 4, 0.5))

class(x)

hchart(x, name = "data", color = "#17b8b6") 

library(forecast)

airforecast <- forecast(auto.arima(AirPassengers), level = 95)

hchart(airforecast)


mpg

class(mpg)

mpgman2 <- count(mpg, manufacturer, year)
mpgman2

class(mpg)

hchart(
  mpgman2, 
  "bar",
  hcaes(x = manufacturer, y = n, group = year),
  color = c("#7CB5EC", "#F7A35C"),
  name = c("Year 1999", "Year 2008"),
  showInLegend = c(TRUE, FALSE) # only show the first one in the legend
)



# Ejemplo simple para verificar la funcionalidad de highcharter
hchart(data.frame(x = c("A", "B"), y = c(10, 20)), "bar", hcaes(x = x, y = y))
