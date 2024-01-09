####################################
#  Parámetros: datos + dashboard   #
####################################

#############
#  Millones #
#############

Millones <- 1000000

################################
#  Delimitación años análisis  #
################################

Anos_analisis <- 2007

####################
#  Años referencia #
####################

Ano_actual    <- as.numeric(substr(Sys.Date(),1,4))
Ano_pasado_1  <- as.numeric(substr(Sys.Date(),1,4))-1  
Ano_pasado_2  <- as.numeric(substr(Sys.Date(),1,4))-2

####################
#  Mes  referencia #
####################

mes_actual <- as.numeric(substr(Sys.Date(),6,7))


####################
#  Mes referencia  #
####################

Mes_actual <- substr(Sys.Date(),6,7)

#######################
#  Faltante de meses  #
#######################

Faltante_mes <- 12 - as.numeric(substr(Sys.Date(),6,7))






