**Repository summary**

1. Intro



2. Tech Stack


3. Features


4. Process


5. Learning


6. Improvement


7. Running the Project


8. More

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# MUM
## Muestreo de Unidades Monetarias

Este proyecto implementa una aplicación Shiny para realizar muestreo por unidades monetarias (MUM), una técnica de muestreo utilizada en auditorías para evaluar la importancia relativa de las transacciones y balances.

<table>
  <tr>
    <td><img src="/Recoursos/Shinylogo.png" alt="LogoShiny" style="width: 150px;"/></td>
    <td><img src="/Recoursos/rshiny.png" alt="LogoShiny2" style="width: 150px;"/></td>
  </tr>
</table>

## Características

La aplicación permite:

- Cargar conjuntos de datos y seleccionar variables de interés.
- Especificar parámetros de muestreo como el error tolerable, el error esperado y el nivel de confianza.
- Realizar el cálculo del tamaño de la muestra y seleccionar las unidades de muestreo según su importancia relativa.
- Visualizar y descargar la muestra seleccionada.
- Comparar las distribuciones de los datos originales y la muestra seleccionada.

## Cómo empezar

Para utilizar la aplicación, sigue estos pasos:

1. Clone el repositorio a su máquina local utilizando `git clone <url-del-repositorio>`.
2. Abra el proyecto en RStudio y asegúrese de tener instalados todos los paquetes necesarios listados en 'requirements.txt'.
3. Ejecute la aplicación desde el archivo `app.R` haciendo clic en 'Run App'.

## Requisitos

Asegúrese de tener instaladas las siguientes librerías de R:

- shiny
- shinyWidgets
- reactable
- highcharter
- (otros paquetes utilizados en tu proyecto)

Puede instalarlos utilizando `install.packages("nombre_del_paquete")`.

## Código principal

La app recibdo todos los elementos del shiny en el archivo AppAuditSample.R

```r
#####################################################################
#                                                                    #
#                                                                    #
#                     Muestreo financiero                            #
#                                                                    #   
#                                                                    #
######################################################################

#######################
#  Opciones generales # 
#######################

options(shiny.maxRequestSize = 100 * 1024 * 1024)
options(encoding="utf-8") 
options(scipen=999)

################
#  Directorio  #
################


setwd("C:/Users/Oscar Centeno/Desktop/Oscar/CGR/2024/MUM/App/Scripts_dashboard")

#################
#   Librerias   # 
#################

suppressWarnings(source("Librerias.R"))

###################################
#     Creacion del dashboard      # 
###################################
############################
#          header          # 
############################

suppressWarnings(source("header.R"))

############################
#          sidebar         # 
############################

suppressWarnings(source("sider.R"))


############################
#          body            # 
############################

suppressWarnings(source("body.R"))

##########################################################
#                Contenido del ui                        # 
##########################################################

suppressWarnings(source("ui.R"))

##########################################################
#                Contenido del server                    # 
##########################################################

suppressWarnings(source("server.R"))


###################################
#     Cargar la App de Shiny      # 
###################################

#require(shiny)

x <- system("ipconfig", intern=TRUE)
z <- x[grep("IPv4", x)]
ip <- gsub(".*? ([[:digit:]])", "\\1", z)

print(paste0("the Shiny Web application runs on: http://", ip, ":7701/"))

#runApp( "AppAuditSample.R", host = "localhost", port = 80, launch.browser = FALSE, display.mode = "fullscreen" ) #, port = 7704 , host = ip
                                                             # )
runApp(list(ui=ui, server=server),  host = getOption("shiny.host", "127.0.0.2"), port = 1001,launch.browser = TRUE)

###########
## Run App  
###########

#shinyApp(ui, server)
```

## Contribuir

Si desea contribuir al proyecto, por favor:

1. Haga Fork del repositorio.
2. Cree una nueva rama para sus características o correcciones (`git checkout -b feature/AmazingFeature`).
3. Realice commit de sus cambios (`git commit -m 'Añadida una increíble característica'`).
4. Haga Push a la rama (`git push origin feature/AmazingFeature`).
5. Abra una Pull Request.

## Sobre el proyecto

Este se divide en lo que sería la intoducción, estadísticas descriptivas, el muestreo mum, muestreo les y la evaluación de la muestra. 

1.  Introducción de la App
![MUMDashboard1](/Recoursos/MUM1.png)

2. Análisis descriptivos de los datos
![MUMDashboard2](/Recoursos/MUM2.png)

3. Anális del MUM
![MUMDashboard3](/Recoursos/MUM3.png)

4. Evaluación de la muestra auditada
![MUMDashboard4](/Recoursos/MUM4.png)

## Licencia

Este proyecto está bajo la Licencia MIT. Vea el archivo `LICENSE` para más detalles.

## Contacto

Su nombre - [@su_twitter](https://twitter.com/su_twitter)

![ChatGPT](https://img.shields.io/badge/chatGPT-74aa9c?style=for-the-badge&logo=openai&logoColor=white)
![R](https://img.shields.io/badge/r-%23276DC3.svg?style=for-the-badge&logo=r&logoColor=white)
![Python](https://img.shields.io/badge/python-3670A0?style=for-the-badge&logo=python&logoColor=ffdd54)
![PyTorch](https://img.shields.io/badge/PyTorch-%23EE4C2C.svg?style=for-the-badge&logo=PyTorch&logoColor=white)
![Apache](https://img.shields.io/badge/apache-%23D42029.svg?style=for-the-badge&logo=apache&logoColor=white)

Link del proyecto: [https://github.com/su_usuario/su_repositorio](https://github.com/su_usuario/su_repositorio)
**
