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
set.seed(Sys.time())

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



