############################
#          body            # 
############################


body <- dashboardBody( 
  tabItems(  
    
    
    #################
    #  Presentacion #   
    #################
    
    tabItem(tabName = "inicio",
            h1("Análisis de los ingresos nacionales", align = "center"),
            br(),
            h2(paste("Fecha de actualización: el",substr(Sys.Date(),9,10),"-",substr(Sys.Date(),6,7),"-",substr(Sys.Date(),1,4), ", a las",substr(Sys.time(),12,20))),
           
            br(),
            br(),
          
            box( imageOutput("picture.ingresos", height = "auto")) 
              
            
            )
    ,

    #################
    #   Definición  #   
    #################
    
    tabItem(tabName = "defi",
            h1("Definición de ciertos términos presente en el presente Dashboard.", align = "center"),
            br(),
            br(),
            box(reactableOutput("conceptos.1"))

            
            
            
    )
    ,
    
    
    ##############################
    #  Indicadores del ingreso   #
    ##############################
    
    tabItem(tabName = "alertas",
            h1(paste0("Indicadores del ingreo ", Ano_actual, "."), align = "center"),
            br(),
            br(),
            h2("Importante: los siguientes indicadores son referentes a los ingresos corrientes."),
            br(),
            br(),
            h2(paste0("Recaudación acumulada"),Ano_actual, "."),
            br(),
            fluidRow(
              box(
                title = paste0("Recaudación acumulada al ",Sys.Date()), status = "primary", 
                solidHeader = TRUE, 
                width = 12,
                valueBoxOutput("index1")
              )
              
            ),
            h2(paste0("Carga tributaria"),Ano_actual, "."),
            br(),
            fluidRow(
              box(
                title = paste0("Carga tributaria al ",Sys.Date()), status = "primary", 
                solidHeader = TRUE, 
                width = 12,
                valueBoxOutput("index2")
              )
              
            ),
            
            h2(paste0("Ejecución"),Ano_actual, "."),
            br(),
            
            fluidRow(
              box(
                title = paste0("Ejecución al ",Sys.Date()), status = "primary", 
                solidHeader = TRUE, 
                width = 12,
                valueBoxOutput("index3")
              )
              
            ),
            h2(paste0("Variación porcentual interanual."),Ano_actual, "."),
            br(),
            
            fluidRow(
              box(
                title = paste0("Variación al ",Sys.Date()), status = "primary", 
                solidHeader = TRUE, 
                width = 12,
                valueBoxOutput("index4")
              )
              
            ),
            
            h2(paste0("Variación porcentual acumulada."),Ano_actual, "."),
            br(),
            
            fluidRow(
              box(
                title = paste0("Variación porcentual acumulada al ",Sys.Date()), status = "primary", 
                solidHeader = TRUE, 
                width = 12,
                valueBoxOutput("index5")
              )
              
            ),
            
            br(),
            h2(paste0("Variación acumulada al mes."),Ano_actual, "."),
            br(),
            fluidRow(
              box(
                title = paste0("Variación acumulada al mes ",Sys.Date()), status = "primary",
                solidHeader = TRUE,
                width = 12,
                valueBoxOutput("index6")
              )
              
            ),
            br(),
            br(),
            h2("Para ver los valores en términos compatirvos en el tiempo, ir a la sección de evolución mensual.")
            
            
            
            
            
    ),
            
    ###########################################
    #      Evolucion de los presupuestos      #
    ###########################################
    
    ####################
    #        Anual     #
    ####################
    
    tabItem(tabName = "HC1",
            h1("Evolución anual del presupuesto: actual, inicial, ajustado e ingresos", align = "center"),
            br(),
            br(),
            h2("Importante: no se poseen los datos del presupuesto ajustado, de forma anual, del 2007 al 2012, por lo que se este se visualiza a partir
               del 2013 en adelante."),
            fluidRow(
              box(
                solidHeader = TRUE, 
                width = 12,
                highchartOutput("HCIA_1",height = "500px")
              )
            ),
            br(),
            br(),

            
            
            ),
    
    
    ####################
    #      Mensual     #
    ####################
    
    tabItem(tabName = "HC2",
            h1("Evolución del ingreso mensual.", align = "center"), 
            br(),

            h2("Ingresos mensuales"),
            br(),
            br(),
            fluidRow(
              box(
                solidHeader = TRUE, 
                width = 12,
                highchartOutput("HCIM_1",height = "500px")
              )
            ),
            h2("Variaciones"),
            br(),
            br(),
            fluidRow(
              box(
                solidHeader = TRUE, 
                width = 12,
                highchartOutput("HCIM_2",height = "500px")
              )
            ),
            br(),
            br(),
            h2("Recaudación acumulada"),
            br(),
            fluidRow(
              box(
                solidHeader = TRUE, 
                width = 12,
                highchartOutput("HCIM_3",height = "500px")
              )
            ),
            br(),
            br(),
            h2("Carga tributaria"),
            br(),
            fluidRow(
              box(
                solidHeader = TRUE, 
                width = 12,
                highchartOutput("HCIM_4",height = "500px")
              )
            ),
            br(),
            br(),
            h2("Ejecución  mensual"),
            br(),
            fluidRow(
              box(
                solidHeader = TRUE, 
                width = 12,
                highchartOutput("HCIM_5",height = "500px")
              )
            )
    ),
    
    ###################################
    #           Impuestos             # 
    ###################################
    
    tabItem(tabName = "HC5",
            h1("Prueba de las elecciones de los impuestos", align = "left"),
            br(),
            h2(" Selecione los impuestos a partir del impuesto : 'Impuestos a los ingresos y Utilidades-ISR' "),
            br(),

            varSelectInput("variable", "Variables:", Impuestos),
            br(),
          
            
 #           actionButton('select_1', 'Seleccionar'),
            
    #       reactableOutput("datos3"),
            
            h3("Visualización del impuesto en el tiempo"),
            br(),
            br(),

            fluidRow(
              box(
                solidHeader = TRUE, 
                width = 12,
                highchartOutput("HC_I",height = "500px")
              )
            ),
 
            h3("Visualización de la variación del impuesto en el tiempo"),
 
              fluidRow(
                box(
                  solidHeader = TRUE, 
                  width = 12,
                  highchartOutput("HC_VI",height = "500px")
                )
              )
 
               
    ),
    
    ###################################
    #           Avanzado              # 
    ###################################
    
    tabItem(tabName = "HC4",
            h1("Prueba de las elecciones múltiples", align = "center"),
            
            selectInput(
              inputId = "f_clase",
              label = "Clase:",
              choices = c(unique(as.character(Ingresos_mensual$clase))),
              selected = "INGRESOS CORRIENTES"
            ),
            
            selectInput(
              inputId = "f_subclase",
              label = "Subclase:",
              choices = c("Todo",unique(as.character(Ingresos_mensual$subclase))),
              selected = "Todo"
              
            ),
            
            selectInput(
              inputId = "f_grupo",
              label = "Grupo:",
              choices = c("Todo",unique(as.character(Ingresos_mensual$grupo))),
              selected = "Todo"
              
            ),
            
            selectInput(
              inputId = "f_subgrupo",
              label = "Subgrupo:",
              choices = c("Todo",unique(as.character(Ingresos_mensual$subgrupo))),
              selected = "Todo"
              
            ),
            
            selectInput(
              inputId = "f_partida",
              label = "Partida:",
              choices = c("Todo",unique(as.character(Ingresos_mensual$partida))),
              selected = "Todo"
              
            ),
            
            selectInput(
              inputId = "f_subpartida",
              label = "Subpartida:",
              choices = c("Todo",unique(as.character(Ingresos_mensual$subpartida))),
              selected = "Todo"
              
            ),
            
            selectInput(
              inputId = "f_renglon",
              label = "Renglon:",
              choices = c("Todo",unique(as.character(Ingresos_mensual$renglon))),
              selected = "Todo"
              
            ),
            
            selectInput(
              inputId = "f_subrenglon",
              label = "Subrenglon:",
              choices = c("Todo",unique(as.character(Ingresos_mensual$subrenglon))),
              selected = "Todo"
              
            ),
            
            br(),
            
            actionButton('select_2', 'Seleccionar'),
            
            h3("Visualización del gráfico de evolución"), 
            
            fluidRow(
              box(
                solidHeader = TRUE, 
                width = 12,
                highchartOutput("HC_P",height = "500px")
              )
            ),
            
            h3("Visualización de la variación en el tiempo"),
            
            fluidRow(
              box(
                solidHeader = TRUE, 
                width = 12,
                highchartOutput("HC_VP",height = "500px")
              )
            ),

            
    ),
    
   

    #################################
    #         Proyecciones          #
    #################################
    
    #######################
    #         Total       #
    #######################
    
    tabItem(tabName = "pronos1",
            h1(paste0("Proyeccion del ingreso mensual total", "."), align = "center"),
            br(),
            br(),
            sliderInput("horizonte.1", "Meses a pronosticar:",
                        min = 1, max = 24, value = 12),
            br(),
            h3("Visualización y proyección mensuales del ingreso total."),
            fluidRow(
              box(
                solidHeader = TRUE, 
                width = 12,
                highchartOutput("forecast1")
              )
            ),
            br(),
            h3("Estadísticos de bondad y de ajuste"),
            fluidRow(
              box(
                solidHeader = TRUE, 
                width = 12,
                reactableOutput("GoF.1")  
              )
            ),
            
            br(),
            h3("Tabla con las cifras pronóstico,"),
            br(),
            h4("ARIMA"),
            reactableOutput("tabla.forcast.1.1"),
            br(),
            h4("ETS"),
            reactableOutput("tabla.forcast.1.2"),
            br(),
            h4("AUTO.ARIMA"),
            reactableOutput("tabla.forcast.1.3"),
            br(),
            h4("Regresión"),
            reactableOutput("tabla.forcast.1.4")
            
            
            
    ),
    
 ##########################
 #   Según clasificador   #
 ##########################
#
#  tabItem(tabName = "pronos2",
#          h1(paste0("Proyeccion del ingreso mensual según clasificador" ,"."),align = "center"),
#          h2("Nivel de clasificador"),
#          selectInput("clasi.2_2", "Seleccione el nivel:",  
#                      list(`Tipo` = list("Clase",
#                                         "Subclase", 
#                                         "Grupo",
#                                         "Subgrupo",
#                                         "Partida",
#                                         "Subpartida",
#                                         "Renglón",
#                                         "Subrenglón"
#                      )
#                      )
#          ),
#           sliderInput("horizonte.2", "Meses a pronosticar:",
#                      min = 1, max = 24, value = 12),
#          br(),
#  
#          h3("Visualización mensuales del ingreso según clasificador."),
#          fluidRow(
#            box(
#              solidHeader = TRUE, 
#              width = 12,
#              highchartOutput("forecast2")
#            )
#          ),
#          br(),
#          h3("Estadísticos de bondad y de ajuste"),
#          fluidRow(
#            box(
#              solidHeader = TRUE, 
#              width = 12,
#              reactableOutput("GoF.2")  
#            )
#          ),
#          br(),
#          h3("Tabla con las cifras pronóstico,"),
#          br(),
#          h4("ARIMA"),
#          reactableOutput("tabla.forcast.2.1"),
#          br(),
#          h4("ETS"),
#          reactableOutput("tabla.forcast.2.2"),
#          br(),
#          h4("AUTO.ARIMA"),
#          reactableOutput("tabla.forcast.2.3"),
#          br(),
#          h4("Regresión"),
#          reactableOutput("tabla.forcast.2.4")
#          
#  ),
 
 ##########################
 #        Impuestos       #
 ##########################
 
 tabItem(tabName = "pronos3",
         h1(paste0("Proyeccion del ingreso mensual según Impuestos" ,"."),align = "center"),
         
         br(),
         h2(" Selecione los impuestos a partir del impuesto : 'Impuestos a los ingresos y Utilidades-ISR' "),
         br(),
         
         varSelectInput("variable2", "Variables:", Impuestos),
         
         sliderInput("horizonte.3", "Meses a pronosticar:",
                     min = 1, max = 24, value = 12),
         br(),
         
         h3("Visualización mensuales del ingreso por impuesto."),
         fluidRow(
           box(
             solidHeader = TRUE, 
             width = 12,
             highchartOutput("forecast3")
           )
         ),
         h3("Estadísticos de bondad y de ajuste"),
         fluidRow(
           box(
             solidHeader = TRUE, 
             width = 12,
             reactableOutput("GoF.3")  
           )
         ),
         br(),
         br(),
         h3("Tabla con las cifras pronóstico,"),
         br(),
         h4("ARIMA"),
         reactableOutput("tabla.forcast.3.1"),
         br(),
         h4("ETS"),
         reactableOutput("tabla.forcast.3.2"),
         br(),
         h4("AUTO.ARIMA"),
         reactableOutput("tabla.forcast.3.3"),
         br(),
         h4("Regresión"),
         reactableOutput("tabla.forcast.3.4")
         
 ),
 
 ##########################
 #         Avanzado       #
 ##########################
 
 tabItem(tabName = "pronos4",
         h1(paste0("Proyeccion del ingreso mensual avanzado" ,"."),align = "center"),
         
         selectInput(
           inputId = "f_clase_2",
           label = "Clase:",
           choices = c(unique(as.character(Ingresos_mensual$clase))),
           selected = "INGRESOS CORRIENTES"
         ),
         
         selectInput(
           inputId = "f_subclase_2",
           label = "Subclase:",
           choices = c("Todo",unique(as.character(Ingresos_mensual$subclase))),
           selected = "Todo"
           
         ),
         
         selectInput(
           inputId = "f_grupo_2",
           label = "Grupo:",
           choices = c("Todo",unique(as.character(Ingresos_mensual$grupo))),
           selected = "Todo"
           
         ),
         
         selectInput(
           inputId = "f_subgrupo_2",
           label = "Subgrupo:",
           choices = c("Todo",unique(as.character(Ingresos_mensual$subgrupo))),
           selected = "Todo"
           
         ),
         
         selectInput(
           inputId = "f_partida_2",
           label = "Partida:",
           choices = c("Todo",unique(as.character(Ingresos_mensual$partida))),
           selected = "Todo"
           
         ),
         
         selectInput(
           inputId = "f_subpartida_2",
           label = "Subpartida:",
           choices = c("Todo",unique(as.character(Ingresos_mensual$subpartida))),
           selected = "Todo"
           
         ),
         
         selectInput(
           inputId = "f_renglon_2",
           label = "Renglon:",
           choices = c("Todo",unique(as.character(Ingresos_mensual$renglon))),
           selected = "Todo"
           
         ),
         
         selectInput(
           inputId = "f_subrenglon_2",
           label = "Subrenglon:",
           choices = c("Todo",unique(as.character(Ingresos_mensual$subrenglon))),
           selected = "Todo"
           
         ),
         
         br(),
         
         actionButton('select_3', 'Seleccionar'),
         
         sliderInput("horizonte.4", "Meses a pronosticar:",
                     min = 1, max = 24, value = 12),
         br(),
         
         h3("Visualización mensuales del ingreso"),
         fluidRow(
           box(
             solidHeader = TRUE, 
             width = 12,
             highchartOutput("forecast4")
           )
         ),
         h3("Estadísticos de bondad y de ajuste"),
         fluidRow(
           box(
             solidHeader = TRUE, 
             width = 12,
             reactableOutput("GoF.4")  
           )
         ),
         br(),
         h3("Tabla con las cifras pronóstico,"),
         br(),
         h4("ARIMA"),
         reactableOutput("tabla.forcast.4.1"),
         br(),
         h4("ETS"),
         reactableOutput("tabla.forcast.4.2"),
         br(),
         h4("AUTO.ARIMA"),
         reactableOutput("tabla.forcast.4.3"),
         br(),
         h4("Regresión"),
         reactableOutput("tabla.forcast.4.4")
         
 ),
 
  ############################
  #      Otros análisis      #
  ############################
  
    tabItem(tabName = "SI1",
            h1(paste0("Estacionalidad general"), align = "center"),
            br(),
            br(),
            h2("Las siguientes estacionalidades se llevan a cabo mediante el cálculo del ingresos general o total."),
            br(),
            br(),
            h3(paste0("Estacionalidad mensual - parámetro")),
            br(),
            fluidRow(
              box(
                solidHeader = TRUE, 
                width = 12,
                highchartOutput("Estacionalidad_1")
              )
            ),
            
            
            h3(paste0("Estacionalidad acumulada - parámetro"), align = "center"),
            br(),
            
            fluidRow(
              box(
                solidHeader = TRUE, 
                width = 12,
                highchartOutput("Estacionalidad_2")
              )
            ),
            
            h3(paste0("Estacionalidad mensual GC")),
            br(),
            fluidRow(
              box(
                solidHeader = TRUE, 
                width = 12,
                highchartOutput("Estacionalidad_3")
              )
            )
            
            
  ),
  
               tabItem(tabName = "SI2",
                       h1(paste0("Estacional según el Ingreso tributario"), align = "center"),
                       
                       h2(paste0("Estacionalidad mensual del Ingreso Tributario - parámetro"), align = "center"),
                       br(),
                       fluidRow(
                         box(
                           solidHeader = TRUE, 
                           width = 12,
                           highchartOutput("Estacionalidad_IT_1")
                         )
                       ),
                       
                       h2(paste0("Estacionalidad acumulada del Ingreso Tributario - parámetro"), align = "center"),
                       br(),
                       
                       fluidRow(
                         box(
                           solidHeader = TRUE, 
                           width = 12,
                           highchartOutput("Estacionalidad_IT_2")
                         )
                       ),
                       
                       h2(paste0("Estacionalidad mensual del Ingreso Tributario - GC"), align = "center"),
                       br(),
                       fluidRow(
                         box(
                           solidHeader = TRUE, 
                           width = 12,
                           highchartOutput("Estacionalidad_IT_3")
                         )
                       )
                       
                       
                 ),
             

             tabItem(tabName = "SI3",
                     h1(paste0("Estacional según impuestos"), align = "center"),
                     br(),
                     h2(" Selecione los impuestos a partir del impuesto : 'Impuestos a los ingresos y Utilidades-ISR' "),
                     br(),
                     varSelectInput("variable5", "Variables:", Impuestos),
                     
                     h2(paste0("Evolución del impuesto"), align = "center"),
                     
                     fluidRow(
                       box(
                         solidHeader = TRUE, 
                         width = 12,
                         highchartOutput("Estacionalidad_Impuestos_1")
                       )
                     ),
                     h2(paste0("Estacionalidad mensual de los impuestos - parámetro"), align = "center"),
                     br(),
                     
                     fluidRow(
                       box(
                         solidHeader = TRUE, 
                         width = 12,
                         highchartOutput("Estacionalidad_Impuestos_2")
                       )
                     ),
                     
                     h2(paste0("Estacionalidad acumuladadel de los impuestos - parámetro"), align = "center"),
                     br(),
                     
                     fluidRow(
                       box(
                         solidHeader = TRUE, 
                         width = 12,
                         highchartOutput("Estacionalidad_Impuestos_3")
                       )
                     ),
                     
                     
             ),

tabItem(tabName = "download",
        h1("Descargar achivo de datos.", align = "center"),
        br(),
        br(),
        h2("Descarga archivo de los ingresos."),
        actionButton("show2", "Descargar archivo"),
        br(),
        br(),
        h2("Descarga información de los impuestos."),
        actionButton("show1", "Descargar archivo")
      
        
)


 )  
)
