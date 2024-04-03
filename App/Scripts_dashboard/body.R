############################
#          body            # 
############################


body <- dashboardBody( 
  tabItems(    
    
    #################################################################################
    #################################################################################
    #                                Introducción                                   #
    #################################################################################
    #################################################################################
    
    tabItem(tabName = "p1",
            
            h1("Guía de usuario para la aplicación de análisis de muestras en unidades monetarias.", align = "center"),
            br(),
            h2("Introducción", align = "left", style = "font-weight: bold; text-decoration: underline;"),
            br(),
            h4("Bienvenidos(as) a la aplicación especializada en el análisis de muestras para unidades monetarias.",align = "left"),
            br(),
            h4("Esta herramienta interactiva ha sido diseñada para facilitar el proceso de descripción, de muestreo y de evaluación de una muestra de unidades monetaria." ,align = "left"),
            br(),
            h2("¡Inicie utilizando la App!", align = "left", style = "font-weight: bold; text-decoration: underline;"),
            br(),
            h4("Para comenzar, en cada sección de la barra lateral izquierda, deberá:",align = "left"),
            br(),
            h4("Navegar",align = "left", style = "font-weight: bold"),
            h4("Navegue entre los diferentes módulos utilizando las pestañas dispuestas en la interfaz del usuario."),
            h4("Cargar Datos", align = "left", style = "font-weight: bold"), 
            h4("Utilice el botón gris en las apartados de Cargar  Datos para trabajar su conjunto de datos." ,align = "left"),
            h4("Analizar",align = "left", style = "font-weight: bold"),
            h4("Siga las instrucciones específicas en cada sección para realizar el análisis requerido."),
            br(),
            h2("Estructura de la Aplicación", align = "left", style = "font-weight: bold; text-decoration: underline;"),
            br(),
            h4("La aplicación se divide en tres módulos, cada uno enfocado en un aspecto crítico del muestreo en unidades monetarias:",align = "left"),
            br(),
            tags$ul(
              style = "list-style-type: disc; padding-left: 20px;",  # Estilo para la lista: disc es una viñeta redonda
              tags$li(h4("Análisis Descriptivo", align = "left")),
              tags$li(h4("Proceso de Muestreo", align = "left")),
              tags$li(h4("Evaluación de la Muestra", align = "left"))
            ),
            br(),
            h4("A continuación, exploramos en detalle cada uno de estos módulos.",align = "left"),
            br(),
            h2("Análisis Descriptivo", align = "left", style = "font-weight: bold; text-decoration: underline;"),
            br(),
            h4("Antes de abordar el proceso de muestreo (tamaño de muestra y selección de las unidades), es esencial comprender el conjunto de datos con el que se trabajará. En el módulo de Análisis Descriptivo, los usuarios podrán:",align = "left"),
            br(),
            tags$ul(
              style = "list-style-type: disc; padding-left: 20px;",  # Estilo para la lista: disc es una viñeta redonda
              tags$li(h4("Analizar las principales estadísticas descriptivas de la variable de interés.", align = "left")),
              tags$li(h4("Analizar la distribución de la variable de interés.", align = "left")),
              tags$li(h4("Según el análisis de la distribución de la variable de interés, tener una mejor perspectiva del ajuste de la función de distribución.", align = "left"))
            ),
            br(),
            h2("Proceso de Muestreo (MUM y LES)", align = "left", style = "font-weight: bold; text-decoration: underline;"),
            br(),
            h4("Tras un entendimiento  del conjunto de datos, se procede con la etapa del Muestreo. Esta etapa se conforma por:",align = "left"),
            br(),
            tags$ul(
              style = "list-style-type: disc; padding-left: 20px;",  # Estilo para la lista: disc es una viñeta redonda
              tags$li(h4("Determinar el tamaño de muestra.", align = "left")),
              tags$li(h4("Visualizar la selección de los casos (filas) según la determinación del tamaño de muestra.", align = "left")),
              tags$li(h4("Comparar las distribuciones entre los datos orginales y los obtenidos por la muestra.", align = "left")),
              tags$li(h4("Descargar los datos seleccionados en el proceso de muestreo (obtenidos por la muestra).", align = "left"))
            ),
            br(),
            h2("Muestreo por Atriburos", align = "left", style = "font-weight: bold; text-decoration: underline;"),
            br(),
            h4("Se explicar el obtener una muestra a partir de una variable de atributo. Esta etapa se conforma por:",align = "left"),
            br(),
            tags$ul(
              style = "list-style-type: disc; padding-left: 20px;",  # Estilo para la lista: disc es una viñeta redonda
              tags$li(h4("Determinar el tamaño de muestra.", align = "left")),
              tags$li(h4("Visualizar la selección de los casos (filas) según la determinación del tamaño de muestra.", align = "left")),
              tags$li(h4("Comparar los porcentajes de las categorías entre los datos orginales y los obtenidos por la muestra.", align = "left")),
              tags$li(h4("Descargar los datos seleccionados en el proceso de muestreo (obtenidos por la muestra).", align = "left"))
            ),
            br(),
            h2("Evaluación de la Muestra", align = "left", style = "font-weight: bold; text-decoration: underline;"),
            br(),
            h4("El último módulo es para obtener un contraste empírico referente al proceso de la auditoria de la muestra obtenida con anterioridad.",align = "left"),
            h4("El foco está en comparar los valores observados (selecionados por la muestra), contra los valores auditados (obtenidos o revisados en el proceso de auditoria).",align = "left"),
            h4("En esta sección de contraste empírico o comparación de datos:",align = "left"),
            br(),
            tags$ul(
              style = "list-style-type: disc; padding-left: 20px;",  # Estilo para la lista: disc es una viñeta redonda
              tags$li(h4("Se describen y comparan los resultados de una muestra antes seleccionados a nivel visual y los valores que presentan diferencias.", align = "left")),
              tags$li(h4("Se presentan indicadores de riego según la comparación entre los valores observados y auditados.", align = "left")),
              tags$li(h4("Se expone la selección de criterios o umbrales máximos tolerables en la evaluación de la muestra.", align = "left"))
            ),
            h2("Reportes de análisis", align = "left", style = "font-weight: bold; text-decoration: underline;"),
            br(),
            h4("Al final de cada sección se presenta un 'Descargar Reporte', lo cual permite obtener obtener los resultados del análisis es un formato más adecuado.",align = "left"),
            h4("Cada una de las secciones de análisis puede ser descargada en formato .docx.",align = "left"),
            br(),
            h2("Sobre la carga de datos", align = "left", style = "font-weight: bold; text-decoration: underline;"),
            br(),
            h4("Para cada uno de los módulos anteriores, deberá cargar un archivo de datos. La aplicación admite múltiples formatos incluyendo .xlsx, .txt y .csv. Tenga en cuenta que:",align = "left"),
            br(),
            tags$ul(
              style = "list-style-type: disc; padding-left: 20px;",  # Estilo para la lista: disc es una viñeta redonda
              tags$li(h4("Cada archivo cargado debe contener una sola tabla.", align = "left", style = "font-weight: bold")),
              tags$li(h4("Los datos deben estar limpios, listo para ser analizados.", align = "left", style = "font-weight: bold")),
              tags$li(h4("El peso máximo permitido por archivo es de 100 megabytes, asegurando así la fluidez y eficiencia de la aplicación.", align = "left", style = "font-weight: bold"))
            )
    ),
            
    
    #################################################################################
    #################################################################################
    #                                 Análisis descriptivo                          #
    #################################################################################
    #################################################################################
    
    tabItem(tabName = "p2",
            
            h1("Análisis Descriptivos", align = "center"),
            br(),
            h2("En este sección:", align = "left"),
            br(),
            h4("Se analiza de forma descriptiva el conjunto de datos.", align ="left"),
            br(),
            h4("Cargado los datos, usted podrá:"),
            br(),
            tags$ul(
              style = "list-style-type: disc; padding-left: 20px;",  # Estilo para la lista: disc es una viñeta redonda
              tags$li(h4("Conocer las principales estadísticas descriptivas de la variable seleccionada.", align = "left")),
              tags$li(h4("Visualizar la distribución de la variable seleccionada (densidad).", align = "left",)),
              tags$li(h4("Comprar la distribución de la variable seleccionada con respecto a una distribución de poisson o binomial.", align = "left")),
              tags$li(h4("Descargar los resultados generados en formato '.docx'.", align = "left"))
            ),
            br(),
            h4("Este último punto lo guiará en el proceso de la siguiente sección, en donde deberá seleccionar la distribución que aproximada más al conjunto de datos em la determinación del tamaño de muestra"),
            br(),
            
            h3("Cargar datos", align = "left"),
            # Input para cargar archivos
            # Input para cargar archivos
            fileInput("file1", "Importar datos",
                      accept = c(
                        ".csv",
                        ".txt",
                        ".xlsx",
                        "text/csv",
                        "text/plain",
                        "text/tab-separated-values",
                        "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
                      )),
            uiOutput("variable_select_1"),
            h4("IMPORTANTE: Debe seleccionar variables numéricas.",align = "left", style = "font-weight: bold"),
            uiOutput("negativesAlert_1"),
            br(),
            actionButton("start_analysis", "Iniciar Análisis Descriptivos", class = "btn-primary"),
            uiOutput("analysis_output"),
            br(),
            h3("Estadísticas descriptivas", align = "left"),
            br(),
            h4("Se presentan las principales para el análisis de la variable numérica seleccionada."),
            reactableOutput("stats"), # Salida para la tabla de estadísticas   ---->  reactableOutput()
            br(),
            h3("Análisis de distribuciones", align = "left"),
            br(),
            h4("Análisis de la densidad de la variable numérica seleccionada."),
            br(),                                                  # histogram1
            
            highchartOutput("histogram1"),
            # highchartOutput("histogramX"),
            
            # highchartOutput("histogram1"),

            h3("Comparación de Ajuste de Distribuciones", align = "left"),
            br(),
            h4("Por favor, guiarse según las siguientes gráficas de distrubución."),
            fluidRow(
              box(
                title = "Comparación de distribuciones",
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                width = 12,
                fluidRow(
                  column(
                    width = 6,
                    plotOutput("binomialPlot")
                  ),
                  column(
                    width = 6,
                    plotOutput("poissonPlot")
                  )
                )
              )
            ),
            h4("Si posee datos aglomerados o consistentes en todo el rango de posibles valores, es mejor que opte por un ajuste Binomial. Caso contrario,
               datos que están muy alejados de la aglomeración y presentan uno o varios valores extremos, es mejor que opte por un ajuste de distribución
               de poisson.", align = "left"),
            # Botón para descargar el reporte
            h3("Descargar Reporte", align = "left"),
            downloadButton("downloadReport1", "Descargar Reporte Análisis Descriptivo")
            

    ),
    
    
    ##############################################################################################################
    ##############################################################################################################
    #                                               Muestreo MUM                                                 #
    ##############################################################################################################
    ##############################################################################################################
    
    tabItem(tabName = "p3",
                       
                       
                       h1("Muestreo", align = "center"),
    
            br(),
            h2("En este sección:", align = "left"),
            br(),
            h4("Se lleva a cabo el proceso de muestreo: tamaño y selección de la unidades", align ="left"),
            br(),
            h4("Cargado los datos, usted podrá:"),
            br(),
            tags$ul(
              style = "list-style-type: disc; padding-left: 20px;",  # Estilo para la lista: disc es una viñeta redonda
              tags$li(h4("Calcular el tamaño de muestra.", align = "left")),
              tags$li(h4("Visualizar las unidades seleccionadas", align = "left",)),
              tags$li(h4("Comparar los datos cargados vs los datos obtenidos por la muestra.", align = "left")),
              tags$li(h4("Descargar los datos de la muestra en formato, ya sea .csv, .txt y .xlsx", align = "left")),
              tags$li(h4("Descargar los resultados generados en formato '.docx'.", align = "left"))
            ),
            br(), 
            
            h3("Cargar datos", align = "left"),
                       br(),
                       fileInput("file2", "Importar datos del muestreo",
                                 accept =  c(
                                   ".csv",
                                   ".txt",
                                   ".xlsx",
                                   "text/csv",
                                   "text/plain",
                                   "text/tab-separated-values",
                                   "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
                                 )),
                       br(),
                       uiOutput("variable_select_MUM"),
                       h4("IMPORTANTE: Debe seleccionar variables numéricas.",align = "left", style = "font-weight: bold"),
                       uiOutput("negativesAlertMuestreoMUM"),
                       br(),
            #           plotOutput("histogram2"),
                       h2("Muestreo: tamaño y selección", align = "left"),
                       br(),
                       h4("El proceso de muestreo consta de dos etapas: selección del tamaño de la muestra y la selección de las unidades",align = "left"),
                       h4("Se aborda primeramente la determinación del tamaño de la muestra. Se deberán seleccionar el tamaño según la elección de los parámetros de erores tolarebles, esperados u nivel de confianza."),  
                       h4("La selección de las unidades para completar el tamaño de muestra, se visualiza en términos de una tabla. Se aplicó el método de selección Proporcional Por Tamaño, la cual brinda mayor probabilidad de ser seleccinadas a las unidades con montos mayores."),
                       br(),
                       h3("Cálculo de tamaño de muestra"),
                       br(),
                       h4("Cuando estás determinando el tamaño de una muestra para tu estudio, hay varios factores clave a considerar
                          que influyen directamente en la cantidad de datos que necesitas recolectar:",align = "left"),
                       br(),
                       h4("Margen de Tolerancia (Tolerable)",align = "left", style = "font-weight: bold"),
                       h4("Este valor representa el máximo error de estimación que estás dispuesto a aceptar
                          en tus resultados. Un margen mayor sugiere que estás tolerando una mayor incertidumbre, lo que puede resultar en una
                          muestra más pequeña. En contraste, un margen más ajustado requiere una muestra más grande para garantizar que tus estimaciones
                          estén dentro de ese rango estrecho.",align = "left"),
                       h4("Error Esperado (Esperado)",align = "left", style = "font-weight: bold"),
                       h4("Este es el error que anticipas podría existir en tu población. Un valor más alto implica que esperas
                          más variabilidad en los datos, lo que se traduce en necesitar una muestra más grande para obtener estimaciones precisas.",align = "left"),
                       h4("Nivel de Confianza",align = "left", style = "font-weight: bold"),
                       h4("Cuanto mayor sea el nivel de confianza que desees tener en los resultados de tu muestra, mayor deberá
                          ser el tamaño de la misma. Esto se debe a que un nivel de confianza más alto indica que quieres estar más seguro de que tu 
                          muestra representa correctamente a toda la población.",align = "left"),
                       
                       br(),
                       h3("Tabla de sugencia para determinar el tamaño de la muestra.",align = "left"),
                       br(),
                       h4("El tamaño de muestra depende de la capacidad operativa y las característica de la auditoría. ",align = "left"),
                       h4("A continuación, Se presente una tabla con recomendaciones de tamaños de muestras, categorizadas en muestras de tamaño: inferiores a 50, entre 50 y 100 y superiores a 
                           a las 100 unidades de muestreo.",align = "left"),
                       br(),
                       

            
            fluidRow(
              box(
                title = "Tabla de Datos",
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                width = 8,  # Ocupará todo el ancho disponible
                div(style = "height: 180px;",  # Establece el alto de la tabla
                    reactableOutput("SugerenciasTamaño_MUM")
              )
              )
            ),
            
                       h4("Utilizando los controles deslizantes de tu aplicación, los usuarios pueden ajustar estos parámetros para determinar un tamaño 
                          de muestra que sea adecuado para sus necesidades específicas.",align = "left"),
                       br(),
                       h4("Nota: defina los parámetros, y luego presione sobre 'Análisis de muestreo. Recuerde la distribución aproximada de la sección 'Descriptivo'." ,align = "left"),
                       sliderInput("freq1_MUM",
                                   "Tolerable:",
                                   min = 0.01,  max = 0.99, value = 0.05),
                       sliderInput("freq2_MUM",
                                   "Esperado:",
                                   min = 0.01,  max = 0.99, value = 0.01), 
                       h6("Importante: el 'Tolerable' debe siempre ser superior al 'Esperado'. Caso contrario, desaparece el botón de 'Análisis del muestreo' por asignación incorrecta de los parámetros."),
            
                       selectInput("distri_1", "Seleccione el nivel:",  
                                   list(`Tipo` = list("poisson",
                                                      "binomial"
                                                      
                                   )
                                   )
                       ),
                       sliderInput("freq3_MUM",
                                   "Nivel de confianza:",
                                   min = 0.01,  max = 0.99, value = 0.95),
            
                     
                      conditionalPanel(
                             condition = "(!output.hasNegatives_MUM) && (input.freq2_MUM < input.freq1_MUM)",
                             actionButton("update_MUM", "Análisis del muestreo.", class = "btn-primary")),
                       br(),
                       br(),
            
            ########################
            # Tamaño de la muestra #
            ########################
            
                       fluidRow(
                         box(
                           solidHeader = TRUE, 
                           width = 12,
                           reactableOutput("SampleSize_MUM")  
                         )
                       ),
                       br(),
                       br(),
            
            #######################
            # Valor de la semilla #
            #######################
          
            fluidRow(
              box(
                solidHeader = TRUE, 
                width = 12,
                reactableOutput("seedvalue_MUM")  
              )
            ),
            br(),
            br(),
            
            #########################         
            # Valores seleccionados #
            #########################
            
            
                       fluidRow(
                         box(
                           solidHeader = TRUE, 
                           width = 12,
                           reactableOutput("sample_MUM")  
                         )
                       ),
                       br(),
            #################################################
            #    Comparación de datos originales y muestra  #
            #################################################
                       
                  h3("Comparación de datos cargados vs muestra seleccionada"),
                  br(),
            fluidRow(
              box(
                title = "Comparación de distribuciones entre datos cargados y las unidaddes seleccionadas a partir de la muestra de datos",
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                width = 8,
                highchartOutput("comp_dist_MUM")  
              )
            ),
            
                       #################################
                       #         Descargar muestra     #
                       #################################
                       
                       h3("Descargar la muestra seleccionada"),
                       br(),
                       actionButton("show1_MUM", "Descargar archivo"),
            br(),
            h3("Descargar Reporte", align = "left"),
            downloadButton("downloadReport2", "Descargar Reporte Muestreo MUM")
  ),
  
  
  ##############################################################################################################
  ##############################################################################################################
  #                                               Muestreo LES                                                 #
  ##############################################################################################################
  ##############################################################################################################
  
  tabItem(tabName = "p4",
          
            
                       h1("Muestreo LES", align = "center"),
    
            br(),
            h2("En este sección:", align = "left"),
            br(),
            h4("Se lleva a cabo el proceso de muestreo: tamaño y selección de la unidades según el LES.", align ="left"),
            br(),
            h4("Cargado los datos, usted podrá:"),
            br(),
            tags$ul(
              style = "list-style-type: disc; padding-left: 20px;",  # Estilo para la lista: disc es una viñeta redonda
              tags$li(h4("Calcular el tamaño de muestra.", align = "left")),
              tags$li(h4("Visualizar las unidades seleccionadas", align = "left",)),
              tags$li(h4("Comparar los datos cargados vs los datos obtenidos por la muestra.", align = "left")),
              tags$li(h4("Descargar los datos de la muestra en formato, ya sea .csv, .txt y .xlsx", align = "left")),
              tags$li(h4("Descargar los resultados generados en formato '.docx'.", align = "left"))
            ),
            br(), 
            
            h3("Cargar datos", align = "left"),
                       br(),
          fileInput("file3", "Importar datos del muestreo",
                    accept =  c(
                      ".csv",
                      ".txt",
                      ".xlsx",
                      "text/csv",
                      "text/plain",
                      "text/tab-separated-values",
                      "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
                    )),
          br(),
          uiOutput("variable_select_LES"),
          h4("IMPORTANTE: Debe seleccionar variables numéricas.",align = "left", style = "font-weight: bold"),
          uiOutput("negativesAlertMuestreoLES"),
          br(),
          #           plotOutput("histogram2"),
          h2("Muestreo: tamaño y selección", align = "left"),
          br(),
          h4("El proceso de muestreo consta de dos etapas: selección del tamaño de la muestra y la selección de las unidades",align = "left"),
          h4("Se aborda primeramente la determinación del tamaño de la muestra. Se deberán seleccionar el tamaño según la elección de los parámetros de erores tolarebles, esperados u nivel de confianza."),  
          h4("La selección de las unidades para completar el tamaño de muestra, se visualiza en términos de una tabla. Se aplicó el método de selección Proporcional Por Tamaño, la cual brinda mayor probabilidad de ser seleccinadas a las unidades con montos mayores."),
          br(),
          h3("Cálculo de tamaño de muestra"),
          br(),
          h4("Cuando estás determinando el tamaño de una muestra para tu estudio, hay varios factores clave a considerar
                          que influyen directamente en la cantidad de datos que necesitas recolectar:",align = "left"),
          br(),
          h4("Margen de Tolerancia (Tolerable)",align = "left", style = "font-weight: bold"),
          h4("Este valor representa el máximo error de estimación que estás dispuesto a aceptar
                          en tus resultados. Un margen mayor sugiere que estás tolerando una mayor incertidumbre, lo que puede resultar en una
                          muestra más pequeña. En contraste, un margen más ajustado requiere una muestra más grande para garantizar que tus estimaciones
                          estén dentro de ese rango estrecho.",align = "left"),
          h4("Error Esperado (Esperado)",align = "left", style = "font-weight: bold"),
          h4("Este es el error que anticipas podría existir en tu población. Un valor más alto implica que esperas
                          más variabilidad en los datos, lo que se traduce en necesitar una muestra más grande para obtener estimaciones precisas.",align = "left"),
          h4("Nivel de Confianza",align = "left", style = "font-weight: bold"),
          h4("Cuanto mayor sea el nivel de confianza que desees tener en los resultados de tu muestra, mayor deberá
                          ser el tamaño de la misma. Esto se debe a que un nivel de confianza más alto indica que quieres estar más seguro de que tu 
                          muestra representa correctamente a toda la población.",align = "left"),
          
          br(),
          h3("Tabla de sugencia para determinar el tamaño de la muestra.",align = "left"),
          br(),
          h4("El tamaño de muestra depende de la capacidad operativa y las característica de la auditoría. ",align = "left"),
          h4("A continuación, Se presente una tabla con recomendaciones de tamaños de muestras, categorizadas en muestras de tamaño: inferiores a 50, entre 50 y 100 y superiores a 
                           a las 100 unidades de muestreo.",align = "left"),
          br(),
          
          
          fluidRow(
            box(
              title = "Tabla de Datos",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = 8,  # Ocupará todo el ancho disponible
              div(style = "height: 180px;",  # Establece el alto de la tabla
                  reactableOutput("SugerenciasTamaño_LES")
              )
            )
          ),
          
          #### Selección de los parámetros para el tamaño de muestra #####
          
          h4("Utilizando los controles deslizantes de tu aplicación, los usuarios pueden ajustar estos parámetros para determinar un tamaño 
                          de muestra que sea adecuado para sus necesidades específicas.",align = "left"),
          br(),
          h4("Nota: defina los parámetros, y luego presione sobre 'Análisis de muestreo. Recuerde la distribución aproximada de la sección 'Descriptivo'." ,align = "left"),
          sliderInput("freq1_LES",
                      "Tolerable:",
                      min = 0.01,  max = 0.99, value = 0.05),
          sliderInput("freq2_LES",
                      "Esperado:",
                      min = 0.01,  max = 0.99, value = 0.01), 
          selectInput("distri_2", "Seleccione el nivel:",  
                      list(`Tipo` = list("poisson",
                                         "binomial"
                                         
                      )
                      )
          ),
          h6("Importante: el 'Tolerable' debe siempre ser superior al 'Esperado'. Caso contrario, desaparece el botón de 'Análisis del muestreo' por asignación incorrecta de los parámetros."),
          sliderInput("freq3_LES",
                      "Nivel de confianza:",
                      min = 0.01,  max = 0.99, value = 0.95),
          br(),
          h4("Valor LES.",align = "left"),
          numericInput("LES", "Valor del LES:", min = 0, value = 100000),
          
          conditionalPanel(
                               condition = "(!output.hasNegatives_LES) && (input.freq2_LES < input.freq1_LES)",
                               actionButton("update_LES", "Análisis del muestreo.", class = "btn-primary")
          ),
          br(),
          br(),
          
          ########################
          # Tamaño de la muestra #
          ########################
          
          fluidRow(
            box(
              solidHeader = TRUE, 
              status = "primary",
              collapsible = TRUE,
              width = 8,
              reactableOutput("SampleSize_LES")  
            )
          ),
          br(),
          
          ##################################
          #   Conteo de valores según LES  # 
          ##################################
          
          fluidRow(
            box(
              title = "Conteo de Valores según LES",
              solidHeader = TRUE,
              status = "primary",
              collapsible = TRUE,
              width = 8,
              reactableOutput("ConteoLes")
            )
          ),

          #######################
          # Valor de la semilla #
          #######################
          
          fluidRow(
            box(
              solidHeader = TRUE, 
              status = "primary",
              collapsible = TRUE,
              width = 8,
              reactableOutput("seedvalue_LES")  
            )
          ),
          br(),
          br(),
          
 
          ######################## 
          # Muestra seleccionada 
          ########################
          
          fluidRow(
            box(
              title = "Muestra Seleccionada",
              solidHeader = TRUE,
              status = "primary",
              collapsible = TRUE,
              width = 8,
              reactableOutput("MuestraLES")
            )
          ),
          
          br(),
          

          #################################################
          #    Comparación de datos originales y muestra  #
          #################################################
          
          
          h3("Comparación de datos cargados vs muestra seleccionada"),
          br(),
          fluidRow(
            box(
              title = "Comparación de distribuciones entre datos cargados y las unidades seleccionadas a partir de la muestra de datos",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = 8,
              highchartOutput("comp_dist_LES")  
            )
          ),
          
          br(),
          
          #################################
          #         Descargar muestra     #
          #################################
          
          h3("Descargar la muestra seleccionada"),
          br(),
          actionButton("show1_LES", "Descargar archivo"),
          br(),
          h3("Descargar Reporte", align = "left"),
          downloadButton("downloadReport3", "Descargar Reporte Muestreo LES"),
          
          
          
          ),
  
         ##############################################################################################################
         ##############################################################################################################
         #                                               Muestreo Atribuos                                            #
         ##############################################################################################################
         ##############################################################################################################
  tabItem(tabName = "p5",
          
          
          h1("Muestreo por atributos.", align = "center"),
          br(),
          h2("Se lleva a cabo el proceso de muestreo por atribuross: tamaño y selección de la unidades.", align = "left"),
          br(),
          h4("Una vez cargada la información, y seleccionadas las variables correspondientes a los datos observados y auditados,  usted podrá:"),
          br(),
          tags$ul(
            style = "list-style-type: disc; padding-left: 20px;",  # Estilo para la lista: disc es una viñeta redonda
            tags$li(h4("Detarminar los niveles de error tolerable , esperado y nivel de cobfianza", align = "left")),
            tags$li(h4("Determinar el tamaño de muestra.", align = "left")),
            tags$li(h4("Visualizar la muestra seleccionada.", align = "left")),
            tags$li(h4("Comparar los porcentajes de las categorías para los datos originales y los seleccionados por la muestra.", align = "left")),
            tags$li(h4("Descagar la muestra seleccionada.", align = "left")),
            tags$li(h4("Descargar los resultados generados en formato '.docx'.", align = "left"))
            ),
          br(),
          h3("Cargar datos", align = "left"),
          br(),
          fileInput("file4", "Importar datos del muestreo",
                    accept =  c(
                      ".csv",
                      ".txt",
                      ".xlsx",
                      "text/csv",
                      "text/plain",
                      "text/tab-separated-values",
                      "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
                    )),
          br(),
          uiOutput("variable_select_Atri"),
          h4("IMPORTANTE: Debe seleccionar variables de atributo.",align = "left", style = "font-weight: bold"),
          # METER ACA LA ADVERTENCIA 
          br(),
          #           plotOutput("histogram2"),
          h2("Muestreo: tamaño y selección", align = "left"),
          br(),
          h4("El proceso de muestreo consta de dos etapas: selección del tamaño de la muestra y la selección de las unidades",align = "left"),
          h4("Se aborda primeramente la determinación del tamaño de la muestra. Se deberán seleccionar el tamaño según la elección de los parámetros de erores tolarebles, esperados u nivel de confianza."),  
          h4("La selección de las unidades para completar el tamaño de muestra, se visualiza en términos de una tabla. Se aplicó el método de selección Proporcional Por Tamaño, la cual brinda mayor probabilidad de ser seleccinadas a las unidades con montos mayores."),
          br(),
          h3("Cálculo de tamaño de muestra"),
          br(),
          h4("Cuando estás determinando el tamaño de una muestra para tu estudio, hay varios factores clave a considerar
                          que influyen directamente en la cantidad de datos que necesitas recolectar:",align = "left"),
          br(),
          h4("Margen de Tolerancia (Tolerable)",align = "left", style = "font-weight: bold"),
          h4("Este valor representa el máximo error de estimación que estás dispuesto a aceptar
                          en tus resultados. Un margen mayor sugiere que estás tolerando una mayor incertidumbre, lo que puede resultar en una
                          muestra más pequeña. En contraste, un margen más ajustado requiere una muestra más grande para garantizar que tus estimaciones
                          estén dentro de ese rango estrecho.",align = "left"),
          h4("Error Esperado (Esperado)",align = "left", style = "font-weight: bold"),
          h4("Este es el error que anticipas podría existir en tu población. Un valor más alto implica que esperas
                          más variabilidad en los datos, lo que se traduce en necesitar una muestra más grande para obtener estimaciones precisas.",align = "left"),
          h4("Nivel de Confianza",align = "left", style = "font-weight: bold"),
          h4("Cuanto mayor sea el nivel de confianza que desees tener en los resultados de tu muestra, mayor deberá
                          ser el tamaño de la misma. Esto se debe a que un nivel de confianza más alto indica que quieres estar más seguro de que tu 
                          muestra representa correctamente a toda la población.",align = "left"),
          
          br(),
          h3("Tabla de sugencia para determinar el tamaño de la muestra.",align = "left"),
          br(),
          h4("El tamaño de muestra depende de la capacidad operativa y las característica de la auditoría. ",align = "left"),
          h4("A continuación, Se presente una tabla con recomendaciones de tamaños de muestras, categorizadas en muestras de tamaño: inferiores a 50, entre 50 y 100 y superiores a 
                           a las 100 unidades de muestreo.",align = "left"),
          br(),
          
          
          fluidRow(
            box(
              title = "Tabla de Datos",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = 8,  # Ocupará todo el ancho disponible
              div(style = "height: 180px;",  # Establece el alto de la tabla
                  reactableOutput("SugerenciasTamaño_Atri")
              )
            )
          ),
          
          h4("Utilizando los controles deslizantes de tu aplicación, los usuarios pueden ajustar estos parámetros para determinar un tamaño 
                          de muestra que sea adecuado para sus necesidades específicas.",align = "left"),
          br(),
          h4("Nota: defina los parámetros, y luego presione sobre 'Análisis de muestreo. Recuerde la distribución aproximada de la sección 'Descriptivo'." ,align = "left"),
          sliderInput("freq1_Atri",
                      "Tolerable:",
                      min = 0.01,  max = 0.99, value = 0.05),
          sliderInput("freq2_Atri",
                      "Esperado:",
                      min = 0.01,  max = 0.99, value = 0.01), 
          selectInput("distri_3", "Seleccione el nivel:",  
                      list(`Tipo` = list("poisson",
                                         "binomial"
                                         
                      )
                      )
          ),
          h6("Importante: el 'Tolerable' debe siempre ser superior al 'Esperado'. Caso contrario, desaparece el botón de 'Análisis del muestreo' por asignación incorrecta de los parámetros."),
          sliderInput("freq3_Atri",
                      "Nivel de confianza:",
                      min = 0.01,  max = 0.99, value = 0.95),
          br(),

          
          conditionalPanel(
            condition = "(input.freq2_Atri < input.freq1_Atri)",
            actionButton("update_Atri", "Análisis del muestreo.", class = "btn-primary")
          ),
          br(),
          br(),
          
          ########################
          # Tamaño de la muestra #
          ########################
          h3("Tamaño de la muestra.",align = "left"),
          fluidRow(
            box(
              solidHeader = TRUE, 
              status = "primary",
              collapsible = TRUE,
              width = 8,
              reactableOutput("SampleSize_Atri")  
            )
          ),
          br(),

          #######################
          # Valor de la semilla #
          #######################
          h4("Semilla del proceso de selección.",align = "left"),
          fluidRow(
            box(
              solidHeader = TRUE, 
              status = "primary",
              collapsible = TRUE,
              width = 8,
              reactableOutput("seedvalue_Atri")  
            )
          ),
          br(),
          br(),
          
          ######################## 
          # Muestra seleccionada 
          ########################
          h3("Muestra seleccionada.",align = "left"),
          fluidRow(
            box(
              solidHeader = TRUE, 
              status = "primary",
              collapsible = TRUE,
              width = 8,
              reactableOutput("tablaMuestraAtri")  
            )
          ),
          br(),
          
          #################################################
          #             Tabla con porcentaje              #
          #################################################
          
          #          fluidRow(
          #  box(
          #    title = "Tabla de Origen con Porcentaje",
          #    status = "primary",
          #    solidHeader = TRUE,
          #    collapsible = TRUE,
          #    width = 12,  # Ajusta según necesidad
          #    reactableOutput("tablaOrigenPorceOut") # Este ID debe coincidir con el usado en server.R
          #  )
          #  ),
          # br(),
          
          
          # fluidRow(
          #  box(
          #    title = "Tabla de Origen con Porcentaje",
          #    status = "primary",
          #    solidHeader = TRUE,
          #    collapsible = TRUE,
          #    width = 12,  # Ajusta según necesidad
          #    reactableOutput("tablaMuestraPorce") # Este ID debe coincidir con el usado en server.R
          #  )
          # ),
          #    br(),
          
          
          #################################################
          #    Comparación de datos originales y muestra  #
          #################################################
          
          #        fluidRow(
          #  box(
          #    title = "Gráfico Comparativo",
          #    solidHeader = TRUE, 
          #    status = "primary",
          #    collapsible = TRUE,
          #    width = 12, # Ajusta según sea necesario para el diseño de tu UI
          #    highchartOutput("graficoComparativo")  # Cambia a highchartOutput para gráficos Highcharter
          #  )
          #  ),
          #  br(),
          h3("Comparación porcentual entre datos originales y obtenidos por la muestra.",align = "left"),
          fluidRow(
            box(
              title = "Gráfico Comparativo entre original y muestra",
              solidHeader = TRUE, 
              status = "primary",
              collapsible = TRUE,
              width = 12, # Ajusta según sea necesario para el diseño de tu UI
              highchartOutput("graficoComparativo2")  # Cambia a highchartOutput para gráficos Highcharter
            )
          ),
          br(),
          
          #################################
          #         Descargar muestra     #
          #################################
          
          h3("Descargar la muestra seleccionada"),
          br(),
          actionButton("show1_Atri", "Descargar archivo"),
          br(),
          h3("Descargar Reporte", align = "left"),
          downloadButton("downloadReport4", "Descargar Reporte Muestreo Atributos")
          
        
          
  ),
  
  ##############################################################################################################
  ##############################################################################################################
  #                                                  Evaluación                                                #
  ##############################################################################################################
  ##############################################################################################################
  
  tabItem(tabName = "p6",
          
          
          h1("Evaluación de la auditoría.", align = "center"),
          br(),
          h2("En este sección:", align = "left"),
          br(),
          h4("Una vez cargada la información, y seleccionadas las variables correspondientes a los datos observados y auditados,  usted podrá:"),
          br(),
          tags$ul(
            style = "list-style-type: disc; padding-left: 20px;",  # Estilo para la lista: disc es una viñeta redonda
            tags$li(h4("Comparar la información de los datos observados vs los datos auditados.", align = "left")),
            tags$li(h4("De forma descriptiva, valorar las diferencias.", align = "left")),
            tags$li(h4("Analizar ciertos Indicadores de Riesgo en el proceso de comparación entre la información de los datos observados vs los datos auditados.", align = "left",)),
            tags$li(h4("Evaluar criterios empíricos en la determinación del umbral máximo permitido o tolerable.", align = "left")),
            tags$li(h4("Descargar los resultados generados en formato '.docx'.", align = "left"))
          ),
          br(), 
          
          h3("Cargar datos", align = "left"),
          fileInput("file5", "Importar datos para la evaluación del muestreo.",
                    accept =  c(
                      ".csv",
                      ".txt",
                      ".xlsx",
                      "text/csv",
                      "text/plain",
                      "text/tab-separated-values",
                      "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
                    )),
          h3("Seleccionar los parametros para la evaluación de los valores observados y auditados."),
          br(),
          #      sliderInput("freq20",
          #            "Tolerable:",
          #            min = 0.01,  max = 0.99, value = 0.05),
     #     selectInput("method", "Seleccione el método de evaluación:",  
     #                 list(`Tipo` = list( "poisson", 
     #                                      "binomial"
     #                  )
     #                  )
     #      ),
     #      sliderInput("freq21",
     #                  "Nivel de confianza:",
     #                  min = 0.01,  max = 0.99, value = 0.95),
          
          uiOutput("var1"),
          uiOutput("var2"),
          h4("IMPORTANTE: Debe seleccionar variables numéricas.",align = "left", style = "font-weight: bold"),
          br(),
          actionButton("analizar", "Evaluación", class = "btn-primary"),
          br(),
          br(),
          h2("Comparar la información de los datos observados vs. los datos auditados."),
          br(),
          h4("Se presentan los datos en forma de tabla, gráfico de disperción, y las diferencias encontradas.",  align = "left"),
          h4("Si desea, puede descargar los casos u observaciones con diferencias.",  align = "left"),
         br(),
          fluidRow(
            box(
              title = "Tabla de Datos",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = 8,  # Ocupará todo el ancho disponible
              div(style = "height: 400px; overflow-y: auto;",   # Establece el alto de la tabla
                  reactableOutput("Tabla2"))
            ),
            div(style = "height: 30px;"),  # Espacio entre los boxes
            box(
              title = "Gráfico de Dispersión",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = 8,  # Ocupará todo el ancho disponible
              highchartOutput("ScatterPlot", height = "400px")  # Establece el alto del gráfico Highcharter
            ),
            box(
              title = "Diferencias",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = 8,  # Ocupará todo el ancho disponible
              div(style = "height: 400px; overflow-y: auto;",  # Establece el alto de la tabla
                  reactableOutput("Tabla3"))
            )
            
          ),
     
     h4("Descargar tabla de diferencias."),
     br(),
     actionButton("show2", "Descargar archivo"),
          

          br(),
          br(),
          h2("Indicadores de riesgo en la comparación de la información de los datos observados vs los datos auditados."),
          br(),
          h4("Los indicadores de riesgo son medidas que ayudan en la comparación entre los valores observados y los valores auditados."),
          h4("Estos se representan en términos de medidas y mediante una representación gráfica."),
          h4("Las medidas son relativas a la suma de los montos, montos promedios, total de observaciones, cantidad de casos diferentes, "),
          h4("cantidad y suma de sabrevaloraciones,cantidad y suma de infravolaraciones, y porcentaje de diferencia entre las variables."),
          h4("Por otra parte,de forma visual se presente el gráfico de dispersión pero ahora con intervalores de confianza."),
          br(),
          h4("La tabla de 'Indicadores de riesgo de evaluación' y 'Gráfico de Dispersión con intervalos de confianza' evaluan las diferencias entre los valores observados y auditados."),
          br(),
          fluidRow(
             box(
                title = "Indicados de riesgo de evaluación",
                status = "primary",
             solidHeader = TRUE,
         collapsible = TRUE,
         width = 8,  # Ocupará todo el ancho disponible
         div(style = "height: 400px; overflow-y: auto;",   # Establece el alto de la tabla
             reactableOutput("Riesgo"))
                ),
         box(
           title = "Gráfico de Dispersión con intervalos de confianza",
           status = "primary",
           solidHeader = TRUE,
           collapsible = TRUE,
           width = 8,  # Ocupará todo el ancho disponible
           highchartOutput("ScatterPlot_limit", height = "400px")  # Establece el alto del gráfico Highcharter
         )
         
         
             ),
          br(),
          h2("Criterio empírico del máximo umbral permitido o tolerado."),
          br(),
       h4("Se debe seleccionar límites permisibles que esté dispuesto a aceptar o tolerar entre la comparación de los valores observados y auditados."),
       h4("Para esto, debe seleccionar valores, los cuales serán criteriores, los cuales esperaría que en la evaluación no sea superiores a este."),
       h4("Los criteriores que debe seleccionar son en razón a:"),
       br(),
     
       h3("Monto máximo", align = "left", style = "font-weight: bold; text-decoration: underline;"),
       h4("Monto máximo absoluto tolerable entre los valores observados y auditados."),
       h3("Porcentaje máximo tolerado", align = "left", style = "font-weight: bold; text-decoration: underline;"),
       h4("Razón entre las diferencias absolutas, dividido entre el monto total auditado, por 100."),
       h3("Conteo máximo de diferencias", align = "left", style = "font-weight: bold; text-decoration: underline;"),
       h4("El número máximo de diferencias que está dispuesto a tolerar."),
       h3("Conteo máximo fuera de los límites de confianza", align = "left", style = "font-weight: bold; text-decoration: underline;"),
       h4("El número máximo de diferencias, fuera de los límites de confianza, que está dispuesto a tolerar."),
     br(),
      h3("Nota: dentro de la tabla 'criterireos de evaluación', seleccione los valores máximos tolerables recien comentados. Presione el botón de 'Evaluación'"),
     fluidRow(
       box(
         title = "Criterios de Evaluación",
         status = "primary",
         solidHeader = TRUE,
         collapsible = TRUE,
         width = 8,
         numericInput("monto_maximo", "Monto máximo tolerable:",
                      min = 0, value = 5000),
         sliderInput("porcentaje_umbral", "Porcentaje máximo tolerado:",
                     min = 0.01,  max = 0.99, value = 0.15),
         sliderInput("conteo_umbral", "Conteo máximo de diferencias:",
                     min = 0, max = 100, value = 15),
         sliderInput("casos_umbral", "Conteo máximo fuera de los límites de confianza:",
                     min = 0, max = 100, value = 10),
         actionButton("auditEval", "Evaluación", class = "btn-primary")
       )
     ),
     fluidRow(
       conditionalPanel(
         condition = "input.auditEval > 0",
         box(
           title = "Evaluación auditoría",
           status = "primary",
           solidHeader = TRUE,
           collapsible = TRUE,
           width = 8,
           reactableOutput("Eval")
         )
       )
     ),
     br(),
     h3("Descargar Reporte", align = "left"),
     downloadButton("downloadReport5", "Descargar Reporte Evaluación")
          
  )
  )
  
  
)