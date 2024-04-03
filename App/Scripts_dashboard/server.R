############################
#   Contenido del server   # 
############################


###################################################################################
# El server se estructura como sigue                                              #
#                                                                                 #
# 1. Importación de los datos                                                     #
# 2. Análisis descriptivos de las variables de interés                            #
# 3. Cálculo del tamaño de muestra según la selección hecha en el body            #
# 4. Visualización de la selección de los elementos                               #
# 5. Descarga de las unidades de la muestra                                       #
# 6. Evaluación de la muestra según datos observados y auditados                  #
#                                                                                 #
###################################################################################

server <- function(input, output, session) {
  
  #################################################################################
  #################################################################################
  #                           Análisis descriptivo                                #
  #################################################################################
  #################################################################################
  
  ################################################
  #          Importar Datos: data                #
  ################################################
  
  
  # Leer los datos basados en el tipo de archivo
  

  
  data1 <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) {
      return(NULL)
    }
  
  
  ext <- tools::file_ext(inFile$datapath)
  
  # Dependiendo de la extensión del archivo, usar la función correspondiente
  switch(ext,
         csv = read.csv(inFile$datapath, stringsAsFactors = FALSE),
         txt = read.delim(inFile$datapath, stringsAsFactors = FALSE),
         xlsx = read_excel(inFile$datapath),
         stop("Tipo de archivo no soportado")
  )
})
  
  # Selección de la variable
  
  output$variable_select_1 <- renderUI({
    if (is.null(data1())) {
      return(NULL)
    } else {
      selectInput("variable1", "Elija una variable:", names(data1()))
    }
  })
  
  
  # At the beginning of your server function, initialize a reactive value
  has_negatives_1 <- reactiveVal(FALSE)
  
  observe({
    # Ensure that the dataset and selected variable are available
    req(data1(), input$variable1)
    
    # Check for negative values in the selected variable
    if (any(data1()[[input$variable1]] < 0, na.rm = TRUE)) {
      has_negatives_1(TRUE)  # Update the reactive value if negatives are found
    } else {
      has_negatives_1(FALSE)  # Update the reactive value if no negatives are found
    }
  })
  
  # Create an output to use in the UI for showing/hiding the alert
  output$negativesAlert_1 <- renderUI({
    if (has_negatives_1()) {
      tags$div(class = "alert alert-danger", 
               strong("¡Se detectaron valores negativos!"), "Verifique si es correcto. Caso contrario, proceda a tomar medidas.")
    }
  })
  
  # Make sure the UI can always access this output
  outputOptions(output, 'negativesAlert_1', suspendWhenHidden = FALSE)

  # Genera datos binomiales
  n_binom <- 10000  # Número de observaciones
  size <- 100       # Número de ensayos
  prob <- 0.5       # Probabilidad de éxito
  datos_binom <- rbinom(n_binom, size, prob)
  
  # Genera datos de Poisson
  n_pois <- 10000  # Número de observaciones
  lambda <- 40     # Tasa promedio de éxito
  set.seed(123)    # Para reproducibilidad
  outliers <- c(sample(80:100, size = 10, replace = TRUE)) # Genera algunos valores extremos
  datos_pois_extremos <- c(rpois(n_pois, lambda), outliers)
  

    observeEvent(input$start_analysis, {
        # Tabla de estadísticas descriptivas
      
      
      
        output$stats <- renderReactable({
            req(data1())
          
          
          Datos <- as.data.frame(data1()[[input$variable1]])  %>% 
            dplyr::rename(
              Monto = `data1()[[input$variable1]]`
              
            )
          
          
          Stats <- Datos %>%
            
            summarise(  ConteoCasos = sum(!is.na(Monto)),  # Agrega esta línea para contar casos válidos
                        ValoresNegativos = sum(Monto < 0, na.rm = TRUE),
                        ValoresFaltantes = sum(is.na(Monto)),
                        Minimo = min(Monto, na.rm = TRUE),
                        Maximo = max(Monto, na.rm = TRUE),
                        Promedio = mean(Monto, na.rm = TRUE),
                        Mediana = median(Monto, na.rm = TRUE),
                        Moda = as.numeric(names(sort(table(Monto), decreasing = TRUE)[1])),
                        DesviacionEstandar = sd(Monto, na.rm = TRUE),
                        Percentil10 = quantile(Monto, 0.1, na.rm = TRUE),
                        Percentil25 = quantile(Monto, 0.25, na.rm = TRUE),
                        Percentil50 = quantile(Monto, 0.50, na.rm = TRUE),
                        Percentil75 = quantile(Monto, 0.75, na.rm = TRUE),
                        Percentil90 = quantile(Monto, 0.90, na.rm = TRUE)
                        
                        
                        
            ) %>% pivot_longer(
              cols = everything(),
              names_to = "Medida",
              values_to = "Valor"
            )  %>%
            mutate(Valor = round(Valor, 1))
          
          
          
          reactable(Stats, defaultPageSize = 15)
        })

        
        
        # Histograma de una variable
        output$histogram1 <- renderHighchart({
          
            req(data1(), input$variable1)
          
          
          req(input$variable1)
          
          datos <- as.data.frame(data1()[[input$variable1]])  %>% 
            dplyr::rename(
              Monto = `data1()[[input$variable1]]`
              
            )
          
          x = datos$Monto
          
          hist <-  hchart(density(x), type = "area", color = "skyblue", name = "Monto")  %>%
            hc_exporting(enabled = TRUE) %>%
            hc_tooltip(crosshairs = T,valueDecimals = 1, shared = TRUE, borderWidth = 5) %>%
            hc_chart(zoomType = "xy") 
           
          
          hist 
        })

        
        
        # Gráfico binomial
        output$binomialPlot <- renderPlot({
          
          ggplot(data.frame(Valor = datos_binom), aes(x = Valor)) +
            geom_histogram(binwidth = 1, fill = 'skyblue', color = 'black') +
            labs(title = "Distribución Binomial", x = "", y = "Frecuencia")
          
        })
        
        

        # Gráfico de Poisson
        output$poissonPlot <- renderPlot({
          
          ggplot(data.frame(Valor = datos_pois_extremos), aes(x = Valor)) +
            geom_histogram(bins = 120, fill = 'skyblue', color = 'black') +
            labs(title = "Distribución de Poisson con valores extremos",
                 x = "", y = "Frecuencia")
          
        })
    })

    ###########################################
    #     Reporte del análisis desciptivo     #
    ###########################################

    output$downloadReport1 <- downloadHandler(
      filename = function() {
        paste("Reporte_Analisis_Descriptivo_", Sys.Date(), ".docx", sep = "")
      },
      content = function(file) {
        req(data1(), input$variable1)
        
        # Crea un nuevo documento Word
        doc <- read_docx()
        
        # Añade el título y los metadatos al documento
        doc <- doc %>% 
          body_add_par("Análisis Descriptivo", style = "heading 1") %>%
          body_add_par(paste("Archivo de datos:", input$file1$name), style = "heading 2") %>%
          body_add_par(paste("Variable seleccionada:", input$variable1), style = "heading 2")
        
        # Calcula las estadísticas descriptivas
        var_data <- data1()[[input$variable1]]
        Stats <- tibble(
          Medida = c("Conteo de Casos", "Valores Negativos", "Valores Faltantes", "Mínimo", 
                     "Máximo", "Promedio", "Mediana", "Moda", "Desviación Estándar", 
                     "Percentil 10", "Percentil 25", "Percentil 50", "Percentil 75", "Percentil 90"),
          Valor = c(
            sum(!is.na(var_data)),
            sum(var_data < 0, na.rm = TRUE),
            sum(is.na(var_data)),
            min(var_data, na.rm = TRUE),
            max(var_data, na.rm = TRUE),
            mean(var_data, na.rm = TRUE),
            median(var_data, na.rm = TRUE),
            as.numeric(names(sort(table(var_data), decreasing = TRUE)[1])),
            sd(var_data, na.rm = TRUE),
            quantile(var_data, 0.1, na.rm = TRUE),
            quantile(var_data, 0.25, na.rm = TRUE),
            quantile(var_data, 0.50, na.rm = TRUE),
            quantile(var_data, 0.75, na.rm = TRUE),
            quantile(var_data, 0.90, na.rm = TRUE)
          )
        ) %>%
          mutate(Valor = round(Valor, 1))
        
        # Crea una tabla flextable con las estadísticas
        ft <- flextable(Stats)
        
        # Añade la tabla al documento
        doc <- doc %>% 
          body_add_flextable(ft)
        
        
        # Genera el gráfico de densidad con ggplot2
        plot <- ggplot(data1(), aes(x = .data[[input$variable1]])) +
          geom_density(fill = 'skyblue', color = 'blue', alpha = 0.5) +
          labs(title = paste("Distribución de", input$variable1))
        
        # Guarda el gráfico como una imagen temporal
        plot_file <- tempfile(fileext = ".png")
        ggsave(plot_file, plot, width = 5, height = 4, dpi = 300)
        
        # Añade la imagen al documento
        doc <- doc %>% 
          body_add_img(src = plot_file, width = 5, height = 4)
        
        # Guarda el documento
        print(doc, target = file)
        
        # Elimina el archivo temporal de la imagen
        unlink(plot_file)
      }
    )
    
    
    

  
  #################################################################################
  #################################################################################
  #                                  Muestreo   MUM                               #
  #################################################################################
  #################################################################################
  
  
  ##############################
  #          Importar Datos    #
  ##############################
  
  # Data la volvemos un objeto reactivo 
  #  data ---> data()
  
    data2 <- reactive({
      inFile <- input$file2
      if (is.null(inFile)) {
        return(NULL)
      }
      
      ext <- tools::file_ext(inFile$datapath)
      
      # Dependiendo de la extensión del archivo, usar la función correspondiente
      switch(ext,
             csv = read.csv(inFile$datapath, stringsAsFactors = FALSE),
             txt = read.delim(inFile$datapath, stringsAsFactors = FALSE),
             xlsx = read_excel(inFile$datapath),
             stop("Tipo de archivo no soportado")
      )
    })
    
    ##################################################
    #         Alerta para valores negativos  en MUM  #
    ##################################################
    
    output$variable_select_MUM <- renderUI({
      if (is.null(data2())) {
        return(NULL)
      } else {
        selectInput("variable2", "Elija una variable:", names(data2()))
      }
    })
    
    # Initialize a reactive value for tracking negative values in the Muestreo section
    has_negatives_muestreo_MUM <- reactiveVal(FALSE)
    
    observe({
      # Ensure that the dataset and selected variable for Muestreo are available
      req(data2(), input$variable2)
      
      # Check for negative values in the selected variable for Muestreo
      if (any(data2()[[input$variable2]] < 0, na.rm = TRUE)) {
        has_negatives_muestreo_MUM(TRUE)  # Update if negatives are found
      } else {
        has_negatives_muestreo_MUM(FALSE)  # Update if no negatives are found
      }
    })
    
    # Create an output for the Muestreo negatives alert
    output$negativesAlertMuestreoMUM <- renderUI({
      if (has_negatives_muestreo_MUM()) {
        tags$div(class = "alert alert-danger", 
                 strong("¡Se detectaron valores negativos!"), "No es posible proceder con el muestreo MUM con valores o montos negativos. Procede a corregirlos para para poder proceder con el muestreo.")
      }
    })
    
    # Ensure the UI can always access this output
    outputOptions(output, 'negativesAlertMuestreoMUM', suspendWhenHidden = FALSE)
    
  
    ##################################################
    #              Tablas de referencia              #
    ##################################################
    
  # Datos para la tabla de sugerencias de tamaño de muestra
  sugerencias_tamaño <- data.frame(
    `Tamaño de Muestra` = c("Inferiores (<=50)", "Entre (50-100)", "Superiores (100-400)"),
    `Margen de Tolerancia (Tolerable)` = c("0.2 - 0.3", "0.03 - 0.05", "0.01 - 0.03"),
    `Error Esperado` = c("0.05 - 0.10", "0.02 - 0.05", "0.01 - 0.02"),
    `Nivel de Confianza` = c("0.90 - 0.95", "0.95 - 0.99", "> 0.99")
  )
  
  # Genera la tabla reactiva
  output$SugerenciasTamaño_MUM <- renderReactable({
    reactable(sugerencias_tamaño, bordered = TRUE, highlight = TRUE)
    
    
  })
  
  
  # output$histogram2 <- renderPlot({
  #  if (is.null(input$variable)) {
  #    return(NULL)
  #  }
  #  ggplot(data(), aes_string(input$variable)) + geom_histogram(binwidth = 10) + labs(title = paste("Distribución de", input$variable), x = input$variable, y = "Frecuencia")
  # })
  
  
  
  
  #################################
  #   Condicional     #
  #################################
  
  output$hasNegatives_MUM <- reactive({
    has_negatives_muestreo_MUM()  # Esta es tu variable reactiva que ya tienes definida
  })
  shiny::outputOptions(output, "hasNegatives_MUM", suspendWhenHidden = FALSE)
  
  
  observeEvent(input$update_MUM, {
    if (input$freq2_MUM >= input$freq1_MUM) {
      showModal(modalDialog(
        title = "Advertencia",
        "No se procede con el análisis por la mala especificación de los parámetros de los errores tolerables y esperados. Especifique bien los valores. Recordar que el valor esperado siempre debe ser inferior al error tolerable.",
        easyClose = TRUE,
        footer = NULL
      ))
    } else {
  
      
      #################################
      #    Cálculo tamaño muestra     #
      #################################
      
      # Objeto reactivo para el tamaño de muestra
      sample_size <- reactiveVal()  # Inicializa como un valor reactivo
      
      observeEvent(input$update_MUM, {  # Cuando 'update' se presiona, se ejecuta el código dentro de observeEvent
        stage1 <- planning(materiality = input$freq1_MUM, 
                           expected = input$freq2_MUM,
                           likelihood = input$distri_1, 
                           conf.level = input$freq3_MUM
        )
        
        sample_size(data.frame(`Muestra` = stage1$n))  # Asigna el valor al reactivo
      })
      
      # Renderizar la tabla de tamaño de muestra
      output$SampleSize_MUM <- renderReactable({
        req(sample_size())  # Asegúrate de que el valor reactivo no sea NULL
        reactable(sample_size())  # Renderiza el valor reactivo en una tabla
      })
      
      
      #################################
      #    Valor de la semilla MUM    #
      #################################
      
      set.seed(Sys.time())
      
      # Función reactiva para generar y almacenar la semilla
      reactive_seed <- reactiveVal()  # Inicializa como un valor reactivo
      
      observeEvent(input$update_MUM, {  # Actualiza la semilla cuando se presiona 'update'
        seed_number <- sample(1:100000, 1)  # Genera un número aleatorio entre 1 y 100000
        reactive_seed(seed_number)  # Asigna el número a reactive_seed
      })
      
      
      # Crear una tabla reactiva para mostrar la semilla
      output$seedvalue_MUM <- renderReactable({
        req(reactive_seed())  # Asegúrate de que la semilla no sea NULL
        reactable(data.frame(`Semilla` = reactive_seed()))  # Muestra la semilla en una tabla
      })
      
      #################################
      #    Muestra PPT y Aleatoria    #
      #################################
      
      # Objeto reactivo para la selección de las unidades
      Muestra <- reactive({
        req(input$update_MUM)  # Asegúrate de que el botón de actualizar se ha pulsado
        req(sample_size())  # Asegúrate de que el valor reactivo no sea NULL
        req(reactive_seed())  # Asegúrate de que la semilla reactiva no sea NULL
        
        n_muestra <- sample_size()$Muestra
        datos <- data2()
        
        # Asegúrate de que hay datos para procesar
        if (is.null(datos) || is.null(n_muestra)) {
          return(NULL)
        }
        
        # Calcula las probabilidades de selección
        total_valor <- sum(datos[[input$variable2]], na.rm = TRUE)
        if (total_valor == 0) return(NULL)  # Evita división por cero
        
        prob_seleccion <- datos[[input$variable2]] / total_valor
        
        # Utiliza la semilla aleatoria para la selección de muestras
        set.seed(reactive_seed())
        
        # Selecciona las unidades de la muestra según sus probabilidades
        muestra_ids <- sample(
          x = seq_len(nrow(datos)), 
          size = n_muestra, 
          replace = FALSE, 
          prob = prob_seleccion
        )
        
        # Devuelve las filas seleccionadas para la muestra
        datos[muestra_ids, ]
      })
      
      output$sample_MUM <- renderReactable({
        req(Muestra())  # Asegúrate de que el objeto reactivo no sea NULL
        reactable(Muestra())  # Renderiza el objeto reactivo en una tabla
      })
      
      #################################################
      #    Comparación de datos originales y muestra  #
      #################################################
      
      output$comp_dist_MUM <- renderHighchart({
        # Asegúrate de que tanto los datos originales como la muestra estén disponibles
        req(data2(), Muestra(), input$variable2)
        
        # Calcular la densidad para los datos originales
        dens_orig <- density(data2()[[input$variable2]], na.rm = TRUE)
        dens_orig_df <- data.frame(x = dens_orig$x, y = dens_orig$y)
        
        # Calcular la densidad para la muestra
        dens_muestra <- density(Muestra()[[input$variable2]], na.rm = TRUE)
        dens_muestra_df <- data.frame(x = dens_muestra$x, y = dens_muestra$y)
        
        # Crear el gráfico de densidad comparativa
        highchart() %>%
          hc_add_series(name = "Datos Originales", data = list_parse(dens_orig_df), type = "area", color = "skyblue") %>%
          hc_add_series(name = "Muestra", data = list_parse(dens_muestra_df), type = "area", color = "green") %>%
          hc_tooltip(crosshairs = TRUE, valueDecimals = 1, shared = TRUE, borderWidth = 5) %>%
          hc_chart(zoomType = "xy") %>%
          hc_title(text = "Comparación de Densidades")  %>%
          hc_exporting(enabled = TRUE)
      })
      
      #################################
      #         Descargar muestra     #
      #################################
      
      observeEvent(input$show1_MUM, {
        
        showModal(modalDialog(
          title = "Descargar los datos ", br(),
          br(),
          downloadButton("download2.1",".csv file"),
          br(),
          br(),
          downloadButton("download2.2",".txt file"),
          br(),
          br(),
          downloadButton("download2.3",".xlsx file"),
          
          footer = modalButton("Close"),
          easyClose = TRUE)
        )
        
      })
      
      output$download2.1 <- downloadHandler(
        
        
        filename = function() {
          paste("Muestra_MUM-", Sys.Date(), ".csv", sep="")
        },
        
        content = function(file) {
          write.csv(Muestra(), file)
        }
      )
      
      output$download2.2 <- downloadHandler(
        
        filename = function() {
          paste("Muestra_MUM-", Sys.Date(), ".txt", sep="")
        },
        content = function(file) {
          write.table(Muestra(), file)
        }
      )
      
      output$download2.3 <- downloadHandler(
        filename = function() {
          paste("Muestra_MUM-", Sys.Date(), ".xlsx", sep="")
        },
        content = function(file) {
          # Suponiendo que Muestra() es una función que retorna el dataframe que quieres descargar
          write.xlsx(Muestra(), file)
        }
      )
      
      
    }
    
    
    # Ponerlo acá mejor la parte de reporte
    
    ################################################################################################
    ####                                    Reporte  del MUM                                       #
    ################################################################################################
    
    # Funcion de densidad 
    
    generarGraficoDensidadMUM <- function(datosOriginales, datosMuestra, variable) {
      p <- ggplot() +
        geom_density(data = datosOriginales, aes(x = variable), fill = "blue", alpha = 0.5) +
        geom_density(data = datosMuestra, aes(x = variable), fill = "lightgreen", alpha = 0.5) +
        labs(title = "Comparación entre datos Original vs Muestra",
             x = variable,
             y = "Densidad") +
        theme_minimal()
      
      return(p)
    }
    
    # Descarga del reporte MUM
    
    output$downloadReport2 <- downloadHandler(
      filename = function() {
        paste("Muestreo_MUM_", Sys.Date(), ".docx", sep = "")
      },
      content = function(file) {
        req(data2(), input$variable2, sample_size(), reactive_seed(), Muestra())
        
        # Crear un nuevo documento de Word
        doc <- read_docx()
        
        # Añadir título principal y otros detalles como antes...
        doc <- doc %>%
          body_add_par("Muestreo por Unidades Monetarias", style = "heading 1") %>%
          body_add_par("Parámetros", style = "heading 2") %>%
          body_add_par(paste("Nombre del archivo de datos:", input$file2$name), style = "Normal") %>%
          body_add_par(paste("Variable seleccionada:", input$variable2), style = "Normal") %>%
          body_add_par(paste("Error Tolerable:", input$freq1_MUM), style = "Normal") %>%
          body_add_par(paste("Error Esperado:", input$freq2_MUM), style = "Normal") %>%
          body_add_par(paste("Nivel de confianza:", input$freq3_MUM), style = "Normal") %>%
          body_add_par(paste("Selección de la distribución:", input$distri_1), style = "Normal") %>%
          body_add_par("Información de Muestreo", style = "heading 2")
        
        # Añadir "Tamaño de Muestra" y su valor en la misma línea
        doc <- doc %>%
          body_add_par(paste("Tamaño de Muestra:", as.character(sample_size()$Muestra)), style = "Normal")  %>%
          body_add_par(paste("Semilla para selección por PPT:", as.character(reactive_seed())), style = "Normal")
        
        
        
        # Generar y guardar el gráfico de densidad como imagen temporal
        datosOriginales <- data2()  # Asegúrate de que estos son los datos completos
        datosMuestra <- Muestra()  # Asegúrate de que estos son los datos de la muestra
        variable <- input$variable2
        
        grafico <- generarGraficoDensidadMUM(datosOriginales, datosMuestra, variable)
        rutaImagen <- tempfile(fileext = ".png")
        ggsave(rutaImagen, plot = grafico, width = 7, height = 5, dpi = 300)
        
        # Añadir el gráfico al documento
        doc <- doc %>%
          body_add_par("Gráfico comparativo entre valores originales y obtenidos por la muestra.", style = "heading 2") %>%
          body_add_img(src = rutaImagen, width = 7, height = 5)
        
        
        # Añadir la tabla
        
        # Añadir sección de "1.4 Muestra Seleccionada"
        doc <- doc %>%
          body_add_par("1.4 Muestra Seleccionada", style = "heading 2")
        
        # Asegúrate de que Muestra() devuelva un data frame
        datosMuestra <- Muestra()
        
        # Convertir datos de la muestra en una tabla de Word
        if (!is.null(datosMuestra)) {
          doc <- doc %>%
            body_add_table(datosMuestra, style = "table_template")  # Eliminado el argumento autofit
        } else {
          doc <- doc %>%
            body_add_par("No hay datos de muestra disponibles.", style = "Normal")
        }
        
        # Guardar el documento
        print(doc, target = file)
        
        # Limpiar eliminando la imagen temporal
        unlink(rutaImagen)
      }
    )
    
  })
  
  

  
  #################################################################################
  #################################################################################
  #                          Muestreo a Juicio  LES                               #
  #################################################################################
  #################################################################################
  
  ##############################
  #          Importar Datos    #
  ##############################
  
  # Data la volvemos un objeto reactivo 
  #  data ---> data()
  
  data3 <- reactive({
    inFile <- input$file3
    if (is.null(inFile)) {
      return(NULL)
    }
    
    ext <- tools::file_ext(inFile$datapath)
    
    # Dependiendo de la extensión del archivo, usar la función correspondiente
    switch(ext,
           csv = read.csv(inFile$datapath, stringsAsFactors = FALSE),
           txt = read.delim(inFile$datapath, stringsAsFactors = FALSE),
           xlsx = read_excel(inFile$datapath),
           stop("Tipo de archivo no soportado")
    )
  })
  
  output$variable_select_LES <- renderUI({
    if (is.null(data3())) {
      return(NULL)
    } else {
      selectInput("variable3", "Elija una variable:", names(data3()))
    }
  })
  
  
  ##################################################
  #         Alerta para valores negativos  en MUM  #
  ##################################################
  
  output$variable_select_LES <- renderUI({
    if (is.null(data3())) {
      return(NULL)
    } else {
      selectInput("variable3", "Elija una variable:", names(data3()))
    }
  })
  
  # Initialize a reactive value for tracking negative values in the Muestreo section
  has_negatives_muestreo_LES <- reactiveVal(FALSE)
  
  observe({
    # Ensure that the dataset and selected variable for Muestreo are available
    req(data3(), input$variable3)
    
    # Check for negative values in the selected variable for Muestreo
    if (any(data3()[[input$variable3]] < 0, na.rm = TRUE)) {
      has_negatives_muestreo_LES(TRUE)  # Update if negatives are found
    } else {
      has_negatives_muestreo_LES(FALSE)  # Update if no negatives are found
    }
  })
  
  # Create an output for the Muestreo negatives alert
  output$negativesAlertMuestreoLES <- renderUI({
    if (has_negatives_muestreo_LES()) {
      tags$div(class = "alert alert-danger", 
               strong("¡Se detectaron valores negativos!"), "No es posible proceder con el muestreo LES con valores o montos negativos. Procede a corregirlos para para poder proceder con el muestreo.")
    }
  })
  
  # Ensure the UI can always access this output
  outputOptions(output, 'negativesAlertMuestreoLES', suspendWhenHidden = FALSE)
  
  
  
  ##################################################
  #              Tablas de referencia              #
  ##################################################
  
  # Datos para la tabla de sugerencias de tamaño de muestra
  sugerencias_tamaño_2 <- data.frame(
    `Tamaño de Muestra` = c("Inferior (<=50)", "Entre (50-100)", "Superior (100)"),
    `Margen de Tolerancia (Tolerable)` = c("0.2 - 0.3", "0.03 - 0.05", "0.01 - 0.03"),
    `Error Esperado` = c("0.05 - 0.10", "0.02 - 0.05", "0.01 - 0.02"),
    `Nivel de Confianza` = c("0.90 - 0.95", "0.95 - 0.99", "> 0.99")
  )
  
  # Genera la tabla reactiva
  output$SugerenciasTamaño_LES <- renderReactable({
    reactable(sugerencias_tamaño_2, bordered = TRUE, highlight = TRUE)
    
    
  })
  
  
  
  #################################
  #   Condicional     #
  #################################
  
  output$hasNegatives_LES <- reactive({
    has_negatives_muestreo_LES()  # Esta es tu variable reactiva que ya tienes definida
  })
  shiny::outputOptions(output, "hasNegatives_LES", suspendWhenHidden = FALSE)
  
  observeEvent(input$update_LES, {
    if (input$freq2_LES >= input$freq1_LES) {
      showModal(modalDialog(
        title = "Advertencia",
        "No se procede con el análisis por la mala especificación de los parámetros de los errores tolerables y esperados. Especifique bien los valores. Recordar que el valor esperado siempre debe ser inferior al error tolerable.",
        easyClose = TRUE,
        footer = NULL
      ))
    } else {
      
  
  
  #################################
  #    Cálculo tamaño muestra     #
  #################################
  
  # Objeto reactivo para el tamaño de muestra
  sample_size <- reactiveVal()  # Inicializa como un valor reactivo
  
  observeEvent(input$update_LES, {  # Cuando 'update' se presiona, se ejecuta el código dentro de observeEvent
    stage1 <- planning(materiality = input$freq1_LES, 
                       expected = input$freq2_LES,
                       likelihood = input$distri_2, 
                       conf.level = input$freq3_LES
    )
    
    sample_size(data.frame(`Muestra` = stage1$n))  # Asigna el valor al reactivo
  })
  
  # Renderizar la tabla de tamaño de muestra
  output$SampleSize_LES <- renderReactable({
    req(sample_size())  # Asegúrate de que el valor reactivo no sea NULL
    reactable(sample_size())  # Renderiza el valor reactivo en una tabla
  })
  
  
  ######################################
  #    Selección unidades según LES    #
  ######################################
  
  reactive_seed <- reactiveVal()  # Inicializa como un valor reactivo
  
  observeEvent(input$update_LES, {
    seed_number <- sample(1:100000, 1)
    reactive_seed(seed_number)
    # ... [Resto del código para actualizar la muestra] ...
  })
  
  
  # Crear una tabla reactiva para mostrar la semilla
  output$seedvalue_LES <- renderReactable({
    req(reactive_seed())  # Asegúrate de que la semilla no sea NULL
    reactable(data.frame(`Semilla` = reactive_seed()))  # Muestra la semilla en una tabla
  })
  
  
  # Objeto reactivo para la selección de las unidades
  Muestra_2 <- reactive({
    req(input$update_LES)
    req(sample_size())
    req(data3())
    
    LES <- input$LES
    n_muestra <- sample_size()$Muestra
    datos <- data3()
    
    datos_mayores <- datos[datos[[input$variable3]] > LES, ]
    
    # Si hay más datos mayores que LES que el tamaño de muestra, selecciona los más grandes
    if (nrow(datos_mayores) > n_muestra) {
      datos_muestra <- head(datos_mayores[order(-datos_mayores[[input$variable3]]), ], n_muestra)
    } else {
      n_adicional <- n_muestra - nrow(datos_mayores)
      datos_menores <- datos[datos[[input$variable3]] <= LES, ]
      set.seed(reactive_seed())  # Usa la semilla aleatoria generada
      if (nrow(datos_menores) > 0 && n_adicional > 0) {
        ids_adicionales <- sample(nrow(datos_menores), n_adicional, replace = FALSE)
        datos_adicionales <- datos_menores[ids_adicionales, ]
        datos_muestra <- rbind(datos_mayores, datos_adicionales)
      } else {
        datos_muestra <- datos_mayores
      }
    }
    
    return(datos_muestra)
  })
  
  
  output$MuestraLES <- renderReactable({
    req(Muestra_2())
    reactable(Muestra_2())
  })
  
  #################################
  #    Valor de la semilla MUM    #
  #################################
  
  set.seed(Sys.time())
  
  # Función reactiva para generar y almacenar la semilla
  reactive_seed <- reactiveVal()  # Inicializa como un valor reactivo
  
  observeEvent(input$update_LES, {  # Actualiza la semilla cuando se presiona 'update'
    seed_number <- sample(1:100000, 1)  # Genera un número aleatorio entre 1 y 100000
    reactive_seed(seed_number)  # Asigna el número a reactive_seed
  })
  
  
  # Crear una tabla reactiva para mostrar la semilla
  output$seedvalue_LES <- renderReactable({
    req(reactive_seed())  # Asegúrate de que la semilla no sea NULL
    reactable(data.frame(`Semilla` = reactive_seed()))  # Muestra la semilla en una tabla
  })
  
  #################################
  #    Conteo de los valores LES  #
  #################################
  
  conteoLES <- reactive({
    req(input$update_LES)
    req(Muestra_2())
    
    LES <- input$LES
    muestra <- Muestra_2()
    
    conteo_mayores <- sum(muestra[[input$variable3]] > LES, na.rm = TRUE)
    conteo_menores <- sum(muestra[[input$variable3]] <= LES, na.rm = TRUE)
    
    data.frame(
      `Categoría` = c("Mayores que LES", "Menores o iguales a LES"),
      `Conteo` = c(conteo_mayores, conteo_menores)
    )
  })
  
  # Renderizar la tabla de conteo LES para la muestra seleccionada
  output$ConteoLes <- renderReactable({
    req(conteoLES())  # Asegúrate de que el objeto reactivo no sea NULL
    reactable(conteoLES())  # Renderiza el objeto reactivo en una tabla
  })
  
  
  #################################################
  #    Comparación de datos originales y muestra  #
  #################################################
  
  output$comp_dist_LES <- renderHighchart({
    # Asegúrate de que tanto los datos originales como la muestra estén disponibles
    req(data3(), Muestra_2(), input$variable3)
    
    # Calcular la densidad para los datos originales
    dens_orig <- density(data3()[[input$variable3]], na.rm = TRUE)
    dens_orig_df <- data.frame(x = dens_orig$x, y = dens_orig$y)
    
    # Calcular la densidad para la muestra
    dens_muestra <- density(Muestra_2()[[input$variable3]], na.rm = TRUE)
    dens_muestra_df <- data.frame(x = dens_muestra$x, y = dens_muestra$y)
    
    # Crear el gráfico de densidad comparativa
    highchart() %>%
      hc_add_series(name = "Datos Originales", data = list_parse(dens_orig_df), type = "area", color = "skyblue") %>%
      hc_add_series(name = "Muestra LES", data = list_parse(dens_muestra_df), type = "area", color = "green") %>%
      hc_tooltip(crosshairs = TRUE, valueDecimals = 1, shared = TRUE, borderWidth = 5) %>%
      hc_chart(zoomType = "xy") %>%
      hc_title(text = "Comparación de Densidades")  %>%
      hc_exporting(enabled = TRUE)
  })
  
  
  
  #################################################
  #     Descargar datos del muestreo por LES      #
  #################################################
  
  observeEvent(input$show1_LES, {
    
    showModal(modalDialog(
      title = "Descargar los datos ", br(),
      br(),
      downloadButton("download4.1",".csv file"),
      br(),
      br(),
      downloadButton("download4.2",".txt file"),
      br(),
      br(),
      downloadButton("download4.3",".xlsx file"),
      
      footer = modalButton("Close"),
      easyClose = TRUE)
    )
    
  })
  
  output$download4.1 <- downloadHandler(
    
    
    filename = function() {
      paste("MuestraLES-", Sys.Date(), ".csv", sep="")
    },
    
    content = function(file) {
      write.csv(Muestra_2(), file)
    }
  )
  
  output$download4.2 <- downloadHandler(
    
    filename = function() {
      paste("MuestraLES-", Sys.Date(), ".txt", sep="")
    },
    content = function(file) {
      write.table(Muestra_2(), file)
    }
  )
  
  output$download4.3 <- downloadHandler(
    filename = function() {
      paste("MuestraLES-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      # Suponiendo que Muestra() es una función que retorna el dataframe que quieres descargar
      write.xlsx(Muestra_2(), file)
    }
  )
  
  
  
  ###########################
  #    Reporte del MUM      #
  ###########################
  
  generarGraficoDensidadLES <- function(datosOriginales, datosMuestra, variable) {
    p <- ggplot() +
      geom_density(data = datosOriginales, aes(x = .data[[variable]]), fill = "blue", alpha = 0.5) +
      geom_density(data = datosMuestra, aes(x = .data[[variable]]), fill = "lightgreen", alpha = 0.5) +
      labs(title = "Comparación entre datos Original vs Muestra LES",
           x = variable,
           y = "Densidad") +
      theme_minimal()
    
    return(p)
  }
  
  
  output$downloadReport3 <- downloadHandler(
    filename = function() {
      paste("Muestreo_LES_", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      req(data3(), input$variable3, sample_size(), reactive_seed())
      
      # Iniciar un nuevo documento de Word
      doc <- read_docx()
      
      # Añadir título general y subtítulo con los parámetros
      doc <- doc %>%
        body_add_par("Muestreo LES", style = "heading 1") %>%
        body_add_par("Parámetros", style = "heading 2") %>%
        body_add_par(paste("Nombre del archivo de datos:", input$file3$name), style = "Normal") %>%
        body_add_par(paste("Variable seleccionada:", input$variable3), style = "Normal") %>%
        body_add_par(paste("Error Tolerable:", input$freq1_LES), style = "Normal") %>%
        body_add_par(paste("Error Esperado:", input$freq2_LES), style = "Normal") %>%
        body_add_par(paste("Nivel de confianza:", input$freq3_LES), style = "Normal") %>%
        body_add_par(paste("Selección de la distribución:", input$distri_2), style = "Normal")
      
      # Continuar añadiendo contenido al documento según sea necesario
      
      doc <- doc %>%
      body_add_par("Información de Muestreo", style = "heading 2") %>%
      body_add_par(paste("Tamaño de Muestra:", as.character(sample_size()$Muestra)), style = "Normal")  %>%
      body_add_par(paste("Semilla para selección aleatoria inferior al LES:", as.character(reactive_seed())), style = "Normal")
      
      ###############################
      #      Gráfico comparativo    #
      ###############################
      
      # Generar y guardar el gráfico de densidad como imagen temporal
      datosOriginales <- data3()  # Asegúrate de que estos son los datos completos
      datosMuestra <- Muestra_2()  # Asegúrate de que estos son los datos de la muestra
      variable <- input$variable3
      
      grafico <- generarGraficoDensidadLES(datosOriginales, datosMuestra, variable)
      rutaImagen <- tempfile(fileext = ".png")
      ggsave(rutaImagen, plot = grafico, width = 7, height = 5, dpi = 300)
      
      
      doc <- doc %>%
        body_add_par("Gráfico comparativo entre valores originales y obtenidos por la muestra.", style = "heading 2") %>%
        body_add_img(src = rutaImagen, width = 7, height = 5)
      
      ######################################################
      #    Generar la tabla con los datos de la muestre    #
      ######################################################
      
      doc <- doc %>%
        body_add_par("1.4 Muestra Seleccionada", style = "heading 2")
      
      # Asegúrate de que Muestra() devuelva un data frame
      datosMuestra2 <- Muestra_2()
      
      # Convertir datos de la muestra en una tabla de Word
      if (!is.null(datosMuestra2)) {
        doc <- doc %>%
          body_add_table(datosMuestra2, style = "table_template")  # Eliminado el argumento autofit
      } else {
        doc <- doc %>%
          body_add_par("No hay datos de muestra disponibles.", style = "Normal")
      }
      
      # Guardar el documento
      print(doc, target = file)
      
      # Limpiar eliminando la imagen temporal
      unlink(rutaImagen)
    }
  )
  
  
  
    }
    
    
    
  })
  
  ###########################################################################################################
  ###########################################################################################################
  #                                             Muestreo Atributos                                          #
  ###########################################################################################################
  ###########################################################################################################
  
  ##############################
  #          Importar Datos    #
  ##############################
  
  # Data la volvemos un objeto reactivo 
  #  data ---> data()
  
  data4 <- reactive({
    inFile <- input$file4
    if (is.null(inFile)) {
      return(NULL)
    }
    
    ext <- tools::file_ext(inFile$datapath)
    
    # Dependiendo de la extensión del archivo, usar la función correspondiente
    switch(ext,
           csv = read.csv(inFile$datapath, stringsAsFactors = FALSE),
           txt = read.delim(inFile$datapath, stringsAsFactors = FALSE),
           xlsx = read_excel(inFile$datapath),
           stop("Tipo de archivo no soportado")
    )
  })
  
  output$variable_select_Atri <- renderUI({
    if (is.null(data4())) {
      return(NULL)
    } else {
      selectInput("variable4", "Elija una variable:", names(data4()))
    }
  })
  
  ##################################################
  #              Tablas de referencia              #
  ##################################################
  
  # Datos para la tabla de sugerencias de tamaño de muestra
  sugerencias_tamaño_3 <- data.frame(
    `Tamaño de Muestra` = c("Inferior (<=50)", "Entre (50-100)", "Superior (100)"),
    `Margen de Tolerancia (Tolerable)` = c("0.2 - 0.3", "0.03 - 0.05", "0.01 - 0.03"),
    `Error Esperado` = c("0.05 - 0.10", "0.02 - 0.05", "0.01 - 0.02"),
    `Nivel de Confianza` = c("0.90 - 0.95", "0.95 - 0.99", "> 0.99")
  )
  
  # Genera la tabla reactiva
  output$SugerenciasTamaño_Atri <- renderReactable({
    reactable(sugerencias_tamaño_3, bordered = TRUE, highlight = TRUE)
    
    
  })
  

  
 
  
  observeEvent(input$update_Atri, {  # Cuando 'update' se presiona, se ejecuta el código dentro de observeEvent
    
    
    #################################
    #    Cálculo tamaño muestra     #
    #################################
    
    
    # Objeto reactivo para el tamaño de muestra
    sample_size <- reactiveVal()  # Inicializa como un valor reactivo
    
    
    
    
    stage1 <- planning(materiality = input$freq1_Atri, 
                       expected = input$freq2_Atri,
                       likelihood = input$distri_3, 
                       conf.level = input$freq3_Atri
    )
    
    sample_size(data.frame(`Muestra` = stage1$n))  # Asigna el valor al reactivo
    
    
    # Renderizar la tabla de tamaño de muestra
    output$SampleSize_Atri <- renderReactable({
      req(sample_size())  # Asegúrate de que el valor reactivo no sea NULL
      reactable(sample_size())  # Renderiza el valor reactivo en una tabla
    })
    
    
    
    ########################################
    #    Selección unidades por Atributos  #
    ########################################
    
    #################################
    #    Valor de la semilla Atri   #
    #################################
    
    set.seed(Sys.time())
    
    # Función reactiva para generar y almacenar la semilla
    reactive_seed <- reactiveVal()  # Inicializa como un valor reactivo
    
    observeEvent(input$update_Atri, {  # Actualiza la semilla cuando se presiona 'update'
      seed_number <- sample(1:100000, 1)  # Genera un número aleatorio entre 1 y 100000
      reactive_seed(seed_number)  # Asigna el número a reactive_seed
    })
    
    
    # Crear una tabla reactiva para mostrar la semilla
    output$seedvalue_Atri <- renderReactable({
      req(reactive_seed())  # Asegúrate de que la semilla no sea NULL
      reactable(data.frame(`Semilla` = reactive_seed()))  # Muestra la semilla en una tabla
    })
    
    #################################
    #   Selección de las unidades   #
    #################################
    
    MuestraAtri <- eventReactive(input$update_Atri, {
      # Asegura que los datos estén disponibles y que el tamaño de muestra sea válido
      req(data4(), sample_size())
      
      tamaño_muestra <- as.integer(sample_size())
      if(is.na(tamaño_muestra) || tamaño_muestra <= 0) {
        stop("El tamaño de la muestra no es válido")
      }
      
      set.seed(reactive_seed())
      
      data4() %>% sample_n(size = tamaño_muestra)
    })
    
    observeEvent(input$update_Atri, {
      # Asegura que los datos estén disponibles
      req(data4())
      
      output$tablaMuestraAtri <- renderReactable({
        reactable(MuestraAtri())
      })
      
    })
    
    ########################################
    #         Tabla por porcentajes        #
    ########################################
    
    # Tabla Original
    
    tablaOrigenPorce <- reactive({
      req(data4(), input$variable4) # Asegura que los datos y la variable seleccionada estén disponibles
      
      datos <- data4() %>%
        group_by(Categoria = .data[[input$variable4]]) %>%
        tally(name = "Total") %>% # Utiliza tally() para contar las observaciones
        mutate(Porcentaje = (Total / sum(Total)) * 100) %>%
        ungroup() # Retira el agrupamiento
      
      return(datos)
    })
    
    # Tabla de la muestra
    
    tablaMuestraPorce <- reactive({
      req(MuestraAtri(), input$variable4) # Asegúrate de que la muestra y la variable están disponibles
      
      datosMuestra <- MuestraAtri() %>%
        group_by(Categoria = .data[[input$variable4]]) %>%
        tally(name = "Total") %>%
        mutate(Porcentaje = round((Total / sum(Total)) * 100, digits = 1)) %>%
        ungroup() # No se necesita renombrar aquí, ya se ha asignado el nombre 'Categoria'
      
      return(datosMuestra)
    })
    

  #############################
  #  Gráfico comparativo      #
  #############################
    
    
      output$graficoComparativo2 <- renderHighchart({
        # Requerimos que ambas tablas estén disponibles
        req(tablaOrigenPorce(), tablaMuestraPorce())
        
        # Obtenemos los datos de las tablas reactivas
        datosOrigen <- tablaOrigenPorce()
        datosMuestra <- tablaMuestraPorce()
        
        # Unimos los datos por la columna 'Categoria'
        datosCombinados <- merge(datosOrigen, datosMuestra, by = "Categoria", all = TRUE)
        
        # Creamos el gráfico con Highcharter
        highchart() %>%
          hc_chart(type = "bar") %>%
          hc_title(text = "Comparación de Porcentajes por Categoría") %>%
          hc_xAxis(categories = datosCombinados$Categoria) %>%
          hc_yAxis(title = list(text = "Porcentaje")) %>%
          hc_add_series(name = "Original", data = datosCombinados$Porcentaje.x) %>%
          hc_add_series(name = "Muestra", data = datosCombinados$Porcentaje.y) %>%
          hc_plotOptions(series = list(dataLabels = list(enabled = TRUE, format = '{y}%'))) %>%
          hc_tooltip(shared = TRUE, pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y}%</b><br/>') %>%
          hc_legend(enabled = TRUE) %>%
          hc_exporting(enabled = TRUE)
      })

    
    #################################################
    #     Descargar datos del muestreo por Atri      #
    #################################################
    
    observeEvent(input$show1_Atri, {
      
      showModal(modalDialog(
        title = "Descargar los datos ", br(),
        br(),
        downloadButton("download5.1",".csv file"),
        br(),
        br(),
        downloadButton("download5.2",".txt file"),
        br(),
        br(),
        downloadButton("download5.3",".xlsx file"),
        
        footer = modalButton("Close"),
        easyClose = TRUE)
      )
      
    })
    
    output$download5.1 <- downloadHandler(
      
      
      filename = function() {
        paste("MuestraAtributo-", Sys.Date(), ".csv", sep="")
      },
      
      content = function(file) {
        write.csv(MuestraAtri(), file)
      }
    )
    
    output$download5.2 <- downloadHandler(
      
      filename = function() {
        paste("MuestraAtributo-", Sys.Date(), ".txt", sep="")
      },
      content = function(file) {
        write.table(MuestraAtri(), file)
      }
    )
    
    output$download5.3 <- downloadHandler(
      filename = function() {
        paste("MuestraAtributo-", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        # Suponiendo que Muestra() es una función que retorna el dataframe que quieres descargar
        write.xlsx(MuestraAtri(), file)
      }
    )
    
    #################################################
    #     Reporte del Muestreo por Atributos       #
    #################################################

    generarGraficoPorcentajesAtri <- function(datosOriginales, datosMuestra) {
      ggplot() +
        geom_bar(data = datosOriginales, aes(x = Categoria, y = Porcentaje, fill = "Original"), 
                 stat = "identity", position = position_dodge(width = 0.8), width = 0.35) +
        geom_bar(data = datosMuestra, aes(x = Categoria, y = Porcentaje, fill = "Muestra"), 
                 stat = "identity", position = position_dodge(width = 0.8), width = 0.35) +
        scale_fill_manual(values = c("Original" = "lightblue", "Muestra" = "lightgreen")) +
        labs(title = "Comparación de Porcentajes por Categoría", x = "Categoría", y = "Porcentaje") +
        theme_minimal() +
        coord_flip() + # Para hacer las barras horizontales
        theme(legend.position = "bottom") # Para mover la leyenda al fondo
    }
    
    
    
    output$downloadReport4 <- downloadHandler(
      filename = function() {
        paste("Muestreo_Atributos_", Sys.Date(), ".docx", sep = "")
      },
      content = function(file) {
        req(data4(), input$variable4, sample_size(), reactive_seed(), MuestraAtri())
        
        # Iniciar un nuevo documento de Word
        doc <- read_docx()
        
        # Añadir título general y subtítulo con los parámetros
        doc <- doc %>%
          body_add_par("Muestreo Atributos", style = "heading 1") %>%
          body_add_par("Parámetros", style = "heading 2") %>%
          body_add_par(paste("Nombre del archivo de datos:", input$file4$name), style = "Normal") %>%
          body_add_par(paste("Variable seleccionada:", input$variable4), style = "Normal") %>%
          body_add_par(paste("Error Tolerable:", input$freq1_Atri), style = "Normal") %>%
          body_add_par(paste("Error Esperado:", input$freq2_Atri), style = "Normal") %>%
          body_add_par(paste("Nivel de confianza:", input$freq3_Atri), style = "Normal") %>%
          body_add_par(paste("Selección de la distribución:", input$distri_3), style = "Normal")
        
        # Continuar añadiendo contenido al documento según sea necesario
        
        
        
        doc <- doc %>%
          body_add_par("Información de Muestreo", style = "heading 2") %>%
          body_add_par(paste("Tamaño de Muestra:", as.character(sample_size()$Muestra)), style = "Normal")  %>%
          body_add_par(paste("Semilla para selección aleatoria:", as.character(reactive_seed())), style = "Normal")
        
        ###############################
        #      Gráfico comparativo    #
        ###############################
        
        # Generar gráfico comparativo
        datosOrigen <- tablaOrigenPorce()
        datosMuestra <- tablaMuestraPorce()
        grafico <- generarGraficoPorcentajesAtri(datosOrigen, datosMuestra)
        rutaImagen <- tempfile(fileext = ".png")
        ggsave(rutaImagen, plot = grafico, width = 8, height = 6, dpi = 300)

        
        doc <- doc %>%
          body_add_par("Gráfico comparativo entre valores originales y obtenidos por la muestra.", style = "heading 2") %>%
          body_add_img(src = rutaImagen, width = 8, height = 6)
        
        ######################################################
        #    Generar la tabla con los datos de la muestre    #
        ######################################################
        
        doc <- doc %>%
          body_add_par("Muestra Seleccionada", style = "heading 2")
        
        datosMuestra <- MuestraAtri()  # Asegúrate de que estos son los datos de la muestra
        
        
        # Convertir los datos de la muestra en una tabla de Word
        if (!is.null(datosMuestra) && nrow(datosMuestra) > 0) {
          doc <- doc %>%
            body_add_table(value = datosMuestra, style = "table_template")
        } else {
          doc <- doc %>%
            body_add_par("No hay datos de muestra disponibles.", style = "Normal")
        }
        
        
        # Guardar el documento
        print(doc, target = file)
        
        # Limpiar eliminando la imagen temporal
        unlink(rutaImagen)
        

      }
    )
    
    
    
  })
  
  
  ###########################################################################################################
  ###########################################################################################################
  #                                               Evaluación                                                #
  ###########################################################################################################
  ###########################################################################################################
  
  ###### Datos a reactive 
  
  data5 <- reactive({
    inFile <- input$file5
    if (is.null(inFile)) {
      return(NULL)
    }
    
    ext <- tools::file_ext(inFile$datapath)
    
    # Dependiendo de la extensión del archivo, usar la función correspondiente
    switch(ext,
           csv = read.csv(inFile$datapath, stringsAsFactors = FALSE),
           txt = read.delim(inFile$datapath, stringsAsFactors = FALSE),
           xlsx = read_excel(inFile$datapath),
           stop("Tipo de archivo no soportado")
    )
    
  })
  
  
  ###### Variables
  
  output$var1 <- renderUI({
    selectInput("select_var1", "Seleccione Variable 1: Observado", names(data5()))
  })
  
  output$var2 <- renderUI({
    selectInput("select_var2", "Seleccione Variable 2: Auditado", names(data5()))
  })
  
  ##### Crear data frame reactive ---> DatosEval 
  
  DatosEval <- reactive({
    req(data5(), input$select_var1, input$select_var2)  # Asegúrate de que los datos y las entradas estén disponibles
    
    data5() %>%
      dplyr::rename(
        Observado = input$select_var1,
        Auditado = input$select_var2
      )
  })
  
  Diferencias <- reactive({
    datos <- DatosEval()  # Asume que esta función devuelve tus datos
    diferencias <- datos %>%
      mutate(Diferencia = abs(Observado - Auditado)) %>%
      filter(Diferencia != 0) %>%
      arrange(desc(Diferencia))
    diferencias  # Devuelve el dataframe resultante
  })
  
  
  ###### Aplicar el boton activar 
  
  ##################################################
  # Acá el ObserveEvent
  ###################################################
  
  
  
  observeEvent(input$analizar, {
  
    
    
    # Dataframe de los datos 
    
    output$Tabla2 <- renderReactable({
      
      reactable(DatosEval())
      
    })
    
    # Scatter plot  de  datos de evaluación
    
    output$ScatterPlot <- renderHighchart({
      hc <- DatosEval() %>% 
        hchart('scatter', hcaes(x = Observado, y = Auditado)) %>% 
        hc_add_series(data = list_parse(data.frame(x = c(0, max(DatosEval()$Observado)), 
                                                   y = c(0, max(DatosEval()$Observado)))), 
                      type = 'line', name = 'y = x') %>% 
        
        hc_chart(zoomType = "xy")  %>%
        hc_exporting(enabled = TRUE)
      hc 
    })
    
    # Evaluación únicamente de las diferencias
    

    
    output$Tabla3 <- renderReactable({
      
      reactable(Diferencias()) 
    })
    
    # Evaluación únicamente de las diferencias
    
    output$Riesgo <- renderReactable({
      
      Riesgo <- DatosEval() %>%
        mutate(Diferencia = abs(Observado - Auditado)) %>%
        filter(Diferencia != 0) %>% 
        arrange(desc(Diferencia))
      
      reactable(Riesgo)
    })
    
    
    ####################################
    #         Descargar diferencias    #
    ####################################
    
    observeEvent(input$show2, {
      
      showModal(modalDialog(
        title = "Descargar las diferencias ", br(),
        br(),
        downloadButton("download3.1",".csv file"),
        br(),
        br(),
        downloadButton("download3.2",".txt file"),
        br(),
        br(),
        downloadButton("download3.3",".xlsx file"),
        
        footer = modalButton("Close"),
        easyClose = TRUE)
      )
      
    })
    
    output$download3.1 <- downloadHandler(
      
      
      filename = function() {
        paste("Diferencias-", Sys.Date(), ".csv", sep="")
      },
      
      content = function(file) {
        write.csv(Diferencias(), file)
      }
    )
    
    output$download3.2 <- downloadHandler(
      
      filename = function() {
        paste("Diferencias-", Sys.Date(), ".txt", sep="")
      },
      content = function(file) {
        write.table(Diferencias(), file)
      }
    )
    
    output$download3.3 <- downloadHandler(
      filename = function() {
        paste("Diferencias-", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        # Suponiendo que Muestra() es una función que retorna el dataframe que quieres descargar
        write.xlsx(Diferencias(), file)
      }
    )
    
    
    
    
    # Creación de la función de indicadores de riesgo
    
    IndicadoresRiesgo <- function(datos) {
      suma_obs <- round(sum(datos$Observado, na.rm = TRUE), 1)
      suma_aud <- round(sum(datos$Auditado, na.rm = TRUE), 1)
      n_obs <- nrow(datos)
      n_aud <- nrow(datos)
      promedio_obs <- round(mean(datos$Observado, na.rm = TRUE), 1)
      promedio_aud <- round(mean(datos$Auditado, na.rm = TRUE), 1)
      conteo_dif <- sum(datos$Observado != datos$Auditado, na.rm = TRUE)
      sobrevaloraciones <- sum(datos$Observado > datos$Auditado, na.rm = TRUE)
      infravaloraciones <- sum(datos$Observado < datos$Auditado, na.rm = TRUE)
      suma_sobrevaloraciones <- round(sum(datos$Observado[datos$Observado > datos$Auditado] - datos$Auditado[datos$Observado > datos$Auditado], na.rm = TRUE), 1)
      suma_infravaloraciones <- round(sum(datos$Auditado[datos$Observado < datos$Auditado] - datos$Observado[datos$Observado < datos$Auditado], na.rm = TRUE), 1)
      dif_total <- round(sum(abs(datos$Observado - datos$Auditado), na.rm = TRUE), 1)
      porcentaje_dif <- round((dif_total / suma_aud) * 100, 1)
      
      data.frame(
        Indicador = c("Suma total Observados", "Suma total Auditados", "n Observados", "n Auditados", 
                      "Monto promedio Observado", "Monto promedio Auditado", "Conteo Observados vs Auditado",
                      "Cantidad de sobrevaloraciones", "Cantidad de infravaloraciones",
                      "Diferencia total Observados y Auditados", "Suma de sobrevaloraciones", 
                      "Suma de infravaloraciones", "Porcentaje de diferencia"),
        Valor = c(suma_obs, suma_aud, n_obs, n_aud, promedio_obs, promedio_aud, conteo_dif, 
                  sobrevaloraciones, infravaloraciones, dif_total, suma_sobrevaloraciones, 
                  suma_infravaloraciones, porcentaje_dif)
      )
    }
    
    # Tabla de los indicadores de riesgo  
    
    output$Riesgo <- renderReactable({
      req(DatosEval())  # Asegúrate de que DatosEval esté disponible
      tabla_riesgo <- IndicadoresRiesgo(DatosEval())
      reactable(tabla_riesgo)
    })
    
    output$ScatterPlot_limit <- renderHighchart({
      req(DatosEval())  # Asegúrate de que los datos están disponibles
      
      # Calcular los límites de confianza
      std_dev <- sd(DatosEval()$Observado - DatosEval()$Auditado, na.rm = TRUE)
      limite_inferior <- -1.96 * std_dev
      limite_superior <- 1.96 * std_dev
      
      # Crear el gráfico de dispersión base
      hc <- DatosEval() %>% 
        hchart('scatter', hcaes(x = Observado, y = Auditado)) %>% 
        hc_add_series(data = list_parse(data.frame(x = c(0, max(DatosEval()$Observado)), 
                                                   y = c(0, max(DatosEval()$Observado)))), 
                      type = 'line', name = 'y = x')  %>%
        hc_add_series(data = list_parse(data.frame(x = c(0, max(DatosEval()$Observado)), 
                                                   y = c(limite_inferior, limite_inferior + max(DatosEval()$Observado)))), 
                      type = 'line', name = 'Límite Inferior' , color = "blue") %>%
        hc_add_series(data = list_parse(data.frame(x = c(0, max(DatosEval()$Observado)), 
                                                   y = c(limite_superior, limite_superior + max(DatosEval()$Observado)))), 
                      type = 'line', name = 'Límite Superior', color = "blue") %>% 
        
        hc_chart(zoomType = "xy") %>%
        hc_exporting(enabled = TRUE)
      
      hc
    })
    
    calculaIndicadoresEvaluacion <- function(datos) {
      # Monto en diferencia total Observados y Auditados
      monto_diferencia_total <- sum(abs(datos$Observado - datos$Auditado))
      
      # Porcentaje de diferencia
      porcentaje_diferencia <- (monto_diferencia_total / sum(datos$Auditado)) * 100
      
      # Conteo Observados vs Auditado
      conteo_diferencias <- sum(datos$Observado != datos$Auditado)
      
      # Casos que son superiores o inferiores a los límites de confianza
      std_dev <- sd(datos$Observado - datos$Auditado)
      limite_inferior <- -1.96 * std_dev
      limite_superior <- 1.96 * std_dev
      casos_fuera_limites <- sum(datos$Observado - datos$Auditado < limite_inferior | 
                                   datos$Observado - datos$Auditado > limite_superior)
      
      data.frame(
        Indicador = c("Monto Diferencia Total", "Porcentaje de Diferencia", 
                      "Conteo Diferencias", "Casos Fuera de Límites"),
        Valor = round(c(monto_diferencia_total, porcentaje_diferencia, 
                        conteo_diferencias, casos_fuera_limites), 2)
      )
    }
    
    
    ##################################################
    ###########Tabla final de evaluación   ###########
    ##################################################
    
    # Define la función para calcular los umbrales de decisión fuera del observeEvent
    calculaIndicadoresDecision <- function(datos, monto_maximo, porcentaje_umbral, conteo_umbral, casos_umbral) {
     
      if (is.null(datos$Observado) || is.null(datos$Auditado)) {
        stop("Los datos de 'Observado' o 'Auditado' están vacíos o son NULL.")
      }
      
      monto_diferencia_total <- sum(abs(datos$Observado - datos$Auditado), na.rm = TRUE)
      if (length(monto_diferencia_total) != 1) {
        stop("El cálculo de 'monto_diferencia_total' no devolvió un único valor.")
      }
      
       monto_diferencia_total <- round(sum(abs(datos$Observado - datos$Auditado)), 1)
      porcentaje_diferencia <- round((monto_diferencia_total / sum(datos$Auditado)) * 100, 1)
      conteo_diferencias <- sum(datos$Observado != datos$Auditado)
      
      std_dev <- sd(datos$Observado - datos$Auditado, na.rm = TRUE)
      limite_inferior <- -1.96 * std_dev
      limite_superior <- 1.96 * std_dev
      casos_fuera_limites <- sum(datos$Observado - datos$Auditado < limite_inferior | datos$Observado - datos$Auditado > limite_superior)
      
      valores <- c(monto_diferencia_total, porcentaje_diferencia, conteo_diferencias, casos_fuera_limites)
      umbrales <- c(monto_maximo, porcentaje_umbral, conteo_umbral, casos_umbral)
      decisiones <- ifelse(valores <= umbrales, "Aceptable", "No Aceptable")
      
      data.frame(
        Indicador = c("Monto Diferencia Total", "Porcentaje de Diferencia", "Conteo Diferencias", "Casos Fuera de Límites"),
        Valor = c(monto_diferencia_total, porcentaje_diferencia, conteo_diferencias, casos_fuera_limites),
        Umbral = c(monto_maximo, porcentaje_umbral, conteo_umbral, casos_umbral),
        Decision = decisiones
      )
    }
    
    
    # En tu server.R, define la variable reactiva en la parte superior
    tablaDecisionReactiva <- reactiveVal()
    
    
    # Observa cuando se presiona el botón de "Evaluación"
    observeEvent(input$auditEval, {
      req(data5())  # Asegúrate de que los datos necesarios están cargados
      
      # Asegúrate de que los umbrales son números válidos
      monto_maximo <- as.numeric(input$monto_maximo)
      porcentaje_umbral <- as.numeric(input$porcentaje_umbral)
      conteo_umbral <- as.numeric(input$conteo_umbral)
      casos_umbral <- as.numeric(input$casos_umbral)
      
      # Calcula la tabla de decisión usando la función definida
      tabla_decision <- calculaIndicadoresDecision(
        DatosEval(),
        monto_maximo,
        porcentaje_umbral,
        conteo_umbral,
        casos_umbral
      )
      
      # Almacena la tabla de decisión en la variable reactiva
      tablaDecisionReactiva(tabla_decision)
      
      # Forzar actualización de la tabla en la UI
      output$Eval <- renderReactable({
        reactable(tabla_decision)
      })
    })
    
    ####################################################################
    #                     Reporte de la evaluación                     #
    ####################################################################
    
    
    EvalScatterPlot <- function(datos) {
      p <- ggplot(datos, aes(x = Observado, y = Auditado)) +
        geom_point(color = "blue") +
        geom_smooth(method = "lm", color = "red") +
        labs(title = "Gráfico de dispersión entre los valores Observados vs Auditados.",
             x = "Observado",
             y = "Auditado") +
        theme_minimal()
      return(p)
    }
    
    ScatterPlotLim <- function(datos) {
      # Asegurarse de que 'datos' es un data frame y contiene las columnas 'Observado' y 'Auditado'
      if (!("Observado" %in% names(datos) && "Auditado" %in% names(datos))) {
        stop("Los datos deben contener las columnas 'Observado' y 'Auditado'.")
      }
      
      # Calcular los límites de confianza
      diferencia <- datos$Observado - datos$Auditado
      std_dev <- sd(diferencia, na.rm = TRUE)
      mean_diff <- mean(diferencia, na.rm = TRUE)
      
      # Coeficientes de la línea y = x
      intercept <- 0
      slope <- 1
      
      # Calcular interceptos para las líneas de límite
      intercept_inf <- intercept + (mean_diff - 1.96 * std_dev)
      intercept_sup <- intercept + (mean_diff + 1.96 * std_dev)
      
      # Crear el gráfico
      p <- ggplot(datos, aes(x = Observado, y = Auditado)) +
        geom_point(color = "blue") +
        geom_abline(intercept = intercept, slope = slope, linetype = "dashed", color = "red") +
        geom_abline(intercept = intercept_inf, slope = slope, linetype = "dashed", color = "darkgreen") +
        geom_abline(intercept = intercept_sup, slope = slope, linetype = "dashed", color = "darkgreen") +
        labs(title = "Scatter Plot con Límites de Confianza",
             x = "Observado",
             y = "Auditado") +
        theme_minimal()
      
      return(p)
    }
    
    output$downloadReport5 <- downloadHandler(
      filename = function() {
        paste("Evaluacion_Auditoria_", Sys.Date(), ".docx", sep = "")
      },
      content = function(file) {
        req(data5(), input$select_var1, input$select_var2, DatosEval(), Diferencias(), tablaDecisionReactiva())
        
        # Iniciar un nuevo documento de Word
        doc <- read_docx()
        
        # Añadir título y subtítulos
        doc <- doc %>%
          body_add_par("Evaluación", style = "heading 1") %>%
          body_add_par("Parámetros", style = "heading 2") %>%
          body_add_par(paste("Nombre del archivo:", input$file5$name), style = "Normal") %>%
          body_add_par(paste("Variable Observada:", input$select_var1), style = "Normal") %>%
          body_add_par(paste("Variable Auditada:", input$select_var2), style = "Normal")
        
        # Añadir título y subtítulos
        doc <- doc %>%
          body_add_par("Evaluación", style = "heading 1") %>%
          body_add_par("1. Análisis de Diferencias", style = "heading 2")
        
        # Generar el gráfico y guardarlo en un archivo temporal
        datos_para_grafico <- DatosEval() # Asegúrate de que esta función devuelve tus datos de forma correcta
        p <- EvalScatterPlot(datos_para_grafico)
        ruta_imagen <- tempfile(fileext = ".png")
        ggsave(ruta_imagen, plot = p, width = 7, height = 5, dpi = 300)
        
        
        # Aquí se agregan las partes del documento...
        
        # Añadir la tabla de diferencias si es que hay alguna
        if (!is.null(Diferencias()) && nrow(Diferencias()) > 0) {
          doc <- doc %>%
            body_add_par("1. Análisis de Diferencias", style = "heading 2") %>%
            body_add_table(Diferencias(), style = "table_template")
        }
        
        # Generar y añadir el primer gráfico de dispersión
        datos_para_grafico <- DatosEval()  # Asegúrate de que esta función devuelve tus datos de forma correcta
        p <- EvalScatterPlot(datos_para_grafico)
        ruta_imagen <- tempfile(fileext = ".png")
        ggsave(ruta_imagen, plot = p, width = 7, height = 5, dpi = 300)
        
        # Añadir el gráfico de dispersión
        doc <- doc %>%
          body_add_par("Gráfico de dispersión entre los valores Observados vs Auditados.", style = "heading 2") %>%
          body_add_img(src = ruta_imagen, width = 7, height = 5)
        
        # Añadir sección de Indicadores de Riesgo
        doc <- doc %>%
          body_add_par("3. Indicadores de Riesgo", style = "heading 2") %>%
          body_add_table(IndicadoresRiesgo(DatosEval()), style = "table_template")
        
        # Generar y añadir el segundo gráfico de dispersión con límites de confianza
        q <- ScatterPlotLim(DatosEval())
        ruta_imagen2 <- tempfile(fileext = ".png")
        ggsave(ruta_imagen2, plot = q, width = 7, height = 5, dpi = 300)
        
        # Añadir el gráfico de dispersión con límites de confianza
        doc <- doc %>%
          body_add_par("Scatter Plot con Límites de Confianza", style = "heading 2") %>%
          body_add_img(src = ruta_imagen2, width = 7, height = 5)
        
        # Añade la tabla de decisión almacenada en la variable reactiva
        doc <- doc %>%
          body_add_par("3. Criterio empírico de la evaluación de la auditoría", style = "heading 2") %>%
          body_add_table(tablaDecisionReactiva(), style = "table_template")
        
        # Guardar el documento
        print(doc, target = file)
        
        # Limpiar eliminando las imágenes temporales
        unlink(ruta_imagen)
        unlink(ruta_imagen2)
      }
    )
    
    
 
    
    
    ##################################################
    #   Hasta acá el Observt Event
    ##################################################
    
    
  })
  
 
  

  
}