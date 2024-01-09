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
    
    if (is.null(inFile)) return(NULL)
    
    ext <- tools::file_ext(inFile$name)
    
    if (ext == "xlsx") {
      openxlsx::read.xlsx(inFile$datapath)
    } else if (ext == "csv") {  # Corregido aquí
      read.csv(inFile$datapath, stringsAsFactors = FALSE)
    } else if (ext == "txt") {  # Y aquí
      read.table(inFile$datapath, stringsAsFactors = FALSE, header = TRUE)
    } else {
      stop("Archivo no soportado.")
    }
  })
  
  # Selección de la variable
  
  output$variable_select_1 <- renderUI({
    if (is.null(data1())) {
      return(NULL)
    } else {
      selectInput("variable1", "Elija una variable:", names(data1()))
    }
  })
  
  
  # Histograma de una variable #
    
  output$histogram1 <- renderHighchart({
    
    req(input$variable1)
    
    datos <- as.data.frame(data1()[[input$variable1]])  %>% 
      dplyr::rename(
        Monto = `data1()[[input$variable1]]`
        
      )
    
    x = datos$Monto
    
   hist <-  hchart(density(x), type = "area", color = "skyblue", name = "Monto")  %>%
                
     hc_tooltip(crosshairs = T,valueDecimals = 1, shared = TRUE, borderWidth = 5) %>%
     hc_chart(
       zoomType = "xy"
     )
    
    
   hist 
   
    #  ggplot(data1(), aes_string(input$variable1)) + geom_histogram(binwidth = 10) + labs(title = paste("Distribución de frecuencia.", input$variable), x = input$variable, y = "Frecuencia")
  })
  

  # Tabla de estadísticas descriptivas #
  
  output$stats <- renderReactable({
    
    req(input$variable1)
    
    Datos <- as.data.frame(data1()[[input$variable1]])  %>% 
                                              dplyr::rename(
                                                Monto = `data1()[[input$variable1]]`
                                                
                                              )
    
    
    Stats <- Datos %>%
      
                             summarise(
                                         Minimo = min(Monto, na.rm = TRUE),
                                         Maximo = max(Monto, na.rm = TRUE),
                                         Promedio = mean(Monto, na.rm = TRUE),
                                          Mediana = median(Monto, na.rm = TRUE),
                                         Moda = as.numeric(names(sort(table(Monto), decreasing = TRUE)[1])),
                                          DesviacionEstandar = sd(Monto, na.rm = TRUE),
                                          Percentil10 = quantile(Monto, 0.1, na.rm = TRUE),
                                          Percentil25 = quantile(Monto, 0.25, na.rm = TRUE),
                                           Percentil75 = quantile(Monto, 0.75, na.rm = TRUE),
                                           Percentil90 = quantile(Monto, 0.9, na.rm = TRUE),
                                          ValoresNegativos = sum(Monto < 0, na.rm = TRUE),
                                             ValoresFaltantes = sum(is.na(Monto)) 
      ) %>% pivot_longer(
        cols = everything(),
        names_to = "Medida",
        values_to = "Valor"
      )  %>%
               mutate(Valor = round(Valor, 1))
      
    
        
        reactable(Stats)

  })
  
  
  # Función ajustar distribuciones y calcular AIC #
  

  
  #################################################################################
  #################################################################################
  #                                  Muestreo                                     #
  #################################################################################
  #################################################################################
  
  
  ##############################
  #          Importar Datos    #
  ##############################
  
  # Data la volvemos un objeto reactivo 
  #  data ---> data()
  
  data <- reactive({
    inFile <- input$file2
    if (is.null(inFile)) {
      return(NULL)
    }
    read.csv(inFile$datapath)
  })
  
  
  # Histograma de una variable #
  
  output$variable_select <- renderUI({
    if (is.null(data())) {
      return(NULL)
    } else {
      selectInput("variable", "Elija una variable:", names(data()))
    }
  })
  
  # output$histogram2 <- renderPlot({
  #  if (is.null(input$variable)) {
  #    return(NULL)
  #  }
  #  ggplot(data(), aes_string(input$variable)) + geom_histogram(binwidth = 10) + labs(title = paste("Distribución de", input$variable), x = input$variable, y = "Frecuencia")
  # })
  
  
  #################################
  #    Cálculo tamaño muestra     #
  #################################
  
  output$SampleSize <- renderReactable({
    
    if (input$update) {
      
      stage1 <- planning(materiality = input$freq1, 
                         expected = input$freq2,
                         likelihood = input$distri, 
                         conf.level = input$freq3
      )
      
      sample_size <- data.frame(`Muestra` = stage1$n)
      reactable(sample_size)
      
    } else {
      NULL
    }
    
  })
  
  #################################
  #    Visualización de la tabla  #
  #################################
  
  
  output$sample  <- renderReactable({
    
    if (input$update) {
      
      stage1 <- planning(materiality = input$freq1, 
                         expected = input$freq2,
                         likelihood = input$distri, 
                         conf.level = input$freq3
      )
      
      stage2 <- selection(
        data = data(), 
        size = stage1,      #### Stage1 previous defined 
        units = "values", 
        values = input$variable,   #### Column from  data
        method = "random", start = 2
      )
      
      sample <- stage2[["sample"]]
      
      reactable(sample)
      
    } else {
      NULL
    }
    
  })
  
  
  ########################
  #  Datos Reactive      #
  ########################
  
  Muestra <- reactive({
    
    stage1 <- planning(materiality = input$freq1, 
                       expected = input$freq2,
                       likelihood = input$distri, 
                       conf.level = input$freq3
    )
    
    stage2 <- selection(
      data = data(), 
      size = stage1,      #### Stage1 previous defined 
      units = "values", 
      values = input$variable,   #### Column from  data
      method = "random", start = 2
    )
    
    sample <- stage2[["sample"]]
    sample
  })
  
  
  #################################
  #         Descargar muestra     #
  #################################
  
  observeEvent(input$show1, {
    
    showModal(modalDialog(
      title = "Descargar los datos ", br(),
      br(),
      downloadButton("download2.1",".csv file"),
      br(),
      br(),
      downloadButton("download2.2",".txt file"),
      footer = modalButton("Close"),
      easyClose = TRUE)
    )
    
  })
  
  output$download2.1 <- downloadHandler(
    
    
    filename = function() {
      paste("Muestra-", Sys.Date(), ".csv", sep="")
    },
    
    content = function(file) {
      write.csv(Muestra(), file)
    }
  )
  
  output$download2.2 <- downloadHandler(
    
    filename = function() {
      paste("Muestra-", Sys.Date(), ".txt", sep="")
    },
    content = function(file) {
      write.table(Muestra(), file)
    }
  )
  
  
  #################################################################################
  #################################################################################
  #                                  Evaluación                                   #
  #################################################################################
  #################################################################################
  
  data2 <- reactive({
    inFile <- input$file3
    if (is.null(inFile)) {
      return(NULL)
    }
    read.csv(inFile$datapath)
  })
  
  output$var1 <- renderUI({
    selectInput("select_var1", "Seleccione Variable 1:", names(data2()))
  })
  
  output$var2 <- renderUI({
    selectInput("select_var2", "Seleccione Variable 2:", names(data2()))
  })
  
  observeEvent(input$analizar, {
    output$plotCorrelation <- renderPlot({
      plot(data2()[,input$select_var1], data2()[,input$select_var2], 
           main=paste("Correlación entre", input$select_var1, "y", input$select_var2), 
           xlab=input$select_var1, ylab=input$select_var2)
    })
  })
  
  
  # Bivariado con HighCharter #
  
  output$scatter <- renderHighchart({
    
    
    req(input$select_var1)
    req(input$select_var2)
    
    Datos2 <- as.data.frame(c(data2()[[input$select_var1]],data2()[[input$select_var2]]))  %>% 
      dplyr::rename(
        Observado = `data2()[[input$select_var1]]`,
        Auditado = `data2()[[input$select_var2]]`
        
      )
    
    scatter <-  hchart(Datos2, "scatter", hcaes(x = Observado, y = Auditado ))
    
             
    
    scatter
    
  })
  
  # Structura #
  
  
  observeEvent(input$analizar, {
    output$eval <- renderPrint({
      
      stage4 <- evaluation(
        materiality = 0.03, 
        method = "stringer",
        conf.level = 0.95, 
        data = data2(),
        values = input$select_var1,
        values.audit = input$select_var2
      )
      
      summary(stage4)
      
    })
  })
  
}