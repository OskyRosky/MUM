############################
#         App Muestreo     #
############################

###############
#  Librerías  # 
###############

suppressMessages(library(shiny))
suppressMessages(library(shinydashboard))
suppressMessages(library(readxl))
suppressMessages(library(dplyr))
suppressMessages(library(DT))
suppressMessages(library(plyr))
suppressMessages(library(readr))
suppressMessages(library(janitor))
suppressMessages(library(shiny))
suppressMessages(library(shinydashboard))
suppressMessages(library(shinydashboardPlus))
suppressMessages(library(highcharter))
suppressMessages(library(formattable))
suppressMessages(library(highcharter))
suppressMessages(library(viridisLite))
suppressMessages(library(stringi))
suppressMessages(library(data.table))
suppressMessages(library(tidyr))
suppressMessages(library(forecast))
suppressMessages(library(kableExtra))
suppressMessages(library(shinyWidgets))
suppressMessages(library(png))
suppressMessages(library(scales))
suppressMessages(library(gt))
suppressMessages(library(reactable))
suppressMessages(library(RcppRoll))
suppressMessages(library(sunburstR))
suppressMessages(library(htmltools))
suppressMessages(library(d3r))
suppressMessages(library(jfa))
suppressMessages(library(readxl))
suppressMessages(library(dplyr))


##############
##   Header  #
##############


header <- dashboardHeader(title = "Load Forecasting")

##############
##   Sider   #
##############

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Intro",tabName = "p1", icon = icon("chalkboard")),
    menuItem("Importar",tabName = "p2", icon = icon("line")),
    menuItem("Muestra",tabName = "p3", icon = icon("computer")),
    menuItem("Evaluación",tabName = "p4", icon = icon("flag"))
  )
)

##########
## Body  #
##########

body <- dashboardBody( 
  tabItems(    tabItem(tabName = "p1",
                     
                       h1("Introducción", align = "center"),
                       br(),
                       br(),
                       h2("Poner una intro", align = "left"),
                       
                       ),
               
               tabItem(tabName = "p2",
                      
                       ####################################################
                       #                      IMPORTAR                    #
                       ####################################################
                       
                       ##############################
                       #          Importar Datos    #
                       ##############################
                       
                       h1("Importar de los datos", align = "center"),
                       br(),
                       fileInput("file", "Importar datos del muestreo"),
                       #   actionButton("load", "Load"),
                       
                       ###############################
                       #    Visualizar los datos     #
                       ###############################
                       
                       reactableOutput("data"),
                       verbatimTextOutput("str"),
                       plotOutput("hist")
                       
               ),
               
               tabItem(tabName = "p3",
                       h1("Muestreo: tamaño y selección", align = "center"),
                       br(),
                       br(),
                       h2("Cálculo de tamaño de muestra"),
                       br(),
                       sliderInput("freq1",
                                   "Materialidad:",
                                   min = 0.01,  max = 0.99, value = 0.05),
                       sliderInput("freq2",
                                   "Esperado:",
                                   min = 0.01,  max = 0.99, value = 0.01), 
                       selectInput("distri", "Seleccione el nivel:",  
                                                                       list(`Tipo` = list("poisson",
                                                                                           "binomial", 
                                                                                           "hypergeometric"
                                                         )
                                                         )
                                             ),
                       sliderInput("freq3",
                                   "Nivel de confianza:",
                                   min = 0.01,  max = 0.99, value = 0.95),
                      actionButton("update", "Calcular"),
                      hr(),
                       fluidRow(
              box(
                solidHeader = TRUE, 
                width = 12,
                reactableOutput("SampleSize")  
              )
            ),
            br(),
            br(),
            h2("Muestra seleccionada"),
            br(),
            br(),
            reactableOutput("Sampe"),
            br(),
            br(),
            
            #################################
            #         Descargar muestra     #
            #################################
            
            h2("Descargar la muestra seleccionada"),
            br(),
            actionButton("show1", "Descargar archivo")
            
            
            
                       
               ),
               
               tabItem(tabName = "p4",
                       h1("Evaluación de la muestra", align = "center"),
                       br(),
                       br(),
                       fileInput("file2", "Seleccione sus datos para la evaluación"),
                       #   actionButton("load", "Load"),
                       reactableOutput("data2"),
                       h2("Proceso de evaluación", align = "center"),
                       br(),
                       br(),
                       h2("Determine: materialidad, método, nivel de confianza, valores observador y valores auditados.", align = "center"),
                       sliderInput("freq10",
                                   "Materialidad:",
                                   min = 0.01,  max = 0.99, value = 0.05),
                       sliderInput("freq3",
                                   "Nivel de confianza:",
                                   min = 0.01,  max = 0.99, value = 0.95),
                       
               )
    
  )
  
)

##########
#   Ui   #
##########

ui <- dashboardPage(

  header,
  sidebar,
  body
)

###########
#  Server #
###########

server <- function(input, output) {
  
  ####################################################
  #                      IMPORTAR                    #
  ####################################################
  
  ##############################
  #          Importar Datos    #
  ##############################
  
  # Data la volvemos un objeto reactivo 
  #  data ---> data()
  
  data <- reactive({
    if (is.null(input$file)) {
      return(NULL)
    }
    read.csv(input$file$datapath)
  })
  
  ###############################
  #    Visualizar los datos     #
  ###############################
  
  # Tabla #
  
  output$data <- renderReactable({
    reactable(head(data(), 10))
  })
  
  # Structura #
  
  output$str <- renderPrint({
    str(data())
  })
  
  # Histograma de una variable #
  
  output$hist <- renderPlot({
    hist(data()[, "Healthy.life.expectancy"])
  })
  
  ####################################################
  #                     Muestra                      #
  ####################################################
  
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
  
  
  Sample <- reactive({
    if (is.null(data())) {
      return(NULL)
    }
    stage1 <- planning(materiality = input$freq1, 
                       expected = input$freq2,
                       likelihood = input$distri, 
                       conf.level = input$freq3
    )
    
    stage1$n
   
  })
  
  #################################
  #    Visualización de la tabla  #
  #################################
  
  sample <- reactive({
  
    
      if (is.null(input$file)) {
      return(NULL)
    }
    read.csv(input$file$datapath)
    
    
  })
  
  
  output$Sampe <- renderReactable({
    
    if (input$update) {
    
    data2 <-   isolate(sample_n(data(), sample()))
      
    reactable(data2)
    
    } else {
      NULL
    }
      
  })
  
 # output$data <- renderReactable({
#    reactable(head(data(), 10))
 # })
  
  #################################
  #         Descargar muestra     #
  #################################
  
  observeEvent(input$show1, {
    
    showModal(modalDialog(
      title = "Download Load Forecasting values", br(),
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
      paste("LoadForecast-", Sys.Date(), ".csv", sep="")
    },
    
    content = function(file) {
      write.csv(Forecasting_hours_data, file)
    }
  )
  
  output$download2.2 <- downloadHandler(
    
    filename = function() {
      paste("LoadForecast-", Sys.Date(), ".txt", sep="")
    },
    content = function(file) {
      write.table(Forecasting_hours_data, file)
    }
  )
  
  
}

###########
## Run App  
###########

shinyApp(ui, server)



