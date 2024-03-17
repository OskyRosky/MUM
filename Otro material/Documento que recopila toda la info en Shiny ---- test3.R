library(shiny)
library(officer)
library(flextable)
library(dplyr)

# Simulando algunos datos para el ejemplo
set.seed(123)
datos <- data.frame(
  Parametro1 = runif(10, 1, 100),
  Parametro2 = sample(letters[1:5], 10, replace = TRUE),
  Parametro3 = rnorm(10),
  Parametro4 = runif(10, 10, 50),
  Parametro5 = sample(1:100, 10),
  Parametro6 = rnorm(10, 50, 15),
  Parametro7 = runif(10, 0, 1)
)

ui <- fluidPage(
  titlePanel("Descarga de Reporte con Parámetros"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("parametros", "Selecciona los parámetros:",
                         choices = names(datos), 
                         selected = names(datos)[1:5]),
      actionButton("generar", "Generar Tabla")
    ),
    mainPanel(
      tableOutput("tabla"),
      downloadButton("downloadReport", "Descargar Reporte")
    )
  )
)

server <- function(input, output) {
  tabla_reactiva <- eventReactive(input$generar, {
    req(input$parametros)
    datos %>% 
      select(all_of(input$parametros))
  })
  
  output$tabla <- renderTable({
    tabla_reactiva()
  })
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("reporte-", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      doc <- read_docx()
      my_table <- tabla_reactiva()
      
      ft <- flextable(my_table)
      
      doc <- body_add_flextable(doc, value = ft)
      
      print(doc, target = file)
    }
  )
}

shinyApp(ui = ui, server = server)
