####################################################
#    Aplicación de documento de descarga en Shiny  #
####################################################

library(shiny)
library(officer)
library(flextable)

ui <- fluidPage(
  downloadButton("downloadReport", "Descargar Reporte")
)

server <- function(input, output) {
  # Suponiendo que tienes una tabla reactiva llamada reactive_table
  reactive_table <- reactive({
    data.frame(
      Columna1 = c(1, 2, 3),
      Columna2 = c("A", "B", "C")
    )
  })
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("reporte-", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      doc <- read_docx()
      my_table <- reactive_table()
      
      # Usando flextable para convertir tu dataframe a un formato compatible con Word
      ft <- flextable(my_table)
      
      # Añadiendo la tabla al documento
      doc <- body_add_flextable(doc, value = ft)
      
      # Guardando el documento
      print(doc, target = file)
    }
  )
}

shinyApp(ui = ui, server = server)