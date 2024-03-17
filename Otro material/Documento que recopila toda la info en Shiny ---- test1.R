####################################################
#    Aplicación de documento de descarga en Shiny  #
####################################################

library(shiny)
library(rmarkdown)


ui <- fluidPage(
  # Selectores para los parámetros
  selectInput("param1", "Parámetro 1", choices = c("A", "B", "C")),
  selectInput("param2", "Parámetro 2", choices = 1:10),
  selectInput("param3", "Parámetro 3", choices = c(TRUE, FALSE)),
  
  # Botón para descargar el reporte
  downloadButton("downloadReport", "Descargar Reporte")
)

server <- function(input, output) {
  
  # Función para generar el reporte basado en los parámetros seleccionados
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("reporte-", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      # Crea un reporte temporal de R Markdown
      tempReport <- file.path(tempdir(), "reporte.Rmd")
      
      # Escribe un archivo Rmd con los parámetros especificados
      file.create(tempReport)
      writeLines(
        c(
          "---",
          "title: \"Reporte Dinámico\"",
          "output: word_document",
          "---",
          
          "# Parámetros Seleccionados",
          
          "- Parámetro 1: `r input$param1`",
          "- Parámetro 2: `r input$param2`",
          "- Parámetro 3: `r input$param3`",
          
          "# Gráfico de Demostración",
          
          "```{r pressure, echo=FALSE}",
          "plot(pressure)",
          "```"
        ),
        tempReport
      )
      
      # Renderiza el reporte de R Markdown y escribe el output en el archivo destino
      render(tempReport, output_format = "word_document", output_file = file)
    }
  )
}

shinyApp(ui, server)