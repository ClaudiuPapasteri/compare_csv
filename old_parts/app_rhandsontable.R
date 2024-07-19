library(shiny)
library(rhandsontable)


ui <- shinyUI(fluidPage(
  
  titlePanel("Yehhaaa"),
  sidebarLayout(
    sidebarPanel(
      helpText("Import files (.csv) or paste data directly. First line should have variable names.", 
      ),
    
        
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      wellPanel(
        h3("Download"), 
        downloadButton("downloadResults", "Download Results", style='padding:4px; font-size:100%'),
      )        
      
    ),
    
    mainPanel(
      
      rHandsontableOutput("hotable"),        # hottable
      
      br(),

    )
  )
))

server <- shinyServer(function(input, output, session) {

  values <- reactiveValues()
  
  observe({
    if (is.null(input$file1)) {
      values$DF <- mtcars
    } else {
      values$DF <- read.csv(input$file1$datapath,
                            header = TRUE,
                            sep = ";")
    }
  })
  
  
  output$hotable <- renderRHandsontable({ rhandsontable(values$DF, height = 700, selectCallback = TRUE, readOnly = FALSE) })
  
  hotable_reac <-  reactive({
    if(is.null(input$hotable)){return(values$DF)}
    else if(!identical(values$DF,input$hotable)){
      as.data.frame(hot_to_r(input$hotable))
    }
  })

  # Downloadable csv of selected dataset ----
  output$downloadResults <- downloadHandler(
    filename = function() {
      paste0("Rezultate_", format(Sys.time(), "%d_%H-%M-%S"), ".csv")
    },  
    content = function(file) {
      readr::write_csv2(hotable_reac(), file)
    }
  )
  
})

################################################################################################################################
################ RUN
################################################################################################################################

runApp(list(ui = ui, server = server))


