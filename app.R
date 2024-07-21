library(shiny)
library(readr)
library(compareDF)
library(rhandsontable)


# Workaround for Chromium Issue 468227
# https://shinylive.io/r/examples/#r-file-download
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}

ui <- fluidPage(
  fluidPage(
    titlePanel("Compara rezultate"),
    sidebarLayout(
      sidebarPanel(
        helpText("Uploadeaza csv-urile exportate din FormScanner."),
        wellPanel(
          fileInput(inputId = "csv_1",
                    label = "Upload csv 1",
                    accept = c(".csv")),
          fileInput(inputId = "csv_2",
                    label = "Upload csv 2",
                    accept = c(".csv")),
          checkboxInput("keep_unchanged_rowsButton", "Randuri neschimbate", value = FALSE),
          checkboxInput("keep_unchanged_colsButton", "Coloane neschimbate", value = FALSE),
          actionButton("compareButton", "Compara")
        ),
        
        helpText("Editeaza un csv si exporta."),
        wellPanel(
          radioButtons("chose_dfButton", "Alege csv:",
            c("Primul" = "1", 
              "Al doilea" = "2"
            ),
            selected = "1"
          ),
          downloadButton("downloadResults", "Download csv editat"),
        ),
        
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Compara csv", uiOutput("filetable")),
          tabPanel("Editeaza csv", rHandsontableOutput("hotable"))
        )  
      )
    )
  )
)  


server <-  function(input, output) {
  

  # Compare csv -------------------------------------------------------------

  csv_1_reac <- reactive({
    readr::read_csv2(input$csv_1$datapath, col_types = cols(.default = col_character()))
    # read.csv2(input$csv_1$datapath)    # readr fully supported so no need
  })
  csv_2_reac <- reactive({
    readr::read_csv2(input$csv_2$datapath, col_types = cols(.default = col_character()))
    # read.csv2(input$csv_2$datapath)      # readr fully supported so no need
  })
  
  compare_csvs_reac <- eventReactive(input$compareButton, {
    HTML(
      compareDF::create_output_table(
        compareDF::compare_df(
          df_new = csv_1_reac(),
          df_old = csv_2_reac(),
          group_col = names(csv_1_reac())[1],   # 1st col of csv, default name should be `File name`
          keep_unchanged_rows	= input$keep_unchanged_rowsButton,
          keep_unchanged_cols = input$keep_unchanged_colsButton
        ),  
        output_type = "html", limit = 1000
      )
    )
  })
  output$filetable <- renderUI({compare_csvs_reac()})
  

  # Edit csv ----------------------------------------------------------------

  values <- reactiveValues()
  
  observe({
    if (is.null(input$csv_1) & is.null(input$csv_2)) {
      values$DF <- data.frame()
    } else if(input$chose_dfButton == "1") {   
      values$DF <- csv_1_reac()
    } else if(input$chose_dfButton == "2") {  
      values$DF <- csv_2_reac()
    }
  })
  
  
  output$hotable <- renderRHandsontable({ rhandsontable(values$DF, height = 700, selectCallback = TRUE, readOnly = FALSE) })
  
  hotable_reac <-  reactive({
    if(is.null(input$hotable)){return(values$DF)}
    else if(!identical(values$DF, input$hotable)){
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
      # write.csv2(hotable_reac(), file)   # readr fully supported so no need
    }
  )  
  
  # outputOptions(output, "downloadResults", suspendWhenHidden = FALSE)  # this was not the cause of the error
  
}

shinyApp(ui, server)


