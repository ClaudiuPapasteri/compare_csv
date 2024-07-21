
library(shiny)
library(readr)
library(compareDF)
library(rhandsontable)
library(shinyjs)

ui <- fluidPage(
  shinyjs::useShinyjs(),  # Include shinyjs
  tags$head(
    tags$script(
      HTML(
        "
        function downloadCSV(csvContent, fileName) {
          var blob = new Blob([csvContent], { type: 'text/csv;charset=utf-8;' });
          if (navigator.msSaveBlob) { // IE 10+
            navigator.msSaveBlob(blob, fileName);
          } else {
            var link = document.createElement('a');
            if (link.download !== undefined) { // feature detection
              // Browsers that support HTML5 download attribute
              var url = URL.createObjectURL(blob);
              link.setAttribute('href', url);
              link.setAttribute('download', fileName);
              link.style.visibility = 'hidden';
              document.body.appendChild(link);
              link.click();
              document.body.removeChild(link);
            }
          }
        }
        "
      )
    )
  ),
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
          actionButton("save_hotableButton", "Salveaza editare"),
          br(),
          shinyjs::hidden(actionButton("downloadResults", "Download csv editat"))
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


server <- function(input, output) {
  csv_1_reac <- reactive({
    req(input$csv_1)
    read_csv2(input$csv_1$datapath, col_types = cols(.default = col_character()))
  })
  
  csv_2_reac <- reactive({
    req(input$csv_2)
    read_csv2(input$csv_2$datapath, col_types = cols(.default = col_character()))
  })
  
  compare_csvs_reac <- eventReactive(input$compareButton, {
    req(input$csv_1, input$csv_2)
    HTML(
      compareDF::create_output_table(
        compareDF::compare_df(
          df_new = csv_1_reac(),
          df_old = csv_2_reac(),
          group_col = names(csv_1_reac())[1],   
          keep_unchanged_rows = input$keep_unchanged_rowsButton,
          keep_unchanged_cols = input$keep_unchanged_colsButton
        ),  
        output_type = "html", limit = 1000
      )
    )
  })
  output$filetable <- renderUI({ compare_csvs_reac() })
  
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
  
  output$hotable <- renderRHandsontable({
    req(values$DF)
    rhandsontable(values$DF, height = 700, selectCallback = TRUE, readOnly = FALSE)
  })
  
  hotable_reac <- eventReactive(input$save_hotableButton, {
    if (is.null(input$hotable)) {
      return(NULL)
    } else if (!identical(values$DF, input$hotable)) {
      as.data.frame(hot_to_r(input$hotable))
    }
  })
  
  observe({
    if (is.null(hotable_reac())) {
      shinyjs::hide("downloadResults")
    } else {
      shinyjs::show("downloadResults")
    }
  })
  
  observeEvent(input$downloadResults, {
    req(hotable_reac())
    shinyjs::runjs(sprintf("downloadCSV(`%s`, 'Rezultate_%s.csv')", 
                           gsub("`", "\\`", hotable_reac()), 
                           format(Sys.time(), "%d_%H-%M-%S")))
  })
}

shinyApp(ui, server)
