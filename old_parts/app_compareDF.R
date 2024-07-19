library("shiny")
library("compareDF")
library("readr")


ui <- fluidPage(
  fluidPage(
    titlePanel("Compara rezultate"),
    sidebarLayout(
      
      sidebarPanel(
        fileInput(inputId = "csv_1",
                  label = "Upload csv 1",
                  accept = c(".csv")),
        fileInput(inputId = "csv_2",
                  label = "Upload csv 2",
                  accept = c(".csv")),
        checkboxInput("keep_unchanged_rowsButton", "Randuri neschimbate", value = FALSE),
        checkboxInput("keep_unchanged_colsButton", "Coloane neschimbate", value = FALSE),
        actionButton("compareButton", "Compara"),
      ),
      
      mainPanel(
        # htmlOutput("filetable")
        uiOutput("filetable")
      )
    )
  )
)  


server <-  function(input, output) {
  csv_1_reac <- reactive({
    readr::read_csv2(input$csv_1$datapath, col_types = cols(.default = col_character()))
  })
  csv_2_reac <- reactive({
    readr::read_csv2(input$csv_2$datapath, col_types = cols(.default = col_character()))
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
  
  
}

shinyApp(ui, server)






# Dead code ---------------------------------------------------------------

# folder <- r'-(C:\Users\User\Desktop\compare_csv)-'
# setwd(folder)
# 
# filename_csv_1 <- dir(pattern = ".csv$")[1]
# filename_csv_2 <- dir(pattern = ".csv$")[2]
# 
# csv_1 <- readr::read_csv2(filename_csv_1, col_types = cols(.default = col_character()))
# csv_2 <- readr::read_csv2(filename_csv_2, col_types = cols(.default = col_character()))
# 
# compareDF::create_output_table(
#   compareDF::compare_df(csv_1, csv_2),
#   output_type = "html", limit = 1000
# )  