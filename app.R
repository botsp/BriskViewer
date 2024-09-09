
library(teal)
library(haven)
library(teal.data)
library(teal.modules.general)
library(teal.modules.clinical)
library(sparkline)
library(readr)  # Reading CSV files
library(readxl) # Reading Excel files

options(shiny.useragg = FALSE)
options(shiny.maxRequestSize = 120*1024^2)
## App header and footer ----
nest_logo <- "https://raw.githubusercontent.com/insightsengineering/hex-stickers/main/PNG/nest.png"

header <- tags$span(
  style = "display: flex; align-items: center; justify-content: space-between; margin: 10px 0 10px 0;",
  tags$span("TabulationViewer Demo App", style = "font-size: 25px;"),
  tags$span(
    style = "display: flex; align-items: center;",
    tags$img(src = nest_logo, alt = "NEST logo", height = "45px", style = "margin-right:10px;"),
    tags$span(style = "font-size: 24px;", "NEST @TabViewer")
  )
)

footer <- tags$p(style = "font-family: Arial, sans-serif; font-size: 13px;",
                 "This demo app is developed from the NEST Team at Roche/Genentech.
        For more information, please contact the developer: progsupp89@gmail.com"
)
app <- init(
  title = build_app_title("TabulationViewer Demo App", nest_logo),
  header = header,
  footer = footer,
  data = teal_data_module(
    ui = function(id) {
      ns <- NS(id)
      fluidPage(
        mainPanel(
          shiny::fileInput(ns("file"), "Upload a file", multiple = TRUE,
                           accept = c(".csv", ".xlsx", ".xpt", ".sas7bdat")),
          actionButton(ns("submit"), "Submit"),
          DT::dataTableOutput(ns("preview"))
        ),
        fluidRow(
          column(12,
                 tags$footer(
                   'The supported file types for upload include ".csv", ".xlsx", ".xpt", and ".sas7bdat". Please note that do not upload data files with the same name across all types. For example: ["ae.xpt" & "ae.xpt"] or ["dm.xpt" & "dm.sas7bdat"] are invalid.',
                   style = "text-align: left; padding: 13px;")
          )
        )
      )
    },
    server = function(id) {
      moduleServer(id, function(input, output, session) {
        
        data <- eventReactive(input$submit, {
          req(input$file)
          
          file_paths <- input$file$datapath
          file_names <- tools::file_path_sans_ext(input$file$name)
          td <- teal_data()
          
          for (i in seq_along(file_paths)) {
            td <- within(
              td, 
              file_ext=tools::file_ext(file_paths[i]),
              data_name <- switch(
                  file_ext,
                  "csv" = read_csv(data_path),
                  "xlsx" = read_excel(data_path),
                  "xpt" = read_xpt(data_path),
                  "sas7bdat" = read_sas(data_path),
                  stop("Please ensure that the uploaded file type is valid.")
                ),
              data_name = file_names[i], 
              data_path = file_paths[i]
            )
          }
          datanames(td) <-  file_names
          td
        })
        data
      })
    }
  ),
  modules = modules(
    tm_front_page(
      label = "App Info",
      header_text = c("Info about input data source" = "This app enables the upload of data files from the local drive."),
      tables = list(`NEST packages used in this demo app` = data.frame(
        Packages = c(
          "teal.modules.general",
          "teal.modules.clinical",
          "haven"
        )
      ))
    ),
    tm_data_table("Data Table"),
    tm_variable_browser("Variable Browser")
  )
)

shinyApp(app$ui, app$server)

