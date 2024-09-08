
library(teal)
library(haven)
library(teal.data)
library(teal.modules.general)
library(teal.modules.clinical)
library(sparkline)
library(readr)  # 用于读取CSV文件
library(readxl) # 用于读取Excel文件


options(shiny.useragg = FALSE)
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
          file_names <- tools::file_path_sans_ext(input$file$name)
          # data_list <- lapply(input$file$datapath, read_xpt)
          file_exts <- tools::file_ext(input$file$name)
          data_list <- lapply(seq_along(input$file$datapath), function(i) {
            file_path <- input$file$datapath[i]
            file_ext <- file_exts[i]
            switch(
              file_ext,
              "csv" = read_csv(file_path),
              "xlsx" = read_excel(file_path),
              "xpt" = read_xpt(file_path),
              "sas7bdat" = read_sas(file_path),
              stop("Unsupported file type")
            )
          })
          
          
          for (i in seq_along(data_list)) {
            assign(paste0("upload_",file_names[i]), data_list[[i]], envir = .GlobalEnv)
          } 
          
          td <- within(teal_data(), {
            data_names <- ls(.GlobalEnv, pattern = "^upload_")
            for (name in data_names) {
              assign(sub("^upload_", "", name), get(name, envir = .GlobalEnv))
            }
            rm(name)
            rm(data_names)
          })
          datanames(td) <- file_names
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
