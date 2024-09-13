library(teal)
library(haven)
library(teal.data)
library(teal.modules.general)
library(teal.modules.clinical)
library(sparkline)
library(readr)  # Reading CSV files
library(readxl) # Reading Excel files
library(Hmisc)
options(shiny.useragg = FALSE)
# File size set to 120MB
options(shiny.maxRequestSize = 120*1024^2)
########################################################################################
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
########################################################################################
# Self-defined function
#Import sort_key that attached with sas datasets
import_sort_list <- function(file_path, sheet_name = "Sheet1", column_label = "Column_Name") {
  # Read the Excel file
  SortInfo <- read_excel(file_path, sheet = sheet_name)
  
  # Filter specific column
  SortInfo_srt <- subset(SortInfo, namelabel == column_label)
  
  # Create an empty list to store vectors
  vectors_list <- list()
  
  # Iterate through each row and create named vectors
  for (i in 1:nrow(SortInfo_srt)) {
    row_data <- SortInfo_srt[i, ]
    memname <- toupper(row_data$memname)
    
    # Extract columns with prefix COL and remove NA values
    vector_values <- unlist(row_data[grepl("^COL", names(row_data))])
    vector_values <- vector_values[!is.na(vector_values)]
    
    # Store the vector in the list, without keeping column names
    vectors_list[[memname]] <- unname(vector_values)
  }
  # Convert the vector values to uppercase
  vector_values <- toupper(vector_values)
  
  # Store the vector in the list, without keeping column names
  vectors_list[[memname]] <- unname(vector_values)
  # Return the result
  return(vectors_list)
}

# To generate join_key objects
generate_join_keys <- function(sort_list) {
  # Check if sort_list is "NOT UPLOADED"
  if (identical(sort_list, "NOT UPLOADED")) {
    return(join_keys())
  }
  # Convert vector names to lowercase for comparison
  names_lower <- tolower(names(sort_list))
  
  # Check if it is adsl(ADaM) or dm(SDTM)
  center_dataset <- if ("adsl" %in% names_lower) names(sort_list)[which(names_lower == "adsl")] else names(sort_list)[which(names_lower == "dm")]
  
  primary_keys <- lapply(names(sort_list), function(name) {
    do.call(join_key, list(name, keys = sort_list[[name]]))
  })
  
  foreign_keys <- lapply(setdiff(names(sort_list), center_dataset), function(name) {
    do.call(join_key, list(center_dataset, name, keys = c("STUDYID", "USUBJID")))
  })
  
  all_keys <- c(primary_keys, foreign_keys)
  
  do.call(join_keys, all_keys)
}

# cs_arm_var <-
#   choices_selected(
#     choices = variable_choices(ADSL, subset = c("ARM")),
#     selected = "ARM"
#   )

########################################################################################

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
          actionButton(ns("checkButton"), "Check Project Type"),
          actionButton(ns("submit"), "Submit"),
          # checkboxInput("checkbox", "CDISC Project(Valid SDTM/ADam", value = FALSE),
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
        ns <- session$ns
        
      
        # 创建 reactiveValues 存储项目类型
        check_project <- reactiveValues(projectType = NULL)
        
        # 处理项目类型的事件反应
        observeEvent(input$checkButton, {
          showModal(modalDialog(
            title = "Project Type",
            radioButtons(ns("projectTypeInput"), "Please select project type:",
                         choices = c("CDISC" = "CDISC", "Non-CDISC" = "Non-CDISC")),
            footer = tagList(
              modalButton("Cancel"),
              actionButton(ns("confirmProjectType"), "Confirm")
            )
          ))
        })
        
        observeEvent(input$confirmProjectType, {
          removeModal()
          prj <- input$projectTypeInput
          if (is.null(prj)) {
            showNotification("Please select a project type.", type = "warning")
          } else {
            check_project$projectType <- prj
          }
        })
        data <- eventReactive(input$submit, {
          req(input$file)
          
          file_paths <<- input$file$datapath
          file_names <<- tools::file_path_sans_ext(input$file$name)
          file_names <<- toupper(file_names)
          
          td<<-teal_data()
          
          # Target SortInfo file
          target_file <- "SORTINFO"
          valid_file_names <- file_names[file_names != target_file]
          # Find the index of the target file
          SortInfo_index <- which(file_names == target_file)
          if (length(SortInfo_index) == 0) {
            showNotification("The specified file was not uploaded. Summary table won't be applied.", type = "warning")
            SortInfo_path<-"NOT UPLOADED"
            SortInfo_list<-"NOT UPLOADED"
          }
          else {
            # Get the path of the target file
            SortInfo_path <- file_paths[SortInfo_index]
            
            # Read the target file using function `import_sort_list`
            SortInfo_list <- import_sort_list(SortInfo_path)
          }
          
          for (i in seq_along(file_paths)) {
            if (file_paths[i] == SortInfo_path) {
              next  # Skip the target file
            }
            td<<-within(td,    
                        {
                          convert_column_types <- function(data_path,ds_name) {
                            
                            # Get file extension
                            file_ext <- tools::file_ext(data_path)
                            # Read data based on file extension
                            df <- switch(
                              file_ext,
                              "csv" = read_csv(data_path),
                              "xlsx" = read_excel(data_path),
                              "xpt" = read_xpt(data_path),
                              "sas7bdat" = read_sas(data_path),
                              stop("Please ensure that the uploaded file type is valid.")
                            )
                            if ("ACTARM" %in% names(df)) {
                              # If "ds_name" is "ADSL" or "DM", filter rows where ARM is not empty
                              # if (toupper(ds_name) %in% c("ADSL", "DM")) {
                              #   df <- df[!is.na(df$ACTARM) & df$ACTARM != "", ]
                              # }
                              # If "ACTARM" is empty, replace with "Acutal ARM is Null"
                              df$ACTARM[is.na(df$ACTARM) | df$ACTARM == ""] <- "Acutal ARM is Null"
                            }
                            # Get all column names
                            column_names <- names(df)
                            
                            # Iterate over each column in the dataframe
                            for (col in column_names) {
                              # Get column label
                              col_label <- label(df[[col]])
                              
                              # Check various conditions
                              if (is.character(df[[col]])) {
                                if (col %in% c("STUDYID", "DOMAIN", "USUBJID", "SUBJID")) {
                                  next
                                }
                                else if (col_label %in% c("Specimen ID", "Group ID", "Sponsor-Defined Identifier", "Link ID", "Link Group ID", "Reference ID", "Dose Description")) {
                                  next
                                }
                                else if (grepl("Unit", col_label, ignore.case = TRUE) || grepl("Units", col_label, ignore.case = TRUE) || grepl("Duration", col_label, ignore.case = TRUE)) {
                                  next
                                }
                                else if (grepl("DTC$|DTM$|DUR$|ENTPT$|ORRES$|ORRESU$|ORNRLO$|ORNRHI$|STRESC$|STRESU$|STNRC$|STREFC$|STTPT$", col)) {
                                  next
                                }
                                else if (grepl("^COVAL", col)) {
                                  next
                                }
                                else if (grepl("^AVALC", col) || grepl("^BASEC", col)) {
                                  if (!grepl("Category", col_label, ignore.case = TRUE)) {
                                    next
                                  }
                                }
                                # If none of the conditions are met, convert to factor
                                df[[col]] <- as.factor(df[[col]])
                                # Add label of each factor column back
                                label(df[[col]]) <- col_label
                              }
                            }
                            
                            return(df)
                          }
                          
                          data_name<-convert_column_types(data_path,ds_name)
                          print(data_name)
                        },
                        data_path = file_paths[i],
                        ds_name = file_names[i],
                        data_name = file_names[i]
            )
          }
          if (check_project$projectType=="CDISC") {
              td<<-within(td,{This_is_CDISC<-function(id){flagid<-"TRUE"}})
              }
          datanames(td) <<- valid_file_names
          # Generate join_key object using function `generate_join_keys`
          join_keys(td)<<-generate_join_keys(SortInfo_list)
          td
        })
        data
        mmpp<<-data
      }
      )
    }
  ),
  modules = 

  modules(
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
