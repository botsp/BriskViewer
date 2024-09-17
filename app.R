
library(teal)
library(teal.modules.general)
library(teal.modules.clinical)

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
  SortInfo <- readxl::read_excel(file_path, sheet = sheet_name)
  
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
# Similar to dynamically load modules, this would be based on a reactive `data()`
generate_join_keys <- function(sort_list) {
  # Check if sort_list is "NOT UPLOADED"
  if (identical(sort_list, "NOT UPLOADED")) {
    return(teal.data::join_keys())
  }
  # Convert vector names to lowercase for comparison
  names_lower <- tolower(names(sort_list))
  
  # Check if it is adsl(ADaM) or dm(SDTM)
  center_dataset <- if ("adsl" %in% names_lower) names(sort_list)[which(names_lower == "adsl")] else names(sort_list)[which(names_lower == "dm")]
  
  primary_keys <- lapply(names(sort_list), function(name) {
    do.call(teal.data::join_key, list(name, keys = sort_list[[name]]))
  })
  
  foreign_keys <- lapply(setdiff(names(sort_list), center_dataset), function(name) {
    do.call(teal.data::join_key, list(center_dataset, name, keys = c("STUDYID", "USUBJID")))
  })
  
  all_keys <- c(primary_keys, foreign_keys)
  
  do.call(teal.data::join_keys, all_keys)
}
# automatically obtain the required data name for a module from `data()@datanames`
get_dsname_for_module <- function(type = c("DEMO","VS", "AE")) {
  # Convert S4 datanames to lowercase for case-insensitive comparison
  S4dsname<-data()@datanames
  type <- match.arg(type)
  
  if (type == "VS") {
    # Check for "advs" first
    if ("advs" %in% tolower(S4dsname)) {
      return(S4dsname[tolower(S4dsname) == "advs"])
    }
    
    # Check for "vs" if "advs" is not found
    if ("vs" %in% tolower(S4dsname)) {
      return(S4dsname[tolower(S4dsname) == "vs"])
    }
  } else if (type == "AE") {
    # Check for "adae" first
    if ("adae" %in% tolower(S4dsname)) {
      return(S4dsname[tolower(S4dsname) == "adae"])
    }
    
    # Check for "ae" if "adae" is not found
    if ("ae" %in% tolower(S4dsname)) {
      return(S4dsname[tolower(S4dsname) == "ae"])
    }
  }else if (type == "DEMO") {
    # Check for "adsl" first
    if ("adsl" %in% tolower(S4dsname)) {
      return(S4dsname[tolower(S4dsname) == "adsl"])
    }
    
    # Check for "dm" if "adsl" is not found
    if ("dm" %in% tolower(S4dsname)) {
      return(S4dsname[tolower(S4dsname) == "dm"])
    }
  }
}

########################################################################################
# Set UI of `teal_data_module`
teal_data_module_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    mainPanel(
      shiny::fileInput(ns("file"), "Upload datasets", multiple = TRUE,
                       accept = c(".csv", ".xlsx", ".xpt", ".sas7bdat")),
      actionButton(ns("checkButton"), "Check Project Type"),
      actionButton(ns("submit"), "Submit"),
      # checkboxInput("checkbox", "CDISC Project(Valid SDTM/ADam", value = FALSE),
      DT::dataTableOutput(ns("preview"))
    ),
    fluidRow(
      column(12,
             tags$footer(
               'The supported file types for upload include ".csv", ".xlsx", ".xpt", and ".sas7bdat". Please note that do not upload data files with the same name across all types. For example: ["ae.xpt" & "ae.xpt"] or ["dm.xpt" & "dm.sas7bdat"] are invalid.
               If you want to load teal.modules.clinical, please use valid CDISC datasets. I have prepared some example datasets in *xpt, *sas7bdat formats for you to experience this app.',
               style = "text-align: left; padding: 15px;")
      )
    )
  )
}
# Set Server of `teal_data_module`
teal_data_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Create reactiveValues to store project type
    check_project <- reactiveValues(projectType = NULL)
    
    # Handle the event reaction for project type
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
      
      td<<-teal.data::teal_data()
      
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
                      
                      convert_column_types <- function(data_path, ds_name) {
                        # Get file extension
                        file_ext <- tools::file_ext(data_path)
                        
                        # Read data based on file extension
                        df <- switch(
                          file_ext,
                          "csv" = readr::read_csv(data_path),
                          "xlsx" = readxl::read_excel(data_path),
                          "xpt" = haven::read_xpt(data_path),
                          "sas7bdat" = haven::read_sas(data_path),
                          stop("Please ensure that the uploaded file type is valid.")
                        )
                        
                        # If "ACTARM" is empty, replace with "Acutal ARM is Null"
                        if ("ACTARM" %in% names(df)) {
                          df$ACTARM[is.na(df$ACTARM) | df$ACTARM == ""] <- "Acutal ARM is Null"
                        }
                        
                        # Get column label
                        col_labels <- teal.data::col_labels(df)
                        # Get all column names
                        column_names <- names(df)
                        # Create an empty list to store columns that need to have their labels reset
                        relabel_list <- list()
                        
                        # Iterate through each column in the DataFrame
                        for (col in column_names) {
                          # Get column label
                          col_label <- col_labels[[col]]
                          
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
                            # Add columns that need to have their labels reset to the list
                            relabel_list[[col]] <- col_label
                          }
                        }
                        
                        # Use the col_relabel function to reset column labels
                        df <- do.call(teal.data::col_relabel, c(list(df), relabel_list))
                        return(df)
                      }
                      
                      df_converted <- convert_column_types(data_path,ADSL)
                      
                      data_name<-convert_column_types(data_path,ds_name)
                    },
                    data_path = file_paths[i],
                    ds_name = file_names[i],
                    data_name = file_names[i]
        )
      }
      if (check_project$projectType=="CDISC") {
        td<<-within(td,{This_is_CDISC<-function(id){flagid<-"TRUE"}})
      }
      teal.data::datanames(td) <<- valid_file_names
      # Generate join_key object using function `generate_join_keys`
      teal.data::join_keys(td)<<-generate_join_keys(SortInfo_list)
      td
    })
    data
  }
  )
}

########################################################################################

app <- teal::init(
  title = build_app_title("TabulationViewer Demo App", nest_logo),
  header = header,
  footer = footer,
  data = teal_data_module(
          ui = teal_data_module_ui,
          server = teal_data_module_server),

  filter = teal_slices(
    count_type = "all",
    teal_slice(dataname = "ADSL", varname = "SAFFL", selected = "Y"),
    teal_slice(dataname = "ADVS", varname = "PARAMCD", selected = "PULSE"),
    teal_slice(dataname = "ADAE", varname = "AESER", selected="N"),
    teal_slice(dataname = "ADAE", varname = "AESEV", selected="MODERATE")
  ),
  modules = 
    modules(
      tm_front_page(
        label = "App Info",
        header_text = c("Info about input data source" = "This app enables the upload of data files from the local drive."),
        tables = list(`NEST packages used in this demo app` = data.frame(
          Packages = c(
            "teal.modules.general",
            "teal.modules.clinical"
          )
        ))
      ),
      tm_data_table("Data Listing",
                    dt_options = list(searching = TRUE, pageLength = 30, lengthMenu = c(5, 15, 30, 100), scrollX = TRUE)),
      tm_variable_browser("Variable Browser"),
      tm_t_summary(
        label = "Demographic Table",
        dataname = "ADSL",
        arm_var = choices_selected(c("ACTARM","ARM","TRT01A"), "ARM"),
        summarize_vars = choices_selected(c("SEX", "RACE", "AGE"),selected = c("SEX", "AGE", "RACE"))
      ),
      modules(
        label = "Adverse Events",
      # tm_t_events_summary(
      #   label = "AE Summary",
      #   dataname = "ADAE",
      #   arm_var = choices_selected(c("ACTARM","ARM","TRT01A"), "ARM"),
      #   flag_var_anl = choices_selected(ae_anl_vars,c("TMPFL_SER"),keep_order = TRUE),
      #   # flag_var_aesi = choices_selected(aesi_vars,aesi_vars,keep_order = TRUE),
      #   add_total = TRUE
      # ),
      tm_t_events(
        label = "Adverse Event by SOC and PT",
        dataname = "ADAE",
        arm_var = choices_selected(c("ACTARM","ARM","TRT01A"), "ARM"),
        llt = choices_selected(c("AETERM", "AEDECOD"), c("AEDECOD")),
        hlt = choices_selected(c("AEBODSYS", "AESOC"), c("AEBODSYS")),
        add_total = TRUE,
        event_type = "adverse event"),
        tm_t_events(
          label = "Adverse Event by SOC and PT",
          dataname = "ADAE",
          arm_var = choices_selected(c("ACTARM","ARM","TRT01A"), "ARM"),
          llt = choices_selected(c("AETERM", "AEDECOD"), c("AEDECOD")),
          hlt = choices_selected(c("AEBODSYS", "AESOC"), c("AEBODSYS")),
          add_total = TRUE,
          event_type = "adverse event")
      ),
        tm_t_summary_by(
          label = "Vital Signs Summary",
          dataname = "ADVS",
          arm_var = choices_selected(c("ACTARM","ARM","TRT01A"), "ARM"),
          by_vars = choices_selected(c("PARAM", "AVISIT"),c("PARAM", "AVISIT"),fixed = TRUE),
          summarize_vars = choices_selected(c("AVAL", "CHG"), c("AVAL")),
          paramcd = NULL)
  )
)


shinyApp(app$ui, app$server)
