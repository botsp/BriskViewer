
convert_column_types1 <- function(data_path, ds_name) {
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
