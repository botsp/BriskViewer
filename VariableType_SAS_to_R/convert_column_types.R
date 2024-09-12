library(Hmisc)

convert_column_types <- function(df) {
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
      # # If none of the conditions are met, convert to factor
      df[[col]] <- as.factor(df[[col]])
    }
  }
  
  return(df)
}
df_converted <- convert_column_types(ADSL)

