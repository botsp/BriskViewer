library(Hmisc)

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
df_converted <- data_name<-convert_column_types(data_path,ADSL)

