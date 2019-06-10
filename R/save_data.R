

save_data <- function(df, path) {

  # Adds the name of the file to be saved to the path.
  if(stringr::str_sub(path, start = -1) != "/") {
    path <- paste(path, "/FRIEND contribution data.xlsx", sep = "")} else {
      path <- paste(path, "FRIEND contribution data.xlsx", sep = "")
    }

  # Saves the file.
  write_xlsx(df, path = path,
             col_names = TRUE, format_headers = TRUE)
}


