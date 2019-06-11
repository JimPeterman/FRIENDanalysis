#' Save Data File
#'
#' @description
#' Exports a data file in Excel (.xslx) format.
#'
#' @import tibble
#' @import writexl
#' @import stringr
#'
#' @param df Name of the dataframe to save.
#' @param path Location to save the file.
#'
#' @seealso To make it easier to define the path (where you want to save the files),
#' use \code{\link[FRIENDanalysis]{folder_location}}.
#'

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


