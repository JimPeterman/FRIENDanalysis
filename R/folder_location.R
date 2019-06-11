#' Folder path creation
#'
#' @description
#' Determines the path for where a folder is located on the computer.
#'
#' @import easycsv
#'
#' @export

folder_location <- function() {
  if ((Sys.info()["sysname"])=="Windows") {
    choose.dir()
  } else if ((Sys.info()["sysname"])=="Darwin") {
    choose_dir()
  } else {
    print("Only Windows and Mac are currently supported.")
  }
}

