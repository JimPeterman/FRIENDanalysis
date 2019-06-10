


folder_location <- function() {
  if ((Sys.info()["sysname"])=="Windows") {
    choose.dir()
  } else if ((Sys.info()["sysname"])=="Darwin") {
    choose_dir()
  } else {
    print("Only Windows and Mac are currently supported.")
  }
}

### Need easycsv

