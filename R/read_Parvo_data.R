#' Summary of maximal Parvo data
#'
#' @description
#' Creates a summary file of maximal data from a Parvo metabolic cart.
#'
#' \emph{Data from the Parvo must be downloaded/saved in .PRN
#' or .CSV formats.}
#'
#' @import dplyr
#' @import janitor
#' @import tibble
#' @import writexl
#' @import stringr
#'
#' @param path Location of the Parvo files
#'
#' @return Creates a dataframe summarizing all of the Parvo metabolic files.
#'
#' @seealso To make it easier to define the path (find the location of the files),
#' use the function: \code{\link[FRIENDanalysis]{folder_location}}.
#'
#' @export

read_Parvo_data <- function(path) {

    # Make sure that path is correct format (adds "/" if needed):
    if(stringr::str_sub(path, start = -1) != "/") {
      path <- paste(path, "/*.PRN", sep = "")} else {
        path <- paste(path, "*.PRN", sep = "")
      }
    # Find all files in the folder:
    files <- Sys.glob(paths = path)
    # Changes the path if files are CSV
    if(length(files)==0) {
      path <- str_replace_all(path,".PRN",".CSV")
    }
    # Find all files in the folder:
    files <- Sys.glob(paths = path)
    if(length(files)==0) {
      print("The Parvo files need to be saved in either PRN or CSV formats.")
    }
    # Drops files that are open/hidden within the folder.
    files <- files[!grepl('/~',files)]

    # Initialize empty data frame that'll compile the data.
    data <- tibble()

    # Read in files as temporary data frame:
    for(i in 1:length(files)) {
      temp_vec <- readLines(files[i],warn = F)
      ind <- str_which(temp_vec, "TIME")+4
      n_cols <- str_count(temp_vec[ind], ",")+1
      temp <- read.table(files[i], header = F,sep = ",", fill = T,col.names = c(1:n_cols), quote = "",
                         colClasses = "character")
      # If the file has been opened and saved, a \" appears throughout so this drops that.
      temp[] <- lapply(temp, gsub, pattern="\"", replacement="")
      temp <- temp %>% mutate_all(trimws)

      # Inialize empty data frame that'll have summary data from subject file.
      new_data <- tibble()

      # Add in the subject data.
      # Name.
      new_data[1,'Name (DELETE before submitting)'] <- paste(temp[[5,2]],temp[[5,3]],sep = ", ")
      # ID number.
      new_data[1,'Individual #'] <- i
      # Facility
      new_data[1,'Facility'] <- temp[1,1]
      # Test Date.
      new_data[1,'Test Date'] <- paste(c(temp[[3,4]],"/", temp[[3,6]], "/",temp[[3,2]]),collapse = "")
      # Age.
      new_data[1,'Age'] <- temp[[6,2]]
      # Gender.
      new_data[1,'Gender'] <- temp[[6,5]]
      # Height.
      new_data[1,'Height'] <- as.numeric(temp[[7,2]])
      # Weight.
      new_data[1,'Weight'] <- round(as.numeric(temp[[7,7]]),1)
      # Test mode.
      new_data[1,'Test Mode'] <- temp[[11,2]]
      # Metabolic cart.
      new_data[1,'Met Cart'] <- "Parvo"
      # Criteria for VO2peak.
      new_data[1,'Criteria for peak VO2'] <- "20sec avg (FRIEND script)"

      #Create data frame of just respiratory data from the Parvo file.
      # Creates a data frame that starts at the "TIME" header and goes to the bottom of the file.
      resp_data <- temp[which(temp=="TIME"):nrow(temp),]
      # Add in the column names (combining the first and second rows).
      colnames(resp_data) <- paste(resp_data[1,], resp_data[2,])
      # Drops any rows that have missing cells.
      resp_data[resp_data==""]<-NA
      resp_data <- resp_data[complete.cases(resp_data),]
      # Drops the first row so it's just the numbers.
      resp_data <- resp_data[-1,]
      # Converts data frame to all numeric.
      resp_data <- resp_data %>%
        mutate_all(as.numeric) %>%
        janitor::clean_names() %>%
        round(.,2)

      # Creates data frame of just respiratory variables of interest (time,VO2,RER).
      vo2 <- resp_data %>%
        select(time,vo2_kg_stpd,rer)
      # Find the highest VO2 (/kg) and the values on either side of that value.
      index <- which.max(vo2$vo2_kg_stpd)
      vo2 <- vo2[c((index-1):(index + 1)),]

      # The highest VO2 could occur at the last row which messes up the check below.
      # So if the highest is the last row, this adds a VO2 value that is -1ml/kg/min to let the check below run.
      if(is.na(vo2[3,'vo2_kg_stpd'])) {vo2[3,'vo2_kg_stpd'] <- (vo2[2,'vo2_kg_stpd']-1)}

      # Does a check to see if the peak value was within +/- 2ml/kg/min of the previous values.
      # Prints "Good" if okay and "Check" if not.
      # Reduces the vo2 data frame to just the max vo2 and time.
      vo2 <- vo2 %>%
        mutate(vo2_check = if_else(max(vo2_kg_stpd) - lead(vo2_kg_stpd)  < 2 | max(vo2_kg_stpd) - lag(vo2_kg_stpd)  < 2, "Good", "Check")) %>%
        filter(vo2_kg_stpd == max(vo2_kg_stpd))

      # Combines data frames so there's one summary data frame for the subject.
      new_data <- bind_cols(new_data, vo2)

      # Adds in other variables if present in the Parvo file.
      new_data[1,'Peak VO2 (L)'] <- ifelse("vo2_stpd" %in% colnames(resp_data), resp_data[index,'vo2_stpd'], NA)
      new_data[1,'Peak HR'] <- ifelse("hr" %in% colnames(resp_data), resp_data[index,'hr'], NA)
      # Peak ventilation.
      new_data[1,'Peak VE (BTPS)'] <- ifelse("ve_btps" %in% colnames(resp_data), resp_data[index,'ve_btps'], NA)
      # Peak petCO2.
      new_data[1,'Peak PetCO2'] <- ifelse("pet_co2_na" %in% colnames(resp_data), resp_data[index,'pet_co2_na'], NA)
      # O2 saturation at peak.
      new_data[1,'Peak O2 sat'] <- ifelse("spo2_na" %in% colnames(resp_data), resp_data[index,'spo2_na'], NA)

      # Average ventalitory efficiency (calculated from the data).
      # new_data[1,'VE/VCO2 slope'] <- ifelse("ve_btps" %in% colnames(resp_data) & "vco2_stpd" %in% colnames(resp_data),
      #                                    round(lm(resp_data$ve_btps ~ resp_data$vco2_stpd)$coefficients[[2]],2),NA)

      # Ventilatory efficiency (as reported from the Parvo output).
      new_data[1,'VE/VCO2 slope'] <- ifelse(length(which(temp=="Ve/Vco2 Slope"))>0, temp[which(temp=="Ve/Vco2 Slope"),2], NA)
      new_data[1,'Peak Speed'] <-  ifelse("tm_spd" %in% colnames(resp_data), resp_data[index,'tm_spd'], NA)
      new_data[1,'Peak Grade'] <-  ifelse("tm_grd" %in% colnames(resp_data), resp_data[index,'tm_grd'], NA)
      new_data[1,'Peak Workrate Cycle'] <-  ifelse("bike_meas" %in% colnames(resp_data), resp_data[index,'bike_meas'], NA)


      # Renames some columns.
      new_data <- new_data %>% rename("ET time"="time","Peak VO2 (ml/kg/min)"="vo2_kg_stpd",
                                      "Peak RER"="rer", "VO2 Quality Check"="vo2_check")
      # Moves the quality check column to first in the data frame and rearranges order of output.
      new_data <- new_data %>% select("VO2 Quality Check", everything())
      new_data <- new_data %>% select("VO2 Quality Check":"Met Cart","ET time", "Peak Speed",	"Peak Grade",	"Peak Workrate Cycle",
                                      "Peak RER",	"Peak VO2 (L)",	"Peak VO2 (ml/kg/min)",	"Criteria for peak VO2",	"Peak VE (BTPS)",
                                      "Peak PetCO2",	"Peak HR",	"Peak O2 sat",	"VE/VCO2 slope")
      # Recodes the Mode.
      new_data$`Test Mode` <- recode(new_data$`Test Mode`,"Bike"="CY", "Treadmill"="TM")

      # Combines subject data frame into one master data frame and drops the other data frames.
      data <- bind_rows(data, new_data)
      rm(temp, vo2, new_data, resp_data,index,ind,n_cols,temp_vec)
    }
    # Reorders so "Check" is at top.
    data <- data[order(data$`VO2 Quality Check`),]
    return(data)
  }
