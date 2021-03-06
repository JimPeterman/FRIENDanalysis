
#' Summary of maximal Cosmed data
#'
#' @description
#' Creates a summary file of maximal data from a Cosmed metabolic cart.
#'
#' \emph{Data from the Cosmed must be saved in .xlsx formats.}
#'
#' @import dplyr
#' @import janitor
#' @import tibble
#' @import readxl
#' @import stringr
#'
#' @param path Location of the Cosmed files
#'
#' @return Creates a dataframe summarizing all of the Cosmed metabolic files.
#'
#' @seealso To make it easier to define the path (find the location of the files),
#' use the function: \code{\link[FRIENDanalysis]{folder_location}}.
#'
#' @export


# ------------------------------------------------
read_Cosmed_data <- function(path) {

  # Make sure that path is correct format (adds "/" if needed):
  if(stringr::str_sub(path, start = -1) != "/") {
    path <- paste(path, "/*.xlsx", sep = "")} else {
      path <- paste(path, "*.xlsx", sep = "")
    }
  # Find all files in the folder:
  files <- Sys.glob(paths = path)

  # Drops files that are open/hidden within the folder.
  files <- files[!grepl('/~',files)]

  # Initialize empty data frame that'll compile the data.
  data <- tibble()

  # Read in files as temporary data frame:
  for(i in 1:length(files)) {
    temp <- suppressMessages(read_excel(files[i], col_names = F))

    # Inialize empty data frame that'll have summary data from subject file.
    new_data <- tibble()

    # Add in the subject data.
    # Name.
    new_data[1,'Name (DELETE before submitting)'] <- paste(temp[[2,2]],temp[[3,2]],sep = ", ")
    # ID number.
    new_data[1,'Individual #'] <- i
    # Facility
    new_data[1,'Facility'] <- NA
    # Test Date.
    new_data[1,'Test Date'] <- paste(temp[[1,5]])
    # Age.
    new_data[1,'Age'] <- temp[[5,2]]
    # Gender.
    new_data[1,'Gender'] <- temp[[4,2]]
    # Height.
    new_data[1,'Height'] <- as.numeric(temp[[6,2]])
    # Weight.
    new_data[1,'Weight'] <- round(as.numeric(temp[[7,2]]),1)
    # Metabolic cart.
    new_data[1,'Met Cart'] <- "Cosmed"
    # Criteria for VO2peak.
    new_data[1,'Criteria for peak VO2'] <- "20sec avg (FRIEND script)"

    # Create data frame of just respiratory data from the file.
    # Creates a data frame that starts at the "TIME" header and goes to the bottom of the file.
    resp_data <- temp[,-(1:9)]
    # Add in the column names (combining the first and second rows).
    colnames(resp_data) <- paste(resp_data[1,], resp_data[2,])
    # Drops the first 3 rows so it's just the numbers.
    resp_data <- resp_data[-(1:3),]
    # Cleans column names.
    resp_data <- resp_data %>%
      janitor::clean_names()
    resp_data[] <- suppressWarnings(lapply(resp_data, as.numeric))
    # Adjusts units for certain variables.
    resp_data$t_s <- (resp_data$t_s)*1440
    resp_data$vo2_m_l_min <- (resp_data$vo2_m_l_min)/1000
    resp_data <- resp_data %>%
      round(.,2)

    # Creates a few columns if they are missing.
    if(!"rq" %in% colnames(resp_data)) {
      resp_data$rq <- NA
    }

    # Creates data frame of just respiratory variables of interest (time,VO2,RER).
    vo2 <- resp_data %>%
      select(t_s, vo2_kg_m_l_min_kg, rq)
    # Find the highest VO2 (/kg) and the values on either side of that value.
    index <- which.max(vo2$vo2_kg_m_l_min_kg)
    vo2 <- vo2[c((index-1):(index + 1)),]

    # The highest VO2 could occur at the last row which messes up the check below.
    # So if the highest is the last row, this adds a VO2 value that is -1ml/kg/min to let the check below run.
    if(is.na(vo2[3,'vo2_kg_m_l_min_kg'])) {vo2[3,'vo2_kg_m_l_min_kg'] <- (vo2[2,'vo2_kg_m_l_min_kg']-1)}

    # Does a check to see if the peak value was within +/- 2ml/kg/min of the previous values.
    # Prints "Good" if okay and "Check" if not.
    # Reduces the vo2 data frame to just the max vo2 and time.
    vo2 <- vo2 %>%
      mutate(vo2_check = if_else(max(vo2_kg_m_l_min_kg) - lead(vo2_kg_m_l_min_kg)  < 2 |
                                   max(vo2_kg_m_l_min_kg) - lag(vo2_kg_m_l_min_kg)  < 2, "Good", "Check")) %>%
      filter(vo2_kg_m_l_min_kg == max(vo2_kg_m_l_min_kg))

    # Combines data frames so there's one summary data frame for the subject.
    new_data <- bind_cols(new_data, vo2)

    new_data[1,'Peak VO2 (L)'] <- ifelse("vo2_m_l_min" %in% colnames(resp_data), resp_data[index,'vo2_m_l_min'], NA)
    new_data[1,'Peak HR'] <- ifelse("hr_bpm" %in% colnames(resp_data), resp_data[index,'hr_bpm'], NA)
    new_data[1,'Peak VE (BTPS)'] <- ifelse("ve_l_min" %in% colnames(resp_data), resp_data[index,'ve_l_min'], NA)
    new_data[1,'Peak PetCO2'] <- ifelse("pet_co2_mm_hg" %in% colnames(resp_data), resp_data[index,'pet_co2_mm_hg'], NA)
    new_data[1,'Peak O2 sat'] <- ifelse("sp_o2_percent" %in% colnames(resp_data), resp_data[index,'sp_o2_percent'], NA)

    # Adds in other variables if present in the Cosmed file.
    new_data[1,'Peak VO2 (L)'] <- ifelse("vo2_m_l_min" %in% colnames(resp_data), resp_data[index,'vo2_m_l_min'], NA)
    new_data[1,'Peak HR'] <- ifelse("hr_bpm" %in% colnames(resp_data), resp_data[index,'hr_bpm'], NA)
    new_data[1,'Peak VE (BTPS)'] <- ifelse("ve_l_min" %in% colnames(resp_data), resp_data[index,'ve_l_min'], NA)
    new_data[1,'Peak PetCO2'] <- ifelse("pet_co2_mm_hg" %in% colnames(resp_data), resp_data[index,'pet_co2_mm_hg'], NA)
    new_data[1,'Peak O2 sat'] <- ifelse("sp_o2_percent" %in% colnames(resp_data), resp_data[index,'sp_o2_percent'], NA)
    # Ventalitory efficiency (determined as the value at max).
    new_data[1,'VE/VCO2 slope'] <- ifelse("ve_vco2" %in% colnames(resp_data), resp_data[index,'ve_vco2'], NA)

    #############################################

    new_data[1,'Peak Speed'] <- ifelse("speed_mph" %in% colnames(resp_data), resp_data[index,'speed_mph'], NA)
    new_data[1,'Peak Grade'] <- ifelse("grade_percent" %in% colnames(resp_data), resp_data[index,'grade_percent'], NA)
    new_data[1,'Peak Workrate Cycle'] <- ifelse("power_watt" %in% colnames(resp_data), resp_data[index,'power_watt'], NA)
    #############################################

    # Test mode.
    new_data[1,'Test Mode'] <- ifelse("speed_mph" %in% colnames(resp_data), "TM",
                                      ifelse("power_watt" %in% colnames(resp_data), "CY", NA))

    # When RER is missing, error message printed.
    if(is.na(vo2$rq)) {new_data$vo2_check <- "RER/RQ Missing"}

    # Renames some columns.
    new_data <- new_data %>% rename("ET time"="t_s","Peak VO2 (ml/kg/min)"="vo2_kg_m_l_min_kg",
                                    "Peak RER"="rq", "Quality Check"="vo2_check")
    # Moves the quality check column to first in the data frame and rearranges order of output.
    new_data <- new_data %>% select("Quality Check", everything())

    new_data <- new_data %>% select("Quality Check":"Weight", "Test Mode", "Met Cart","ET time", "Peak Speed",
                                    "Peak Grade",	"Peak Workrate Cycle", "Peak RER",	"Peak VO2 (L)",
                                    "Peak VO2 (ml/kg/min)",	"Criteria for peak VO2",	"Peak VE (BTPS)",
                                    "Peak PetCO2",	"Peak HR",	"Peak O2 sat",	"VE/VCO2 slope")


    # Combines subject data frame into one master data frame and drops the other data frames.
    data <- bind_rows(data, new_data)
    rm(temp, vo2, new_data, resp_data,index)
  }
  # Reorders so "Check" is at top.
  data <- data[order(factor(data$`Quality Check`, levels = c("RER Missing", "Check", "Good"))),]
  return(data)
}
