
#' Determine Ventilatory Threshold FRIEND percentile
#'
#' Calculates age- and sex-based ventilatory threshold percentiles.\cr
#' Percentiles are from the FRIEND Registry (Vainshelboim et al. Chest 2019)
#'
#' @import dplyr
#'
#' @param VT The ventilatory threshold (ml/kg/min).
#' @param age Participant's age (years).
#' @param sex Participant's sex (must begin with 'm' for male/man and
#' 'f' or 'w' for female/woman - not case sensitive).
#' @param ex_mode The exercise test mode (must begin with 't' for treadmill or
#' 'c' for cycling - not case sensitive).
#'
#' @return Returns age- and sex-based percentiles.\cr
#' When exercise test mode is unknown, the treadmill reference standard is used.
#' Also percentiles above 95 are classified as 98 percent and
#' percentiles below 5 are classified as 3 percent.\cr
#' \emph{Only returns percentiles for those aged 20-79 years.}
#'
#'
#' @export




VT_FRIENDpercentile <- function(VT, age, sex, ex_mode){
  # Create dataframes for the TREADMILL classifications
  Perc <- c(95, 90, 85, 80, 75, 70, 65, 60, 55, 50, 45, 40, 35,
            30, 25, 20, 15, 10, 5)
  m20 <- c(34.4, 30.4, 28.4, 27.5, 26.3, 25.7, 24.5, 23.3, 22.5, 22, 21, 20.4,
           19.7, 18.7, 17.9, 17.1, 16.6, 15.6, 13.8)
  m30 <- c(33.3, 30.6, 28.9, 27, 26.2, 25.6, 24.8, 24.5, 22.8, 21.3, 20.8,
           20.5, 19.9, 19.2, 17.8, 17.1, 16.5, 15.2, 14)
  m40 <- c(32.6, 28.8, 26.3, 25.3, 24.1, 23.5, 22.6, 22, 21.2, 20.1, 19.6,
           18.8, 18.3, 17.4, 16.7, 16.3, 15.2, 14.4, 12.9)
  m50 <- c(30.4, 27.2, 25, 23.8, 22.8, 22.2, 21.4, 20, 19.3, 18.1, 17.4,
           17, 16.5, 16, 15.6, 15.1, 14.8, 13.7, 12.6)
  m60 <- c(27.9, 24.8, 22.9, 21.5, 20.9, 20.3, 19.1, 18.8, 18.2, 17.7,
           17.2, 16.5, 16.2, 15.5, 15, 14.2, 13.5, 12.5, 12.1)
  m70 <- c(25.5, 24.2, 23.5, 22.9, 22, 21.9, 20.8, 19.3, 17.9, 17.2,
           16.1, 14.9, 14.5, 14.1, 13.7, 12.6, 12.5, 11.6, 10.1)

  f20 <- c(40.6, 34.2, 32.7, 30.3, 27.7, 26.7, 25.4, 24.9, 23.1, 21.7,
           20.9, 20, 18, 16.9, 16.3, 15.4, 14.7, 13.7, 11.7)
  f30 <- c(27.1, 23.1, 21.6, 19.8, 19.2, 18.8, 18, 17.6, 17.1, 16.6,
           15.8, 15.1, 14.6, 14.2, 13.7, 13, 12.8, 12.5, 11.6)
  f40 <- c(24.9, 23.4, 21.6, 20.7, 19.3, 18.5, 17.5, 17.4, 16.5, 16.2,
           15.9, 15.3, 15.1, 14.5, 14.3, 13.7, 12.9, 12, 10.7)
  f50 <- c(23.1, 20.7, 19.8, 18.2, 17.5, 17, 16.3, 16, 15.6, 15, 14.5,
           14.1, 13.8, 13.6, 13.1, 12.9, 12.5, 12, 10.6)
  f60 <- c(19.7, 18.8, 16.9, 16.4, 16.1, 15.7, 15.3, 14.8, 14, 13.6,
           13.5, 13, 12.9, 12.6, 12.1, 11.6, 11.2, 10.5, 8.9)
  f70 <- c(19.7, 17.8, 16.2, 15.3, 14.1, 14, 13.5, 13.1, 12.8, 12.3,
           11.7, 11.1, 10.9, 10.6, 10.2, 10.1, 9.5, 8.9, 7.2)
  df_tm <- data.frame(Perc, m20, m30,m40, m50, m60, m70,
                      f20, f30,f40, f50, f60, f70)
  rm(Perc, m20, m30,m40, m50, m60, m70, f20, f30,f40, f50, f60, f70)

  # Create dataframes for the CYCLING classifications
  Perc <- c(95, 90, 85, 80, 75, 70, 65, 60, 55, 50, 45, 40, 35,
            30, 25, 20, 15, 10, 5)
  m20 <- c(25.8, 23.4, 22.5, 21.5, 20.9, 20, 19, 18.4, 18,
           17.8, 17, 17, 16, 16, 15, 15, 14, 13, 12)
  m30 <- c(24, 22, 21, 20, 19, 18.2, 18, 17, 17, 16, 16, 15,
           15, 14, 14, 13, 13, 12, 11)
  m40 <- c(22, 20, 19, 18, 17.3, 17, 16, 16, 15, 15, 14, 14,
           13.3, 13, 13, 12, 12, 11, 10)
  m50 <- c(20, 18.9, 17.2, 17, 16, 15.7, 15, 14.3, 14, 14,
           13, 13, 12.6, 12, 12, 11, 11, 10.4, 10)
  m60 <- c(19.1, 17, 16.1, 16, 15, 14.1, 14, 14, 13, 13,
           12.4, 12, 12, 11.9, 11, 11, 10.6, 10, 9.1)
  m70 <- c(16.1, 15, 14.2, 14, 13.1, 13, 12.1, 12, 11.8,
           11, 11, 11, 10.9, 10, 10, 10, 9.7, 8.7, 8)

  f20 <- c(24.8, 20, 18.9, 18, 16.9, 15.8, 15, 14.8, 14.1,
           13.9, 13.5, 13, 12.4, 12.1, 11.5, 11, 10.2, 9.4, 8.5)
  f30 <- c(22.8, 19.1, 18, 16.9, 15.8, 15, 14, 13.1, 12.9,
           12.3, 11.8, 11.5, 11, 10.9, 10.2, 9.9, 9.5, 9.1, 8.2)
  f40 <- c(21, 19, 17.8, 16.4, 15.6, 15, 14, 13.4, 12.8, 12.3,
           11.9, 11.4, 11, 10.5, 10.1, 9.8, 9.2, 8.6, 7.9)
  f50 <- c(19.9, 17.7, 16, 15.1, 14.2, 13.4, 12.8, 12.5, 12.2,
           11.9, 11.4, 11.1, 10.9, 10.4, 10, 9.6, 9.1, 8.6, 7.7)
  f60 <- c(18.2, 16.4, 14.5, 13.8, 12.9, 12.6, 12.3, 12, 11.5,
           11.2, 10.9, 10.8, 10.4, 9.8, 9.5, 9.1, 8.5, 7.7, 7.2)
  f70 <- c(14.7, 13.8, 13.1, 11.7, 11.6, 11.3, 11.1, 10.8, 10.7,
           10.7, 9.9, 9.6, 9, 9, 8.9, 8.5, 8, 7.8, 5.7)
  df_c <- data.frame(Perc, m20, m30,m40, m50, m60, m70,
                     f20, f30,f40, f50, f60, f70)
  rm(Perc, m20, m30,m40, m50, m60, m70, f20, f30,f40, f50, f60, f70)


  FRIENDpercVT <- vector(mode = "integer", length = length(VT))
  for(i in 1:length(VT)){

    # Sort by the exercise test mode.
    if(substr(tolower(ex_mode[i]), 1, 1) != "c" | is.na(ex_mode[i])){
      temp_df <- df_tm
    }
    if(!(is.na(ex_mode[i])) & substr(tolower(ex_mode[i]), 1, 1) == "c"){
      temp_df <- df_c
    }

    # Determine age and sex so you can use that column/df.
    if(!(is.na(sex[i])) & substr(tolower(sex[i]), 1, 1) == "m"){
      name_col <- ifelse(age[i] >= 20 & age[i] < 30, "m20",
                         ifelse(age[i] >= 30 & age[i] < 40, "m30",
                                ifelse(age[i] >= 40 & age[i] < 50, "m40",
                                       ifelse(age[i] >= 50 & age[i] < 60, "m50",
                                              ifelse(age[i] >= 60 & age[i] < 70, "m60",
                                                     ifelse(age[i] >= 70 & age[i] < 80, "m70", NA))))))
    }

    if(!(is.na(sex[i])) &
       (substr(tolower(sex[i]), 1, 1) == "f" | substr(tolower(sex[i]), 1, 1) == "w")){
      name_col <- ifelse(age[i] >= 20 & age[i] < 30, "f20",
                         ifelse(age[i] >= 30 & age[i] < 40, "f30",
                                ifelse(age[i] >= 40 & age[i] < 50, "f40",
                                       ifelse(age[i] >= 50 & age[i] < 60, "f50",
                                              ifelse(age[i] >= 60 & age[i] < 70, "f60",
                                                     ifelse(age[i] >= 70 & age[i] < 80, "f70", NA))))))
    }

    if(is.na(sex[i])){
      name_col <- NA
    }

    if(!(is.na(name_col))) {

      if(!is.na(VT[i])){
        if(VT[i] < temp_df[1, name_col] & VT[i] > temp_df[19, name_col]){
          # Find percentiles/values above/below of the given VT.

          # When a value is the same across multiple percentiles, use the largest.
          if((length(which(temp_df[,name_col] == VT[i])))>1){
            FRIENDpercVT[i] <- temp_df[(min(which(temp_df[,name_col] == VT[i]))), "Perc"]
          } else {
            # Otherwise find difference between two closest percentiles.
            highPerc <- max(which(temp_df[,name_col] >= VT[i]))
            lowPerc <- min(which(temp_df[,name_col] < VT[i]))

            highVT <- temp_df[highPerc, name_col]
            lowVT <- temp_df[lowPerc, name_col]

            # Determine the FRIEND percentile.
            # VT per percentile
            percPerVT <- (5 / (highVT - lowVT))

            overLowVT <- round((VT[i] - lowVT), 2)

            addPerc <- percPerVT * overLowVT

            FRIENDpercVT[i] <- round(((temp_df[lowPerc, "Perc"]) + addPerc),0)
          }

        }

        if(VT[i] > temp_df[1, name_col]){
          FRIENDpercVT[i] <- 98
        }

        if(VT[i] == temp_df[1, name_col]){
          FRIENDpercVT[i] <- temp_df[1, "Perc"]
        }

        if(VT[i] < temp_df[19, name_col]){
          FRIENDpercVT[i] <- 3
        }

        if(VT[i] == temp_df[19, name_col]){
          FRIENDpercVT[i] <- temp_df[19, "Perc"]
        }
      }
    }

    if(is.na(VT[i])){
      FRIENDpercVT[i] <- NA
    }

    if(is.na(name_col)) {
      FRIENDpercVT[i] <- NA
    }
    rm(name_col, temp_df)
  }
  return(FRIENDpercVT)
}













