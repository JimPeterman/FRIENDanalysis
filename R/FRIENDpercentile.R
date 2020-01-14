#' Determine FRIEND fitness percentile
#'
#' @description
#' Calculates age- and sex-based fitness percentiles from relative VO2max values.
#' Fitness percentiles are from the FRIEND Registry (a database of
#' directly-measured VO2max tests) for males and females aged 20-79 years.
#'
#' Treadmill fitness percentiles are from Kaminsky et al. Mayo Clin Proc 2015
#' Cycling fitness percentiles are from Kaminsky et al. Mayo Clin Proc 2017
#'
#' \emph{When exercise test mode is unknown, the treadmill reference standard is used.
#' Also note, those with a percentile above 95 are classified as 98% and those with
#' a percentile below 5 are classified as 3%.}
#'
#'
#' @import dplyr
#'
#' @param VO2 The relative VO2max (ml/kg/min)
#' @param age Participant's age in years
#' @param sex Participant's sex (must begin with "m" for male and "f" for female)
#' @param ex_mode The exercise test mode (must begin with "t" for treadmill or "c" for cycling)
#'
#' @return Calculates age- and sex-based fitness percentiles from relative VO2max values.
#'
#'
#' @export




FRIENDpercentile <- function(VO2, age, sex, ex_mode){
  # Create dataframes for the TREADMILL classifications
  Perc <- c(95, 90, 85, 80, 75, 70, 65, 60, 55, 50, 45, 40, 35,
            30, 25, 20, 15, 10, 5)
  m20 <- c(66.3, 61.8, 59.3, 57.1, 55.2, 53.7, 52.1, 50.2, 49.0,
           48.0, 46.5, 44.9, 43.5, 41.9, 40.1, 38.1, 35.4, 32.1, 29.0)
  m30 <- c(59.8, 56.5, 54.2, 51.6, 49.2, 48.0, 46.6, 45.2, 43.8,
           42.4, 41.3, 39.6, 38.5, 37.4, 35.9, 34.1, 32.7, 30.2, 27.2)
  m40 <- c(55.6, 52.1, 49.3, 46.7, 45.0, 43.9, 42.1, 40.3, 38.9,
           37.8, 36.7, 35.7, 34.6, 33.3, 31.9, 30.5, 29.0, 26.8, 24.2)
  m50 <- c(50.7, 45.6, 43.2, 41.2, 39.7, 38.2, 36.3, 35.1, 33.8,
           32.6, 31.6, 30.7, 29.5, 28.4, 27.1, 26.1, 24.4, 22.8, 20.9)
  m60 <- c(43.0, 40.3, 38.2, 36.1, 34.5, 32.9, 31.6, 30.5, 29.1,
           28.2, 27.2, 26.6, 25.7, 24.6, 23.7, 22.4, 21.2, 19.8, 17.4)
  m70 <- c(39.7, 36.6, 35.5, 31.4, 30.4, 28.4, 27.6, 26.9, 25.6,
           24.4, 24.0, 22.8, 22.4, 21.2, 20.4, 19.2, 18.2, 17.1, 16.3)

  f20 <- c(56.0, 51.3, 48.3, 46.5, 44.7, 43.2, 41.6, 40.6, 38.9,
           37.6, 35.9, 34.6, 33.6, 32.0, 30.5, 28.6, 26.2, 23.9, 21.7)
  f30 <- c(45.8, 41.4, 39.3, 37.5, 36.1, 34.6, 33.5, 32.2, 31.2,
           30.2, 29.3, 28.2, 27.4, 26.4, 25.3, 24.1, 22.5, 20.9, 19.0)
  f40 <- c(41.7, 38.4, 36.0, 34.0, 32.4, 31.1, 30.0, 28.7, 27.7,
           26.7, 25.9, 24.9, 24.1, 23.3, 22.1, 21.3, 20.0, 18.8, 17.0)
  f50 <- c(35.9, 32.0, 30.2, 28.6, 27.6, 26.8, 26.0, 25.2, 24.4,
           23.4, 22.7, 21.8, 21.2, 20.6, 19.9, 19.1, 18.3, 17.3, 16.0)
  f60 <- c(29.4, 27.0, 25.6, 24.6, 23.8, 23.1, 22.0, 21.2, 20.5,
           20.0, 19.6, 18.9, 18.4, 17.9, 17.2, 16.5, 15.6, 14.6, 13.4)
  f70 <- c(24.1, 23.1, 22.2, 21.3, 20.8, 20.5, 19.9, 19.4, 19.2,
           18.3, 17.8, 17.0, 16.8, 15.9, 15.6, 15.1, 14.6, 13.6, 13.1)
  df_tm <- data.frame(Perc, m20, m30,m40, m50, m60, m70,
                      f20, f30,f40, f50, f60, f70)
  rm(Perc, m20, m30,m40, m50, m60, m70, f20, f30,f40, f50, f60, f70)

  # Create dataframes for the CYCLING classifications
  Perc <- c(95, 90, 85, 80, 75, 70, 65, 60, 55, 50, 45, 40, 35,
            30, 25, 20, 15, 10, 5)
  m20 <- c(58.5, 55.5, 53.9, 51.4, 49.5, 47.9, 46.0, 44.5, 43.1, 41.9,
           40.2, 38.3, 37.6, 36.2, 34.7, 33.2, 31.8, 29.5, 25.5)
  m30 <- c(44.7, 41.7, 38.1, 36.2, 35.0, 33.9, 31.8, 31.1, 30.7, 30.1,
           29.4, 28.1, 27.5, 26.9, 26.2, 25.4, 23.9, 21.8, 19.3)
  m40 <- c(41.9, 37.1, 34.9, 34.2, 31.8, 30.4, 29.3, 28.6, 28.0, 27.1,
           26.2, 25.4, 24.9, 24.0, 22.9, 22.2, 21.6, 20.6, 18.9)
  m50 <- c(37.4, 34.0, 32.1, 30.7, 29.3, 28.2, 27.1, 26.3, 25.7, 24.8,
           24.2, 23.6, 23.0, 22.6, 22.1, 21.5, 20.8, 20.4, 18.1)
  m60 <- c(32.4, 29.9, 27.8, 26.7, 25.5, 24.5, 24.0, 23.2, 22.9, 22.4,
           21.9, 21.4, 21.0, 20.2, 19.7, 19.0, 18.4, 17.3, 15.3)
  m70 <- c(34.0, 28.1, 26.4, 24.5, 22.5, 21.9, 21.1, 20.4, 19.9, 19.5,
           18.7, 18.5, 18.3, 17.5, 17.1, 16.7, 16.1, 15.8, 14.4)

  f20 <- c(45.2, 42.6, 40.9, 38.8, 37.1, 35.6, 34.6, 33.6, 32.4, 31.0,
           29.8, 28.1, 26.6, 25.6, 23.2, 21.6, 20.4, 19.3, 17.1)
  f30 <- c(33.2, 30.0, 27.8, 26.0, 25.1, 24.2, 23.3, 22.5, 22.1, 21.6,
           21.0, 20.1, 19.5, 18.8, 17.9, 17.0, 16.3, 15.2, 14.4)
  f40 <- c(29.3, 26.2, 24.4, 23.4, 22.6, 22.0, 21.4, 20.7, 20.0, 19.4,
           18.8, 18.4, 17.9, 17.1, 16.5, 15.8, 15.4, 14.6, 13.5)
  f50 <- c(25.0, 22.6, 21.5, 20.7, 20.1, 19.3, 18.9, 18.2, 17.7, 17.3,
           17.0, 16.6, 16.2, 15.7, 15.3, 14.9, 14.4, 13.7, 12.8)
  f60 <- c(22.0, 20.5, 19.3, 18.8, 18.3, 17.8, 17.3, 16.7, 16.3, 16.0,
           15.7, 15.4, 15.1, 14.7, 14.4, 14.0, 13.5, 13.0, 12.2)
  f70 <- c(19.2, 18.0, 17.4, 16.9, 16.5, 16.1, 15.8, 15.4, 15.1, 14.8,
           14.5, 14.2, 13.7, 13.6, 13.2, 12.8, 12.5, 12.0, 11.3)
  df_c <- data.frame(Perc, m20, m30,m40, m50, m60, m70,
                     f20, f30,f40, f50, f60, f70)
  rm(Perc, m20, m30,m40, m50, m60, m70, f20, f30,f40, f50, f60, f70)


  FRIENDperc <- vector(mode = "integer", length = length(VO2))
  for(i in 1:length(VO2)){

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

    if(!(is.na(sex[i])) & substr(tolower(sex[i]), 1, 1) == "f"){
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

      if(!is.na(VO2[i])){
        if(VO2[i] < temp_df[1, name_col] & VO2[i] > temp_df[19, name_col]){
          # Find percentiles/values above/below of the given VO2.
          highPerc <- max(which(temp_df[,name_col] >= VO2[i]))
          lowPerc <- min(which(temp_df[,name_col] < VO2[i]))

          highVO2 <- temp_df[highPerc, name_col]
          lowVO2 <- temp_df[lowPerc, name_col]

          # Determine the FRIEND percentile.
          # VO2 per percentile
          percPerVO2 <- (5 / (highVO2 - lowVO2))

          overLowVO2 <- round((VO2[i] - lowVO2), 2)

          addPerc <- percPerVO2 * overLowVO2

          FRIENDperc[i] <- round(((temp_df[lowPerc, "Perc"]) + addPerc),0)
        }

        if(VO2[i] > temp_df[1, name_col]){
          FRIENDperc[i] <- 98
        }

        if(VO2[i] == temp_df[1, name_col]){
          FRIENDperc[i] <- temp_df[1, "Perc"]
        }

        if(VO2[i] < temp_df[19, name_col]){
          FRIENDperc[i] <- 3
        }

        if(VO2[i] == temp_df[19, name_col]){
          FRIENDperc[i] <- temp_df[19, "Perc"]
        }
      }
    }

    if(is.na(VO2[i])){
      FRIENDperc[i] <- NA
    }

    if(is.na(name_col)) {
      FRIENDperc[i] <- NA
    }
    rm(name_col, temp_df)
  }
  return(FRIENDperc)
}

