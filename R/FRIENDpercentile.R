
#' Determine FRIEND fitness percentile
#'
#' Calculates age- and sex-based fitness percentiles from relative VO2max values.\cr
#' Fitness percentiles are from the FRIEND Registry.
#'
#' @import dplyr
#'
#' @param VO2 The relative VO2max (ml/kg/min).
#' @param age Participant's age (years).
#' @param sex Participant's sex (must begin with 'm' for male/man and
#' 'f' or 'w' for female/woman - not case sensitive).
#' @param ex_mode The exercise test mode (must begin with 't' for treadmill or
#' 'c' for cycling - not case sensitive).
#' @param ref_edition Default uses standards from 2022 publication.
#' Use '1' (numeric or character) for original 2015 (TM) and 2017 (CY) standards.
#'
#' @return Returns age- and sex-based fitness percentile.\cr
#' When exercise test mode is unknown, the treadmill reference standard is used.
#' The 2022 reference standards used are based on inclusion criteria of RER ≥ 1.10.\cr
#'
#' \emph{Only returns percentiles for those aged 20-89 years
#' (only 20-79 years if using 2015/2017 standards).}
#'
#'
#' @export




FRIENDpercentile <- function(VO2, age, sex, ex_mode, ref_edition=2){

  Perc <- c(99.8, 95, 90, 85, 80, 75, 70, 65, 60, 55, 50, 45, 40, 35,
            30, 25, 20, 15, 10, 5, 0.2)

  # Create dataframes for the TREADMILL classifications.
  if(ref_edition == 1 | ref_edition == "1"){
    m20 <- c(81.9, 66.3, 61.8, 59.3, 57.1, 55.2, 53.7, 52.1, 50.2, 49.0,
             48.0, 46.5, 44.9, 43.5, 41.9, 40.1, 38.1, 35.4, 32.1, 29.0, 11.2)
    m30 <- c(74.4, 59.8, 56.5, 54.2, 51.6, 49.2, 48.0, 46.6, 45.2, 43.8,
             42.4, 41.3, 39.6, 38.5, 37.4, 35.9, 34.1, 32.7, 30.2, 27.2, 15.3)
    m40 <- c(77.1, 55.6, 52.1, 49.3, 46.7, 45.0, 43.9, 42.1, 40.3, 38.9,
             37.8, 36.7, 35.7, 34.6, 33.3, 31.9, 30.5, 29.0, 26.8, 24.2, 15.2)
    m50 <- c(68.1, 50.7, 45.6, 43.2, 41.2, 39.7, 38.2, 36.3, 35.1, 33.8,
             32.6, 31.6, 30.7, 29.5, 28.4, 27.1, 26.1, 24.4, 22.8, 20.9, 10.7)
    m60 <- c(58.4, 43.0, 40.3, 38.2, 36.1, 34.5, 32.9, 31.6, 30.5, 29.1,
             28.2, 27.2, 26.6, 25.7, 24.6, 23.7, 22.4, 21.2, 19.8, 17.4, 11.1)
    m70 <- c(44.1, 39.7, 36.6, 35.5, 31.4, 30.4, 28.4, 27.6, 26.9, 25.6,
             24.4, 24.0, 22.8, 22.4, 21.2, 20.4, 19.2, 18.2, 17.1, 16.3, 13.9)
    m80 <- NA

    f20 <- c(64.4, 56.0, 51.3, 48.3, 46.5, 44.7, 43.2, 41.6, 40.6, 38.9,
             37.6, 35.9, 34.6, 33.6, 32.0, 30.5, 28.6, 26.2, 23.9, 21.7, 12.2)
    f30 <- c(72.2, 45.8, 41.4, 39.3, 37.5, 36.1, 34.6, 33.5, 32.2, 31.2,
             30.2, 29.3, 28.2, 27.4, 26.4, 25.3, 24.1, 22.5, 20.9, 19.0, 12.5)
    f40 <- c(57.9, 41.7, 38.4, 36.0, 34.0, 32.4, 31.1, 30.0, 28.7, 27.7,
             26.7, 25.9, 24.9, 24.1, 23.3, 22.1, 21.3, 20.0, 18.8, 17.0, 9.9)
    f50 <- c(48.2, 35.9, 32.0, 30.2, 28.6, 27.6, 26.8, 26.0, 25.2, 24.4,
             23.4, 22.7, 21.8, 21.2, 20.6, 19.9, 19.1, 18.3, 17.3, 16.0, 9.9)
    f60 <- c(39.9, 29.4, 27.0, 25.6, 24.6, 23.8, 23.1, 22.0, 21.2, 20.5,
             20.0, 19.6, 18.9, 18.4, 17.9, 17.2, 16.5, 15.6, 14.6, 13.4, 9.2)
    f70 <- c(28.5, 24.1, 23.1, 22.2, 21.3, 20.8, 20.5, 19.9, 19.4, 19.2,
             18.3, 17.8, 17.0, 16.8, 15.9, 15.6, 15.1, 14.6, 13.6, 13.1, 7.9)
    f80 <- NA

  } else {
    m20 <- c(74.7, 62.1, 57.8, 56.0, 54.2, 52.6, 50.9, 49.4, 48.2, 46.9,
             45.4, 44.2, 42.8, 41.0, 39.2, 37.3, 34.8, 32.0, 28.8, 24.8, 11.6)
    m30 <- c(68.4, 57.9, 54.3, 51.2, 48.7, 46.5, 45.1, 43.5, 41.9, 40.1,
             38.6, 37.4, 35.9, 34.3, 32.8, 31.3, 29.4, 27.2, 25.0, 20.6, 7.9)
    m40 <- c(64.0, 53.2, 49.5, 46.1, 44.0, 41.8, 40.0, 38.5, 37.2, 36.0,
             34.8, 33.4, 32.1, 31.1, 29.7, 28.4, 26.9, 25.4, 22.9, 19.7, 11.2)
    m50 <- c(61.5, 46.8, 42.7, 40.1, 37.5, 35.5, 34.1, 32.9, 31.8, 30.7,
             29.4, 28.2, 27.2, 26.2, 25.3, 23.9, 22.7, 21.4, 19.2, 16.5, 8.6)
    m60 <- c(52.0, 40.2, 36.4, 33.6, 31.6, 29.9, 28.7, 27.6, 26.5, 25.3,
             24.4, 23.6, 22.8, 21.8, 20.8, 19.7, 18.6, 17.5, 16.1, 13.8, 7.2)
    m70 <- c(44.0, 35.2, 29.6, 27.6, 26.3, 25.0, 23.9, 22.9, 22.3, 21.5,
             20.6, 19.9, 19.3, 18.5, 17.6, 16.8, 16.0, 15.0, 13.6, 11.6, 6.5)
    m80 <- c(29.7, 25.6, 23.6, 22.3, 21.8, 20.9, 20.4, 19.5, 18.8, 18.3,
             17.7, 17.0, 16.7, 16.3, 16.1, 15.9, 15.3, 13.9, 13.2, 12.2, 10.2)

    f20 <- c(61.7, 50.1, 47.3, 45.4, 44.1, 42.2, 41.2, 39.6, 38.0, 36.8,
             35.6, 34.2, 32.7, 31.3, 29.9, 28.6, 26.6, 24.5, 22.2, 19.3, 12.4)
    f30 <- c(56.1, 45.5, 41.1, 38.4, 36.2, 34.5, 33.3, 31.9, 30.7, 29.4,
             28.3, 27.3, 26.4, 25.4, 24.3, 23.1, 22.1, 20.7, 19.2, 16.6, 11.4)
    f40 <- c(54.6, 40.7, 37.5, 34.7, 32.8, 30.9, 29.8, 28.6, 27.7, 26.6,
             25.9, 24.9, 24.2, 23.2, 22.2, 21.3, 20.0, 19.0, 17.4, 15.3, 10.7)
    f50 <- c(47.1, 35.3, 31.8, 30.0, 28.4, 27.3, 26.4, 25.5, 24.7, 23.7,
             23.1, 22.3, 21.7, 21.0, 20.3, 19.5, 18.7, 17.6, 16.6, 14.5, 7.9)
    f60 <- c(40.1, 29.7, 27.3, 25.5, 24.1, 23.1, 22.2, 21.6, 20.8, 20.1,
             19.4, 18.8, 18.3, 17.7, 17.0, 16.4, 15.5, 14.7, 13.5, 12.0, 8.2)
    f70 <- c(32.0, 24.2, 22.8, 21.4, 20.6, 20.0, 19.2, 18.8, 18.2, 17.7,
             17.1, 16.7, 16.1, 15.7, 15.3, 14.8, 14.1, 13.7, 12.3, 11.3, 7.4)
    f80 <- c(45.1, 20.7, 19.9, 18.5, 18.0, 17.2, 16.6, 15.8, 15.5, 15.3,
             15.1, 14.6, 14.3, 13.8, 13.4, 12.8, 12.4, 11.9, 11.4, 10.7, 9.0)

  }

  df_tm <- data.frame(Perc, m20, m30, m40, m50, m60, m70, m80,
                      f20, f30, f40, f50, f60, f70, f80)
  rm(m20, m30, m40, m50, m60, m70, m80, f20, f30, f40, f50, f60, f70, f80)


  # Create dataframes for the CYCLING classifications
  if(ref_edition == 1 | ref_edition == "1"){
    m20 <- c(69.2, 58.5, 55.5, 53.9, 51.4, 49.5, 47.9, 46.0, 44.5, 43.1, 41.9,
             40.2, 38.3, 37.6, 36.2, 34.7, 33.2, 31.8, 29.5, 25.5, 11.6)
    m30 <- c(60.6, 44.7, 41.7, 38.1, 36.2, 35.0, 33.9, 31.8, 31.1, 30.7, 30.1,
             29.4, 28.1, 27.5, 26.9, 26.2, 25.4, 23.9, 21.8, 19.3, 14.3)
    m40 <- c(49.2, 41.9, 37.1, 34.9, 34.2, 31.8, 30.4, 29.3, 28.6, 28.0, 27.1,
             26.2, 25.4, 24.9, 24.0, 22.9, 22.2, 21.6, 20.6, 18.9, 14.3)
    m50 <- c(50.8, 37.4, 34.0, 32.1, 30.7, 29.3, 28.2, 27.1, 26.3, 25.7, 24.8,
             24.2, 23.6, 23.0, 22.6, 22.1, 21.5, 20.8, 20.4, 18.1, 11.4)
    m60 <- c(40.1, 32.4, 29.9, 27.8, 26.7, 25.5, 24.5, 24.0, 23.2, 22.9, 22.4,
             21.9, 21.4, 21.0, 20.2, 19.7, 19.0, 18.4, 17.3, 15.3, 11.4)
    m70 <- c(44.3, 34.0, 28.1, 26.4, 24.5, 22.5, 21.9, 21.1, 20.4, 19.9, 19.5,
             18.7, 18.5, 18.3, 17.5, 17.1, 16.7, 16.1, 15.8, 14.4, 10.9)
    m80 <- NA

    f20 <- c(61.7, 45.2, 42.6, 40.9, 38.8, 37.1, 35.6, 34.6, 33.6, 32.4, 31.0,
             29.8, 28.1, 26.6, 25.6, 23.2, 21.6, 20.4, 19.3, 17.1, 11.0)
    f30 <- c(53.1, 33.2, 30.0, 27.8, 26.0, 25.1, 24.2, 23.3, 22.5, 22.1, 21.6,
             21.0, 20.1, 19.5, 18.8, 17.9, 17.0, 16.3, 15.2, 14.4, 10.9)
    f40 <- c(37.5, 29.3, 26.2, 24.4, 23.4, 22.6, 22.0, 21.4, 20.7, 20.0, 19.4,
             18.8, 18.4, 17.9, 17.1, 16.5, 15.8, 15.4, 14.6, 13.5, 11.3)
    f50 <- c(37.5, 25.0, 22.6, 21.5, 20.7, 20.1, 19.3, 18.9, 18.2, 17.7, 17.3,
             17.0, 16.6, 16.2, 15.7, 15.3, 14.9, 14.4, 13.7, 12.8, 10.9)
    f60 <- c(32.5, 22.0, 20.5, 19.3, 18.8, 18.3, 17.8, 17.3, 16.7, 16.3, 16.0,
             15.7, 15.4, 15.1, 14.7, 14.4, 14.0, 13.5, 13.0, 12.2, 10.6)
    f70 <- c(22.4, 19.2, 18.0, 17.4, 16.9, 16.5, 16.1, 15.8, 15.4, 15.1, 14.8,
             14.5, 14.2, 13.7, 13.6, 13.2, 12.8, 12.5, 12.0, 11.3, 8.7)
    f80 <- NA
  } else {
    m20 <- c(73.3, 64.4, 61.0, 58.3, 55.8, 53.8, 51.4, 49.2, 47.2, 44.9,
             43.6, 42.0, 40.5, 38.3, 37.0, 35.7, 33.7, 31.9, 28.9, 24.6, 11.0)
    m30 <- c(70.6, 53.3, 45.6, 41.8, 38.1, 36.7, 35.3, 33.9, 31.5, 30.8,
             30.1, 29.1, 28.1, 27.2, 26.4, 24.5, 23.4, 21.4, 19.4, 16.2, 9.2)
    m40 <- c(56.4, 45.2, 40.5, 36.3, 34.6, 32.6, 31.3, 29.8, 28.9, 28.1,
             27.5, 26.3, 25.5, 24.9, 23.9, 22.8, 22.1, 21.3, 20.0, 18.3, 9.7)
    m50 <- c(60.4, 42.1, 35.8, 33.1, 31.4, 30.0, 28.4, 27.3, 26.4, 25.6,
             24.7, 24.0, 23.3, 22.7, 22.4, 21.7, 20.9, 19.6, 18.0, 14.9, 10.5)
    m60 <- c(53.3, 36.0, 30.5, 28.2, 26.6, 25.2, 24.4, 23.8, 23.2, 22.6,
             21.8, 21.3, 20.8, 20.1, 19.2, 18.7, 18.0, 16.7, 15.4, 12.8, 6.5)
    m70 <- c(43.9, 30.7, 27.1, 24.5, 23.2, 21.9, 21.3, 20.5, 19.9, 19.4,
             18.5, 18.2, 17.6, 17.1, 16.7, 16.0, 15.7, 14.4, 12.8, 10.1, 6.5)
    m80 <- c(37.7, 18.9, 18.8, 17.9, 17.5, 17.3, 17.2, 16.5, 15.5, 14.8,
             13.9, 13.3, 12.6, 12.2, 11.2, 10.8, 9.4, 9.0, 8.4, 7.9, 6.5)

    f20 <- c(65.4, 50.2, 45.0, 42.6, 40.9, 38.9, 37.2, 35.3, 34.0, 33.1,
             31.4, 30.3, 28.9, 27.3, 25.9, 24.1, 22.7, 20.9, 19.4, 17.3, 8.9)
    f30 <- c(66.6, 36.5, 31.7, 28.6, 26.8, 25.8, 24.6, 23.7, 23.1, 22.2,
             21.5, 20.8, 19.9, 19.3, 18.8, 17.9, 17.3, 16.4, 15.2, 13.9, 10.5)
    f40 <- c(49.8, 30.5, 26.8, 24.7, 23.5, 22.7, 21.9, 21.3, 20.5, 19.7,
             19.0, 18.6, 18.1, 17.3, 16.8, 16.0, 15.5, 14.9, 13.8, 12.7, 8.4)
    f50 <- c(38.5, 25.0, 22.6, 21.5, 20.7, 20.0, 19.3, 18.8, 18.2, 17.7,
             17.3, 16.9, 16.5, 16.1, 15.6, 15.2, 14.7, 14.3, 13.5, 12.5, 9.5)
    f60 <- c(33.1, 21.9, 20.3, 19.1, 18.6, 18.1, 17.6, 16.9, 16.5, 16.1,
             15.9, 15.5, 15.3, 14.9, 14.6, 14.2, 13.7, 13.2, 12.5, 11.3, 8.2)
    f70 <- c(22.3, 19.1, 18.0, 17.4, 16.8, 16.5, 16.0, 15.7, 15.3, 14.9,
             14.6, 14.4, 13.7, 13.6, 13.2, 12.8, 12.5, 12.0, 11.2, 10.1, 5.6)
    f80 <- c(18.2, 15.6, 14.1, 13.4, 13.1, 13.0, 12.7, 12.1, 11.6, 11.2,
             10.9, 10.4, 9.9, 9.4, 9.3, 9.0, 8.7, 8.6, 7.8, 6.8, 5.8)
  }

  df_c <- data.frame(Perc, m20, m30,m40, m50, m60, m70, m80,
                     f20, f30,f40, f50, f60, f70, f80)
  rm(Perc, m20, m30,m40, m50, m60, m70, m80, f20, f30,f40, f50, f60, f70, f80)


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
                                                     ifelse(age[i] >= 70 & age[i] < 80, "m70",
                                                            ifelse(age[i] >= 80 & age[i] < 90, "m80", NA)))))))
    }

    if(!(is.na(sex[i])) &
       (substr(tolower(sex[i]), 1, 1) == "f" | substr(tolower(sex[i]), 1, 1) == "w")){
      name_col <- ifelse(age[i] >= 20 & age[i] < 30, "f20",
                         ifelse(age[i] >= 30 & age[i] < 40, "f30",
                                ifelse(age[i] >= 40 & age[i] < 50, "f40",
                                       ifelse(age[i] >= 50 & age[i] < 60, "f50",
                                              ifelse(age[i] >= 60 & age[i] < 70, "f60",
                                                     ifelse(age[i] >= 70 & age[i] < 80, "f70",
                                                            ifelse(age[i] >= 80 & age[i] < 90, "f80", NA)))))))
    }

    if((ref_edition == 1 | ref_edition == "1") & (age[i] >= 80 | is.na(age[i]))){
      name_col <- NA
    }

    if(is.na(sex[i])){
      name_col <- NA
    }

    if(!(is.na(name_col))) {

        if(!is.na(VO2[i])){

          if(!((ref_edition == 1 | ref_edition == "1") & age[i] >= 80)){

            if(VO2[i] < temp_df[2, name_col] & VO2[i] > temp_df[20, name_col]){
              # Find percentiles/values above/below of the given VO2.

              # When a value is the same across multiple percentiles, use the largest.
              if((length(which(temp_df[,name_col] == VO2[i])))>1){
                FRIENDperc[i] <- temp_df[(min(which(temp_df[,name_col] == VO2[i]))), "Perc"]
              } else {
                # Otherwise find difference between two closest percentiles.
                highPerc <- max(which(temp_df[,name_col] >= VO2[i]))
                lowPerc <- min(which(temp_df[,name_col] < VO2[i]))

                highVO2 <- temp_df[highPerc, name_col]
                lowVO2 <- temp_df[lowPerc, name_col]

                # Determine the FRIEND percentile.
                # VO2 per percentile
                percPerVO2 <- (5 / (highVO2 - lowVO2))

                overLowVO2 <- round((VO2[i] - lowVO2), 2)

                addPerc <- percPerVO2 * overLowVO2

                FRIENDperc[i] <- round(((temp_df[lowPerc, "Perc"]) + addPerc), 1)
              }

            } else if(VO2[i] > temp_df[1, name_col]) {
            FRIENDperc[i] <- 99.9
          } else if(VO2[i] == temp_df[1, name_col]) {
            FRIENDperc[i] <- 99.8
          } else if(VO2[i] > temp_df[3, name_col]){

            # When a value is the same across multiple percentiles, use the largest.
            if((length(which(temp_df[,name_col] == VO2[i])))>1){
              FRIENDperc[i] <- temp_df[(min(which(temp_df[,name_col] == VO2[i]))), "Perc"]
            } else {
              # Otherwise find difference between two closest percentiles.
              highPerc <- max(which(temp_df[,name_col] >= VO2[i]))
              lowPerc <- min(which(temp_df[,name_col] < VO2[i]))

              highVO2 <- temp_df[highPerc, name_col]
              lowVO2 <- temp_df[lowPerc, name_col]

              # Determine the FRIEND percentile.
              # VO2 per percentile
              percPerVO2 <- (4.8 / (highVO2 - lowVO2))

              overLowVO2 <- round((VO2[i] - lowVO2), 2)

              addPerc <- percPerVO2 * overLowVO2

              FRIENDperc[i] <- round(((temp_df[lowPerc, "Perc"]) + addPerc), 1)
            }

          } else if(VO2[i] < temp_df[21, name_col]){
            FRIENDperc[i] <- 0.1
          } else if(VO2[i] == temp_df[21, name_col]){
            FRIENDperc[i] <- 0.2
          } else if (VO2[i] < temp_df[19, name_col]){
            # When a value is the same across multiple percentiles, use the largest.
            if((length(which(temp_df[,name_col] == VO2[i])))>1){
              FRIENDperc[i] <- temp_df[(min(which(temp_df[,name_col] == VO2[i]))), "Perc"]
            } else {
              # Otherwise find difference between two closest percentiles.
              highPerc <- max(which(temp_df[,name_col] >= VO2[i]))
              lowPerc <- min(which(temp_df[,name_col] < VO2[i]))

              highVO2 <- temp_df[highPerc, name_col]
              lowVO2 <- temp_df[lowPerc, name_col]

              # Determine the FRIEND percentile.
              # VO2 per percentile
              percPerVO2 <- (4.8 / (highVO2 - lowVO2))

              overLowVO2 <- round((VO2[i] - lowVO2), 2)

              addPerc <- percPerVO2 * overLowVO2

              FRIENDperc[i] <- round(((temp_df[lowPerc, "Perc"]) + addPerc), 1)
            }

          }

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

