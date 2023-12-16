#' Basal Area development for main species groups in Norway.
#'
#' @source Kobra Maleki, Rasmus Astrup, Christian Kuehne, J. Paul McLean &
#' Clara Antón-Fernández (2022) Stand-level growth models for long-term
#' projections of the main species groups in Norway, Scandinavian Journal
#' of Forest Research, DOI: \url{10.1080/02827581.2022.2056632}
#'
#' @param BA Basal Area, m^2 /ha.
#' @param age Total stand age.
#' @param age2 Desired total stand age.
#' @param stems Stems at age.
#' @param stems2 Stems at age2.
#' @param SI40 Site Index H40 (see ?forester::Maleki2022height)
#'
#' @family {Maleki2022}
#'
#' @details
#' \tabular{lrrrr}{
#' Species \tab E \tab MAE \tab RMSE \tab R^2 \cr
#' Norway Spruce \tab -0.067 \tab 1.112 \tab 1.602 \tab 0.984 \cr
#' Scots Pine \tab 0.032  \tab 0.680 \tab 1.007 \tab 0.991 \cr
#' Broadleaves \tab -0.138 \tab 0.978 \tab 1.418 \tab 0.978 \cr
#' }
#'
#'
#' @return Basal Area at age2.
#' @name Maleki2022BA
#' @export
Maleki_2022_BA_m2_ha_Norway_Norway_Spruce <- function(BA,age,age2,SI40,stems,stems2){

  b1 <- 0.4159
  b2 <- 2.0096
  b3 <- 0.7521

  #Calculate dominant-height-at-age.
  H1 <- Maleki_2022_height_trajectory_Norway_Norway_Spruce(dominant_height = SI40,age = 40,age2 = age)
  H2 <- Maleki_2022_height_trajectory_Norway_Norway_Spruce(dominant_height = SI40,age = 40,age2 = age2)

  return(
    BA^((H1/H2)^b1) * exp((b2^(stems2/stems)) * b2*(1-(H1/H2)^b3))
  )
}

#' @rdname Maleki2022BA
#' @export
Maleki_2022_BA_m2_ha_Norway_Scots_Pine <- function(BA,age,age2,SI40,stems,stems2){

  b1 <- 0.5381
  b2 <- 0.96900
  b3 <- 4.1579

  #Calculate dominant-height-at-age.
  H1 <- Maleki_2022_height_trajectory_Norway_Scots_Pine(dominant_height = SI40,age = 40,age2 = age)
  H2 <- Maleki_2022_height_trajectory_Norway_Scots_Pine(dominant_height = SI40,age = 40,age2 = age2)

  return(
    BA^((H1/H2)^b1) * exp((b2^(stems2/stems)) * b2*(1-(H1/H2)^b3))
  )
}

#' @rdname Maleki2022BA
#' @export
Maleki_2022_BA_m2_ha_Norway_Broadleaves <- function(BA,age,age2,SI40,stems,stems2){

  b1 <- 0.2970
  b2 <- 3.6124
  b3 <- 0.2087

  #Calculate dominant-height-at-age.
  H1 <- Maleki_2022_height_trajectory_Norway_Broadleaves(dominant_height = SI40,age = 40,age2 = age)
  H2 <- Maleki_2022_height_trajectory_Norway_Broadleaves(dominant_height = SI40,age = 40,age2 = age2)

  return(
    BA^((H1/H2)^b1) * exp((b2^(stems2/stems)) * b2*(1-(H1/H2)^b3))
  )
}
#' Height trajectories for main species groups in Norway.
#' @source Kobra Maleki, Rasmus Astrup, Christian Kuehne, J. Paul McLean &
#' Clara Antón-Fernández (2022) Stand-level growth models for long-term
#' projections of the main species groups in Norway, Scandinavian Journal
#' of Forest Research, DOI: \url{10.1080/02827581.2022.2056632}
#'
#' @param dominant_height Dominant height, in metres, at age.
#' @param age Total age, years.
#' @param age2 Desired total age, years.
#' @param output "Height" (default) or "Equation"
#'
#' @return Height of the stand in metres at the desired age.
#'
#' @details
#' \tabular{lrrrr}{
#' Species \tab E \tab MAE \tab RMSE \tab R^2 \cr
#' Norway Spruce \tab 0.004 \tab 0.590 \tab 0.821 \tab 0.963 \cr
#' Scots Pine \tab 0.0045 \tab 0.441 \tab 0.644 \tab 0.969 \cr
#' Broadleaves \tab 0.002 \tab 0.668 \tab 0.933 \tab 0.941 \cr
#' }
#'
#' @family {Maleki2022}
#' @name Maleki2022height
#' @export
Maleki_2022_height_trajectory_Norway_Norway_Spruce <- function(dominant_height,age,age2,output="Height"){
  b1 <- 39.5764
  b2 <- -396.3146
  b3 <- 1.6770

  if(output=="Height"){
  return(
    (b1 + ((dominant_height - b1)/(1-(b2*dominant_height*age^-b3))))/(1+b2*((dominant_height-b1)/(1-(b2*dominant_height*age^-b3)))*age2^-b3)
  )
  }

  if(output=="Equation"){
    return(
      paste0(
        "(",b1," + ((",dominant_height," - ",b1,")/(1-(",b2,"*",dominant_height,"*",age^-b3,"))))/(1+",b2,"*((",dominant_height,"-",b1,")/(1-(",b2,"*",dominant_height,"*",age^-b3,")))*age2^",-b3
        )
    )
  }
}

#' @export
#' @rdname Maleki2022height
Maleki_2022_height_trajectory_Norway_Scots_Pine <- function(dominant_height,age,age2,output="Height"){
  b1 <- 43.6698
  b2 <- -24.9476
  b3 <- 1.2967

  if(output=="Height"){
  return(
    (b1 + ((dominant_height - b1)/(1-(b2*dominant_height*age^-b3))))/(1+b2*((dominant_height-b1)/(1-(b2*dominant_height*age^-b3)))*age2^-b3)
  )
  }

  if(output=="Equation"){
    return(
      paste0(
        "(",b1," + ((",dominant_height," - ",b1,")/(1-(",b2,"*",dominant_height,"*",age^-b3,"))))/(1+",b2,"*((",dominant_height,"-",b1,")/(1-(",b2,"*",dominant_height,"*",age^-b3,")))*age2^",-b3
      )
    )
  }
}

#' @export
#' @rdname Maleki2022height
Maleki_2022_height_trajectory_Norway_Broadleaves <- function(dominant_height,age,age2,output="Height"){
  b1 <- 36.6501
  b2 <- -11.8787
  b3 <- 1.0643

  if(output=="Height"){
  return(
    (b1 + ((dominant_height - b1)/(1-(b2*dominant_height*age^-b3))))/(1+b2*((dominant_height-b1)/(1-(b2*dominant_height*age^-b3)))*age2^-b3)
  )
  }

  if(output=="Equation"){
    return(
      paste0(
        "(",b1," + ((",dominant_height," - ",b1,")/(1-(",b2,"*",dominant_height,"*",age^-b3,"))))/(1+",b2,"*((",dominant_height,"-",b1,")/(1-(",b2,"*",dominant_height,"*",age^-b3,")))*age2^",-b3
      )
    )
  }
}
#' Ingrowth for the main tree species groups of Norway.
#'
#' @source Kobra Maleki, Rasmus Astrup, Christian Kuehne, J. Paul McLean &
#' Clara Antón-Fernández (2022) Stand-level growth models for long-term
#' projections of the main species groups in Norway, Scandinavian Journal
#' of Forest Research, DOI: \url{10.1080/02827581.2022.2056632}
#'
#' @param BA Basal Area m^2 / ha.
#' @param QMD Quadratic Mean Diameter (cm).
#' @param stems Total stems per hectare.
#'
#' @family {Maleki2022}
#'
#' @details
#' \tabular{lrrrr}{
#' Total stem density predicted by surviving + ingrowth:\tab\tab\tab\tab\cr
#' Species \tab E \tab MAE \tab RMSE \tab R^2 \cr
#' Norway Spruce \tab 2.319 \tab 76.001 \tab 114.614 \tab 0.971 \cr
#' Scots Pine \tab 1.556  \tab 43.963 \tab 69.482 \tab 0.980 \cr
#' Broadleaves \tab 2.32 \tab 98.054 \tab 143.261 \tab 0.963 \cr
#' }
#'
#' @return Probability and count of ingrowth
#' @export
#' @name Maleki2022ingrowth
Maleki_2022_ingrowth_count_Norway_Norway_Spruce <- function(BA){
  return(
    2.2919-0.3780*sqrt(BA)
  )
}

#' @export
#' @rdname Maleki2022ingrowth
Maleki_2022_ingrowth_count_Norway_Scots_Pine <- function(BA){
  return(
    0.5264-0.0889*sqrt(BA)
  )
}

#' @export
#' @rdname Maleki2022ingrowth
Maleki_2022_ingrowth_count_Norway_Norway_Spruce <- function(QMD){
  return(
    3.0411 - 0.6213*sqrt(QMD)
  )
}



#' @export
#' @rdname Maleki2022ingrowth
Maleki_2022_ingrowth_probability_Norway_Norway_Spruce <- function(QMD, stems){
  return(
    13.6210 - 3.0328*sqrt(QMD) - 0.6868*sqrt(stems) + 0.1483*sqrt(QMD)*sqrt(stems)
  )

}

#' @export
#' @rdname Maleki2022ingrowth
Maleki_2022_ingrowth_probability_Norway_Scots_Pine <- function(QMD, stems){
  return(
    7.6489 - 1.2823*sqrt(QMD) - 0.5173*sqrt(stems) + 0.0915*sqrt(QMD)*sqrt(stems)
  )
}

#' @export
#' @rdname Maleki2022ingrowth
Maleki_2022_ingrowth_probability_Norway_Broadleaves <- function(QMD, stems){
  return(
    7.7009 - 1.6373*sqrt(QMD) - 0.3349*sqrt(stems) + 0.5688*sqrt(QMD)*sqrt(stems)
  )
}
#' Number of surviving stems at the end of a period.
#'
#' @source Kobra Maleki, Rasmus Astrup, Christian Kuehne, J. Paul McLean &
#' Clara Antón-Fernández (2022) Stand-level growth models for long-term
#' projections of the main species groups in Norway, Scandinavian Journal
#' of Forest Research, DOI: \url{10.1080/02827581.2022.2056632}
#'
#' @param age Total stand age.
#' @param age2 Total stand age at time 2.
#' @param stems Stand stem density per hectare.
#' @param SI40 Site Index H40 (see ?forester::Maleki2022height)
#'
#' @family {Maleki2022}
#'
#' @details
#' \tabular{lrrrr}{
#' Species \tab E \tab MAE \tab RMSE \tab R^2 \cr
#' Norway Spruce \tab 2.626 \tab 48.467 \tab 71.777 \tab 0.988 \cr
#' Scots Pine \tab 1.561  \tab 26.192 \tab 37.531 \tab 0.994 \cr
#' Broadleaves \tab 2.117 \tab 57.370 \tab 82.531 \tab 0.986 \cr
#' }
#'
#' @return The number of survived trees at the end of the period
#' @export
#' @name Maleki2022stems

Maleki_2022_stems_surviving_Norway_Norway_Spruce <- function(age,age2,stems,SI40){

  b1 <- 0.6159
  b2 <- -0.0312
  b3 <- 1.0602

  return(
    stems*((age2/age)^b1)*exp(b2-(SI40/1000)*((age2-age)^b3))
  )
}

#' @export
#' @rdname Maleki2022stems
Maleki_2022_stems_surviving_Norway_Scots_Pine <- function(age,age2,stems,SI40){

  b1 <- 0.17881
  b2 <- -0.0308
  b3 <- 0.0695
  return(
    stems*((age2/age)^b1)*exp(b2-(SI40/1000)*((age2-age)^b3))
  )
}

#' @export
#' @rdname Maleki2022stems
Maleki_2022_stems_surviving_Norway_Broadleaves <- function(age,age2,stems,SI40){

  b1 <- 0.4592
  b2 <- -0.0534
  b3 <- 0.9466

  return(
    stems*((age2/age)^b1)*exp(b2-(SI40/1000)*((age2-age)^b3))
  )
}


#' Stems at age2
#'
#' @source Kobra Maleki, Rasmus Astrup, Christian Kuehne, J. Paul McLean &
#' Clara Antón-Fernández (2022) Stand-level growth models for long-term
#' projections of the main species groups in Norway, Scandinavian Journal
#' of Forest Research, DOI: \url{10.1080/02827581.2022.2056632}
#'
#' @param age Total age of stand.
#' @param age2 Total age of stand desired.
#' @param stems Stems at age.
#' @param SI40 Site Index H40 (see ?forester::Maleki2022height)
#'
#' @return Number of stems at age2.
#'
#' @details
#' \tabular{lrrrr}{
#' Species \tab E \tab MAE \tab RMSE \tab R^2 \cr
#' Norway Spruce \tab 10.928 \tab 75.122 \tab 115.755 \tab 0.970 \cr
#' Scots Pine \tab 4.924  \tab 41.354 \tab 69.141 \tab 0.981 \cr
#' Broadleaves \tab 12.065 \tab 94.132 \tab 142.359 \tab 0.963 \cr
#' }
#'
#' @export
#' @name Maleki2022stems2
#' @family {Maleki2022}
Maleki_2022_stem_density_Norway_Norway_Spruce <- function(age,age2,stems,SI40){

  b1 <- 1.5124
  b2 <- -0.00654
  b3 <- 1.4747


  return(
    stems*((age2/age)^b1)*exp(b2-(SI40/1000)*((age2-age)^b3))
  )
}

#' @export
#' @rdname Maleki2022stems2
Maleki_2022_stem_density_Norway_Scots_Pine <- function(age,age2,stems,SI40){

  b1 <- 0.6676
  b2 <- 0.0039 #Not negative?
  b3 <- 0.8662


  return(
    stems*((age2/age)^b1)*exp(b2-(SI40/1000)*((age2-age)^b3))
  )
}

#' @export
#' @rdname Maleki2022stems2
Maleki_2022_stem_density_Norway_Broadleaves <- function(age,age2,stems,SI40){

  b1 <- 1.1395
  b2 <- -0.0162
  b3 <- 1.2682


  return(
    stems*((age2/age)^b1)*exp(b2-(SI40/1000)*((age2-age)^b3))
  )
}
#' Volume Estimation for the main tree species groups in Norway.
#'
#' @source Kobra Maleki, Rasmus Astrup, Christian Kuehne, J. Paul McLean &
#' Clara Antón-Fernández (2022) Stand-level growth models for long-term
#' projections of the main species groups in Norway, Scandinavian Journal
#' of Forest Research, DOI: \url{10.1080/02827581.2022.2056632}
#'
#' @description Observe, this function is used to calculate the standing volume
#' at the end of a period.
#'
#'
#' @param dominant_height2 Dominant height at end of a period.
#' @param BA2 Basal Area m^2 / ha at the end of a period.
#' @param age2 Stand total age at the end of a period.
#'
#' @return Volume cubic metres / hectare.
#'
#' @details
#' \tabular{lrrrr}{
#' Species \tab E \tab MAE \tab RMSE \tab R^2 \cr
#' Norway Spruce \tab 0.497 \tab 12.653 \tab 18.335 \tab 0.977 \cr
#' Scots Pine \tab 0.111  \tab 6.915 \tab 9.982 \tab 0.986 \cr
#' Broadleaves \tab 0.445 \tab 4.62 \tab 7.076 \tab 0.986 \cr
#' }
#'
#' @family {Maleki2022}
#' @export
#' @name Maleki2022Volume
Maleki_2022_volume_m3_ha_Norway_Norway_Spruce <- function(dominant_height2,BA2,age2){

  b1 <- 0.2134
  b2 <- 1.0779
  b3 <- 1.0498
  b4 <- 2.5148


  return(
    b1*(dominant_height2^b2)*(BA2^b3)*exp(b4/age2)
  )
}

#' @export
#' @rdname Maleki2022Volume
Maleki_2022_volume_m3_ha_Norway_Scots_Pine <- function(dominant_height2,BA2,age2){

  b1 <- 0.4830
  b2 <- 0.9128
  b3 <- 0.9913
  b4 <- -1.6105


  return(
    b1*(dominant_height2^b2)*(BA2^b3)*exp(b4/age2)
  )
}

#' @export
#' @rdname Maleki2022Volume
Maleki_2022_volume_m3_ha_Norway_Broadleaves <- function(dominant_height2,BA2,age2){

  b1 <- 0.5564
  b2 <- 0.7667
  b3 <- 1.0318
  b4 <- -1.5229


  return(
    b1*(dominant_height2^b2)*(BA2^b3)*exp(b4/age2)
  )
}
