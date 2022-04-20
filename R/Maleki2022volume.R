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
