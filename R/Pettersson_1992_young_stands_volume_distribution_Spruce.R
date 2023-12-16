#' Volume distribution in young stands of Scots Pine, from Pettersson 1992
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 24.
#'
#' @details
#'
#' function 9-12.
#'
#' f. 9. mean Volume:
#'
#' No. of observations = 100
#' R^2= 0.94
#'
#'
#'
#' f. 10 Standard Deviation of Volume:
#'
#' No. of observations = 100
#' R^2 = 0.55
#'
#'
#'
#' f. 11 Volume skew
#'
#' No. of observations = 100
#' R^2 = 0.32
#'
#'
#'
#' f. 12 Volume kurtosis
#'
#' No. of observations = 100
#' R^2 = 0.42
#'
#'
#' @param dominant_height Dominant height of the stand, in meters.
#' @param diameter_corresponding_to_mean_basal_area_cm Diameter corresponding to the mean basal area stem, in cm.
#' @param initial_stems Initial number of stems.
#'
#' @return List of distribution metrics.
#' @export

Pettersson_1992_young_stands_volume_distribution_Spruce <- function(
  dominant_height,
  diameter_corresponding_to_mean_basal_area_cm,
  initial_stems
){
  mean_volume <- exp(0.5271 + 0.8418*log(diameter_corresponding_to_mean_basal_area_cm))

  #Note unclear double negation in text! Verify.
  volume_standard_deviation <- exp(2.1613 + 0.4634*log(diameter_corresponding_to_mean_basal_area_cm) - -0.6656*log(SI))

  volume_skew_plus_3 <- exp(0.9021 + 0.1278*log(initial_stems)-0.2647*log(SI))

  volume_kurtosis_plus_3 <- exp(1.2019-0.1900*log(initial_stems)+0.4445*log(SI))

  return(
    list(
      "Mean Volume"=mean_volume,
      "Volume Std. Deviation"=volume_standard_deviation,
      "Volume skew + 3"=volume_skew_plus_3,
      "Volume kurtosis + 3"=volume_kurtosis_plus_3
    )
  )

}
