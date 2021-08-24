#' Volume distribution in young stands of Scots Pine, from Pettersson 1992
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 23.
#'
#' @details
#'
#' function 5-8.
#'
#' f. 5. mean Volume:
#'
#' No. of observations = 86.
#' R^2= 0.96
#'
#'
#'
#' f. 6 Standard Deviation of Volume:
#'
#' No. of observations = 86.
#' R^2 = 0.32
#'
#'
#'
#' f. 7 Volume skew
#'
#' No. of observations = 86.
#' R^2 = 0.25
#'
#'
#'
#' f. 8 Volume kurtosis
#'
#' No. of observations = 86
#' R^2 = 0.42
#'
#'
#' @param dominant_height Dominant height of the stand, in meters.
#' @param diameter_corresponding_to_mean_basal_area_cm Diameter corresponding to the mean basal area stem, in cm.
#' @param initial_stems Initial number of stems.
#'
#' @return List of distribution metrics.
#' @export
#'
#' @examples
Pettersson_1992_young_stands_volume_distribution_Pine <- function(
  dominant_height,
  diameter_corresponding_to_mean_basal_area_cm,
  initial_stems
){
  mean_volume <- exp(0.3607+ 0.8345*log(diameter_corresponding_to_mean_basal_area_cm)+0.0675*log(dominant_height))

  volume_standard_deviation <- exp(-0.2696 + 0.1336*log(diameter_corresponding_to_mean_basal_area_cm)+0.3949*log(dominant_height))

  volume_skew_plus_3 <- exp(0.2825+0.0863*log(initial_stems))

  volume_kurtosis_plus_3 <- exp(2.6786-0.1827*log(initial_stems))

  return(
    list(
      "Mean Volume"=mean_volume,
      "Volume Std. Deviation"=volume_standard_deviation,
      "Volume skew + 3"=volume_skew_plus_3,
      "Volume kurtosis + 3"=volume_kurtosis_plus_3
        )
  )

}
