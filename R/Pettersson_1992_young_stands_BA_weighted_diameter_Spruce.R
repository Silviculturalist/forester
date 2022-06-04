#' Basal area weighted mean diameter in young stands of Norway Spruce, from Pettersson 1992
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 25.
#'
#' @details
#'
#' function 18.
#'
#' No. of observations = 100
#' R^2 = 0.95
#'
#'
#' @param dominant_height Dominant height of the stand, in meters.
#' @param diameter_mean_basal_area_stem Diameter corresponding to the mean stem basal area.
#'
#' @return Basal area weighted diameter, in cm.
#' @export
Pettersson_1992_young_stands_BA_weighted_diameter_Spruce <- function(
  dominant_height,
  diameter_mean_basal_area_stem
){
  return(
    exp(
      0.3731+0.0632*log(dominant_height)+0.8332*log(diameter_mean_basal_area_stem)
    )
  )

}
