#' Living basal area in young stands of Norway Spruce, from Pettersson 1992
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 25.
#'
#' @details
#'
#' function 16.
#'
#' No. of observations = 100
#' R^2 = 0.98
#'
#'
#' @param dominant_height Dominant height of the stand, in meters.
#' @param initial_stems Initial number of stems.
#' @param basal_area_m2_ha Basal Area m2/ha.
#'
#' @return Living basal area
#' @export
#'
#' @examples
Pettersson_1992_young_stands_living_basal_area_Spruce <- function(
  dominant_height,
  initial_stems,
  basal_area_m2_ha
){
  return(
    exp(
      0.0803+1.0018*log(basal_area_m2_ha)-0.0072*log(initial_stems)-0.0133*log(dominant_height)
    )
  )

}
