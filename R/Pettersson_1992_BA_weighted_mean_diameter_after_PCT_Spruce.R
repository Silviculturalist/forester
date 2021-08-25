#' Basal area weighted mean diameter after PCT in stands of Norway Spruce, from Pettersson 1992.
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands: The effect of density
#'  after precommercial thinning on volume and structure in Pinus sylvestris and
#'  Picea abies stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 9.
#'
#' @details
#' F=471
#' R^2 = 0.91
#'
#' @param dominant_height Dominant height, metres.
#' @param diameter_mean_basal_area_stem Diameter corresponding the mean basal area stem, cm.
#'
#' @return Basal area weighted mean diameter
#' @export
#'
#' @examples
Pettersson_1992_BA_weighted_mean_diameter_after_PCT_Spruce <- function(
  dominant_height,
  diameter_mean_basal_area_stem
){
  return(
    exp(
      0.194 + 0.129*log(dominant_height) + 0.824*log(diameter_mean_basal_area_stem)
    )
  )
}
