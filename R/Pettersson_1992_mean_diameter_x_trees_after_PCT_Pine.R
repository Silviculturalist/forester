#' Mean diameter of the thickest trees per hectare after PCT in stands of Scots Pine, from Pettersson 1992
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands: The effect of density
#'  after precommercial thinning on volume and structure in Pinus sylvestris and
#'  Picea abies stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 9.
#'
#' @details
#'
#' 800 stems
#'
#' F= 505
#' R^2 = 0.86
#'
#' 400 stems
#' F=557
#' R^2 = 0.86
#'
#'  100 stems
#'  F= 353
#'  R^2 = 0.80
#'
#'
#'
#' @param dominant_height Dominant height of the stand, in meters.
#' @param diameter_mean_basal_area_stem Diameter corresponding to the mean stem basal area, in cm.
#' @param thickest_x_trees One of '800', '400' or '100'.
#'
#' @return Basal area weighted diameter, in cm.
#' @export
Pettersson_1992_mean_diameter_x_trees_after_PCT_Pine <- function(
  dominant_height,
  diameter_mean_basal_area_stem,
  thickest_x_trees
){

  if(!(thickest_x_trees%in%c(100,400,800))){
         stop("The argument 'thickest_x_trees' must be one of 100, 400 or 800.")
  }

  ifelse(thickest_x_trees == 800,
         return(
           exp(0.751 + 0.281 * log(dominant_height) + 0.502 * log(diameter_mean_basal_area_stem))
         ),
         ifelse(thickest_x_trees == 400,
                return(
                  exp(0.756 + 0.339 * log(diameter_mean_basal_area_stem) + 0.472 * log(dominant_height))
                ),
                return(
                  exp(0.858 + 0.351 * log(diameter_mean_basal_area_stem) + 0.459 * log(dominant_height))
                )))



}
