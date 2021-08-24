#' Mean diameter of the thickest trees per hectare in young stands of Norway Spruce, from Pettersson 1992
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 28.
#'
#' @details
#'
#' function 22. Mean diameter of thickest 800 trees per hectare.
#'
#' No. of observations = 100
#' R^2 = 0.91
#'
#'
#' function 23. Mean diameter of thickest 400 trees per hectare.
#'
#' No. of observations = 100
#' R^2 =  0.91
#'
#'
#' function 24. Mean diameter of thickest 100 trees per hectare.
#'
#' No. of observations = 100
#' R^2 = 0.86
#'
#'
#' @param dominant_height Dominant height of the stand, in meters.
#' @param diameter_mean_basal_area_stem Diameter corresponding to the mean stem basal area.
#' @param thickest_x_trees One of '800', '400' or '100'.
#'
#' @return Basal area weighted diameter, in cm.
#' @export
#'
#' @examples
Pettersson_1992_young_stands_mean_diameter_x_trees_Spruce <- function(
  dominant_height,
  diameter_mean_basal_area_stem,
  thickest_x_trees
){

  if(!(thickest_x_trees%in%c(100,400,800))){
    stop("The argument 'thickest_x_trees' must be one of 100, 400 or 800.")
  }

  ifelse(thickest_x_trees == 800,
         return(
           0.7246 + 0.2639 * log(dominant_height) + 0.5345 * log(diameter_mean_basal_area_stem)
         ),
         ifelse(thickest_x_trees == 400,
                return(
                  0.8600 + 0.2045 * log(dominant_height) + 0.5722 * log(diameter_mean_basal_area_stem)
                ),
                return(
                  1.1124 + 0.1263 * log(dominant_height)  + 0.5923 * log(diameter_mean_basal_area_stem)
                )))



}
