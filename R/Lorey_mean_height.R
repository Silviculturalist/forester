#' Lorey's mean height.
#'
#' @description Lorey's mean height is calculated by dividing the sum of the
#' product of the height and basal area of each tree by the total stand basal area.
#'
#' @details This function can handle vectors.
#'
#' @param tree_height_m numeric. Tree height in meters.
#' @param basal_area_m2 numeric. Basal area in m2.
#'
#' @return Lorey's mean height.
#' @export
#'
#' @examples
#' heights <- c(15,12,14,15,10)
#' ba <- c(0.4,0.5,0.6,0.5,0.5)
#'
#' Lorey_mean_height(tree_height_m=heights,basal_area_m2=ba)
#'
Lorey_mean_height <- function(
  tree_height_m,
  basal_area_m2
){
  return(sum(tree_height_m*basal_area_m2)/sum(basal_area_m2))
}
