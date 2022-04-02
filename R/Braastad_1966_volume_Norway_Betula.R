#' Simple Volume function for Birch in Norway
#'
#' @source Braastad, H. 1966. Volume tables for birch. Reports of the Norwegian
#' Forest Research Institute, 21, 265-365.
#'
#' @author Modified from hansoleorka/skogR  2022-04-02.
#'
#' @param diameter_cm Diameter of tree in cm at breast height, 1.3m
#' @param height_m Total height of tree, m.
#'
#' @return Volume of tree above bark, dm^3.
#' @export
Braastad_1966_volume_Norway_Betula <- function(
    diameter_cm,
    height_m){

  return(
    -1.86827 + 0.21461*diameter_cm*diameter_cm + 0.01283 * diameter_cm * diameter_cm * height_m + 0.0138 * diameter_cm * height_m *height_m - 0.06311 * height_m * height_m
  )

}
