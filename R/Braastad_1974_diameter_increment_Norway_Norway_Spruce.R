#' Annual Diameter growth for Norway Spruce in Norway from Braastad 1974
#'
#' @source Braastad, H. 1974. Diametertilvekstfunksjoner for gran. Diameter increment functions for Picea abies. Medd. norsk inst. skogforskning:31:1. function 6.
#'
#' @details
#' Multiple correlation coefficient = 0.740
#' Residual spread = 0.585 mm.
#' Spread about the mean Diameter increment = 18.4 percent.
#'
#' @param SIH40 Site Index H40, e.g. Tveite 1977 [forester::Tveite_1977_height_trajectory_Norway_Norway_Spruce()]. NB. Was included as unpublished.
#' @param age Age at breast height.
#' @param dominant_height Dominant height of stand (arithmetic mean height of the 100 trees with largest diameter).
#' @param QMD_remaining Quadratic Mean Diameter at start.
#' @param BA_m2_ha_after_thinning Basal Area m2/ha at start.
#' @param Volume_remaining Volume m3 at start.
#' @param thinned_BA_m2_ha_last_thinning Basal Area m2/ha removed during the last thinning
#' @param thinned_BA_m2_ha_second_last_thinning Basal Area m2/ha removed during the second last thinning
#' @param thinned_BA_m2_ha_third_last_thinning Basal Area m2/ha removed during the third last thinning
#'
#' @return Diameter increment per annum, cm.
#' @export
Braastad_1974_BA_increment_Norway_Norway_Spruce <- function(
  SIH40,
  age,
  dominant_height,
  QMD_remaining,
  BA_m2_ha_after_thinning,
  Volume_remaining,
  thinned_BA_m2_ha_last_thinning,
  thinned_BA_m2_ha_second_last_thinning,
  thinned_BA_m2_ha_third_last_thinning
){

  return(
    -0.649 + 52.370*(1/age) - 220.215*((1/age)^2) - 7.237 * (1/dominant_height) + 2.314*(QMD_remaining/dominant_height) + 0.188*SIH40 + 0.088*BA_m2_ha_after_thinning+ 22.533*(1/BA_m2_ha_after_thinning) - 0.929*sqrt(BA_m2_ha_after_thinning) - 64.952*(1/Volume_remaining) - 0.00550*SIH40*(Volume_remaining/dominant_height) + 0.0353*(thinned_BA_m2_ha_last_thinning+thinned_BA_m2_ha_second_last_thinning+thinned_BA_m2_ha_third_last_thinning)
  )

}
