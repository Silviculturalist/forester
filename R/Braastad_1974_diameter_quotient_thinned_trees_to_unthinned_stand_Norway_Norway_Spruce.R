#' Diameter quotient between felled trees and unthinned stand for Norway Spruce.
#'
#' @source Braastad, H. 1974. Produksjonstabeller og tilvekstmodeller for gran. Yield tables and Growth models for Picea abies. Meddr. norsk inst. Skogforsk. 31:9. p. 371. function (VI.A-2)
#'
#' @details
#' Multiple correlation coefficient: 0.463
#'
#' Residual spread : 0.134
#'
#' Spread about the mean: 16.8 percent.
#'
#' @param quadratic_mean_diameter_before Quadratic mean diameter of stand before thinning. i.e. [forester::quadratic_mean_diameter()]
#' @param Basal_area_m2_ha_before Basal area m2 / ha before thinning.
#' @param stems_remaining Stems per hectare remaining after thinning.
#' @param SIH40 Site Index H40, e.g. Tveite 1977 [forester::Tveite_1977_height_trajectory_Norway_Norway_Spruce()]. NB. Was included as unpublished.
#' @param age Age at breast height of stand.
#'
#' @return Diameter quotient.
#' @export
#'
#' @examples
Braastad_1974_diameter_quotient_thinned_trees_to_unthinned_stand_Norway_Norway_Spruce <- function(
  quadratic_mean_diameter_before,
  Basal_area_m2_ha_before,
  stems_remaining,
  SIH40,
  age
){

  return(
    0.683 + 0.0385*quadratic_mean_diameter_before - 0.000553*(quadratic_mean_diameter_before^2) - 0.00455*Basal_area_m2_ha_before + 0.0000543*stems_remaining - 0.00828*SIH40 - 0.00178*age
  )
}
