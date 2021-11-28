#' Braastad 1977 growth model for Pine stands in Norway.
#'
#' @param SIH40 Site Index H40, e.g. [forester::Tveite_1976_height_trajectory_Norway_Pine()]
#' @param Age Stand age at breast height.
#' @param stems_per_ha Number of stems per hectare.
#' @param Basal_area_m2_ha Basal area m2/ha.
#' @param thinning_fix_stems Should thinning always remove a fixed number of stems? Default is NA.
#' @param thinning_lower_relative_spacing Set the lower density limit after thinning to a Hart - Becking Index., e.g. [forester::Hart_Becking_relative_spacing_index()]
#' @param thinning_upper_relative_spacing Set the upper density limit after thinning to a Hart - Becking Index., e.g. [forester::Hart_Becking_relative_spacing_index()]
#' @param production_level Default = 1. Set to lower to calculate production for stand less or more productive than average. e.g. 0.90 for 10\% lower production than average.
#' @param height_reduction Default=1. Increase the difference between dominant height and Lorey's mean height, e.g. for stands with extreme snow damage set the value 0.95 to reduce Lorey's mean height by 5\%
#' @param reduce_thinned_diameter_if_number_removed_stems_lower_than Default = 0. If the number of removed stems is less than this value, the stem diameters will be reduced by 10\%
#' @param adjust_diameter_ratio_thinned Adjust ratio between the diameter of removed stems and the initial stand, calculated with [forester::Braastad_1977_diameter_quotient_thinned_trees_to_unthinned_stand_Norway_Pine()]
#' @param BA_m2_ha_MAX Default = NA. Upper limit to Basal Area after thinning given in m2/ha.
#'
#' @return
#' @export
#'
#' @examples
Braastad_1977_stand_growth_model_Pine <- function(
  SIH40,
  Age,
  stems_per_ha,
  Basal_area_m2_ha,
  thinning_fix_stems=NA,
  thinning_lower_relative_spacing,
  thinning_upper_relative_spacing,
  production_level=1,
  height_reduction=1,
  reduce_thinned_diameter_if_number_removed_stems_lower_than=0,
  adjust_diameter_ratio_thinned=1,
  BA_m2_ha_MAX=NA
){

}
