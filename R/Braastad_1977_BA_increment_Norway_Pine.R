#' Percent Basal Area Increment in stands of Scots Pine by Tveite 1976 for Braastad 1977
#'
#' @param SIH40 Site Index H40 e.g. Site Index H40 , e.g. Tveite 1976 ([forester::Tveite_1976_height_trajectory_Pine_Norway()])
#' @param total_stand_age Total stand age, years.
#' @param BA_m2_ha_after_thinning Basal Area, m2/ha after thinning
#' @param stems_after_thinning Stems per hectare, after thinning.
#' @param diameter_mean_basal_area_stem_after_thinning Diameter corresponding to the mean basal area tree after thinning.
#' @param diameter_ratio_thinned_to_initial Ratio of the diameter of the thinned stems to that of the initial stand, e.g. [forester::Braastad_1977_diameter_quotient_thinned_trees_to_unthinned_stand_Norway_Pine()]
#' @param thinned_BA_m2_ha_last_thinning Basal Area m2/ha removed during the last thinning
#' @param thinned_BA_m2_ha_second_last_thinning Basal Area m2/ha removed during the last thinning
#' @param thinned_BA_m2_ha_third_last_thinning Basal Area m2/ha removed during the last thinning
#'
#' @return Basal Area Increment, Percent.
#' @export
Braastad_1977_BA_increment_percent_Norway_Pine <- function(
  SIH40,
  total_stand_age,
  BA_m2_ha_after_thinning,
  stems_after_thinning,
  diameter_mean_basal_area_stem_after_thinning,
  diameter_ratio_thinned_to_initial,
  thinned_BA_m2_ha_last_thinning,
  thinned_BA_m2_ha_second_last_thinning,
  thinned_BA_m2_ha_third_last_thinning
){

  HL70 <- 4.4704 + 0.2235*SIH40 + 0.04177*(SIH40^2)

  Hdiff <- -0.07440 + 0.01776*Loreys_mean_height_after_thinning + 0.02030*HL70 + 0.003197*BA_m2_ha_after_thinning - 0.000250*Loreys_mean_height_after_thinning*sqrt(stems_after_thinning)-0.001674*(Loreys_mean_height_after_thinning^2)+0.000260*(Loreys_mean_height_after_thinning^3)

  #1.050 is the climate value Kpg for low-land forests from Brantseg 1969 p. 64, row 4.
  percent_growth <- 0.94303 - 0.042535*Loreys_mean_height_after_thinning*sqrt(stems_after_thinning/diameter_mean_basal_area_stem_after_thinning) + 46.5657*(1/total_stand_age) - 0.117082*diameter_ratio_thinned_to_initial + 25.76632*(1/diameter_mean_basal_area_stem_after_thinning) + 0.36322*HL70*(sqrt((thinned_BA_m2_ha_last_thinning/2)+thinned_BA_m2_ha_second_last_thinning+(thinned_BA_m2_ha_third_last_thinning/2))/BA_m2_ha_after_thinning) - 0.976235*diameter_mean_basal_area_stem_after_thinning/Loreys_mean_height_after_thinning + 0.507327*1.050 + 60.37340*Hdiff/Loreys_mean_height_after_thinning

  return(
    percent_growth
  )
}
