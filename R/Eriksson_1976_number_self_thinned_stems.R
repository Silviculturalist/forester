#' Calculate the number of self-thinned stems during a period as by Eriksson 1976.
#'
#' @source Eriksson, H. (1976) "Granens produktion i Sverige", translated: "Yield of Norway spruce in Sweden". Report no. 41. Dept. of Forest Yield Research. Royal College of Forestry. Stockholm. p. 107.
#'
#' @param dominant_height_m Dominant height of stand, meters.
#' @param number_trees_per_ha_period_start Number of trees per hectare at start of period.
#' @param basal_area_over_bark_m2_ha_before_thinning Basal area above bark m2/ha before thinning.
#'
#' @return numeric.
#' @export
Eriksson_1976_number_self_thinned_stems <- function(dominant_height_m,
                                                    number_trees_per_ha_period_start,
                                                    basal_area_over_bark_m2_ha_before_thinning,
                                                    SI
                                                    ){

  basal_area_self_thinned <- Eriksson_1976_self_thinning(number_trees_per_ha_period_start = number_trees_per_ha_period_start,
                              dominant_height_m = dominant_height_m,
                              SI=SI)

  basal_area_weighted_mean_diameter <- basal_area_weighted_mean_diameter_cm(basal_area_m2_ha = basal_area_over_bark_m2_ha_before_thinning,
                                                                            stem_count = number_trees_per_ha_period_start)

  diameter_quotient <- Eriksson_1976_self_thinning_mean_diameter_quotient(dominant_height_m = dominant_height_m)

  basal_area_weighted_mean_diameter_self_thinned_stems <- basal_area_weighted_mean_diameter*diameter_quotient

  self_thinned_stems_per_ha <- ((basal_area_self_thinned*40000)/(basal_area_weighted_mean_diameter_self_thinned_stems^2*pi))

  return(self_thinned_stems_per_ha)
}
