#' Basal area increment percentage according to compound interest
#'
#' @param total_age total age
#' @param SIH100 SIH 100 Oak, e.g. [forester::Carbonnier_1975_Sweden_height_trajectories_Oak()]
#' @param silt_fraction %, Soil fraction composed of particles less than 0.06 mm.
#' @param BA_before_thinning Basal Area before thinning.
#' @param BA_before_thinning_understory Basal area of understory before thinning.
#' @param mean_basal_area_diameter_after_thinning Diameter corresponding to the mean basal area after thinning, cm. e.g. [forester::basal_area_weighted_mean_diameter_cm]
#' @param mean_height_Lorey_after_thinning Basal area weighted mean height of stand, per Lorey's formula, e.g. [forester::Lorey_mean_height()]
#' @param stems_after_thinning Stems remaining per hectare after thinning.
#' @param removal_last_thinning_BA BA removed by thinning during the last thinning.
#' @param removal_last_thinning_BA_understory BA removed by thinning from the understory during the last thinning.
#' @param removal_second_last_thinning_BA BA removed by thinning during the second last thinning.
#' @param removal_second_last_thinning_BA_understory BA removed by thinning from the understory during the second last thinning.
#' @param removal_third_last_thinning_BA BA removed by thinning during the third last thinning.
#' @param removal_third_last_thinning_BA_understory BA removed by thinning from the understory during the third last thinning.
#'
#' @return Basal area increment percentage according to compound interest.
#' @export
#'
#' @examples
Carbonnier_1975_compound_basal_area_growth_Oak <- function(
  SIH100,
  silt_fraction,
  total_age,
  BA_before_thinning,
  BA_before_thinning_understory,
  mean_basal_area_diameter_after_thinning,
  mean_height_Lorey_after_thinning,
  stems_after_thinning,
  removal_last_thinning_BA,
  removal_last_thinning_BA_understory,
  removal_second_last_thinning_BA,
  removal_second_last_thinning_BA_understory,
  removal_third_last_thinning_BA,
  removal_third_last_thinning_BA_understory

){

  return(
  -0.7804+
  +0.1728*((1000)/total_age)+
  +0.7061*((100)/mean_basal_area_diameter_after_thinning)+
  -0.1408*((mean_height_Lorey_after_thinning*sqrt(stems_after_thinning))/mean_basal_area_diameter_after_thinning)+
  +0.1841*10*sqrt(
    (((removal_last_thinning_BA+removal_last_thinning_BA_understory)/2)+
      (removal_second_last_thinning_BA+removal_second_last_thinning_BA_understory) +
      ((removal_third_last_thinning_BA+removal_third_last_thinning_BA_understory)/2))/(BA_before_thinning+BA_before_thinning_understory)
  )+
  -0.0945*((10*SIH100)/silt_fraction)
  )
}



