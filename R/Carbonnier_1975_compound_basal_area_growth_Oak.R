#' Basal area increment percentage according to compound interest
#'
#' @param total_age total age
#' @param BA_before_thinning Basal Area before thinning.
#' @param BA_before_thinning_understory Basal area of understory before thinning.
#' @param mean_basal_area_diameter_after_thinning
#' @param mean_height_Lorey_after_thinning
#' @param stems_after_thinning
#' @param removal_last_thinning_BA
#' @param removal_last_thinning_BA_understory
#' @param removal_second_last_thinning_BA
#' @param removal_second_last_thinning_BA_understory
#' @param removal_third_last_thinning_BA
#' @param removal_third_last_thinning_BA_understory
#'
#' @return
#' @export
#'
#' @examples
Carbonnier_1975_compound_basal_area_growth_Oak <- function(
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
  -0.7804+
  +0.1728*((10^3)/total_age)+
  +0.7061*((10^2)/mean_basal_area_diameter_after_thinning)-
  -0.1408*((mean_height_Lorey_after_thinning*sqrt(stems_after_thinning))/mean_basal_area_diameter_after_thinning)+
  +0.1841*10*sqrt(
    (((removal_last_thinning_BA+removal_last_thinning_BA_understory)/2)+
      removal_second_last_thinning_BA+removal_second_last_thinning_BA_understory +
      ((removal_third_last_thinning_BA+removal_third_last_thinning_BA_understory)/2))/(BA_before_thinning+BA_before_thinning_understory)
  )
}
