#' Calculate the mean height in an Oak stand after a thinning.
#'
#' @source Carbonnier, C. (1975) Produktionen i kulturbestånd av ek i södra Sverige:
#' Yield of Oak plantations in southern Sweden. Studia Forestalia Suecica. Nr. 125.
#' Royal College of Forestry. Stockholm. ISBN 91-38-02278-8. p. 72.
#'
#' @param Loreys_mean_height_before_thinning Mean height before thinning as calculated by Lorey's formula, e.g. [forester::Lorey_mean_height]
#' @param basal_area_weighted_mean_diameter_cm_before_thinning Basal area weighted mean diameter before thinning, cm. e.g. [forester::basal_area_weighted_mean_diameter_cm]
#' @param basal_area_weighted_mean_diameter_cm_after_thinning Basal area weighted mean diameter after thinning, cm. e.g. [forester::basal_area_weighted_mean_diameter_cm]
#'
#' @return Mean height of the Oak stand, in meters.
#' @export
#'
#' @examples
Carbonnier_1975_mean_height_after_thinning_Oak <- function(
  Loreys_mean_height_before_thinning,
  basal_area_weighted_mean_diameter_cm_before_thinning,
  basal_area_weighted_mean_diameter_cm_after_thinning
){
  return(
    +1.7553*(Loreys_mean_height_before_thinning*basal_area_weighted_mean_diameter_cm_after_thinning/basal_area_weighted_mean_diameter_cm_before_thinning)+
    -0.7543*((Loreys_mean_height_before_thinning*(basal_area_weighted_mean_diameter_cm_after_thinning^2))/(basal_area_weighted_mean_diameter_cm_before_thinning^2))
  )
}
