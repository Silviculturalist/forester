#' Lorey's mean height for Picea abies from Tveite 1967.
#'
#' @source Tveite, B. 1967. Sambandet mellom grunnflateveid middelhøyde (HL) og noen andre bastandshøyder i gran- og furuskog. Meddr. norske SkogforsVes. 22:483-538. p. 512, table 8. Function 8.22.
#'
#' @param dominant_height dominant height
#' @param stems_per_ha Stems per hectare
#' @param Basal_area_m2_ha Basal Area m2/ha.
#' @param quadratic_mean_diameter Quadratic Mean Diameter at start. e.g. [forester::quadratic_mean_diameter()]
#'
#' @return Lorey's Mean height for Spruce stands in Norway.
#' @export
Tveite_1967_Loreys_mean_height_Norway_Norway_Spruce <- function(
  dominant_height,
  stems_per_ha,
  Basal_area_m2_ha,
  quadratic_mean_diameter
){

  return(
  (dominant_height - (226.439 + 14.37*dominant_height - 0.0329*stems_per_ha + 0.00468*stems_per_ha*dominant_height - 5.91*Basal_area_m2_ha + 0.190*Basal_area_m2_ha*dominant_height - 15.73*quadratic_mean_diameter)/100)
  )
}
