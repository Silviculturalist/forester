#' Lorey's mean height for stands of Scots Pine in Norway from Tveite 1967.
#'
#' @source Tveite, B. 1967. Sambandet mellom grunnflateveid middelhøyde (HL) og noen andra bestandshøyder i gran- og furuskog. Meddr norske SkogforsVes. 22:483-538. p. 513. table 9. function 9.18. in Braastad, Helge 1977. Tilvekstmodellprogram for furu. Growth model computer program for Pinus sylvestris. Meddr. Norsk inst. skogforsk 35:265-359. p 273 function (2)
#'
#' @param dominant_height Dominant height of stand, m.
#' @param stems_per_ha Number of stems per hectare
#' @param basal_area_m2_ha Basal area (m^2/ha)
#' @param diameter_mean_basal_area_stem Diameter corresponding to the mean basal area stem.
#'
#' @return Lorey's mean height for stands of Scots Pine in Norway.
#' @export
#'
#' @examples
Tveite_1967_Loreys_mean_height_Norway_Pine <- function(
  dominant_height,
  stems_per_ha,
  basal_area_m2_ha,
  diameter_mean_basal_area_stem
){
  return(
    dominant_height - (220.075 + 7.698*dominant_height - 0.0300 *stems_per_ha + 0.00344*stems_per_ha*dominant_height - 4.990*basal_area_m2_ha + 0.260*basal_area_m2_ha*dominant_height - 12.26*diameter_mean_basal_area_stem)/100
  )
}
