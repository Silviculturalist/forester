#' Calculate initial Basal Area for stands of Norway Spruce in Norway from Braastad 1974.
#'
#' @source Braastad, H. 1974. Produksjonstabeller og tilvekstmodeller for gran. Yield tables and Growth models for Picea abies. Meddr. norsk inst. Skogforsk. 31:9. p. 369. function (V.1)
#'
#' @details
#' Multiple correlation coefficient: 0.863.
#'
#' Residual spread : 3.2 m^2.
#'
#' Spread about the mean: 16.7 percent of mean Basal area.
#'
#' @param SIH40 Site Index H40, e.g. Tveite 1977 [forester::Tveite_1977_height_trajectory_Norway_Norway_Spruce()]. NB. Was included as unpublished.
#' @param stems_per_ha Stems per hectare.
#' @param dominant_height Dominant height.
#'
#' @return Basal Area m2 / ha.
#' @export
Braastad_1974_initial_Basal_area_Norway_Spruce <- function(
  SIH40,
  stems_per_ha,
  dominant_height
){
  return(
    -29.152 + 0.379*SIH40 + 3.247*dominant_height + 0.0026*stems_per_ha
  )
}
