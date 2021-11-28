#' Calculate the Quadratic Mean Diameter of a stand, cm.
#'
#' @source Curtis, R.O., Marshall, D.D. Technical Note: Why Quadratic Mean Diameter? West. J. Appl. For. 15(3):137-139. Available: \url{https://www.fs.fed.us/pnw/olympia/silv/publications/opt/436_CurtisMarshall2000.pdf}
#'
#' @param Basal_area_m2_ha Basal area m2 / ha.
#' @param stems_per_ha Stems per ha.
#'
#' @return QMD, cm.
#' @export
#'
#' @examples
quadratic_mean_diameter <- function(
  Basal_area_m2_ha,
  stems_per_ha
){

  return(
    sqrt((Basal_area_m2_ha)/((pi/4)*stems_per_ha))
  )

}
