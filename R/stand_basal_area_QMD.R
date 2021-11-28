#' Stand Basal Area from Quadratic mean diameter and stems per hectare.
#'
#' @source Curtis, R.O., Marshall, D.D. 2000. Technical Note: Why Quadratic Mean Diameter? West. J. Appl. For. 15(3):137-139. Available: \url{https://www.fs.fed.us/pnw/olympia/silv/publications/opt/436_CurtisMarshall2000.pdf}
#'
#' @param stems_per_ha Stems per hectare.
#' @param QMD Quadratic mean diameter, cm.
#'
#' @return Basal area, m2/ha.
#' @export
#'
#' @examples
stand_basal_area_QMD <- function(
  stems_per_ha,
  QMD
){
  return(
    stems_per_ha*((pi/4)/10000)*(QMD^2)
  )
}
