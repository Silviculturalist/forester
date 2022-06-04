#' Form quotient for Uneven aged Spruce from Chrimes 2004.
#'
#' @details Chrimes, D. & Lundqvist, L. 2004. Manuscript: Simulated volume increment of managed uneven-aged Picea abies stands in central Sweden. (PhD. dissertation, Swedish University of Agricultural Sciences). Chapter I p. 7.
#'
#' r^2 = 0.83
#' df = 3079
#' mse = 0.93
#' cv(\%)= 10.64
#'
#' @param diameter_cm Diameter of tree at breast height 1.3 m.
#' @param stand_basal_area_m2_ha Stand Basal Area at breast height, m2 / ha.
#'
#' @return Form quotient
#' @export
Chrimes_2004_form_quotient_uneven_aged_Spruce <- function(
  diameter_cm,
  stand_basal_area_m2_ha
){

  return(

    -4.401 + (0.136 * diameter_cm) - (0.002 * diameter_cm^2) + 16.647*(diameter_cm/(diameter_cm+10))+ 0.010*stand_basal_area_m2_ha

  )

}
