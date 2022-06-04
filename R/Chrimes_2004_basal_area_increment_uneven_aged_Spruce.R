#' Basal area increment for Uneven-aged Spruce from Chrimes 2004
#'
#' @source Chrimes, D. & Lundqvist, L. 2004. Manuscript: Simulated volume increment of managed uneven-aged Picea abies stands in central Sweden. (PhD. dissertation, Swedish University of Agricultural Sciences). Chapter I p. 8.
#'
#' @details
#'
#' Details from page 6.
#'
#' r^2 = 0.69
#' df= 606
#' mse= 82.73
#' F= 150.6
#' se(\%)=25.88
#' p<0.0001
#'
#' @param diameter_cm Diameter of tree at breast height 1.3 m.
#' @param residual_stand_basal_area_m2_ha Stand Basal Area at breast height, m2 / ha.
#' @param overtopping_BA_m2_ha The BA sum of all diameter class with a mean height of at least 2 m higher than the target diameter class.
#'
#' @return Over bark Basal area increment for a class. m2/ha/yr
#' @export
#'
Chrimes_2004_basal_area_increment_uneven_aged_Spruce <- function(
  diameter_cm,
  residual_stand_basal_area_m2_ha,
  overtopping_BA_m2_ha
){
  return(
    e^((-13.166+3.295*log(diameter_cm) - 0.382*log(diameter_cm^2) - 0.023*stand_basal_area_m2_ha - 0.000027*(residual_stand_basal_area_m2_ha^2)*overtopping_BA)*0.953133)
  )
}
