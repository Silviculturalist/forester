#' Calculate mean diameter corresponding to the mean basal area per stem.
#'
#' @param basal_area_m2_ha Basal area at breast height 1.3 m.
#' @param stem_count Number of stems.
#'
#' @return Diameter, cm.
#' @export
#'
#' @examples
basal_area_weighted_mean_diameter_cm <- function(basal_area_m2_ha,
                                                          stem_count){
  return(
  2*sqrt((basal_area_m2_ha/stem_count)/pi)*100
  )

}
