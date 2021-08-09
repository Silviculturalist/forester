#' Bark subtraction function for Birch from Agestam 1985
#'
#' @param basal_area_above_bark_Birch Basal Area Birch including bark, m2/ha
#' @param age_at_breast_height Age at breast height for Pine stems.
#' @param latitude Latitude, degrees.
#'
#' @return Bark area at breast height, Birch, m2/ha.
#' @export
#'
#' @examples
Agestam_1985_bark_subtraction_Birch <- function(
  basal_area_above_bark_Birch,
  age_at_breast_height,
  latitude
){
  return(
    exp(
      +0.861*log(basal_area_above_bark_Birch)+
      +0.217*log(age_at_breast_height)+
      -4.47*log(latitude)+
      +16.01
    )
  )

}
