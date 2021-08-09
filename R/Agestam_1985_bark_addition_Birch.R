#' Bark addition function for Birch from Agestam 1985
#'
#' @param basal_area_under_bark_Birch Basal Area Birch under bark, m2/ha
#' @param age_at_breast_height Age at breast height for Pine stems.
#' @param latitude Latitude, degrees.
#'
#' @return Bark area at breast height, Birch, m2/ha.
#' @export
#'
#' @examples
Agestam_1985_bark_addition_Birch <- function(
  basal_area_under_bark_Birch,
  age_at_breast_height,
  latitude
){
  return(
    exp(
      +19.276+
      +0.818*log(basal_area_under_bark_Birch)+
      +0.269*log(age_at_breast_height)+
      -5.25*log(latitude)
    )
  )
}
