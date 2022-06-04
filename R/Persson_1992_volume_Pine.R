#' Volume for Scots Pine from Persson, O. (1992)
#'
#' @source Persson, O. (1992) En produktionsmodell för tallskog i Sverige:
#' A growth simulator for Scots Pine (Pinus sylvestris, L.) in Sweden.
#' Report nr. 31. Dept. of Forest Yield Research.
#' Swedish University of Agricultural Sciences.
#' ISSN 0348-7636. Garpenberg. p. 58. pp. 206.
#'
#' @details
#' number of growth periods: 697
#'
#' Coefficient of determination: 0.9933
#'
#' Standard deviation about the function (sf): 0.0327
#'
#' (sf)/standard deviation about the mean: 8.2%
#'
#' @param basal_area_above_bark Basal area over bark, m2/ha
#' @param dominant_height_m Dominant height, metres.
#' @param stems Number of stems per ha.
#' @param latitude Latitude, degrees.
#'
#' @return Volume on bark cu. m. / ha.
#' @export

Persson_1992_volume_Pine <- function(
  basal_area_above_bark,
  dominant_height_m,
  stems,
  latitude
){
  return(
    exp(
    -0.58147+ #including correction for logarithmic bias.
      +1.11493*log(basal_area_above_bark)+
      +0.73376*log(dominant_height_m)+
      -0.072569*log(stems)+
      +0.160919*log(latitude)
    )
  )
}
