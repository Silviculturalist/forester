#' Bark area for Scots Pine from Persson, O. (1992)
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
#' Coefficient of determination: 0.896
#'
#' Standard deviation about the function (sf): 0.103
#'
#' (sf)/standard deviation about the mean: 32.4%
#'
#' @param basal_area_above_bark Basal area over bark, m2/ha
#' @param SI Site Index, m. e.g. [forester::Hagglund_1974_Sweden_height_trajectories_Pine()]
#' @param stems Number of stems per ha.
#' @param latitude Latitude, degrees.
#'
#' @return Bark Area, m2/ha.
#' @export
Persson_1992_bark_Pine <- function(
  basal_area_above_bark,
  SI,
  stems,
  latitude
){
  return(
    exp(
    8.43648+ #including correction for logarithmic bias.
      +0.94902*log(basal_area_above_bark)+
      -0.176223*log(SI)+
      +0.037108*log(stems)+
      -2.30456*log(latitude)
    )
  )
}
