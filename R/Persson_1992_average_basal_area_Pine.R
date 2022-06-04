#' Average Basal area in stands of Scots Pine from Persson, O. (1992)
#'
#' @source Persson, O. (1992) En produktionsmodell för tallskog i Sverige:
#' A growth simulator for Scots Pine (Pinus sylvestris, L.) in Sweden.
#' Report nr. 31. Dept. of Forest Yield Research.
#' Swedish University of Agricultural Sciences.
#' ISSN 0348-7636. Garpenberg. p. 59. pp. 206.
#'
#' @details
#' number of growth periods: 2269
#'
#' Coefficient of determination: 0.292
#'
#' Standard deviation about the function (sf): 0.282
#'
#' (sf)/standard deviation about the mean: 84.0%
#'
#' @param SI Site Index e.g. [forester::Hagglund_1974_Sweden_height_trajectories_Pine()]
#' @param dominant_height_m Dominant height m.
#'
#' @return Basal area at the end of the period, m2/ha.
#' @export
Persson_1992_average_basal_area_Pine <- function(
  SI,
  dominant_height_m
){
  return(
    exp(
    -0.150317+ #including correction for logarithmic bias.
      +0.50463*log(dominant_height_m)+
      +0.62033*log(SI)
    )
  )
}
