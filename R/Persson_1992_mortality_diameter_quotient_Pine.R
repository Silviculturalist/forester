#' Diameter ratio of the mortality stems in stands of Scots Pine from Persson (1992)
#'
#'@source Persson, O. (1992) En produktionsmodell för tallskog i Sverige:
#' A growth simulator for Scots Pine (Pinus sylvestris, L.) in Sweden.
#' Report nr. 31. Dept. of Forest Yield Research.
#' Swedish University of Agricultural Sciences.
#' ISSN 0348-7636. Garpenberg. p. 61. pp. 206.
#'
#' @details
#' number of growth periods: 1134
#'
#' Coefficient of determination: 0.188
#'
#' Standard deviation about the function (sf): 0.309
#'
#' (sf)/standard deviation about the mean: 90.2 \%
#'
#' @param dominant_height_m Dominant height, m.
#' @param stems Stems per hectare
#' @param thinned TRUE / FALSE if the thinning removal at the start of the period is at least 10\% of the basal area, 1, otherwise 0.
#'
#' @return Ratio between the diameter of self-thinned stems and the diameter of the remaining stems.
#' @export
#'
#' @examples
Persson_1992_mortality_diameter_quotient_Pine <- function(
  dominant_height_m,
  stems,
  thinned
){
  return(
    exp(
      -1.06123+
      +0.38727*log(dominant_height_m)+
      -0.080630*log(stems)+
      +0.140766*thinned
    )
  )
}
