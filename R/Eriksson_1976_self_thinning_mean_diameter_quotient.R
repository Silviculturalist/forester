#' Quotient D4/D1 from Eriksson 1976.
#'
#' @description Gives the quotient between the basal area weighted mean stem diameter for self-thinned trees (D4) and the basal area weighted mean stem diameter for all trees on the plot (D1)
#'
#' @details R = 0.405
#'
#'
#'
#' @source Eriksson, H. (1976) "Granens produktion i Sverige", translated: "Yield of Norway spruce in Sweden". Report no. 41. Dept. of Forest Yield Research. Royal College of Forestry. Stockholm. p.107.
#'
#' @param dominant_height_m Dominant height of stand, in meters.
#'
#' @return Quotient D4/D1.
#' @export
Eriksson_1976_self_thinning_mean_diameter_quotient <- function(dominant_height_m){
  return(
    0.039*(dominant_height_m*10)^0.517
  )
}
