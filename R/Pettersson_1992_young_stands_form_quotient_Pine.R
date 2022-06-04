#' Form quotient in young stands of Scots Pine, from Pettersson 1992
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 24.
#'
#' @details
#'
#' function 13.
#'
#' No. of observations = 239
#' R^2 = 0.98
#'
#'
#' @param dominant_height Dominant height of the stand, in meters.
#' @param initial_stems Initial number of stems.
#'
#' @return Form quotient
#' @export
Pettersson_1992_young_stands_form_quotient_Pine <- function(
  dominant_height,
  initial_stems
){
  return(
    exp(
      0.1666-0.0282*log(initial_stems)+0.7054*log(dominant_height)
    )
  )

}
