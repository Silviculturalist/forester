#' Form quotient in young stands of Norway Spruce, from Pettersson 1992
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 25.
#'
#' @details
#'
#' function 14.
#'
#' No. of observations = 198
#' R^2 = 0.97
#'
#'
#' @param dominant_height Dominant height of the stand, in meters.
#' @param initial_stems Initial number of stems.
#' @param SI SI from Hägglund 1972,1973.
#'
#' @return Form quotient
#' @export

Pettersson_1992_young_stands_form_quotient_Spruce <- function(
  dominant_height,
  initial_stems,
  SI
){
  return(
    exp(
      -0.9204-0.0286*log(initial_stems)+0.8294*log(dominant_height)+0.2265*log(SI)
    )
  )

}
