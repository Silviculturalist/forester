#' Relationship between dominant height and Lorey's mean height in young stands of Scots Pine
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 20.
#'
#' @details Number of observations: 86.
#'
#' R^2 = 0.98.
#'
#' @param dominant_height Dominant height of the stand, in meters.
#' @param initial_stems Initial number of stems.
#'
#' @return Lorey's Mean height of the stand.
#' @export
#'
#' @examples
Pettersson_1992_young_stands_mean_height_Lorey_Pine <- function(
  dominant_height,
  initial_stems
){
  exp(
  -0.1534+
  +1.0637*log(dominant_height)+
  -0.01*log(initial_stems)
  )
}
