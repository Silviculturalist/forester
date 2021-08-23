#' Relationship between dominant height and Lorey's mean height in young stands of Norway Spruce
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 21.
#'
#' @details Number of observations: 100.
#'
#' R^2 = 0.97.
#'
#' @param dominant_height Dominant height of the stand, in meters.
#' @param initial_stems Initial number of stems.
#' @param SI Site Index for Spruce, according to Hägglund 1972,1973, e.g.
#' [forester::Hagglund_1972_northern_Sweden_Height_trajectories_Spruce]
#' [forester::Hagglund_1973_southern_Sweden_Height_trajectories_Spruce]
#'
#' @return Lorey's Mean height of the stand.
#' @export
#'
#' @examples
Pettersson_1992_young_stands_mean_height_Lorey_Spruce <- function(
  dominant_height,
  initial_stems,
  SI
){
  exp(
    -0.4168+
    +1.0095*log(dominant_height)+
    -0.0558*log(initial_stems)+
    +0.2054*log(SI)
  )
}
