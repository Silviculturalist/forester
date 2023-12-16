#' Stems per hectare at time for first thinning in young stands of Scots Pine, from Pettersson 1992
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 29.
#'
#' @details
#'
#' function 25. Stems per hectare at time for first thinning
#'
#' No. of observations = 86
#' R^2 = 0.97
#'
#' @param dominant_height Dominant height of the stand, in meters.
#' @param initial_stems Number of planted stems per hectare
#' @param SI Site index according to Hägglund 1974 [forester::Hagglund_1974_Sweden_height_trajectories_Pine]
#'
#' @return Number of stems at time for first thinning.
#' @export
Pettersson_1992_young_stands_stems_at_first_thinning_Pine <- function(
  dominant_height,
  initial_stems,
  SI
){

  return(
    exp(
      -0.5256-0.2160*log(dominant_height)+0.8760*log(initial_stems)+0.5789*log(SI)
    )
  )


}
