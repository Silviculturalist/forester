#' Stems per hectare at time for first thinning in young stands of Norway Spruce, from Pettersson 1992
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
#' No. of observations = 100
#' R^2 = 0.98
#'
#' @param dominant_height Dominant height of the stand, in meters.
#' @param initial_stems Number of planted stems per hectare
#' @param SI Site index according to Hägglund 1972,1973 [forester::Hagglund_1972_northern_Sweden_Height_trajectories_Spruce]
#' [forester::Hagglund_1973_southern_Sweden_Height_trajectories_Spruce]
#'
#' @return Number of stems at time for first thinning.
#' @export
Pettersson_1992_young_stands_stems_at_first_thinning_Spruce <- function(
  dominant_height,
  initial_stems,
  SI
){

  dummy <- ifelse(SI > 24, 1, ifelse(
    initial_stems < 2500,
    0,
    ifelse(
      initial_stems < 4000,-0.0404,
      ifelse(initial_stems < 6000,-0.0464, -0.0689)
    )
  ))


  return(
    exp(
      -0.5791+0.180*SI*dummy+1.0499*log(initial_stems)+log(dominant_height)*dummy
    )
  )


}
