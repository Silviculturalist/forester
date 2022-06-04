#' Total volume yield after PCT in young stands of Scots Pine, from Pettersson 1992.
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands: The effect of density
#'  after precommercial thinning on volume and structure in Pinus sylvestris and
#'  Picea abies stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 7.
#'
#'  @details R^2 = 0.97, for both classes.
#'
#' @param stems Number of stems per hectare
#' @param dominant_height Dominant height, metres
#' @param SI Site Index, according to Hägglund 1974 [forester::Hagglund_1974_Sweden_height_trajectories_Pine]
#'
#' @return Total volume yield m3/ha.
#' @export
Pettersson_1992_total_volume_yield_after_PCT_Pine <- function(
  stems,
  dominant_height,
  SI
){

  a <- ifelse(SI>25,3422,83.1)
  b <- ifelse(SI>25,-3.130,-1.310)
  c <- ifelse(SI>25,0.445,0.918)
  d <- ifelse(SI>25,-1.765,-2.036)

  return(
    (stems/(a*(dominant_height^b)+stems*c*(dominant_height^d)))

  )


}
