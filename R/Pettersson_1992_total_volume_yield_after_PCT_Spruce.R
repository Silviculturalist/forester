#' Total volume yield after PCT in young stands of Norway Spruce, from Pettersson 1992.
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands: The effect of density
#'  after precommercial thinning on volume and structure in Pinus sylvestris and
#'  Picea abies stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 7.
#'
#'  @details R^2 = 0.98 if SI<= 30, else R^2 = 0.96
#'
#' @param stems Number of stems per hectare
#' @param dominant_height Dominant height, metres
#' @param SI Site Index, according to Hägglund 1972,1973 [forester::Hagglund_1972_northern_Sweden_Height_trajectories_Spruce] [forester::Hagglund_1973_southern_Sweden_Height_trajectories_Spruce]
#'
#' @return Total volume yield m3/ha.
#' @export

Pettersson_1992_total_volume_yield_after_PCT_Spruce <- function(
  stems,
  dominant_height,
  SI
){

  a <- ifelse(SI>30,1000,1000)
  b <- ifelse(SI>30,-2.488,-2.331)
  c <- ifelse(SI>30,1.066,0.259)
  d <- ifelse(SI>30,-2.098,-1.537)

  return(
    (stems/(a*(dominant_height^b)+stems*c*(dominant_height^d)))

  )


}
