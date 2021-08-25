#' Total volume production in young stands of Norway Spruce, from Pettersson 1992
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 21.
#'
#' @details
#'
#' function 4.
#'
#'
#' @param dominant_height Dominant height of the stand, in meters.
#' @param initial_stems Initial number of stems.
#'
#' @return Total volume production, m3sk / ha.
#' @export
#'
#' @examples
Pettersson_1992_young_stands_total_volume_Spruce <- function(
  dominant_height,
  initial_stems
){

  return(
    initial_stems/(
      8094*(dominant_height^-2.8673) + 0.2511*initial_stems*(dominant_height^-1.6611)
    )
  )

}
