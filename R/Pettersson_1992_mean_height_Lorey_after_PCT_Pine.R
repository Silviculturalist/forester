#' Lorey's mean height after PCT in stands of Scots Pine, from Pettersson 1992.
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands: The effect of density
#'  after precommercial thinning on volume and structure in Pinus sylvestris and
#'  Picea abies stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 9.
#'
#' @details
#' F=1530
#' R^2 = 0.94
#'
#' @param dominant_height Dominant height, metres.
#' @param stems Stems per hectare
#'
#' @return Lorey's Mean Height
#' @export
#'
#' @examples
Pettersson_1992_mean_height_Lorey_after_PCT_Pine <- function(
  dominant_height,
  stems
){
  return(
    exp(
      0.109 + 1.009*log(dominant_height) - 0.028*log(stems)
    )
  )
}
