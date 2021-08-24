#' Form quotient after PCT in young stands of Scots Pine, from Pettersson 1992.
#'
#' @source Pettersson Nils (1992) The effect on stand development of different
#' spacing after planting and precommercial thinning in Norway Spruce (Picea abies
#'  (L.) Karst.) and Scots Pine (Pinus sylvestris L.) stands: The effect of density
#'  after precommercial thinning on volume and structure in Pinus sylvestris and
#'  Picea abies stands. Diss. Report no. 34, Dept. of Forest Yield Research.
#'  ISSN 0348-7636. p. 9.
#'
#'  @details
#'
#'  R^2 = 0.82
#'  F=882
#'
#' @param stems Number of stems per hectare
#' @param dominant_height Dominant height, metres
#'
#'
#' @return Living stems per hectare after thinning
#' @export
#'
#' @examples
Pettersson_1992_form_quotient_after_PCT_Pine <- function(
  stems,
  dominant_height,
  latitude
){

  a <- ifelse(latitude>60,-0.460,-0.516)

  return(
    exp(
      a+
        +0.857*log(dominant_height)+
        +0.004*log(stems)
    )

  )


}
