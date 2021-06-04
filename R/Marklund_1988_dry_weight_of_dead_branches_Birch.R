#' Dry weight of dead branches for Birch from Marklund 1988
#'
#' @param diameter.cm Diameter at breast height, in cm.
#' @param height.m Height of tree, in metres.
#' @param altitude Altitude, in km.
#' @param latitude Latitude, RT90
#'
#' @return Dry weight of dead branches, in kilograms.
#' @export
#'
#' @examples
Marklund_1988_dry_weight_of_dead_branches_Birch <- function(
  diameter.cm,
  height.m,
  altitude,
  latitude
){

  #B15
  try(
  dry_weight_of_dead_branches <-
    -5.9507+
    +7.9266*(diameter.cm/(diameter.cm+5))
  )

  #B16
  try(
  dry_weight_of_dead_branches <-
    -6.6237+
    +11.2872*(diameter.cm/(diameter.cm+30))+
    -0.3081*height.m+
    +2.6821*log(height.m)
  )

  #B17
  try(
  dry_weight_of_dead_branches <-
    -0.6700+
    +12.0799*(diameter.cm/(diameter.cm+30))+
    -0.3448*height.m+
    +2.7062*log(height.m)+
    +1.5634*altitude+
    -0.0914*latitude
  )

  return(exp(dry_weight_of_dead_branches))
}
