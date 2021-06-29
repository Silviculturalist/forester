#' Stem Volumes for Hybrid Aspen from Johnsson 1953.
#'
#' @source Johnsson H. (1953). Hybridaspens ungdomsutveckling och ett försök till framtidsprognos.
#' Svenska skogsvårdsföreningens tidsskrift. 51:73-96.
#'
#' Formula copied from Rytter, L., Stener, L-G. (2014). Growth and thinning effects
#' during a rotation period of hybrid aspen in southern Sweden. Scand. J. For. Res. Vol 29. Issue 8. p. 747-756. \url{https://doi.org/10.1080/02827581.2014.968202}
#'
#' @param diameter.cm Diameter of tree at breast height in centimeters.
#' @param height.m Height of tree in meters.
#'
#' @return Stem Volume over bark in m^3.
#' @export
#'
#' @examples
Johnsson_1953_volume_hybrid_aspen <- function(
  diameter.cm,
  height.m
){

  V <-
    0.03186*(diameter.cm^2)*height.m +
    +0.43*height.m +
    +0.0551*(diameter.cm^2) +
    -0.4148*diameter.cm

  return(V/1000) #1000 dm3 in 1 m3.

}
