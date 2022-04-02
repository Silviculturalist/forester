#' Double bark thickness for Norway Spruce in Norway
#'
#' @source Vestjordet, E. 1967. Functions and tables for volume of standing
#' trees. Norway spruce. Reports of the Norwegian Forest Research Institute, 22,
#'  539-574.
#'
#' @author Modified from hansoleorka/skogR on the 2022-04-02
#'
#' @param diameter_cm Diameter of tree at breast height, 1.3m.
#' @param height_m Total height of tree, m.
#'
#' @return Double bark, in mm.
#' @export
Vestjordet_1967_double_bark_thickness_Norway_Norway_Spruce <- function(
  diameter_cm,
  height_m
  ){
  return(
    -0.34 + 0.831648 * diameter_cm - 0.002832 * diameter_cm * diameter_cm - 0.010112 * height_m * height_m + 0.700203 * diameter_cm * diameter_cm / (height_m * height_m)
  )
}

#' Double bark thickness for Norway Spruce in Norway
#'
#' @source Brantseg, A. 1967. Volume functions and tables for Scots pine.
#' South Norway. Reports of the Norwegian Forest Research Institute, 12, 689-739.
#' @author Modified from hansoleorka/skogR on the 2022-04-02
#' @param diameter_cm Diameter of tree at breast height, 1.3m.
#' @param height_m Total height of tree, m.
#'
#' @return Double bark, in mm.
#' @export
Brantseg_1967_double_bark_thickness_Norway_Scots_Pine <- function(
    diameter_cm,
    height_m){
  return(
    2.9571 + 1.1499 * diameter_cm - 0.7304 * diameter_cm / height_m
  )
}


#' Double bark thickness for Norway Spruce in Norway
#'
#'
#' @source Fitje, A. 1995.
#' @author Modified from hansoleorka/skogR on the 2022-04-02
#' @param diameter_cm Diameter of tree at breast height, 1.3m.
#'
#' @return Double bark, in mm.
#' @export
Fitje_1995_double_bark_thickness_Norway_Scots_Pine <- function(
    diameter_cm){
  return(
    2.3 + 1.13*diameter_cm
  )

}

#' Double bark thickness for Birch in Norway
#'
#' @source Braastad, H. 1966. Volume tables for birch. Reports of the Norwegian
#'  Forest Research Institute, 21, 265-365.
#'
#' @author Modified from hansoleorka/skogR on the 2022-04-02
#'
#' @param diameter_cm Diameter of tree at breast height, 1.3m.
#'
#' @return Double bark, in mm.
#' @export
Braastad_1966_double_bark_thickness_Norway_Betula <- function(
    diameter_cm){
  1.046 * diameter_cm
}
