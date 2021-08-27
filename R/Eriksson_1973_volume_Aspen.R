#' Eriksson 1973 Volume for Aspen
#'
#' @source Eriksson, H. 1973. Volymfunktioner för stående träd av ask, asp, klibbal och contortatall. Skogshögskolan, institutionen för skogsproduktion. Rapp. o. Upps. nr 26.
#' @source PM for Heureka 2004-01-20 Björn Elfving. Available: \url{https://www.heurekaslu.org/w/images/9/93/Heureka_prognossystem_%28Elfving_rapportutkast%29.pdf}
#'
#' @param diameter_cm Diameter at breast height, cm.
#' @param height_m Height, in metres.
#'
#' @return Volume, dm3
#' @export
#'
#' @examples
Eriksson_1973_volume_Aspen <- function(
  diameter_cm,
  height_m
){
  return(
    0.01548 * (diameter_cm ^ 2) + 0.03255 * (diameter_cm ^ 2) * height_m - 0.000047 *
      (diameter_cm ^ 2) * (height_m ^ 2) - 0.01333 * diameter_cm + height_m +
      0.004859 * diameter_cm * (height_m ^ 2)
  )
}
