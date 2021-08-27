#' Volumes of Beech or Hornbeam trees, from Matérn 1975.
#'
#' @source Matérn, B. 1975. Volymfunktioner för stående träd av ek och bok. Skogshögskolan, institutionen för skoglig matematisk statistik. Rapp. o. Upps. nr 15.
#' @source PM for Heureka 2004-01-20 Björn Elfving. Available: \url{https://www.heurekaslu.org/w/images/9/93/Heureka_prognossystem_%28Elfving_rapportutkast%29.pdf}
#' @param diameter_cm Diameter at breast height, cm.
#' @param height_m Height of tree, metres.
#'
#' @return Volume, dm3.
#' @export
#'
#' @examples
Matern_1975_volume_Beech_Hornbeam <- function(
  diameter_cm,
  height_m
){
  return(
  0.01275*(diameter_cm^2)*height_m + 0.12368*(diameter_cm^2) + 0.0004701*(diameter_cm^2)*(height_m^2) + 0.00622*diameter_cm*(height_m^2)
  )
}
