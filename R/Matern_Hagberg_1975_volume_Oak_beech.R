#' Volumes of Beech or Oak trees, from Matérn 1975.
#'
#' @source Matérn, B. 1975. Volymfunktioner för stående träd av ek och bok. Skogshögskolan, institutionen för skoglig matematisk statistik. Rapp. o. Upps. nr 15.
#' @source PM for Heureka 2004-01-20 Björn Elfving. Available: \url{https://www.heurekaslu.org/w/images/9/93/Heureka_prognossystem_\%28Elfving_rapportutkast\%29.pdf}
#'
#' @param diameter_cm Diameter at breast height, cm.
#' @param height_m Height of tree, metres.
#'
#' @return Volume, dm3.
#' @export
#' @name MaternVolume
Matern_1975_volume_Sweden_Oak <- function(
  diameter_cm,
  height_m
){
  ifelse(height>=10,
         return(
           0.03522*(diameter^2)*height_m + 0.08772*diameter_cm*height_m - 0.04905*(diameter_cm^2)
         ),
         return(
           0.03522 * (diameter ^ 2) * height_m + 0.08772 * diameter_cm * height_m - 0.04905 *
             (diameter_cm ^ 2) + ((1 - (height_m / 10)) ^ 2) * (
               0.01682 * (diameter_cm ^ 2) * height_m + 0.01108 * diameter_cm * height_m - 0.02167 *
                 diameter_cm * (height_m ^ 2) + 0.04905 * (diameter_cm ^ 2)
             )
         )
         )
}

#' @rdname MaternVolume
#' @export
Matern_1975_volume_Sweden_Beech <- function(
    diameter_cm,
    height_m
){
  return(
    0.01275*(diameter_cm^2)*height_m + 0.12368*(diameter_cm^2) + 0.0004701*(diameter_cm^2)*(height_m^2) + 0.00622*diameter_cm*(height_m^2)
  )
}

