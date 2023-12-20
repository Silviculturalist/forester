#' Volumes of Beech or Oak trees, from Matérn 1975.
#'
#' @source Matérn, B. 1975. Volymfunktioner för stående träd av ek och bok. Skogshögskolan, institutionen för skoglig matematisk statistik. Rapp. o. Upps. nr 15.
#' @source PM for Heureka 2004-01-20 Björn Elfving. Available: \url{https://www.heurekaslu.org/w/images/9/93/Heureka_prognossystem_\%28Elfving_rapportutkast\%29.pdf}
#'
#' @param DBH Diameter at breast height, cm.
#' @param height Height of tree, metres.
#'
#' @return Volume, dm3.
#' @export
#' @name MaternVolume
Matern_1975_volume_Sweden_Oak <- function(
  DBH,
  height
){
  ifelse(height>=10,
         return(
           0.03522*(DBH^2)*height + 0.08772*DBH*height - 0.04905*(DBH^2)
         ),
         return(
           0.03522 * (DBH ^ 2) * height + 0.08772 * DBH * height - 0.04905 *
             (DBH ^ 2) + ((1 - (height / 10)) ^ 2) * (
               0.01682 * (DBH ^ 2) * height + 0.01108 * DBH * height - 0.02167 *
                 DBH * (height ^ 2) + 0.04905 * (DBH ^ 2)
             )
         )
         )
}

#' @rdname MaternVolume
#' @export
Matern_1975_volume_Sweden_Beech <- function(
    DBH,
    height
){
  return(
    0.01275*(DBH^2)*height + 0.12368*(DBH^2) + 0.0004701*(DBH^2)*(height^2) + 0.00622*DBH*(height^2)
  )
}

