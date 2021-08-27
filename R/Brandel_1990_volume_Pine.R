#' Brandel 1990 Volume for Scots Pine
#'
#' @description Volume functions for Scots Pine which are used in Heureka. PM Heureka 2004-01-20 Björn Elfving.
#'
#' @source Brandel, G. 1990. Volymfunktioner för enskilda träd. Tall, gran och björk. SLU, inst. för skogsproduktion. Rapport nr 26.
#' @source Available: \url{https://www.heurekaslu.org/w/images/9/93/Heureka_prognossystem_%28Elfving_rapportutkast%29.pdf}
#'
#' @param diameter_cm Diameter in cm
#' @param height_m Height, metres.
#' @param latitude Latitude
#'
#' @return Volume, in dm3.
#' @export
#'
#' @examples
Brandel_1990_volume_Pine <- function(
  diameter_cm,
  height_m,
  latitude
){
  ifelse(latitude>60,
  return(
    10^(
      -1.20914 + 	1.94740*log10(diameter_cm)	-0.05947*log10(diameter_cm+20) +	1.40958*log10(height_m)	-0.45810*log10(height_m-1.3)
    )
  ),
  return(
    10^(
      -1.38903 +1.84493*log10(diameter_cm) +	0.06563*log10(diameter_cm+20) +	2.02122*log10(height_m)	-1.01095*log10(height_m-1.3)
    )
  )
  )
}
