#' Brandel 1990 Volume for Norway Spruce
#'
#' @description Volume functions for Norway Spruce which are used in Heureka. PM Heureka 2004-01-20 Björn Elfving.
#'
#' @source Brandel, G. 1990. Volymfunktioner för enskilda träd. Tall, gran och björk. SLU, inst. för skogsproduktion. Rapport nr 26.
#'
#'
#' @source Available: \url{https://www.heurekaslu.org/w/images/9/93/Heureka_prognossystem_\%28Elfving_rapportutkast\%29.pdf}
#'
#' @param diameter_cm Diameter in cm
#' @param height_m Height, metres.
#' @param latitude Latitude
#'
#' @return Volume, in dm3.
#' @export
#'
#' @examples
Brandel_1990_volume_Spruce <- function(
  diameter_cm,
  height_m,
  latitude
){
  ifelse(latitude>60,
         return(
           10^(

             	-0.79783 + 	2.07157*log10(diameter_cm)	-0.73882*log10(diameter_cm+20) +	3.16332*log10(height_m)	-1.82622*log10(height_m-1.3)
           )
         ),
         return(
           10^(

             	-1.02039 +2.00128*log10(diameter_cm) +	-0.47473*log10(diameter_cm+20) +	2.87138*log10(height_m)	-1.61803*log10(height_m-1.3)
           )
         )
  )
}
