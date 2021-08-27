#' Brandel 1990 Volume for Birch
#'
#' @description Volume functions for Birch which are used in Heureka. PM Heureka 2004-01-20 Björn Elfving.
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
Brandel_1990_volume_Birch <- function(
  diameter_cm,
  height_m,
  latitude
){
  ifelse(latitude>60,
         return(
           10^(

             	-0.44224 + 	2.47580*log10(diameter_cm)	-1.40854*log10(diameter_cm+20) +	5.16863*log10(height_m)	-3.77147*log10(height_m-1.3)
           )
         ),
         return(
           10^(

            	-0.89359 +	2.27954*log10(diameter_cm) +		-1.18672*log10(diameter_cm+20) +	7.07362*log10(height_m)	-5.45175*log10(height_m-1.3)
           )
         )
  )
}
