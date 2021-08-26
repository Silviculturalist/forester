#' Geographic Site index H50 estimation for Pinus Contorta
#' @source Lindgren, D., Ying C., Elfving, B., Lindgren, K. (1994) Site Index Variation with Latitude and Altitude in IUFRO Pinus contorta Provenance Experiments in Western Canada and Northern Sweden". Scandinavian Journal of Forest Research. Vol. 9. Issue 1-4. Available: https://doi.org/10.1080/02827589409382840
#'
#'@description
#' \strong{Abstract:} Site index was calculated for 66 experimental plantations in western Canada and northern Sweden, containing a wide range of \emph{Pinus contorta} provenances drawn from seed lots belonging to the IUFRO international provenance testing program. Growth data from recommended provenances were used to calculate the site index. Site index was defined as predicted average height of the 100 largest trees by diameter per ha at age 50.
#'
#'Latitude and altitude explained 56\% of site index variation by fitting the function:
#'
#'\strong{Site Index (m) = 99.8 - 1.226 x Latitude (N°) - 0.01205 x Altitude (m)}
#'
#'The site index pattern indicated that forest productivity of the sites investigated decreased approximately by 0.8 m^3 / ha / yr per one-degree latitude northwards and per 100 m increase in altitude.
#'
#' @param Latitude Degrees N.
#' @param Altitude metres above sea level
#' @return Predicted SI H50 for P. contorta.
#' @export
#'
#' @examples
#' Lindgren_1994_IUFRO_contorta_SI_H50(Latitude=64.5, Altitude=150)

Lindgren_1994_IUFRO_contorta_SI_H50 <- function(Latitude, Altitude){

  SI <- (99.8 - (1.226 * Latitude) - (0.01205 * Altitude))

  return(SI)
}
