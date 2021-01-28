#' Geographic Site index estimation for Pinus Contorta
#' @description Provides interface for the site index function from "\emph{Site Index Variation with Latitude and Altitude in IUFRO Pinus contorta Provenance Experiments in Western Canada and Northern Sweden}" (1994),
#' by Dag Lindgren, Cheng C. Ying, Björn Elfving & Katarina Lindgren
#'
#' \strong{Abstract:} Site index was calculated for 66 experimental plantations in western Canada and northern Sweden, containing a wide range of \emph{Pinus contorta} provenances drawn from seed lots belonging to the IUFRO international provenance testing program. Growth data from recommended provenances were used to calculate the site index. Site index was defined as predicted average height of the 100 largest trees by diameter per ha at age 50.
#'
#'Latitude and altitude explained 56% of site index variation by fitting the function:
#'
#'\strong{Site Index (m) = 99.8 - 1.226 x Latitude (N°) - 0.01205 x Altitude (m)}
#'
#'The site index pattern indicated that forest productivity of the sites investigated decreased approximately by 0.8 m^3 / ha / yr per one-degree latitude northwards and per 100 m increase in altitude.
#'
#' @param Latitude Degrees N.
#' @param Altitude metres above sea level
#' @return Predicted SI for P. contorta.
#' @export
#'
#' @examples
#' p_contorta_IUFRO_si(Latitude=64.5, Altitude=150)

p_contorta_IUFRO_si <- function(Latitude, Altitude){

  SI <- (99.8 - (1.226 * Latitude) - (0.01205 * Altitude))

  return(SI)
}
