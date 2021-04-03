#' odin_tsum_1983
#'
#' @param latitude Latitude in decimal degrees.
#' @param altitude Metres above sea level
#'
#' @description Function 7 from Odin, Eriksson & Perttu (1983) Temperature Climate Maps for Swedish Forestry". Reports in Forest Ecology and Forest Soils 45. p. 45.
#'
#'  \emph{Tsum = 4835 - 57,6 x LAT - 0,9 x MASL}
#'
#' @return Returns the estimated annual temperature sum above 5 degrees celsius.
#' @export
#'
#' @examples
#' odin_tsum_1983(latitude=68.88, altitude=38)
odin_tsum_1983 <- function(latitude, altitude){
  4835 - (57.6*latitude) - (0.9*altitude)
}
