#' odin_tsum_1983
#'
#' @param lat Latitude in decimal degrees.
#' @param masl Metres above sea level
#'
#' @description Function 7 from Odin, Eriksson & Perttu (1983) Temperature Climate Maps for Swedish Forestry". Reports in Forest Ecology and Forest Soils 45. p. 45.
#'
#'  \emph{Tsum = 4835 - 57,6 x LAT - 0,9 x MASL}
#'
#' @return Returns the estimated annual temperature sum above 5 degrees celsius.
#' @export
#'
#' @examples
#' odin_tsum_1983(lat=68.88, masl=38)
odin_tsum_1983 <- function(lat_degree,lat_min=0, lat_sec=0, masl){

  lat <- lat_degree + (lat_min/60) + (lat_sec/3600)

  4835 - (57.6*lat) - (0.9*masl)
}
