#' Tor Jonsson's smoothing of Maass' experience tables.
#' @source Jonsson, T. (1914) Om boniteringen av skogsmark, eng: On the
#' estimation of the productivity of forest land. Skogsvårdsföreningens
#' tidsskrift, nr. 5, p. 382.
#'
#' @description A smoothing of the experience tables from Maass on the at a
#' stand age thinned volume as a percentage of the total growth.
#'
#' @param stand_age years.
#'
#' @return Percent thinned of total volume.
#' @export
#'
#' @examples
jonsson_thinned_percentage_1914 <- function(stand_age){
  return(
    9*(stand_age-29)^(1/3)
  )
}
