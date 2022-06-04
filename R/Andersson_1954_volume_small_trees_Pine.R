#' Andersson 1954 volume for small trees, Pine.
#'
#' @description Suitable for diameters < 5 cm.
#'
#' @source Andersson, S.-O. 1954. Funktioner och tabeller för kubering av småträd. Meddelanden från Statens Skogsforskningsinstitut 44:12, 29 s
#' Available: \url{https://www.skogskunskap.se/rakna-med-verktyg/mata-skogen/volymberakning/volymfunktioner/}
#'
#' @param diameter_cm Diameter in cm.
#' @param height_m Height in m.
#'
#' @return Volume in dm3.
#' @export
Andersson_1954_volume_small_trees_Pine <- function(
  diameter_cm,
  height_m
){
  return(
    0.22 + 0.1066*(diameter_cm^2) + 0.02085*(diameter_cm^2)*height_m + 0.008427*diameter_cm*(height_m^2)
  )
}
