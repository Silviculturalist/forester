#' Andersson 1954 volume for small trees, Birch
#'
#' @description Suitable for trees taller than 4 m.
#'
#' @source Andersson, S.-O. 1954. Funktioner och tabeller för kubering av småträd. Meddelanden från Statens Skogsforskningsinstitut 44:12, 29 s
#' Available: \url{https://www.skogskunskap.se/rakna-med-verktyg/mata-skogen/volymberakning/volymfunktioner/}
#'
#' @param diameter_cm Diameter in cm.
#' @param height_m Height in m.
#'
#' @return Volume in dm3.
#' @export
Andersson_1954_volume_small_trees_Birch_height_above_4_m <- function(
  diameter_cm,
  height_m
){
  return(
    exp(
      -4.49213 + 2.10253*log(diameter_cm) + 3.98519*log(height_m) - 2.65900*log(height_m-1.3) - 0.0140970*diameter_cm
    )
  )
}
#' Andersson 1954 volume for small trees, Birch
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
Andersson_1954_volume_small_trees_Birch_under_diameter_5_cm <- function(
  diameter_cm,
  height_m
){
  return(
    0.11 + 0.1302*(diameter_cm^2) + 0.01063*(diameter_cm^2)*height_m+ 0.007981*diameter_cm*(height_m^2)
  )
}
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
#' Andersson 1954 volume for small trees, Spruce.
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
Andersson_1954_volume_small_trees_Spruce <- function(
  diameter_cm,
  height_m
){
  return(
    0.22 + 0.1086*diameter_cm^2 + 0.01712*diameter_cm^2*height_m + 0.008905*diameter_cm*height_m^2
  )
}
