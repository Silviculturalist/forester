#' Volume of the BA weighted mean tree for Norway Spruce in Norway from Braastad 1974
#'
#' @description Uses single tree volume functions from Vestjordet (1967).
#' Height is adjusted by the factor 0.967 such that the functions returned 'measured volume', cf. Eide 1923, as differed from volume estimates from means.
#'
#' @source Braastad, H. 1974. Produksjonstabeller og tilvekstmodeller for gran. Yield tables and Growth models for Picea abies. Meddr. norsk inst. Skogforsk. 31:9. p. 397. Code section: 1160,1162,1163.
#'
#' @param diameter_cm Quadratic mean diameter, cm.
#' @param height_m Lorey's mean height, m. e.g. [forester::Tveite_1967_Loreys_mean_height_Norway_Norway_Spruce()]
#'
#' @return Volume of the BA weighted mean tree (m3).
#' @export
Braastad_1974_QMD_tree_volume_Norway_Norway_Spruce <- function(
  diameter_cm,
  height_m
){

    height_m<- 0.967*height_m

    return(
    ifelse(diameter_cm<10,
           (0.52 + 0.02403*(diameter_cm^2)*height_m + 0.01463*diameter_cm*(height_m^2)-0.10983*(height_m^2) + 0.15195*diameter_cm*height_m)/1000,
           ifelse(diameter_cm<=13,
                  (-31.57+0.0016*diameter_cm*(height_m^2)+0.0186*(height_m^2)+0.63*diameter_cm*height_m - 2.34*height_m + 3.20*diameter_cm)/1000,
                  (10.14 + 0.0124*(diameter_cm^2)*height_m + 0.03117*diameter_cm*(height_m^2) - 0.36381*(height_m^2) + 0.28578*diameter_cm*height_m)/1000
                  )
           )
    )


}
