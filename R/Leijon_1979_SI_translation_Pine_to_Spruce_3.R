#' Height trajectories of Norway Spruce and Scots Pine on equal sites.
#'
#' @source Leijon, B. (1979) Tallen och Granens produktion på lika ståndort. Redovisning till SJFR. SLU, Inst. f. skogsskötsel, intern rapport 100pp.
#'
#' Function 7.6, p. 68.
#'
#' @details
#' Standard deviation, approximately reduced for observational error in H100: 0.0782.
#'
#'
#' Standard deviation of unlogarithmed residuals (dm) 25.8.
#'
#' @param SI_Pine SI Pine H100, meters by Hägglund 1974.
#' @param latitude Degrees N.
#' @param altitude Meters above sea level.
#' @param continental TRUE/FALSE Is the plot located in a locally continental climate. [forester::local_climate_sweden()]
#' @param dry TRUE/FALSE subsoil water depth >2 meters. SWE:"Torr" mark.
#' @param herbs TRUE/FALSE field strata of low- or rich-herbs. SWE: "Örttyper".
#'
#' @return SI Spruce H100, meters by Hägglund 1972,1973.
#'
#' @seealso
#' [forester::Leijon_1979_SI_translation_Pine_to_Spruce_1]
#' [forester::Leijon_1979_SI_translation_Pine_to_Spruce_2]
#' [forester::Leijon_1979_SI_translation_Spruce_to_Pine_1]
#' [forester::Leijon_1979_SI_translation_Spruce_to_Pine_2]
#' [forester::Leijon_1979_SI_translation_Spruce_to_Pine_3]
#'
#' @export
#'
#' @examples
Leijon_1979_SI_translation_Pine_to_Spruce_3 <- function(
  SI_Pine,
  latitude,
  altitude,
  continental,
  dry,
  herbs
){

  if(SI_Pine<12|SI_Pine>30){
    warning("SI H100 Pine Outside of material.")
  }

  return(
    exp(
      +0.005099*log(SI_Pine*10)+
        -1.5904*log(latitude)+
        -0.01122*(altitude/100)+
        -0.08112*continental+
        -0.08125*dry+
        +0.01447*herbs+
        +10.8370
    )/10
  )
}
