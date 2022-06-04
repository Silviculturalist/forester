#' Height trajectories of Norway Spruce and Scots Pine on equal sites.
#'
#' @source Leijon, B. (1979) Tallen och Granens produktion på lika ståndort. Redovisning till SJFR. SLU, Inst. f. skogsskötsel, intern rapport 100pp.
#'
#' Funktion 7.5, p. 67.
#'
#' @details
#' Standard deviation, approximately reduced for observational error in H100: 0.0409.
#'
#'
#' Standard deviation of unlogarithmed residuals (dm) 15.9.
#'
#' @param SI_Spruce SI Spruce H100, meters by Hägglund 1972,1973.
#' @param latitude Degrees N.
#' @param altitude Meters above sea level.
#' @param continental TRUE/FALSE Is the plot located in a locally continental climate. [forester::local_climate_sweden()]
#' @param dry TRUE/FALSE subsoil water depth >2 meters. SWE:"Torr" mark.
#' @param herbs TRUE/FALSE field strata of low- or rich-herbs. SWE: "Örttyper".
#'
#' @return SI Pine H100, meters by Hägglund 1974.
#'
#' #' @seealso
#' [forester::Leijon_1979_SI_translation_Pine_to_Spruce_1]
#' [forester::Leijon_1979_SI_translation_Pine_to_Spruce_2]
#' [forester::Leijon_1979_SI_translation_Spruce_to_Pine_1]
#' [forester::Leijon_1979_SI_translation_Spruce_to_Pine_2]
#' [forester::Leijon_1979_SI_translation_Pine_to_Spruce_3]
#'
#' @export
Leijon_1979_SI_translation_Spruce_to_Pine_3 <- function(
  SI_Pine,
  latitude,
  altitude,
  continental,
  dry,
  herbs
){

  if(SI_Spruce<12|SI_Spruce>35){
    warning("SI H100 Spruce Outside of material.")
  }

  return(
    exp(
      +1.0612*log(SI_Spruce*10)+
      -0.002846*(SI_Spruce*10)+
      -0.002673*((((latitude-60)+(abs(latitude-60)))/2)^2)+
      -0.009990*((((altitude-200)+(abs(altitude-200)))/200)^2)+
      +0.04383*continental+
      +0.03433*dry+
      +0.05197*herbs+
      +0.3637
    )/10
  )
}
